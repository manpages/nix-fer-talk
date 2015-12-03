{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Blog (Blog, post, stamp, defaultPathFn, knownCategory, rsyncTup, rsyncSend,
             TextText, TextTexts, BlogCat, BlogCfg(..), Path, PathGenerator) where

import           Control.Monad.Reader
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Text            (pack, unpack)
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           System.FilePath      (takeBaseName)
import           Turtle               hiding (f, x)

-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Cmd         = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type File        = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Remote      = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Path        = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type BlogCat     = Text
-- | Text -> Text configuration part
type TextText    = Map Text Text
-- | Text -> [Text] configuration part
type TextTexts   = Map Text [Text]
-- | Function that generates path for blog post
type PathGenerator t = FormatTime t => BlogCat -> Path -> Path -> t -> Path
-- | Configuration to be asked from Reader
data BlogCfg = BlogCfg { connection :: TextText      -- ^ Describes how to connect to the blog, for now specifies rsync args
                       , blog_spec  :: TextTexts     -- ^ Describes the categories (and, later, tags) allowed in the blog
                       , pathFn     :: forall t . PathGenerator t -- ^ Builds remote path based on category, local path and remote base path
                       }

-- |
-- Blog connectivity is configured with this type.
-- Keys are other types from this file as Text,
-- values are Text.
type Blog = ReaderT BlogCfg IO ExitCode

-- |
-- With BlogCfg run ReaderT
post :: BlogCfg -> Path -> BlogCat -> IO ExitCode
post x y z = runReaderT (postR y z) x

-- |
-- Based on our ReaderT, posts an entry to the blog, if category is ok
postR :: Path -> BlogCat -> Blog
postR thing category = do
  (BlogCfg { connection = con
           , blog_spec  = spec
           , pathFn     = f}) <- ask
  let (cmd, remote, pth) = rsyncTup con
  liftIO $ putStrLn $ show (category, spec)
  tau <- liftIO $ getCurrentTime
  if knownCategory category spec
    then liftIO $ rsyncSend cmd thing remote $ f category thing pth tau
    else return $ ExitFailure 5

-- | Default path function (works with my blog :))
defaultPathFn :: PathGenerator t
defaultPathFn "draft"  l r t = r <^> slash "drafts"                    <^> (slash $ stamp (bn l) t)
defaultPathFn category l r t = r <^> slash "posts"  <^> slash category <^> (slash $ stamp (bn l) t)

-- | Take basename of a filePath represented as Text
bn :: Text -> Text
bn = pack . takeBaseName . unpack

-- | Prefix a filename with a default hakyll timestamp
stamp :: FormatTime t => Text -> t -> Text
stamp x tau = (pack $ formatTime defaultTimeLocale "%Y-%m-%d" tau) <^> dash x

-- | Rsync a file using Turtle's ``shell`` function.
rsyncSend :: Cmd -> File -> Remote -> Path -> IO ExitCode
rsyncSend ssh file remote path =
  she $ "rsync -Pave" <^> q ssh <^> sq file <^> spc remote <^> cq path <^> ".markdown"

-- | Looks into blog specification and tells if argument is a known category
knownCategory :: BlogCat -> TextTexts -> Bool
knownCategory x spec = x `elem` xs
  where
    xs = fromJust $ M.lookup "BlogCat" spec

-- | Synonim for ``Text.append``
(<^>) :: Text -> Text -> Text
(<^>) = T.append

-- | Quote a string (place it into double  quotes)
q :: Text -> Text
q = T.append "\"" . flip T.append "\""

-- | Prefix a string with a slash
slash :: Text -> Text
slash = T.append "/"

-- | Prefix a string with a dash
dash :: Text -> Text
dash = T.append "-"

-- | Prefix a string with a space
spc :: Text -> Text
spc = T.append " "

-- | Prefix a string with a space and quote it.
sq :: Text -> Text
sq = spc . q

-- | Prefix a string with a colon.
c :: Text -> Text
c = T.append ":"

-- | Prefix a string with a colon and quote it.
cq :: Text -> Text
cq = c . q

-- | Shorthand for executing a command under empty shell.
she :: Text -> IO ExitCode
she = (flip shell) empty

-- | Get rsync configuration out of our map
rsyncTup :: Map Text Text -> (Cmd, Remote, Path)
rsyncTup x = (f "Cmd", f "Remote", f "Path")
  where
    f k = fromJust $ M.lookup k x
