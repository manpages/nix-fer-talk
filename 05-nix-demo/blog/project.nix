{ mkDerivation, base, containers, directory, extensible-exceptions
, filepath, mtl, process, stdenv, text, time, turtle, unix
}:
mkDerivation {
  pname = "blog";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory extensible-exceptions filepath mtl
    process text time turtle unix
  ];
  executableHaskellDepends = [
    base containers directory extensible-exceptions filepath mtl
    process text time turtle unix
  ];
  description = "CLI to instantiate/publish hakyll posts";
  license = stdenv.lib.licenses.bsd2;
}
