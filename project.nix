{ mkDerivation, aeson, base, bytestring, containers, either
, megaparsec, modern-uri, replace-megaparsec, req, safe-exceptions
, stdenv, text, time
}:
mkDerivation {
  pname = "download-keys";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers either megaparsec modern-uri
    replace-megaparsec req safe-exceptions text time
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
