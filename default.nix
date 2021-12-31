{ mkDerivation, base, blaze-colonnade, blaze-html, bytestring
, cassava, Chart, Chart-diagrams, colonnade, fmt, lib
, optparse-applicative, text, time
}:
mkDerivation {
  pname = "stockquotes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-colonnade blaze-html bytestring cassava Chart
    Chart-diagrams colonnade fmt optparse-applicative text time
  ];
  license = lib.licenses.mit;
}
