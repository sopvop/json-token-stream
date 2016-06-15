{ mkDerivation, base, bytestring, scientific, stdenv, tagged, text
}:
mkDerivation {
  pname = "json-token-stream";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring scientific tagged text ];
  description = "Json token stream encoding";
  license = stdenv.lib.licenses.bsd3;
}
