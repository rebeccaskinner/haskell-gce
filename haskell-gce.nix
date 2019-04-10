{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, hoauth2, http-client, http-client-tls, http-types, jwt, lens, mtl
, scientific, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "haskell-gce";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite hoauth2 http-client
    http-client-tls http-types jwt lens mtl scientific text time
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers cryptonite hoauth2 http-client
    http-client-tls http-types jwt lens mtl scientific text time
    transformers
  ];
  homepage = "https://github.com/rebeccaskinner/haskell-gce#readme";
  license = stdenv.lib.licenses.asl20;
}
