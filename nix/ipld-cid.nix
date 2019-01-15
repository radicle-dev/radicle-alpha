{ mkDerivation, base, binary, binary-varint, bytestring, cryptonite
, deepseq, hashable, hedgehog, multibase, multihash-cryptonite
, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "ipld-cid";
  version = "0.1.0.0";
  src = fetchgit {
    url = "http://github.com/oscoin/ipfs";
    sha256 = "1pjf2hbp5qyda725wbra8yrjyzv6v0dnl0iw9idqq04jicz1x313";
    rev = "5627befa49dc756a690455f5bc4ee2993a0859eb";
  };
  libraryHaskellDepends = [
    base binary binary-varint bytestring cryptonite deepseq hashable
    multibase multihash-cryptonite text
  ];
  testHaskellDepends = [
    base bytestring cryptonite hedgehog multibase multihash-cryptonite
    text
  ];
  homepage = "https://github.com/oscoin/ipfs";
  description = "IPLD Content-IDentifiers <https://github.com/ipld/cid>";
  license = stdenv.lib.licenses.bsd3;
}
