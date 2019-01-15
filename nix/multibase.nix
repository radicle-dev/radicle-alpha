{ mkDerivation, base, base16-bytestring, base58-bytestring
, base64-bytestring, bytestring, deepseq, doctest, Glob, hashable
, QuickCheck, sandi, stdenv, fetchgit
}:
mkDerivation {
  pname = "multibase";
  version = "0.1.0.0";
  src = fetchgit {
    url = "http://github.com/oscoin/ipfs";
    sha256 = "1pjf2hbp5qyda725wbra8yrjyzv6v0dnl0iw9idqq04jicz1x313";
    rev = "5627befa49dc756a690455f5bc4ee2993a0859eb";
  };
  libraryHaskellDepends = [
    base base16-bytestring base58-bytestring base64-bytestring
    bytestring deepseq hashable sandi
  ];
  testHaskellDepends = [ base doctest Glob QuickCheck ];
  homepage = "https://github.com/oscoin/ipfs";
  description = "Self-identifying base encodings, implementation of <https://github.com/multiformats/multihash>";
  license = stdenv.lib.licenses.bsd3;
}
