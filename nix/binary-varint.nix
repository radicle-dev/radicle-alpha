{ mkDerivation, base, binary, stdenv, fetchgit }:
mkDerivation {
  pname = "binary-varint";
  version = "0.1.0.0";
  src = fetchgit {
    url = "http://github.com/oscoin/ipfs";
    sha256 = "1pjf2hbp5qyda725wbra8yrjyzv6v0dnl0iw9idqq04jicz1x313";
    rev = "5627befa49dc756a690455f5bc4ee2993a0859eb";
  };
  libraryHaskellDepends = [ base binary ];
  homepage = "https://github.com/oscoin/ipfs";
  description = "VarInt encoding/decoding via Data.Binary";
  license = stdenv.lib.licenses.bsd3;
}
