{ mkDerivation, base, binary, stdenv, fetchgit }:
mkDerivation {
  pname = "binary-varint";
  version = "0.1.0.0";
  src = (fetchgit {
    url = "http://github.com/oscoin/ipfs";
    sha256 = "0vy5a4h5bpyvn6i59gj0h94s9nmwyn3qgy3bs09f252axfnc3vpk";
    rev = "5627befa49dc756a690455f5bc4ee2993a0859eb";
    postFetch = ''
      cd $out
      for l in binary-varint ipld-cid multibase multihash-cryptonite; do
        rm "$l"/LICENSE
        cp LICENSE "$l"/LICENSE
      done'';
  } + "/binary-varint") ;
  libraryHaskellDepends = [ base binary ];
  homepage = "https://github.com/oscoin/ipfs";
  description = "VarInt encoding/decoding via Data.Binary";
  license = stdenv.lib.licenses.bsd3;
}
