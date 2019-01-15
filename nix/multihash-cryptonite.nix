{ mkDerivation, base, binary, binary-varint, bytestring, Cabal
, cabal-doctest, cryptonite, deepseq, doctest, hashable, hedgehog
, memory, stdenv, fetchgit
}:
mkDerivation {
  pname = "multihash-cryptonite";
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
  } + "/multihash-cryptonite") ;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base binary binary-varint bytestring cryptonite deepseq hashable
    memory
  ];
  testHaskellDepends = [ base cryptonite doctest hedgehog ];
  homepage = "https://github.com/oscoin/ipfs";
  description = "Self-identifying hashes, implementation of <https://github.com/multiformats/multihash>";
  license = stdenv.lib.licenses.bsd3;
}
