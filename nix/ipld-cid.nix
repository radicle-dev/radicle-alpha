{ mkDerivation, base, binary, binary-varint, bytestring, cryptonite
, deepseq, hashable, hedgehog, multibase, multihash-cryptonite
, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "ipld-cid";
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
  } + "/ipld-cid") ;
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
