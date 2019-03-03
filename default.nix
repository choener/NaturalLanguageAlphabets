{ mkDerivation, aeson, ansi-wl-pprint, base, binary, bytestring
, cereal, containers, criterion, deepseq, errors, file-embed
, hashable, hashtables, InternedData, lens, mtl, mwc-random
, parsers, QuickCheck, random, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-th, text, transformers, trifecta
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "NaturalLanguageAlphabets";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base bytestring containers errors file-embed
    hashable InternedData lens mtl parsers text transformers trifecta
    unordered-containers utf8-string
  ];
  testHaskellDepends = [
    aeson base binary cereal InternedData mtl QuickCheck tasty
    tasty-hunit tasty-quickcheck tasty-th text unordered-containers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq hashtables InternedData
    mwc-random random unordered-containers vector
  ];
  homepage = "https://github.com/choener/NaturalLanguageAlphabets";
  description = "Simple scoring schemes for word alignments";
  license = stdenv.lib.licenses.bsd3;
}
