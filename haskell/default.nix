{ mkDerivation, aeson, aeson-pretty, base, bytestring, cassava
, containers, criterion, deepseq, genifunctors, hashable, hpack
, megaparsec, mtl, QuickCheck, sbv, Spock, stdenv, tasty, hoogle
, tasty-hspec, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, time, unicode-show, unordered-containers, vector, wai-extra
, parallel-io
}:
mkDerivation {
  pname = "vsat";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring cassava containers deepseq
    genifunctors hashable megaparsec mtl sbv Spock tasty-quickcheck
    text time unicode-show unordered-containers vector wai-extra
    hoogle
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring cassava containers deepseq
    genifunctors hashable megaparsec mtl sbv Spock tasty-quickcheck
    text time unicode-show unordered-containers vector wai-extra
    parallel-io
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring cassava containers deepseq
    genifunctors hashable megaparsec mtl QuickCheck sbv Spock tasty
    tasty-hspec tasty-hunit tasty-quickcheck tasty-smallcheck text time
    unicode-show unordered-containers vector wai-extra
  ];
  benchmarkHaskellDepends = [
    aeson aeson-pretty base bytestring cassava containers criterion
    deepseq genifunctors hashable megaparsec mtl sbv Spock
    tasty-quickcheck text time unicode-show unordered-containers vector
    wai-extra
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/doyougnu/vsat#readme";
  license = stdenv.lib.licenses.bsd3;
}
