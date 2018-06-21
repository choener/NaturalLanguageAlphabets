with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet
    =  (import ../Lib-InternedData).hsSrcSet
    // {NaturalLanguageAlphabets = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.NaturalLanguageAlphabets ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      DPutils
      OrderedBits
    ];
  };
}
