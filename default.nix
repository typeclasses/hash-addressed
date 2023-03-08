let

sources = import ./nix/sources.nix;
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    hash-addressed = ./hash-addressed;
};

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            gambler = new.callPackage ./nix/gambler-0.1.0.0.nix {};
            quaalude = new.callPackage ./nix/quaalude.nix {};
        })
    ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            gambler = new.callPackage ./nix/gambler-0.3.0.0.nix {};
            quaalude = new.callPackage ./nix/quaalude.nix {};
        })
    ];
});

in

symlinkJoin {
    name = "hash-addressed";
    paths = concatMap (x: [x.hash-addressed]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
