let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-gce =
            haskellPackagesNew.callPackage ./haskell-gce.nix {
              zlib = pkgs.zlib.dev;
            };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
rec {
  project1 = pkgs.haskellPackages.callPackage ./haskell-gce.nix { };
}
