{
  description = "Experimental, user-contributed effects and interpreters for polysemy";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/7e63eed145566cca98158613f3700515b4009ce3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    inherit (nixpkgs.lib) mapAttrs' nameValuePair composeManyExtensions versionAtLeast optionalAttrs;

    pkgs = import nixpkgs { inherit system; };

    src = pkgs.nix-gitignore.gitignoreSourcePure [./nix/source-filter] ./.;

    basic = self: super: optionalAttrs (versionAtLeast super.ghc.version "9.4") {
      ghcid = pkgs.haskell.lib.dontCheck super.ghcid;
    } // {
      polysemy-zoo = self.callCabal2nix "polysemy-zoo" src {};
    };

    hsPkgs = compiler: overrides: pkgs.haskell.packages.${compiler}.override {
      overrides = composeManyExtensions ([basic] ++ overrides);
    };

    ghcs = {
      "810" = hsPkgs "ghc810" [];
      "90" = hsPkgs "ghc90" [];
      "92" = hsPkgs "ghc92" [];
      "94" = hsPkgs "ghc94" [];
      default = hsPkgs "ghc94" [(self: super: let
        hackage = pkg: ver: sha256: self.callHackageDirect { inherit pkg ver sha256; } {};
      in {
        polysemy = hackage "polysemy" "1.9.1.0" "05mhzjz6hz0dnxsn3cc0l6yyj5ch35gn8xfnx0a1gn3q8yljfg2a";
        polysemy-plugin = hackage "polysemy-plugin" "0.4.5.0" "0v2k0l42zaangwv050xfv5jdqfrbvdxfr533291ndsxalv8n3xi8";
      })];
    };

    package = version: ghc: nameValuePair "polysemy-zoo-${version}" ghc.polysemy-zoo;

    packages = { inherit (ghcs.default) polysemy-zoo; default = ghcs.default.polysemy-zoo; } // mapAttrs' package ghcs;

    devShell = ghc: ghc.shellFor {
      packages = p: [p.polysemy-zoo];
      buildInputs = with ghc; [cabal-install ghcid haskell-language-server];
      withHoogle = true;
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (devShell g)) ghcs // {
      default = devShell ghcs.default;
    };

  in {
    inherit packages devShells;
    checks = packages;
  });
}
