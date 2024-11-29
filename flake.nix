{
  description = "crem-journal";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = {self, nixpkgs, flake-utils, nix-filter}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # helper function to package flagged as broken in nixpkgs
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        pkgs = nixpkgs.legacyPackages.${system};

        # crem version to use
        cremSrc = pkgs.fetchFromGitHub {
          owner = "marcosh";
          repo = "crem";
          rev = "b2a7fd79ad96267e934261b5b9f05aa81f23da1c";
          sha256 = "sha256-FtX4s1dd3WwgWdRyoARQQzSOjFiu80ea/g+LT7Hp/V0=";
        };

        # override packages with my specific version of crem
        haskellPackages = pkgs.haskellPackages.override
          {
            overrides = hself: hsuper: rec {
              crem = jailbreakUnbreak (hsuper.callCabal2nix "crem" cremSrc {});
            };
          };

        packageName = "crem-journal";

        # folders to look at to know when recompiling
        src = nix-filter.lib {
          root = ./.;
          include =
            [
              "app"
              "spec"
              "src"
              "package.yaml"
              "cabal.project"
            ];
        };
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName src rec {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
            fourmolu
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}