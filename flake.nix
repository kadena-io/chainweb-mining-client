{
  description = "Chainweb Mining Client";
  inputs = {
    hs-nix-infra.url = "github:kadena-io/hs-nix-infra/enis/metadata-experiments";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, hs-nix-infra, flake-utils,  }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      inherit (hs-nix-infra) nixpkgs haskellNix;
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          chainweb-mining-client =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      project = pkgs.chainweb-mining-client;
      flake = project.flake {};
    in {
      # Built by `nix build .`
      packages.default = flake.packages."chainweb-mining-client:exe:chainweb-mining-client";
      packages.recursive = with hs-nix-infra.lib.recursive system;
        wrapRecursiveWithMeta "chainweb-mining-client" "${wrapFlake self}.default";

      devShell = flake.devShell;

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
