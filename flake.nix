{
  description = "Chainweb Mining Client";
  inputs = {
    hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
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
      flake = pkgs.chainweb-mining-client.flake {};
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."chainweb-mining-client:exe:chainweb-mining-client";
    });
}
