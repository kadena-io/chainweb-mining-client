{ compiler ? "ghc8107"
, rev      ? "7a94fcdda304d143f9a40006c033d7e190311b54"
, sha256   ? "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false; # autodocodec
      config.allowUnfree = true;
    }
, mkDerivation ? null
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
      sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
    }) {};
in
pkgs.haskell.packages.${compiler}.developPackage {
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib; {
    configuration-tools = self.callHackageDirect {
      pkg = "configuration-tools";
      ver = "0.6.1";
      sha256 = "0vrml1gj6bb5f6x79m80k9wqn5qvjjzz8c6gf36mqwdqv30irxdv";
    } {};

    streaming-events = doJailbreak (self.callHackageDirect {
      pkg = "streaming-events";
      ver = "1.0.1";
      sha256 = "11v9rrhvlxlq43m5pw63hdfn6n0fkqryphvplild1y920db96wk9";
    } {});

    autodocodec = markUnbroken super.autodocodec;
    validity-aeson = markUnbroken super.validity-aeson;
  };
  source-overrides = {
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
