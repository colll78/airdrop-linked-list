{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/j-mueller/sc-tools"."e1f4359b6fb6a2b939bac0840e229193dd4f6b54" = "sha256-+dVtHGOQHvAjwOZZ0ZyHsQLF2roREUVIoMU31AD3YZs=";
    "https://github.com/Plutonomicon/plutarch-plutus"."e50661e24670974b398be19426617bc6389fdac6" = "sha256-BcqNHF4LCHwGs+Q+nPKNDOAPZwPvBhKDb7f3s/kkFho=";
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "airdrop-contracts";
    compiler-nix-name = "ghc966";
    index-state = "2024-06-17T12:18:52Z";
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project
