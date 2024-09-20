{ repoRoot, inputs, pkgs, lib, system }:
let
  project = repoRoot.nix.project;
in
[
  (
    project.flake // {
      checks = {
        plutarch-import-test = pkgs.runCommand "plutarch-import-test" {} ''
          if [ -d ${pkgs.plutarch} ]; then
            echo "Plutarch found at ${pkgs.plutarch}"
            touch $out
          else
            echo "Plutarch not found"
            exit 1
          fi
        '';
      };
    }
  )
]