{
  pkgs ? import <nixpkgs> { },
}:
let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5;

  gitHooks = import (
    pkgs.fetchFromGitHub {
      owner = "cachix";
      repo = "git-hooks.nix";
      rev = "1ba1dbfe28738c9a71c266be6efa5b89e75491a9";
      hash = "sha256-VAhpCOmrun/KXimYfR8NM88bciKCui7CqHHhThSzgag=";
    }
  );

  preCommitCheck = gitHooks.run {
    src = ./.;
    package = pkgs.prek;
    hooks = {
      dune-fmt = {
        enable = true;
        settings.extraRuntimeInputs = [ ocamlPackages.ocamlformat ];
      };
      nixfmt.enable = true;
      check-merge-conflicts.enable = true;
      end-of-file-fixer.enable = true;
      trim-trailing-whitespace.enable = true;
    };
  };
in
pkgs.mkShell {
  shellHook = preCommitCheck.shellHook + ''
    export OPAMROOT="${toString ./.}/.opam"
    if [ -d "${toString ./.}/_opam" ]; then
      eval "$(opam env --switch="${toString ./.}" --set-switch)"
    fi
  '';

  packages =
    (with ocamlPackages; [
      ocaml
      dune_3
      ocamlformat
      utop
    ])
    ++ (with pkgs; [
      opam
      gmp
      pkg-config
      pkgconf
      zlib
    ])
    ++ preCommitCheck.enabledPackages;
}
