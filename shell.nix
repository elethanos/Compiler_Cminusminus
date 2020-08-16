# In shell.nix. Also integrate it with direnv:
# $ echo "use nix" > .envrc
# $ direnv allow .
{ pkgs ? import <nixpkgs> {} }:
with pkgs;
pkgs.mkShell {
  buildInputs = [ ocaml nasm gdb ddd ];
}

