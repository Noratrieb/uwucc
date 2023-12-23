{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell
{
  buildInputs = with pkgs; [
    clang
    llvmPackages_16.bintools
    rustup
    cargo-insta
    gdb
  ];

  shellHook = ''
    export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
    export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
  '';
}
