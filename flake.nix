{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = { flake-parts, ... }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { pkgs, ... }:
      let
        opkgs = pkgs.ocaml-ng.ocamlPackages_5_2;
      in
      {
        packages.default = opkgs.buildDunePackage {
          pname = "eociaml";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
        };

        devShells.default = pkgs.mkShell {
          packages = with opkgs; [
            ocaml
            dune_3
            ocaml-lsp
            ocamlformat
            utop
            odoc
            alcotest
          ] ++ (with pkgs; [
            slipshow
          ]);
        };
      };
  };
}
