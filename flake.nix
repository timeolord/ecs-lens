{
  description = "ECS lens";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        compiled_package = pkgs.haskellPackages.developPackage {
          root = ./.;
        };
      in
      with pkgs;
      {
        overlays.default = (final: perv: { ecs-lens = compiled_package; });
        packages.default = compiled_package;
        devShells.default = mkShell {
          packages = [
            (haskellPackages.ghcWithPackages (
              pkgs: with pkgs; [
                cabal-install
              ]
            ))
          ];
        };
      }
    );
}
