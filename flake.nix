{
  description = "metodos";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib; eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };
    in with pkgs.lib; {
      defaultPackage = self.packages."${system}".metodos;
      defaultApp = self.apps."${system}".run_methods;

      packages.metodos = let
        drv = { mkDerivation, base, hmatrix, lib, old-time }:
          mkDerivation {
            pname = "metodos";
            version = "0.69.0.0";
            license = lib.licenses.gpl3;

            src = ./.;

            isLibrary = true;
            isExecutable = true;

            libraryHaskellDepends = [ hmatrix ];
            executableHaskellDepends = [ base old-time ];
          };
      in pkgs.haskellPackages.callPackage drv {};

      apps = let
        app = script: flake-utils.lib.mkApp {
          drv = self.defaultPackage."${system}";
          exePath = "/bin/${script}";
        };

        apps = [ "run_methods" "run_example" ];
      in listToAttrs (forEach apps (name: {
        inherit name;
        value = app name;
      }));
    });
}
