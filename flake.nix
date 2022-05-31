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
        version = "0.69.0.0";

        drv =
          { lib, callPackage, mkDerivation, armadillo, base
          , criterion, hmatrix, config, pretty-simple }:
          mkDerivation {
            pname = "metodos";
            inherit version;
            license = lib.licenses.gpl3;

            src = ./.;

            isLibrary = true;
            isExecutable = true;
            doHaddock = false;
            doHoogle = false;

            libraryPkgconfigDepends = [ armadillo (callPackage metodospp {}) ];
            libraryHaskellDepends = [ base hmatrix ];
            executableHaskellDepends = [ base criterion hmatrix pretty-simple ];
          };

        metodospp = { lib, armadillo, cmake, stdenv }:
          stdenv.mkDerivation {
            pname = "metodospp";
            inherit version;

            src = ./Funciones_ANPI/c++;

            nativeBuildInputs = [ cmake ];
            buildInputs = [ armadillo ];
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
