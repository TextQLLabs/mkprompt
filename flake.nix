{
  description = "TextQL demo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc92;

        packageName = "helios";
        pkg = isDev: haskellPackages.developPackage { 
            root = ./backend; 
            returnShellEnv = isDev;
            modifier = drv: if isDev
                then pkgs.haskell.lib.addBuildTools drv (with haskellPackages;
                    [ cabal-install
                      pkgs.nodejs
                      ormolu
                      pkgs.postgresql
                    ])
                else drv;
            };
        staticFiles = pkgs.runCommand "staticFiles"
            {
                preferLocalBuild = true;
                buildInputs = [];
            }
            ''
                mkdir $out
                cp -r ${./static}/* $out
            '';

        mkDocker = args: pkgs.dockerTools.buildImage {
            name = "api";
            copyToRoot = [pkgs.cacert];
            config = {
                Cmd = args;
                ExposedPorts = {
                    "3009/tcp" = {};
                };
            };
        };

      in {
        packages.${packageName} = pkg false;
        packages.dockerImage = mkDocker [];
        devShell = pkg true;
      });
}
