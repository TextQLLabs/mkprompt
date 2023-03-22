{
  description = "MkPrompt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
  };

  outputs = { self, nixpkgs, nix-npm-buildpackage, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc92;

        packageName = "mkprompt";

        frontend = nix-npm-buildpackage.legacyPackages.x86_64-linux.buildNpmPackage 
            rec { 
              src = ./frontend; 
              installPhase = 
                ''
                    runHook preInstall
                    mkdir $out
                    cp -r ./build/* $out
                    runHook postInstall
                '';
              npmBuild = "npm run build"; 
          };

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

        mkDocker = args: pkgs.dockerTools.buildImage {
            name = "mkprompt";
            config = {
                Cmd = args;
                ExposedPorts = {
                    "4080/tcp" = {};
                };
            };
        };

      in {
        packages.${packageName} = pkg false;
        packages.dockerImage = mkDocker [
          "${pkg false}/bin/mkprompt"
          "--static-file-dir" "${frontend}"
        ];
        packages.frontend = frontend;
        devShell = pkg true;
      });
}
