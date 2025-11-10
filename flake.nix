{
  description = "Melodiλ - YouTube to Audio Telegram Bot in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Build with stack
        melodilambda = pkgs.stdenv.mkDerivation {
          pname = "melodilambda";
          version = "0.1.0";
          src = ./.;
          
          nativeBuildInputs = with pkgs; [
            stack
            pkg-config
          ];
          
          buildInputs = with pkgs; [
            zlib
            zlib.dev
            gmp
            gmp.dev
          ];
          
          buildPhase = ''
            export STACK_ROOT=$TMPDIR/stack
            stack setup --system-ghc
            stack build --system-ghc
          '';
          
          installPhase = ''
            mkdir -p $out/bin
            stack install --system-ghc --local-bin-path $out/bin
          '';
        };

        # Runtime dependencies
        runtimeDeps = with pkgs; [
          yt-dlp
          ffmpeg
        ];

      in {
        packages = {
          default = melodilambda;
          melodilambda = melodilambda;
          
          # Docker image
          docker = pkgs.dockerTools.buildLayeredImage {
            name = "melodilambda";
            tag = "latest";
            contents = [ melodilambda ] ++ runtimeDeps;
            config = {
              Cmd = [ "${melodilambda}/bin/melodilambda" ];
              Env = [
                "LANG=C.UTF-8"
              ];
            };
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            stack
            ghc
            zlib
            zlib.dev
            gmp
            gmp.dev
            haskell-language-server
            hlint
            ormolu
            yt-dlp
            ffmpeg
            pkg-config
          ];
          
          # Ensure libraries are found
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.zlib
            pkgs.gmp
          ];
          
          PKG_CONFIG_PATH = "${pkgs.zlib.dev}/lib/pkgconfig:${pkgs.gmp.dev}/lib/pkgconfig";
          
          shellHook = ''
            echo "🎵 Melodiλ Development Environment"
            echo "=================================="
            echo "Stack: $(stack --version)"
            echo "GHC: $(ghc --version)"
            echo ""
            echo "Commands:"
            echo "  stack build  - Build the project"
            echo "  stack run    - Run the bot"
            echo "  stack test   - Run tests"
          '';
        };

        apps.default = {
          type = "app";
          program = "${pkgs.writeShellScript "run-melodilambda" ''
            cd ${./.}
            exec ${melodilambda}/bin/melodilambda
          ''}";
        };
      }
    );
}
