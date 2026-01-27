{
  description = "Melodiλ - YouTube to Audio Telegram Bot in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-stable, flake-utils }:
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

        # Windows Cross-Compilation
        windows-exe = let
          # Use stable nixpkgs for cross-compilation to avoid unstable breakages
          pkgsCross = import nixpkgs-stable {
            inherit system;
            crossSystem = pkgs.lib.systems.examples.mingwW64;
          };
          
          # Use default GHC from stable (usually 9.6 or 9.4 which works well)
          haskellPackages = pkgsCross.haskellPackages.override {
            overrides = self: super: {
              # Add any specific overrides here if dependencies fail
            };
          };
        in haskellPackages.callCabal2nix "melodilambda" ./. { };

        # Runtime dependencies
        runtimeDeps = with pkgs; [
          yt-dlp
          ffmpeg
        ];

      in {
        packages = {
          default = melodilambda;
          melodilambda = melodilambda;
          windows = windows-exe;
          
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
