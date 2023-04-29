# Usage:
#
# With nix installed, navigate to the directory containing this flake and run
# `nix develop --impure`. The `--impure` is necessary in order to store state
# locally from "services", such as PostgreSQL.
{
  description = "Local library app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, devenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
        inputs.devenv.flakeModule
      ];
      perSystem = { self', system, lib, config, inputs', pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # packages.local-library.root = ./.; # Auto-discovered by haskell-flake
          overrides = self: super: { };
          devShell = {
            tools = hp: {
              inherit (hp) haskell-debug-adapter ghci-dap;
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
            hlsCheck.enable = true;
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          flakeFormatter = false; # For https://github.com/numtide/treefmt-nix/issues/55

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          programs.prettier.enable = true;
          settings.formatter.prettier =
            {
              options = [ "--write" ];
              includes = [ "*.yaml" ];
            };

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Dev shell scripts.
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:server" --warnings -T :main
            '';
            category = "Primary";
          };
          test = {
            description = "Run all tests";
            exec = ''
              ghcid -c "cabal repl test:tests" -T :main
            '';
            category = "Primary";
          };
        };

        # Default package.
        packages.default = pkgs.haskell.lib.justStaticExecutables self'.packages.local-library;

        # Default shell.
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
        };

        # Shell using devenv that comes with postgres
        devenv.shells.postgres = {
          # https://devenv.sh/reference/options/

          name = "Local library app";

          # Make use of the Starship command prompt when this development environment
          # is manually activated (via `nix develop --impure`).
          # See https://starship.rs/ for details on the prompt itself.
          starship.enable = true;

          difftastic.enable = true;

          packages = [ pkgs.hello ];

          # The postgres service we need to get running.
          # Configuration options are documented at https://devenv.sh/reference/options/#servicespostgressettings
          services.postgres.enable = true;

          # On the first invocation of `devenv up`, create a database for
          # our app to store data in.
          services.postgres.initdbArgs = [ "--locale=C" "--encoding=UTF8" ];
          services.postgres.initialDatabases = [
            {
              name = "local_library";
              schema = ./schema.sql;
            }
          ];
          services.postgres.listen_addresses = "127.0.0.1";
          # Create a postgres user called 'chief_librarian' which has ownership
          # over the 'local_library' database.
          services.postgres.initialScript = ''
            CREATE USER postgres SUPERUSER;
            CREATE USER chief_librarian;
            ALTER DATABASE local_library OWNER TO chief_librarian;
          '';

          services.adminer.enable = true;
        };
      };
    };
}
