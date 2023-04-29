### Dev Environment

This project uses VS Code's Dev Containers feature to provide nix, shell executables and language dependencies.

### Database

From the integrated terminal run `nix develop .#postgres` to start a nix shell that has postgres.
Inside the nix shell, run `devenv up` to start the database service.

### Template

Generated from https://github.com/srid/haskell-template/
