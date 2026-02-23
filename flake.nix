{
  description = "Better API workspace";

  nixConfig = {
    keep-going = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      fenix,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ fenix.overlays.default ];
        };

        inherit (pkgs) lib;

        craneLib = (crane.mkLib pkgs).overrideToolchain (p: p.fenix.stable.toolchain);

        src = craneLib.cleanCargoSource ./.;

        # For tests we also have to include snap files
        testSrc = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            (craneLib.fileset.commonCargoSources ./.)
            (lib.fileset.fileFilter (file: file.hasExt "snap") ./.)
          ];
        };

        commonArgs = {
          inherit src;
          strictDeps = true;
        };

        # Build just cargo dependencies for reuse.
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        individualCrateArgs = commonArgs // {
          inherit cargoArtifacts;
          inherit (craneLib.crateNameFromCargoToml { inherit src; }) version;

          # We run tests for the whole workspace
          doCheck = false;
        };

        better-api-diagnostic = craneLib.buildPackage (
          individualCrateArgs
          // {
            pname = "better-api-diagnostic";
            cargoExtraArgs = "-p better-api-diagnostic";
          }
        );

        better-api-syntax = craneLib.buildPackage (
          individualCrateArgs
          // {
            pname = "better-api-syntax";
            cargoExtraArgs = "-p better-api-syntax";
          }
        );

        better-api-semantics = craneLib.buildPackage (
          individualCrateArgs
          // {
            pname = "better-api-semantics";
            cargoExtraArgs = "-p better-api-semantics";
          }
        );
      in
      {
        checks = {
          inherit better-api-diagnostic better-api-syntax better-api-semantics;

          better-api-clippy = craneLib.cargoClippy (
            commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets --all-features -- --D warnings";
            }
          );

          # Check formatting
          better-api-fmt = craneLib.cargoFmt {
            inherit src;
          };

          better-api-toml-fmt = craneLib.taploFmt {
            src = pkgs.lib.sources.sourceFilesBySuffices src [ ".toml" ];
          };

          better-api-nextest = craneLib.cargoNextest (
            commonArgs
            // {
              inherit cargoArtifacts;
              src = testSrc;
            }
          );
        };

        devShells.default = craneLib.devShell {
          checks = self.checks.${system};

          packages = [
            pkgs.rust-analyzer-nightly
          ];
        };

        formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      }
    );
}
