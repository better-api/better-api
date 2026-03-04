{
  description = "Better API workspace";

  nixConfig = {
    keep-going = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      rust-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        inherit (pkgs) lib;

        craneLib = (crane.mkLib pkgs).overrideToolchain (
          p:
          p.rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rust-analyzer"
              "llvm-tools"
            ];
          }
        );

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

        packages.test-coverage = craneLib.cargoNextest (
          commonArgs
          // {
            inherit cargoArtifacts;
            src = testSrc;

            withLlvmCov = true;
            cargoLlvmCovExtraArgs = "--workspace --html --output-dir $out";
            LLVM_COV_FLAGS = "-coverage-watermark=80,50";
          }
        );

        devShells.default = craneLib.devShell {
          checks = self.checks.${system};

          packages = [
            pkgs.cargo-insta
          ];
        };

        apps.test-coverage =
          let
            isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
            openCmd = if isDarwin then "open" else "xdg-open";

            preview = pkgs.writeShellApplication {
              name = "test-coverage";

              runtimeInputs = [
                pkgs.nix
              ]
              ++ (lib.optional (!isDarwin) pkgs.xdg-utils);

              text = ''
                out="$(nix build ${self}#test-coverage --no-link --print-out-paths)"
                exec ${openCmd} "file://$out/html/index.html"
              '';
            };
          in
          {
            type = "app";
            program = "${preview}/bin/test-coverage";
          };

        formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      }
    );
}
