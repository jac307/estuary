{
  reflexPlatformVersion ? "9e306f72ed0dbcdccce30a4ba0eb37aa03cf91e3",
  musl ? false,     # build with musl instead of glibc
  linkType ? null   # executable linking mode, null will build the closest to unconfigured for the current platform.
                    # 'static' will completely statically link everything.
                    # 'static-libs' will statically link the Haskell libs and dynamically link system. linux default.
                    # 'dynamic' will dynamically link everything. darwin default.
} @ args:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflexPlatformVersion}.tar.gz";
in

(import reflex-platform {
  nixpkgsFunc = args:
      let origPkgs = import "${reflex-platform}/nixpkgs" args;
      in if musl then origPkgs.pkgsMusl else origPkgs;
}).project ({ pkgs, ghc8_6, ... }:
with pkgs.haskell.lib;
let linkType = if (args.linkType or null) != null
  then args.linkType
  else
    if pkgs.stdenv.isDarwin then "static"
    else if pkgs.stdenv.isLinux then "static-libs"
    else "static-libs"; # fallback to static-libs which is closest to no intervention
in
{
  name = "Estuary";

  packages =
    let filter = name: type:
      pkgs.lib.cleanSourceFilter name type && (
        let baseName = baseNameOf (toString name);
        in !(
          (type == "directory" && (pkgs.lib.hasPrefix ".stack" baseName || baseName == "node_modules"))
          || (type == "file" && (baseName == "stack.yaml" || pkgs.lib.hasSuffix ".cabal" baseName))
        )
      );
    in {
      estuary = pkgs.lib.cleanSourceWith { inherit filter; src = ./client; };
      estuary-common = pkgs.lib.cleanSourceWith { inherit filter; src = ./common; };
      estuary-server = pkgs.lib.cleanSourceWith { inherit filter; src = ./server; };
    };

  shells = {
    ghc = ["estuary-common" "estuary-server"];
    ghcjs = ["estuary" "estuary-common"];
  };

  shellToolOverrides = ghc: super: {
    inherit (ghc8_6) hpack; # always use ghc (not ghcjs) compiled hpack
    python3 = pkgs.python3.withPackages (ps: with ps; [ pyyaml ]);
  };

  # A shell for staging and packaging releases
  passthru = {
    shells.release = pkgs.mkShell {
      buildInputs = (
        with pkgs;
        [closurecompiler gnumake gzip gcc]
      );
    };
  };

  overrides =
    let
      skipBrokenGhcjsTests = self: super:
        # generate an attribute set of
        #   {${name} = pkgs.haskell.lib.dontCheck (super.${name})}
        # if using ghcjs.
        pkgs.lib.genAttrs [
            "Glob" "mockery" "silently" "unliftio" "conduit"
            "yaml" "hpack" "base-compat-batteries" "text-show" "modular-arithmetic"
          ] (name: (if !(self.ghc.isGhcjs or false) then pkgs.lib.id else dontCheck) super.${name});
      # a hacky way of avoiding building unnecessary dependencies with GHCJS
      # (our system is currently building GHC dependencies even for the front-end...
      # ...this gets around that to allow a build on OS X
      disableServerDependenciesOnGhcjs = self: super:
        pkgs.lib.genAttrs [
            "foundation" "memory" "wai-app-static" "asn1-types"
            "asn1-encoding" "asn1-parse" "sqlite-simple" "cryptonite"
            "http-client" "pem" "x509" "connection" "tls" "http-client-tls"
            "hpack"
          ] (name: if (self.ghc.isGhcjs or false) then null else super.${name});

      manualOverrides = self: super: {
        estuary = dontCheck (overrideCabal (appendConfigureFlags super.estuary ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]) (drv: {
          preConfigure = ''
            ${ghc8_6.hpack}/bin/hpack --force;
          '';
          postInstall = ''
            ${pkgs.closurecompiler}/bin/closure-compiler $out/bin/Estuary.jsexe/all.js \
              --compilation_level=SIMPLE \
              --js_output_file=$out/bin/all.min.js \
              --externs=$out/bin/Estuary.jsexe/all.js.externs \
              --jscomp_off=checkVars;
            gzip -fk "$out/bin/all.min.js"
         '';
        }));

        estuary-common = overrideCabal super.estuary-common (drv: {
          preConfigure = ''
            ${ghc8_6.hpack}/bin/hpack --force;
          '';
        });

       estuary-server =
          let configure-flags = map (opt: "--ghc-option=${opt}") (
              []
              ++ (if !pkgs.stdenv.isLinux then [] else ({
                  static = [ "-optl=-pthread" "-optl=-static" "-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
                      "-optl=-L${pkgs.zlib.static}/lib" "-optl=-L${pkgs.glibc.static}/lib"
                    ];
                  dynamic = [ "-dynamic" "-threaded"];
                }.${linkType} or [])
              ) ++ (if !pkgs.stdenv.isDarwin then [] else ({
                  dynamic = [ "-dynamic" "-threaded"];
                }.${linkType} or [])
              )
          );
          in
          overrideCabal (appendConfigureFlags super.estuary-server configure-flags) (drv:
            ({
              dynamic = {
                  # based on fix from https://github.com/NixOS/nixpkgs/issues/26140, on linux when building a dynamic exe
                  # we need to strip a bad reference to the temporary build folder from the rpath.
                  preFixup = (drv.preFixup or "") + (
                    if !pkgs.stdenv.isLinux
                    then ""
                    else ''
                      NEW_RPATH=$(patchelf --print-rpath "$out/bin/EstuaryServer" | sed -re "s|/tmp/nix-build-estuary-server[^:]*:||g");
                      patchelf --set-rpath "$NEW_PATH" "$out/bin/EstuaryServer";
                    ''
                  );
                };
              static = {
                  enableSharedExecutables = false;
                  enableSharedLibraries = false;
                };
            }.${linkType} or {}) // {
              preConfigure = ''
                ${ghc8_6.hpack}/bin/hpack --force;
              '';
            }
        );

        text-show = dontCheck super.text-show;

        text-short = dontCheck super.text-short;

        criterion = dontCheck super.criterion;

        webdirt = import ./deps/webdirt self;

        timeNot = if !(self.ghc.isGhcjs or false) then null else dontHaddock
        #(self.callCabal2nix "timeNot" ../timeNot {});
        (self.callCabal2nix "TimeNot" (pkgs.fetchFromGitHub {
            owner = "afrancob";
            repo = "timeNot";
            sha256 = "1jq2gwszdcw6blgcfj60knpj0pj6yc330dwrvjzq8ys8rp17skvq";
            rev =  "c6c88bd9003afc0c69a651f4156a34e6593044bf";
          }) {});

        punctual = # dontHaddock (self.callCabal2nix "punctual" ../punctual {});
         dontHaddock (self.callCabal2nix "punctual" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "punctual";
          sha256 = "0n98jych5f440ccdkr21wszk14xsc6wim3kx4afj3xb4az498l5c";
          rev = "f2a27f13c306012dccb29c7534dc193e2bce536e";
        }) {});

        musicw = if !(self.ghc.isGhcjs or false) then null else dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "musicw";
          sha256 = "186gcyma9sldki7l0jgy80lzqyhlz6b248iqj4m645nzk4ncv2l3";
          rev = "ac0de845001430af98be168cf7478f55cd439274";
        }) {});

        reflex-dom-contrib = if !(self.ghc.isGhcjs or false) then null else dontHaddock (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
          sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
        }) {});

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions "tidal"
          ( pkgs.fetchgit {
          url = "https://github.com/TidalCycles/Tidal.git";
          sha256 = "1xr30j90yja0h9cxcyp7gm6b4jv4b4z5x1dqkabkaxvvxpjyxxwk";
          rev = "5a0ffd78e45761054b432ba51d33cd40b6c1f778";
          fetchSubmodules = true;
          }) "" {}));

        tidal-parse = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions
#            "tidal-parse" ../tidal/tidal-parse "" {});
            "tidal-parse"
            ( pkgs.fetchgit {
            url = "https://github.com/TidalCycles/Tidal.git";
            sha256 = "1xr30j90yja0h9cxcyp7gm6b4jv4b4z5x1dqkabkaxvvxpjyxxwk";
            rev = "5a0ffd78e45761054b432ba51d33cd40b6c1f778";
            fetchSubmodules = true;
              })
            "--subpath tidal-parse" {}));

        wai-websockets = dontCheck super.wai-websockets; # apparently necessary on OS X

        haskellish = # dontHaddock (self.callCabal2nix "haskellish" ../Haskellish {}); #
        dontHaddock (self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
           owner = "dktr0";
           repo = "Haskellish";
           sha256 = "1lrw14v4n5cdk7b8la9z4bc9sh8n0496hb4s7fcbm6g7p5m8qc0j";
           rev = "bd5daf365086a4b3a75af9ad9c0b6dedf687f48a";
        }) {});

        tempi = # dontHaddock (self.callCabal2nix "tempi" ../tempi {});
         dontHaddock (self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
           owner = "dktr0";
           repo = "tempi";
           sha256 = "0z4fjdnl7riivw77pl8wypw1a98av3nhpmw0z5g2a1q2kjja0sfp";
           rev = "9513df2ed323ebaff9b85b72215a1e726ede1e96";
        }) {});

        seis8s = #dontHaddock (self.callCabal2nix "seis8s" ../seis8s {});
          dontHaddock (self.callCabal2nix "seis8s" (pkgs.fetchFromGitHub {
           owner = "luisnavarrodelangel";
           repo = "seis8s";
           sha256 = "1wyrdzhpyrc7cj7w2cj0ld3x1kd6g6mwd2f70lrxazagalik4g2f";
           rev = "53e4976a17267341592a5583d42c4ee4e518955a";
         }) {});
      };
    in
      pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
        skipBrokenGhcjsTests disableServerDependenciesOnGhcjs manualOverrides
      ];
})
