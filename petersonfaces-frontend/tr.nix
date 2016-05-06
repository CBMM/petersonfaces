{ reflex-platform, ... }:
let
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult deps/reflex-dom-contrib) {});
     tagging-common      = (self.callPackage deps/tagging/tagging-common/default.nix { compilername = "ghcjs"; });
     servant             = (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/servant-snap/deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/yaml-ghcjs) {});
     groundhog-th        = (self.callPackage deps/tagging/deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
  };
}
