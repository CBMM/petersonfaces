{ reflex-platform, ... }:
let
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib   = (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     petersonfaces-common = (self.callPackage (reflex-platform.cabal2nixResult ../petersonfaces-common) {});
     servant              = (self.callPackage (reflex-platform.cabal2nixResult ../deps/tagging/deps/servant-snap/deps/servant/servant) {});
  };
}
