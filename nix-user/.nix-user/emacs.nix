{pkgs ? import <nixpkgs> {} }:
with pkgs;
let

  custom-emacs-platform =
    emacsPackagesNg.override (super: self: {
      emacs = emacs25;

      # Package set overrides
      monokai-theme = self.melpaPackages.monokai-theme;
      linum-relative = self.melpaPackages.linum-relative;
      
      evil = self.melpaPackages.evil;
      undo-tree = self.melpaPackages.undo-tree;
      evil-leader = self.melpaPackages.evil-leader;

      intero = self.melpaPackages.intero;
    }); 

  base-emacs = custom-emacs-platform.emacsWithPackages (epkgs: [
  epkgs.use-package
  epkgs.monokai-theme
	epkgs.smartparens
	epkgs.linum-relative

	epkgs.helm
	epkgs.helm-projectile
	epkgs.company
	epkgs.avy
	epkgs.yasnippet
	epkgs.projectile
	epkgs.magit
	epkgs.flycheck
	epkgs.git-gutter
	epkgs.emamux
	epkgs.multiple-cursors

	epkgs.hl-sexp

	epkgs.nixos-options
	epkgs.nix-sandbox
	epkgs.nix-mode

	epkgs.yaml-mode

	epkgs.ggtags
	epkgs.helm-gtags

	epkgs.haskell-mode
	epkgs.intero

	epkgs.elpy

	epkgs.rust-mode
	epkgs.cargo
	epkgs.racer
	epkgs.flycheck-rust

  epkgs.elm-mode

	epkgs.ess
	epkgs.js3-mode
	epkgs.tide

	epkgs.spaceline
  epkgs.evil
  epkgs.evil-leader
  epkgs.undo-tree

	epkgs.sicp
    ]);

  dotfile = builtins.readFile ./.emacs;

in
  with pkgs; rec {
    custom-emacs = pkgs.stdenv.lib.overrideDerivation base-emacs (oldAttrs: {
      dotemacs = dotfile;
      # emacsWithPackages already wraps emacs to force site-start.el
      # discovery, but we want to add additional lisp code to be evaluated
      # after emacs-with-dependencies, but not as part of emacs (to save
      # having to rebuild all of emacs)
      installPhase = oldAttrs.installPhase + ''
        mkdir -p $out/share/emacs/site-lisp/

        echo "$dotemacs" >$out/share/emacs/site-lisp/site-start.el

        # Rewrap the emacs binaries to find our config file
        for prog in $emacs/bin/*; do # */
          local progname=$(basename "$prog")
          rm -f "$out/bin/$progname"
          makeWrapper "$prog" "$out/bin/$progname" \
            --suffix EMACSLOADPATH ":" "$out/share/emacs/site-lisp:$deps/share/emacs/site-lisp:" \
	    --set RUST_SRC_PATH "${rustPlatform.rust.rustc.src}/src"
        done
      '';
    });
    }
