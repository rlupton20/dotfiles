{pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  base-emacs = emacsWithPackages (epkgs:
      with epkgs.melpaStablePackages; with emacsPackagesNg; [
        use-package
        monokai-theme
	smartparens
	linum-relative

	helm
	helm-projectile
	company
	avy
	yasnippet
	projectile
	magit
	flycheck
	git-gutter
	emamux
	multiple-cursors

	nixos-options
	nix-sandbox
	nix-mode

	yaml-mode

	haskell-mode
	intero

	elpy

	rust-mode
	cargo
	racer
	flycheck-rust

	ess
	js3-mode
	tide

	spaceline
        evil
        evil-leader
        undo-tree

	sicp
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
