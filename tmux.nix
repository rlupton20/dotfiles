{ powerline, tmux, stdenv, pkgs }:
stdenv.mkDerivation {
  name = "custom-tmux";
  src = builtins.readFile ./tmux/.tmux.conf;
  buildInputs = [ tmux powerline ];
  phases = "installPhase";
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/etc

    echo "$src" >$out/etc/tmux.conf
    
    cat > $out/bin/tmux <<EOF
#!/usr/bin/env sh
PATH=${powerline}/bin:$PATH ${tmux}/bin/tmux -f $out/etc/tmux.conf
EOF

    chmod +x $out/bin/tmux
  '';
}
