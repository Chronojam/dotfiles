{ config, pkgs, ... }:

{
  environment = {
   variables = {
     GDK_SCALE = "2";
     GDK_DPI_SCALE = "0.5";
     _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
     EDITOR = pkgs.lib.mkOverride 0 "vim";
   };
   systemPackages = with pkgs; [
      vim
      wget
      git
      firefox
      xterm
      rxvt_unicode
      sxiv
      dmenu
      scrot
      pass
      vscodium
      xclip
      xorg.xmodmap
      bazel
      haskellPackages.xmobar

      # Other important bits.
      discord
      dunst
      zoom-us
      spotify
    ];
  };
}