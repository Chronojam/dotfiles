{ config, pkgs, ... }:

# List of common environment details
let 
  extensions = (with pkgs.vscode-extensions; [

  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "nix-extension-pack";
      publisher = "pinage404";
      version = "1.0.0";
      sha256 = "10hi9ydx50zd9jhscfjiwlz3k0v4dfi0j8p58x8421rk5dspi98x";
    }
  ];
  vscodium-with-extensions =  pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in {
  environment = {
   variables = {
     GDK_SCALE = "2";
     GDK_DPI_SCALE = "0.75";
     _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
     EDITOR = pkgs.lib.mkOverride 0 "vim";
   };
   systemPackages = with pkgs; [
      vim
      wget
      git
      firefox
      rxvt_unicode
      sxiv
      dmenu
      scrot
      pass
      vscodium-with-extensions
      xclip
      xorg.xmodmap
      feh
      dig
      kubectl
      haskellPackages.xmobar

      # Other important bits.
      libreoffice
      dunst
      zoom-us
      spotify
      audacity
    ];
  };
}
