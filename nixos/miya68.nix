{ config, pkgs, ... }:

let
  myCustomLayout = pkgs.writeText "xkb-layout" ''
    keycode 108 = Mode_switch
    keysym numbersign = numbersign asciitilde bar bar  
  '';
in
{
    services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}";
}