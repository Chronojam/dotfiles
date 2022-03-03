{ config, pkgs, ... }:


# configurations for home desktop
{
    networking.hostName = "adrenaline";
    networking.wireless.enable = false;
    networking.interfaces.enp4s0.useDHCP = true;

    systemd.targets.sleep.enable = false;
    systemd.targets.suspend.enable = false;
    systemd.targets.hibernate.enable = false;
    systemd.targets.hybrid-sleep.enable = false;

    services.xserver.dpi = 180;
    services.xserver.layout = "gb";
    services.xserver.xrandrHeads = [
      {
        output = "DP-2";
        primary = true;
      }
      {
        output = "HDMI-1";
        monitorConfig = ''
          Option "RightOf" "DP-2"
        '';
      }
    ];
}
