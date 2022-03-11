{ config, pkgs, ... }:


# configurations for home desktop
{

    # Use the GRUB 2 boot loader.
    boot.loader.grub.enable = true;
    boot.loader.grub.copyKernels = true;
    boot.loader.grub.efiSupport = true;
    boot.loader.grub.efiInstallAsRemovable = true;
    boot.loader.grub.fsIdentifier = "label";
    # Define on which hard drive you want to install Grub.
    boot.loader.grub.devices = [ "nodev" ]; # or "nodev" for efi only
    boot.loader.grub.useOSProber = true;
    boot.loader.grub.extraEntries = ''
      menuentry "Reboot" {
        reboot
      }
      menuentry "Poweroff" {
        halt
      }
    '';
    boot.loader.systemd-boot.enable = true;

    networking.hostName = "adrenaline";
    networking.wireless.enable = false;
    networking.interfaces.enp4s0.useDHCP = true;

    systemd.targets.sleep.enable = false;
    systemd.targets.suspend.enable = false;
    systemd.targets.hibernate.enable = false;
    systemd.targets.hybrid-sleep.enable = false;

    programs.steam.enable = true;
    powerManagement.enable = false;
    services.xserver.dpi = 180;
    services.xserver.layout = "gb";
    services.xserver.resolutions = [
      { x = "2560"; y = "1600"; }
    ];
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

    environment = {
      # Desktop specific packages.
      systemPackages = with pkgs; [
        discord
        audacity

        # :thinking_face
        minecraft
      ];
    };
}
