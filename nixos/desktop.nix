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
    # Nvidia HW drivers.
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.opengl.enable = true;
    hardware.nvidia.powerManagement.enable = true;
    services.xserver.screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';

    services.xserver.resolutions = [
      { x = "3840"; y = "2160"; }
    ];
    services.xserver.displayManager.setupCommands = ''
    ${pkgs.xlibs.xrandr}/bin/xrandr --output DP-0 --off --output DP-1 --off --output DP-2 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP-3 --off --output HDMI-0 --mode 3840x2160 --pos 3840x0 --rotate normal --output DP-4 --off --output DP-5 --off 
    '';

    environment = {
      # Desktop specific packages.
      systemPackages = with pkgs; [
        discord
        audacity
        gnucash
        airshipper

        # :thinking_face
        minecraft
        (wineWowPackages.full.override {
          wineRelease = "staging";
          mingwSupport = true;
        })
      ];
    };
}
