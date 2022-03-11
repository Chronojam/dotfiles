# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix

      # Include machine-specific configuration
      ./miya68.nix
      /etc/nixos/environment.nix

      # Terminal Configurations
      ./xterm.nix

      # General environment configurations
      ./common.nix
    ];

  time.timeZone = "Europe/London";
  nixpkgs.config = {
    allowUnfree = true;
  };
  #nix.gc.automatic = true;
  #nix.autoOptimiseStore = true;
  # Enable docker daemon for development
  virtualisation.docker.enable = true;


  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.xserver = {
    enable = true;

    displayManager = {
      defaultSession = "xfce+xmonad";
      lightdm.enable = false;
      sddm.enable = true;
    };
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages : [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
    };
    desktopManager = {
      xterm = {
        enable = false;
      };

      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
  };

  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.7;
    backend = "glx";
    opacityRules = [ 
      "75:class_g = 'URxvt' && focused"
      "60:class_g = 'URxvt' && !focused" 
    ];
    shadow = false;
    shadowOpacity = 0.75;
  };

  location.provider = "manual";
  location.latitude = 51.4545;
  location.longitude = 2.5879;

  services.redshift = {
    enable = true;

    temperature.day = 3500;
    # 2700
    temperature.night = 3500;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    calliope = {
      isNormalUser = true;
      createHome = true;
      description = "Calliope Gardner";
      group = "users";
      home = "/home/calliope";
      hashedPassword = "$6$iirq/GpNr6IBdewB$2NzMW1w95yGB3.F11fHhe66ZMug7oYs.vMMlj69FD6RY2VjLbGiyxfotFxj2vQZHW9955unpsPuvz.OSUCgsl0";
      extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ and docker
    };
  };

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      corefonts
      dejavu_fonts
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

