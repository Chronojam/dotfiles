#!/usr/bin/env bash

# Link everything.
for f in $(find $HOME/.dotfiles/home -type f); do
      FNAME=$(echo $f | awk -F/ '{print $NF}')
      ln -s -f $f $HOME/$FNAME
done

for f in $(find $HOME/.dotfiles/nixos -type f); do
      FNAME=$(echo $f | awk -F/ '{print $NF}')
      sudo ln -s -f $f /etc/nixos/$FNAME
done

# Setup .ssh config
ln -s -f $HOME/.dotfiles/ssh $HOME/.ssh

# Setup some bits and pieces
mkdir -p $HOME/bin
mkdir -p $HOME/src/github.com/chronojam
mkdir -p $HOME/src/gitlab.com
mkdir -p $HOME/.password-store
mkdir -p $HOME/.gpg

echo "Setting up GPG.."
gpg --import $HOME/.gpg/secret.key
gpg --edit-key 9F81D15BF825E9CC

# Fetch all our passwords and stuff.
git clone git@github.com:chronojam/password-store $HOME/.password-store

# Source .bash_profile, which sources everything else
. $HOME/.bash_profile
