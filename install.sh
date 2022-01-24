#!/usr/bin/env bash

# Link everything.
for f in $(find $HOME/.dotfiles/home -type f); do
      FNAME=$(echo $f | awk -F/ '{print $NF}')
      ln -s -f $f $HOME/$FNAME
done

# Setup .ssh config
ln -s -f $HOME/.dotfiles/ssh $HOME/.ssh

# Interactively get the keys
echo "Github SSH Key:"
GITHUB_KEY=$(</dev/stdin)
echo "$GITHUB_KEY" > ~/.ssh/github.com
chmod 0400 ~/.ssh/github.com

echo "Gitlab SSH Key:"
GITLAB_KEY=$(</dev/stdin)
echo "$GITLAB_KEY" > ~/.ssh/gitlab.com
chmod 0400 ~/.ssh/gitlab.com

# Setup some bits and pieces
mkdir -p $HOME/bin
mkdir -p $HOME/src/github.com/chronojam
mkdir -p $HOME/src/gitlab.com
mkdir -p $HOME/.password-store
mkdir -p $HOME/.gpg

# Fetch GPG Key
echo "GPG Key:"
GPG_KEY=$(</dev/stdin)
echo "$GPG_KEY" > $HOME/.gpg/secret.key
chmod 0400 ~/.gpg/secret.key

echo "Setting up GPG.."
gpg --import $HOME/.gpg/secret.key
gpg --edit-key 9F81D15BF825E9CC

# Fetch all our passwords and stuff.
git clone git@github.com:chronojam/password-store $HOME/.password-store

# Assume debian/ubuntu because we dont use anything else
sudo apt-get update
sudo apt-get install -y pass

# Source .bash_profile, which sources everything else
. $HOME/.bash_profile
