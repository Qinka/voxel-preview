#!/bin/bash

# Copyright (C) 2018 Johann Lee <me@qinka.pro>
#
# This file is part of voxel-preview
#
# Voxel-preview is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Voxel-preview is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Voxel-preview. If not, see <http://www.gnu.org/licenses/>.
#

set -e

echo
echo Setting up environments
echo

export APT=apt-get
echo
echo Updating $APT source

sudo $APT update

echo
echo Fetching system\'s name

export OS_CORENAME=$(lsb_release -c | awk '{print $2}')
export OS_DISTRIBUTOR=$(lsb_release -i | awk '{print $3}')
echo Using $OS_DISTRIBUTOR  $OS_CORENAME

if [ -n "$LLVM"]; then
   echo
   echo Using llvm-$LLVM
fi

echo
echo Setting up ghc-$GHC_VER
export PATH=/opt/ghc/$GHC_VER/bin:$PATH
ghc -V

echo
echo Setting up haskell stack

mkdir -p $HOME/.local/bin
export PATH=$HOME/.local/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

echo
echo Configuring
stack config set system-ghc --global true
stack path --programs $STACKFILE

