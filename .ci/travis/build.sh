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

if [ -n "$LLVM" ]; then
    export LLVM_FLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi

if [ -n "$THREADED" ]; then
	  export THREADED_FLAG=" --ghc-options -threaded "
fi


echo Building libvts
cd libvts
mkdir build.d
cd build.d
cmake -DDOWNLOAD_GTEST=On ..
make
cd $TRAVIS_BUILD_DIR

stack build $THREADED_FLAG $LLVM_FLAG
