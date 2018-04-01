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

if [ -n "$RUN_TEST" ]; then
    echo
    echo Run testing
    cd $TRAVIS_BUILD_DIR
    ./libvts/build.d/vts-test
    stack test
fi
