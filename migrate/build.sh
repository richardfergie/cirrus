#!/bin/bash -xe

# needed because of caching
stack clean

stack build
cp .stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/dbmigrate migrate/dbmigrate
