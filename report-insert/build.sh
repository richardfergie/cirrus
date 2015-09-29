#!/bin/bash -xe

stack build
cp .stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/report-insert ri-deploy/report-insert
