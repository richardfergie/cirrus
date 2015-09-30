#!/bin/bash -xe

source ~/secrets.sh
cd ~/report-download && ./run.sh && cd ~/report-insert && ./run.sh
