#!/bin/bash -xe

source ~/secrets.sh
cd ~/report-download && ./run.sh && cd ~/ri-deploy && ./run.sh
