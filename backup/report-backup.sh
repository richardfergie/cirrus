#!/bin/bash -xe

aws s3 sync /opt/reports/ s3://$ADWORDS_REPORT_S3_BUCKET --region us-east-1

