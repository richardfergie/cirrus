# You can get the first two values from developers.google.com
export ADWORDS_CLIENT_ID=Your Oauth client id
export ADWORDS_CLIENT_SECRET=Oauth client secret
# You need to generate a refresh token. The API docs tell you how
export ADWORDS_REFRESH_TOKEN=Refresh token
# API developer token. You have to apply to AdWords to get one of these
export ADWORDS_DEVELOPER_TOKEN=Dev token
# Password for the postgres database superuser
# This password is set at database creation after which
# changing it here will not update the password in the database
export POSTGRES_PG_PASSWORD="postgres"
# Where the host machine is
export PRODUCTION_HOST=example.com
# User to run as on host.
# Must be able to run docker
export PRODUCTION_USER=user
# Amazon access keys. Must allow up/download from S3
export AWS_ACCESS_KEY_ID=keykeykey
export AWS_SECRET_ACCESS_KEY=secretsecretsecret

# S3 buckets where backups are stored
# These need to be different buckets
export ADWORDS_REPORT_S3_BUCKET=
export POSTGRES_BACKUP_S3_BUCKET=
