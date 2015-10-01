# You can get the first two values from developers.google.com
export ADWORDS_CLIENT_ID=Your Oauth client id
export ADWORDS_CLIENT_SECRET=Oauth client secret
# You need to generate a refresh token. The API docs tell you how
export ADWORDS_REFRESH_TOKEN=Refresh token
# API developer token. You have to apply to AdWords to get one of these
export ADWORDS_DEVELOPER_TOKEN=Dev token
# Password for the postgres database user
export ADWORDS_PG_PASSWORD="adwords"
# Where the host machine is
export PRODUCTION_HOST=example.com
# User to run as on host.
# Must be able to run docker
export PRODUCTION_USER=user
