# Dockerfile to get system ready in production
FROM fpco/stack-run:lts-5
RUN curl -fsSL https://get.docker.com/ | sh
ADD . /run
WORKDIR /run