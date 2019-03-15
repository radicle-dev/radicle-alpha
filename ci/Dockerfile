# Base image for Google Cloud Build build steps.
#
# Includes the following tools
#
# - ghc v8.6.3
# - stack v1.9.1
# - gcloud
# - gsutil
# - docker client v18.09.2
# - docker-compose v1.23.2

FROM haskell:8.6.3

ENTRYPOINT []

# Install `gcloud`, `gsutil` and dependencies of `fpm`.
ADD https://packages.cloud.google.com/apt/doc/apt-key.gpg google-apt-key.gpg
RUN \
  echo "deb http://packages.cloud.google.com/apt cloud-sdk-stretch main" \
    > /etc/apt/sources.list.d/google-cloud-sdk.list && \
  apt-key add google-apt-key.gpg && \
  apt-get update -yqq && \
  apt-get -yq --no-install-recommends install \
    build-essential \
    ruby \
    ruby-dev \
    rubygems \
    google-cloud-sdk && \
  rm -rf /var/lib/apt/lists/*

# Install `fpm`
RUN gem install --no-ri --no-rdoc fpm

# Install `docker`
ADD https://download.docker.com/linux/static/stable/x86_64/docker-18.09.2.tgz docker.tgz
RUN \
  tar --extract --file docker.tgz --strip-components 1 --directory /usr/local/bin/ && \
  rm docker.tgz

# Install `docker-compose`
ADD https://github.com/docker/compose/releases/download/1.23.2/docker-compose-Linux-x86_64 /usr/local/bin/docker-compose
RUN chmod +x /usr/local/bin/docker-compose

# Install `ipfs`
ADD https://dist.ipfs.io/go-ipfs/v0.4.18/go-ipfs_v0.4.18_linux-amd64.tar.gz ipfs.tgz
RUN \
  tar --extract --file ipfs.tgz --strip-components=1 -C /usr/bin go-ipfs/ipfs && \
  rm ipfs.tgz
