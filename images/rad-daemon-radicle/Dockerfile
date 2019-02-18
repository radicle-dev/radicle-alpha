FROM debian:stretch
# The distribution must be the same one the binary is build with to
# ensure binary compatibility. We use the `haskell` images on CI and
# `fpco/stack-build` with when building with `stack build --docker`.

RUN apt-get -qq update && \
    apt-get install -y libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

ADD ./ /

# Executing the binary ensures that all linked libraries are present
RUN rad-daemon-radicle --help >/dev/null

EXPOSE 8000
CMD ["/bin/rad-daemon-radicle"]
