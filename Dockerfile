# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.15 AS build

LABEL "org.opencontainers.image.title"="submark"
LABEL "org.opencontainers.image.licenses"="GPL-3"

RUN apk add --no-cache \
  bash=5.1.16-r0 \
  build-base=0.5-r2 \
  curl=7.80.0-r0 \
  ghc=9.0.1-r1 \
  libffi-dev=3.4.2-r1 \
  ncurses-dev=6.3_p20211120-r0 \
  yq=4.14.1-r0 \
  zlib-dev=1.2.11-r3
RUN curl -sSL https://get.haskellstack.org/ | bash

RUN stack config set system-ghc --global true

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/submark/package.yaml
COPY stack.yaml /src/submark/stack.yaml
RUN yq e -i '.compiler = "ghc-9.0.1"' /src/submark/stack.yaml

WORKDIR /src/submark

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup
RUN stack install --only-dependencies --test --no-run-tests
RUN stack build --only-snapshot --flag submark:static

RUN cp /src/submark/stack.yaml /tmp/stack.yaml.bak
COPY . /src/submark
RUN cp /tmp/stack.yaml.bak /src/submark/stack.yaml

RUN stack build --flag submark:static --copy-bins
RUN stack exec -- submark || true

FROM alpine:3.15
COPY --from=build /root/.local/bin/submark /usr/bin/submark
CMD ["/usr/bin/submark"]