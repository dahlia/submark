# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.6

RUN apk add --no-cache bash build-base curl ghc zlib-dev
RUN curl -sSL https://get.haskellstack.org/ | bash

RUN stack config set system-ghc --global true

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/submark/package.yaml
COPY stack.yaml /src/submark/stack.yaml

WORKDIR /src/submark

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup
RUN stack install --only-dependencies --test --no-run-tests
RUN stack build --only-snapshot --flag submark:static

COPY . /src/submark

RUN stack build --flag submark:static --copy-bins
RUN stack exec -- submark || true
