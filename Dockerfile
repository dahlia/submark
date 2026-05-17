# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.23 AS build

RUN apk add --no-cache \
  bash \
  build-base \
  curl \
  gmp-dev \
  gmp-static \
  libffi-dev \
  ncurses-dev \
  upx \
  zlib-dev \
  zlib-static
RUN curl -sSL https://get.haskellstack.org/ | bash

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

RUN cp /src/submark/stack.yaml /tmp/stack.yaml.bak
COPY . /src/submark
RUN cp /tmp/stack.yaml.bak /src/submark/stack.yaml

RUN stack build --flag submark:static --copy-bins
RUN upx -9 "$(stack path --local-bin)/submark"
RUN stack exec -- submark || true

FROM alpine:3.23
COPY --from=build /root/.local/bin/submark /usr/bin/submark
CMD ["/usr/bin/submark"]

LABEL "org.opencontainers.image.title"="submark"
LABEL "org.opencontainers.image.licenses"="GPL-3"
LABEL "org.opencontainers.image.description"="Extract a part from \
CommonMark/Markdown docs"
