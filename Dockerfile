# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.23 AS build
ARG TARGETARCH
ENV GHC_VERSION=9.10.3
ENV GHC_AARCH64_ALPINE_BINDIST=https://downloads.haskell.org/~ghc/9.10.3/ghc-9.10.3-aarch64-alpine3_18-linux.tar.xz

RUN apk add --no-cache \
  bash \
  build-base \
  curl \
  gmp-dev \
  gmp-static \
  libffi-dev \
  ncurses-dev \
  upx \
  xz \
  zlib-dev \
  zlib-static
RUN case "${TARGETARCH:-$(uname -m)}" in \
      amd64|x86_64) curl -sSL https://get.haskellstack.org/ | bash ;; \
      arm64|aarch64) apk add --no-cache stack ;; \
      *) echo "Unsupported architecture: ${TARGETARCH:-$(uname -m)}" >&2; exit 1 ;; \
    esac

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/submark/package.yaml
COPY stack.yaml /src/submark/stack.yaml

WORKDIR /src/submark

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      stack --ghc-variant custom setup \
        --ghc-bindist "$GHC_AARCH64_ALPINE_BINDIST" ;; \
      *) stack setup ;; \
    esac
RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      stack --ghc-variant custom install --only-dependencies --test --no-run-tests ;; \
      *) stack install --only-dependencies --test --no-run-tests ;; \
    esac
RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      stack --ghc-variant custom build --only-snapshot --flag submark:static ;; \
      *) stack build --only-snapshot --flag submark:static ;; \
    esac

RUN cp /src/submark/stack.yaml /tmp/stack.yaml.bak
COPY . /src/submark
RUN cp /tmp/stack.yaml.bak /src/submark/stack.yaml

RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      stack --ghc-variant custom build --flag submark:static --copy-bins ;; \
      *) stack build --flag submark:static --copy-bins ;; \
    esac
RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      upx -9 "$(stack --ghc-variant custom path --local-bin)/submark" ;; \
      *) upx -9 "$(stack path --local-bin)/submark" ;; \
    esac
RUN case "${TARGETARCH:-$(uname -m)}" in arm64|aarch64) \
      stack --ghc-variant custom exec -- submark || true ;; \
      *) stack exec -- submark || true ;; \
    esac

FROM alpine:3.23
COPY --from=build /root/.local/bin/submark /usr/bin/submark
CMD ["/usr/bin/submark"]

LABEL "org.opencontainers.image.title"="submark"
LABEL "org.opencontainers.image.licenses"="GPL-3"
LABEL "org.opencontainers.image.description"="Extract a part from \
CommonMark/Markdown docs"
