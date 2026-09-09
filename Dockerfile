FROM ocaml/opam:alpine-3.21-ocaml-5.3 AS builder

USER root
RUN apk add --no-cache \
    libsecp256k1-dev \
    postgresql-dev \
    openssl-dev \
    gmp-dev \
    libev-dev \
    zlib-dev \
    linux-headers \
    pkgconf \
    m4 \
    git
USER opam

WORKDIR /home/opam/app
COPY --chown=opam:opam ocaml-nostr-relay.opam .
RUN opam update && opam install --deps-only --yes .

COPY --chown=opam:opam . .
RUN opam exec -- dune build --profile release bin/main.exe \
 && opam exec -- dune runtest

FROM alpine:3.21
RUN apk add --no-cache libpq libsecp256k1 openssl gmp libev ca-certificates
WORKDIR /app
COPY --from=builder /home/opam/app/_build/default/bin/main.exe /app/ocaml-nostr-relay
COPY public ./public
EXPOSE 9001
CMD ["/app/ocaml-nostr-relay"]
