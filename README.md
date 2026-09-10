# OCaml Nostr Relay

A Nostr relay server written in OCaml, built on cohttp and websocket-lwt-unix
for HTTP/WebSocket and PostgreSQL for storage.

## NIP Support

- **NIP-01**: Basic protocol flow
- **NIP-04**: Encrypted direct messages
- **NIP-09**: Event deletion
- **NIP-11**: Relay information document
- **NIP-17**: Private direct messages
- **NIP-26**: Delegated event signing
- **NIP-40**: Expiration timestamp
- **NIP-42**: Authentication of clients to relays
- **NIP-45**: Event counts
- **NIP-59**: Gift wrap
- **NIP-66**: Relay discovery
- **NIP-70**: Protected events
- **NIP-78**: Application-specific data

## Usage

```bash
opam install --deps-only .
dune build
DATABASE_URL='postgres://user:pass@localhost:5432/nostr' ./_build/default/bin/main.exe
```

The relay listens on `ws://localhost:9001` by default.

## Docker

```bash
docker build -t ocaml-nostr-relay .
docker run -p 9001:9001 -e DATABASE_URL='postgres://user:pass@host:5432/nostr' ocaml-nostr-relay
```

## Requirements

- OCaml 4.14 or later
- PostgreSQL
- libsecp256k1 (BIP-340 Schnorr verification is done through libsecp256k1)
- libpq

## Configuration

- `DATABASE_URL`: PostgreSQL connection string, or `DB_HOST` / `DB_USER` / `DB_PASS` / `DB_NAME`
- `PORT`: listen port (default `9001`)
- `RELAY_NAME`, `RELAY_DESCRIPTION`, `RELAY_URL`, `RELAY_PUBKEY`, `RELAY_CONTACT`, `RELAY_ICON`,
  `RELAY_COUNTRIES`: values served in the NIP-11 document

## License

MIT

## Author

Yasuhiro Matsumoto (a.k.a. mattn)
