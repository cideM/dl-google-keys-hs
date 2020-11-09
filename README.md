# Download and Cache Public Google Keys

This is a glorified shell script which you can run as a Cronjob. It downloads Google encryption keys which can be used to implement OAuth2 if you can't use an SDK. The keys are cached in a file, in the form of JSON. If the cached keys are still valid, nothing happens.

## Quickstart

```shell
$ export PUBLIC_KEYS_URL="https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"
$ export KEYS_CACHE_PATH="./foo.json"
$ ./download-keys
"No cached keys found, downloading new keys..."
"Stored new keys at: ./foo.json"
$ ./download-keys
"Cached keys are valid until: 2020-11-09 18:10:20.260362 UTC"
```

## Building

This project uses Nix. You can build the executable with just `nix-build` in the project root. Alternatively Cabal commands also work, such as `cabal v2-run`.

## Info

This project aims to keep things simple. That's why I'm manually parsing environment variables and using `do` notation liberally.
