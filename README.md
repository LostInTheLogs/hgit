# HGit

HGit is a simple "clone" of `git` I made to practice haskell.

## Showcase

TODO: asciinema recording

## Limitations

- Assumes .git/index exists
- Doesn't update the index after `stat`ing files, so it gets slower with time
- No reflog support
- No tags and packed-refs support
- SHA-1 repos only
- No symlinks support
- No submodules support
- Doesn't support blobs larger than ram
- No gc

## Supported features

### Commands

- init
- fetch (smart git wire v1 via http, capabilities: multi_ack,
  multi_ack_detailed, no-done, side-band, side-band-64k)
- status
- add
- commit
- reset
- switch
- log

### Config

Simple config support (read only, no includes)

### Object storage

- loose objects: read, write
- pack.pack (v2): read
- pack.idx (v2): read, write (for `git fetch`)

### Plumbing commands

- diff-index
- checkout-index
- cat-file
- hash-object
- ls-files
- ls-tree
- read-tree
- refs list

## Benchmark

TODO: benchmark
