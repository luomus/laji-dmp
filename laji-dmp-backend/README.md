Setup:
1. install ghc, cabal
2. clone the repo
3. `cabal update && cabal build`
4. `cabal run`

Uses sqlite db. The path can be set with `LAJI_DMP_DATABASE` environment variable, or it will default to `./laji-dmp.db`. Defaults to port 4000.
