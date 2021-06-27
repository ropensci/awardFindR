# Version 1.0.1

- Instead of extracting the 2020s csv from NEH, we use the actual API

# awardFindR (1.0)

- Transferred repo to ropensci
- Added NEWS.md file
- Changed status from "WIP" to "Active"
- Added `verbose` option to all functions that pull HTTP resources, so users can see the status of individual HTTP requests
- Changed all function names to a standardized verb_object format
- `sources` argument in `search_awards()` is now validated properly with `match.args()`
- Various bug fixes
