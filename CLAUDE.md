# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

realtR is a real estate data aggregation package providing programmatic access to residential property information, market data, listings, and pricing from **realtor.com**. The package has a single-source dependency on realtor.com APIs, requiring careful handling of API stability and bot detection avoidance.

**Purpose:** Democratize real estate data access, giving consumers equivalent information to brokers.

## Build/Test Commands

```bash
# Standard R/devtools workflow
Rscript -e "devtools::document()"      # Regenerate NAMESPACE and man/
Rscript -e "devtools::load_all()"      # Load for development
Rscript -e "devtools::install()"       # Install locally
Rscript -e "devtools::test()"          # Run all tests
Rscript -e "devtools::check()"         # Full R CMD check
Rscript -e "pkgdown::build_site()"     # Build documentation site
```

## Architecture

### Data Flow Pattern

All functions follow this pipeline:
1. **URL generation** - Location-based or direct URL construction
2. **HTTP request** - `curl` with user-agent rotation (40+ agents in `utils.R`)
3. **Parse response** - HTML via `rvest`, JSON via `jsonlite`
4. **Munge data** - Type casting, column name translation via dictionaries
5. **Return tibble** - Standardized output format

### Key Source Files

| File | Lines | Purpose |
|------|-------|---------|
| `api.R` | 2,726 | Primary scraping - `listings()`, `vitality()`, `median_prices()`, `mortgage_rates()` |
| `map.R` | 2,069 | Map-based search - `map_listings()`, `table_listings()`, `properties_near()` |
| `listing_api.R` | 987 | Detailed listing parsing - `parse_listing_urls()` |
| `utils.R` | 245 | Helpers, user-agent rotation, cookie generation |
| `aws.R` | 324 | Geocoding - `geocode()` |
| `new_trends.R` | 176 | Market trends - `trends_zipcodes()` |
| `rental.R` | 79 | Rental estimates - `rental_estimates()` |
| `broker_bs.R` | 84 | NLP text summarization - `summarise_broker_bullshit()` |

### Conventions

**Function naming:**
- Public: `verb_noun()` - `geocode()`, `listings()`, `parse_listing_urls()`
- Private: `.dot_prefix()` - `.curl_page()`, `.munge_realtor()`, `.generate_cookies()`

**Column naming (standardized from API):**
- `type*`, `name*`, `date*`, `amount*`, `count*`, `price*`, `url*`
- Booleans: `is*`, `has*`
- IDs: `id*`

**Dictionary pattern:**
- `dictionary_realtor_names()` - Maps API field names → standardized names
- `dictionary_listing_names()` - Listing detail field translation
- `.munge_realtor()` - Auto-detects types via regex on column names

### Data Sources

All data sourced from realtor.com:

| Endpoint Pattern | Functions |
|-----------------|-----------|
| `/realestateandhomes-search/*` | `listings()`, `listing_counts()`, `map_listings()` |
| `/browse_modules/homes_near_street` | `properties_near()` |
| `/validate_geo` | `geocode()` |
| `/median_prices` | `median_prices()` |
| `/home_page/vitality` | `vitality()` |
| `/mrtg_handler/get_trends_data` | `mortgage_rates()` |
| `/realestateandhomes-detail/*` | `parse_listing_urls()` |
| `/myhome/rental-estimate/zip/*` | `rental_estimates()` |
| `/myhome/trends-zip/*` | `trends_zipcodes()` |

### Critical Implementation Notes

- **Bot detection:** User-agent rotation via `generate_url_reference()` is essential - realtor.com blocks repeated identical agents
- **API fragility:** 100% dependency on realtor.com API structure - breaking changes upstream affect entire package
- **Complex JSON:** `new_trends.R` and `listing_api.R` have intricate nested JSON flattening - validate carefully on API changes
- **No error recovery:** API failures propagate directly to user; `possibly()` used sparingly

### NLP Pipeline (broker_bs.R)

`summarise_broker_bullshit()` uses:
- `udpipe` for POS tagging (NOUN, VERB, ADJ extraction)
- `textrank` for PageRank-based sentence importance ranking
- Condenses verbose property descriptions to key points

## Testing

- **Framework:** testthat
- **Test files:** 4 files, 40+ tests
- **Location:** `tests/testthat/`
- **Key test files:**
  - `test-snapshot-baseline.R` - Function existence and return type verification
  - `test-dictionaries.R` - Dictionary function validation
  - `test-nlp.R` - NLP summarization (skipped without udpipe model)

## Dependencies

**Core:** dplyr, purrr, tidyr, tibble, stringr, glue, rlang
**Web:** curl, rvest, xml2, jsonlite
**NLP:** udpipe, textrank
**Parallel:** future, furrr

## Known Issues

- **Broken APIs (as of 2026-01):**
  - `mortgage_rates()` - endpoint `/mrtg_handler/get_trends_data` returns 404
  - `vitality()` - endpoint `/home_page/vitality` returns 404
- Some endpoints return 429 (rate limited) - user-agent rotation helps
- No API versioning or fallback handling
- Package depends on external realtor.com API stability
