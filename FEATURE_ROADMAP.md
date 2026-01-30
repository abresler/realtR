# realtR Feature Roadmap

Generated: 2026-01-30

## Executive Summary

realtR is the **only actively maintained R package** for programmatic access to residential real estate listings data. This roadmap prioritizes features using Bayesian expected value scoring to maximize user impact.

### Strategic Position

- **No direct R competitors** - ZillowR deprecated (2021), fredr serves different purpose (economic data)
- **Unique capabilities** - Free-text geocoder, NLP broker summarization, 40+ user-agent rotation
- **Key gap** - Comparable sales (comps) functionality for property valuation

---

## Feature Priority Matrix

### Phase 1: Foundation (Implemented)

| Feature | EV Score | Status | Notes |
|---------|----------|--------|-------|
| httr2 Retry/Throttle Infrastructure | 7.50 | ✅ DONE | R/http.R with automatic retry, rate limiting |
| Memoise Caching Infrastructure | 6.10 | ✅ DONE | R/cache.R with disk/memory caching |
| Custom Error Classes | 5.20 | ✅ DONE | RealtrApiError, RealtrRateLimitError, RealtrNotFoundError |
| Beginner Vignette | 5.60 | ✅ DONE | vignettes/getting-started.Rmd |

### Phase 2: High-Value Features (Implemented)

| Feature | EV Score | Status | Notes |
|---------|----------|--------|-------|
| Comparable Sales (comps) | 6.00 | ✅ DONE | R/comps.R with comps(), summary_comps(), estimate_value() |

### Phase 3: Strategic Enhancements (Planned)

| Feature | EV Score | Status | Priority |
|---------|----------|--------|----------|
| Historical Price Tracking | 4.10 | ⏳ PLANNED | HIGH |
| Parallel Processing (furrr) | 5.50 | ⏳ PLANNED | HIGH |
| Batch with Checkpointing | 4.60 | ⏳ PLANNED | MEDIUM |
| Multi-Source Integration (Redfin) | 3.80 | ⏳ PLANNED | MEDIUM |
| Field Explanation Helper | 4.30 | ⏳ PLANNED | MEDIUM |
| API Health Dashboard | 3.80 | ⏳ PLANNED | LOW |

### Deferred (Future Consideration)

| Feature | EV Score | Reason for Deferral |
|---------|----------|---------------------|
| Property Valuation/AVM | 2.20 | Requires ML model development |
| Property Watch/Alerts | 2.45 | Requires persistent state management |
| Commercial Real Estate | - | Different domain complexity |
| Webhook Integration | - | Niche use case |

---

## Detailed Feature Specifications

### Comparable Sales (comps) - IMPLEMENTED

**Files:** `R/comps.R`

**Functions:**
- `comps(address, radius_miles, max_age_months, ...)` - Find comparable sales
- `summary_comps(comps_data)` - Summarize comp statistics
- `estimate_value(comps_data, method)` - Estimate property value

**Key Capabilities:**
- Geocode target address automatically
- Search sold listings within radius
- Filter by beds, baths, sqft, property type with configurable tolerance
- Calculate similarity scores (0-100) based on property characteristics
- Haversine distance calculation for geographic filtering

---

### httr2 Infrastructure - IMPLEMENTED

**Files:** `R/http.R`

**Functions:**
- `.fetch_page(url)` - Fetch HTML with retry/throttle
- `.fetch_json(url)` - Fetch JSON with retry/throttle
- `realtr_config()` - Configure HTTP behavior
- `api_status()` - Check endpoint health

**Key Capabilities:**
- Exponential backoff retry (3 attempts)
- Rate limiting (30 requests/minute)
- Custom error classes for API failures
- User-agent rotation integration

---

### Caching Infrastructure - IMPLEMENTED

**Files:** `R/cache.R`, `R/zzz.R`

**Functions:**
- `clear_cache(type)` - Clear cached data
- `cache_info()` - Get cache information

**Key Capabilities:**
- Disk cache for persistent storage
- Memory cache for session-level data
- Cache directory follows OS conventions

---

## Implementation Notes

### Dependencies Added
- `httr2` - Modern HTTP client with retry/throttle
- `memoise` - Function memoization
- `cachem` - Cache backends

### Files Created
- `R/http.R` - HTTP infrastructure
- `R/cache.R` - Caching infrastructure
- `R/comps.R` - Comparable sales
- `R/zzz.R` - Package initialization
- `vignettes/getting-started.Rmd` - Beginner tutorial

### Files Modified
- `DESCRIPTION` - Added dependencies, vignette support
- `R/api.R` - Updated `.curl_page()` and `.curl_json()` to use httr2

---

## Patterns Applied

Based on analysis of successful R packages (httr2, nbastatR, tidyquant):

1. **Request Retry with Exponential Backoff** (from httr2)
2. **Rate Limiting with Token Bucket** (from httr2)
3. **Graceful Degradation** (from nbastatR)
4. **Dictionary-Based Field Mapping** (existing pattern extended)
5. **Unified Multi-Source Interface** (design pattern for future)

---

## Quick Wins Remaining

| Feature | Effort | Impact |
|---------|--------|--------|
| Plain English Aliases | 1 hour | `search_homes <- listings` |
| explain_fields() helper | 2-3 hours | Interactive field dictionary |
| Parallel processing wrapper | 3-4 hours | furrr integration for batch |

---

## Known Issues to Address

1. **Broken Endpoints:**
   - `mortgage_rates()` - Deprecated, use `mortgage_rates_fred()`
   - `vitality()` - Deprecated, endpoint returns 404

2. **Bot Detection:**
   - User-agent rotation helps but not foolproof
   - Rate limiting added to be a good API citizen

3. **API Fragility:**
   - 100% dependency on realtor.com structure
   - HTML parsing can break with site updates
   - Multi-source architecture planned for resilience

---

## Contributing

To add a new feature:

1. Check this roadmap for priority
2. Follow existing patterns in codebase
3. Add tests in `tests/testthat/`
4. Update this roadmap with status

---

## Changelog

### 2026-01-30
- Added httr2 infrastructure with retry/throttle
- Added caching infrastructure
- Added comps() function for comparable sales
- Added beginner vignette
- Created feature roadmap
