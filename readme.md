realtR
================

## Installation

``` r
devtools::install_github("abresler/realtR")
```

### realtR

There are few service industries worse than the real estate brokerage
industry.

Industy actors generally are some of the most overpaid, dishonest,
people you will come across. They hoard information, lie, rarely are
held accountable and often time have no what is truly going on. They are
a wel organized political cartel who ensure regardless of what happens,
they get paid all the while increasing the cost of housing.

The consumer has a hard time educating themselves and can be at the
mercy of these snake-oil salesmen.

The time has come to change this and `realtR` goes a **long** way
towards doing that by empowering anyone who knows a bit of R with access
to nearly all of the available information on listings throughout the
United States and much much more.

In a few lines of code you will have as much, or more information than
brokers.

In addition to listing data the package provides users access to the
most robust geocoding tool I’ve come across a host of current and
historic meta-level market information and much, much more.

Have fun with this tool and please use it to further the cause
transparency and accountability in and an industry devoid of it.

## Functions

  - `geocode()` : Batch geocoder
  - `listing_counts()` : Area summary listings
  - `listings()` : Area detailed listings
  - `vitality()` : Area market vitality
  - `properties_near()` : Listings near a location
  - `trends()` : Area historic trends
  - `map_listings()` : Featured listings from a map
  - `median_prices()` : Area median prices
  - `mortgage_rates()` : Current mortgage rates

## Dictionaries

  - `dictionary_property_types()` : Searchable property types
  - `dictionary_listing_features()` : Searchable property features

## Tools

  - `summarise_broker_bullshit()` : Takes a long-winded broker
    description of a property and summarises into `n` sentences using
    the [PageRank](https://en.wikipedia.org/wiki/PageRank) algorithm
