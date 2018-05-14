realtR
================

## Installation

``` r
devtools::install_github("hadley/emo")
devtools::install_github("abresler/realtR")
```

### realtR

There are few service industries worse than residential real estate
brokerage industry.

Industry actors are some of the most overpaid, dishonest, people you
will encounter.

They hoard information, lie, are rarely held accountable for anything
they do. They are a well organized political cartel who ensure
regardless of what happens, they get paid a sizable chunk of the overall
proceeds from a transaction.

The consumer has a hard time educating themselves and can be at the
mercy of these snake-oil salesmen.

The time has come to change for and `realtR` goes a **long** way towards
doing this.

Anyone with a bit of R skills now has easy functional access to property
information for every location in the United States.

With a few lines of R code you will have access to at-least as much, and
in most cases, <strong>MORE</strong> information than brokers.

You can take a look at a bit of what the package does in
<a href="http://asbcllc.com/r_packages/realtR/2018/introduction/index.html" title="intro" target="_blank">this
introductory tutorial</a>.

## Functions

  - `geocode()` : Batch geocoder
  - `listing_counts()` : Area summary listings
  - `listings()` : Area detailed listings
  - `vitality()` : Area market vitality
  - `properties_near()` : Listings near a location
  - `trends()` : Area historic trends
  - `map_listings()` : Featured listings from a map
  - `median_prices()` : Area median prices
  - `parse_listing_urls()` : Detailed property listing information
  - `mortgage_rates()` : Rolling 30 day mortgage rates by product

## Dictionaries

  - `dictionary_property_types()` : Search-able property types
  - `dictionary_listing_features()` : Search-able property features

## Fun Tools

  - `summarise_broker_bullshit()` : Summarizes a long-winded broker
    property description into `n` sentences using the
    [PageRank](https://en.wikipedia.org/wiki/PageRank) algorithm

[![Travis build
status](https://travis-ci.org/abresler/realtR.svg?branch=master)](https://travis-ci.org/abresler/realtR)
