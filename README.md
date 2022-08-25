
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Linking StatsCan Geographies to ONS Neighbourhoods

<!-- badges: start -->
<!-- badges: end -->

This project is developing a methodology for linking Statistics Canada
(StatsCan) geographies to Ottawa Neighbourhood Study (ONS) geographies.
Specifically, we are linking StatsCan dissemination areas (DAs) to ONS
neighbourhoods.

## Methods Summary

### Many-to-many weighted links

-   We’ve been looking at ways to link dissemination areas (DAs) to ONS
    neighbourhoods, and investigating different ways of setting up
    either a one-to-many DA-to-hood link using a form of weighting, or a
    one-to-one hood link.
    -   I’ll call the many-to-many option the weighted links and the
        one-to-one options single-link indicators (SLI).
-   We created weighted links using DA shapefiles, ONS shapefiles, and
    the OpenStreetMaps (OSM) residential areas shapefiles as follows:
-   Trim DA hoods to only include their subsets that are OSM residential
    zones.
-   For each DA, overlay it on the trimmed ONS neighbourhood shapes, and
    assign it to each hood it overlaps according to the proportion of
    its total overlap that’s in that hood.

### Single-link indicators

-   Next, we wanted to see if we could get similar accuracy using a SLI.
-   The simplest approach is to take the weighted links and, for each
    DA, assign it to the top-weighted hood. This gives us our basic SLI.
-   Next, I built a greedy optimization algorithm to improve the SLIs.
    It uses DA-level household numbers, and compares it to
    “gold-standard” statscan-provided ONS hood household numbers. For
    each DA, it looks at all the hoods it overlaps at all—without
    respect to residential areas—and tries assigning it to each one in
    turn, measuring the total error that results, and choosing the
    assignment that gives the lowest total error. To inject some
    randomness, it goes through the whole set of hoods in random order
    three times. Results were quite stable by the end and had improved
    dramatically (see figure below).
-   The most mature function is `greedy_sli_search2()`, which is generic
    for any set of geographies and also includes a minimum-overlap
    threshold that defaults to 5%. It only condiders DA assignations to
    hoods that overlap more than 5% of the DA’s area. *In other words,
    it is customizable to ignore areas that technically overlap but are
    very small or due to minor error/noise* (e.g. geography borders that
    both align with the same street, but overlap because one goes down
    the centre and the other is 1m to the left).

The result is three optimized SLIs:

-   Mean average error (MAE)
-   Mean average percentage error (MAPE)
-   Mean squared error (MSE)

With the standard 5% minimim overlap threshold, results are as follows:

    #> Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
    #> had status 1

| method                             |   mae |   mape |    mse |
|:-----------------------------------|------:|-------:|-------:|
| Weighted Link                      | 103.7 | 0.0473 |  51228 |
| SLI unoptimized                    | 167.3 | 0.0833 | 109292 |
| SLI mean absolute error            |  90.3 | 0.0543 |  34708 |
| SLI mean absolute percentage error | 104.1 | 0.0468 |  50586 |
| SLI mean squared error             |  87.1 | 0.0475 |  32816 |

**These SLIs saved and datestamped in the /outputs folder.**
