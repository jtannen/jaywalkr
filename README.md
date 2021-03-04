
# jaywalkr

<!-- badges: start -->
<!-- badges: end -->

Crosswalk data between two sf polygons. (Note: the old demo with data now lives at https://github.com/jtannen/crosswalk_demo)

## Installation

You can install the released version of jaywalkr from github with:

``` r
devtools::install_github("https://github.com/jtannen/jaywalkr")
```

## Example

`crosswalk_geoms` expects two sets of polygons, and a set of point-geometry weights. In my running example, I have two sets of political precincts whose boundaries changed over time, and Census Block centroids with populations, which will serve as the weights.


``` r
library(jaywalkr)

data("phila_blocks")
data("divs_201911")
data("divs_2019")

crosswalk_geoms(
  x_geom=divs_2019$geometry,
  y_geom=divs_201911$geometry,
  weight_pts=phila_blocks$geometry,
  weights=phila_blocks$pop,
  x_id="warddiv",
  y_id="warddiv",
  allow_unmatched_weights="distance",
  verbose=TRUE
)

crosswalk_geoms_area(
  x_geom=divs_2019$geometry,
  y_geom=divs_201911$geometry,
  x_id=divs_2019$warddiv,
  y_id=divs_201911$geometry$warddiv,
  allow_unmatched_weights="allow"
)
```

