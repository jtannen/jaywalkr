library(crosswalk)
library(dplyr)
library(sf)

## Prep Test Data

data("divs_2019")
data("divs_201911")
data("phila_blocks")

divs_test_2019 <- divs_2019 %>% filter(substr(warddiv, 1, 2)=="05")
divs_test_201911 <- divs_201911 %>% filter(substr(warddiv, 1, 2)=="05")
cw <- crosswalk_geoms_to_pts(
  geom=divs_test_2019,
  weights=blocks_test,
  geom_id="warddiv",
  weights_id="GEOID10",
  weights_col="pop",
  allow_unmatched_weights="drop"
)
blocks_test <- phila_blocks %>% filter(GEOID10 %in% cw$GEOID10, pop>0)
setwd("tests/testthat/")
saveRDS(divs_test_2019, "divs_test_2019.RDS")
saveRDS(divs_test_201911, "divs_test_201911.RDS")
saveRDS(blocks_test, "blocks_test.RDS")
