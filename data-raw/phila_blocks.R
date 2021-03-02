library(sf)
library(dplyr)
library(readr)

PA_CRS <- 2272

phila_blocks <- st_read("data-raw/phila_blocks.shp")

asnum <- function(x) as.numeric(as.character(x))

# use the interior points, not the polygons
phila_blocks <- st_as_sf(
  as.data.frame(phila_blocks) %>%
    select(GEOID10, INTPTLAT10, INTPTLON10) %>%
    mutate(INTPTLAT10=asnum(INTPTLAT10), INTPTLON10=asnum(INTPTLON10)),
  coords = c("INTPTLON10", "INTPTLAT10"),
  crs = 4326,
  agr = "constant"
) %>%
  st_transform(PA_CRS)

block_pops_2010 <- read_csv("data-raw/block_pops_census10_phila_socialexp.csv")

block_pops_2010 <- block_pops_2010 %>%
  rename(pop = SF1_P0010001) %>%
  mutate(GEOID = as.character(Geo_FIPS)) %>%
  select(GEOID, pop)

phila_blocks <- phila_blocks %>%
	left_join(
		block_pops_2010 %>% select(GEOID, pop),
		by = c("GEOID10" = "GEOID")
	)

usethis::use_data(phila_blocks, overwrite=TRUE)
