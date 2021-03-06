library(sf)
library(dplyr)

PA_CRS <- 2272
PRESENT_VINTAGE <- "201911"

# dir <- sprintf("../../../data/gis/warddivs/%s/", PRESENT_VINTAGE)
# files <- list.files(dir, "Political_Divisions\\.[a-z]+$")
# for(f in files){
#   newname <- gsub("(.*)(\\.[a-z]+)$", paste0("\\1_", PRESENT_VINTAGE, "\\2"), f)
#   file.copy(
#     paste0(dir, f),
#     paste0("data-raw/", newname)
#   )
#   print(newname)
# }


divs <- st_read("data-raw/Political_Divisions_201911.shp")
divs <- st_transform(divs, PA_CRS)

divs <- divs %>%
  mutate(
    warddiv = sprintf("%s-%s", substr(DIVISION_N, 1, 2), substr(DIVISION_N, 3, 4))
  ) %>%
  select(warddiv, geometry)

divs_201911 <- divs

usethis::use_data(divs_201911, overwrite = TRUE)
