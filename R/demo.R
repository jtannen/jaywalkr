# devtools::load_all("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/libs/crosswalk")

# data("phila_blocks")
# data("divs_201911")

# w <- get_block_div_crosswalk(
#   geom=divs_201911,
#   weights=phila_blocks,
#   geom_id="warddiv",
#   weights_id="GEOID10",
#   weights_col="pop",
#   allow_unmatched_weights="distance"
# )
#
# saveRDS(
#   divs_201911 %>% filter(warddiv %in% c("27-14", "27-10")),
#   file="divs_test.RDS"
# )
#
# saveRDS(
#   phila_blocks %>% filter(GEOID10 %in% w[w$warddiv %in% c("27-14", "27-10"), "GEOID10"]),
#   file="blocks_test.RDS"
# )

