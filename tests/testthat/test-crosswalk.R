library(jaywalkr)
library(dplyr)

divs_test_2019 <- readRDS("divs_test_2019.RDS")
divs_test_201911 <- readRDS("divs_test_201911.RDS")
blocks_test <- readRDS("blocks_test.RDS")


test_that("crosswalk_geoms_to_pts", {

  cw <- crosswalk_geoms_to_pts(
    geom=divs_test_2019$geometry,
    weight_pts=blocks_test$geometry,
    weights=blocks_test$pop,
    geom_id=divs_test_2019$warddiv,
    weight_id=blocks_test$GEOID10,
    allow_unmatched_weights="error",
    verbose=FALSE
  )

  expect_equal(nrow(cw), nrow(blocks_test))
  expect_equal(sum(cw$weight), sum(blocks_test$pop))
  expect_equal(sort(unique(cw$geom.id)), sort(unique(divs_test_2019$warddiv)))

  res <- cw %>% group_by(geom.id) %>% summarise(weight = sum(weight))
  expect_equal(res$weight[res$geom.id=="05-01"], 1095)
  expect_equal(res$weight[res$geom.id=="05-02"], 832)
  expect_equal(res$weight[res$geom.id=="05-03"], 888)
})

test_that("crosswalk_geoms_to_pts with missing div", {

    blocks_0503 <- blocks_test %>%
      filter(GEOID10 %in% c("421010010011005", "421010010011007", "421010010011009"))

    cw <- crosswalk_geoms_to_pts(
      geom=divs_test_2019$geometry,
      weight_pts=blocks_0503$geometry,
      weights=blocks_0503$pop,
      weight_ids=blocks_0503$GEOID10,
      geom_ids=divs_test_2019$warddiv,
      allow_unmatched_weights="error",
      verbose=FALSE
    )
    expect_equal(nrow(cw), 3)
    expect_equal(sum(cw$weight), 888)
    expect_true(all(cw$geom.id == "05-03"))
})

test_that("crosswalk_geoms_to_pts error", {

  divs_0501 <- divs_test_2019 %>% filter(warddiv=="05-01")

  expect_error(
    crosswalk_geoms_to_pts(
      geom=divs_0501$geometry,
      geom_ids=divs_0501$warddiv,
      weight_pts=blocks_test$geometry,
      weights=blocks_test$pop,
      weight_ids=blocks_test$GEOID10,
      allow_unmatched_weights="error",
      verbose=FALSE
    ),
    regexp = ".*weights were unmatched. Consider setting allow_unmatched_weights."
  )
})


test_that("crosswalk_geoms_to_pts distance", {

  divs_0501 <- divs_test_2019 %>% filter(warddiv=="05-01")
  cw <- expect_warning(
    crosswalk_geoms_to_pts(
      geom=divs_0501$geometry,
      geom_ids=divs_0501$warddiv,
      weight_pts=blocks_test$geometry,
      weights=blocks_test$pop,
      weight_ids=blocks_test$GEOID10,
      allow_unmatched_weights="distance",
      verbose=FALSE
    )
  )
  expect_equal(nrow(cw), nrow(blocks_test))
  expect_equal(sum(cw$weight), sum(blocks_test$pop))
  expect_true(all(cw$geom.id == "05-01"))
})


test_that("crosswalk_geoms_to_pts drop", {

  divs_0503 <- divs_test_2019 %>% filter(warddiv=="05-03")
  cw <- expect_warning(
      crosswalk_geoms_to_pts(
        geom=divs_0503$geometry,
        geom_ids=divs_0503$warddiv,
        weight_pts=blocks_test$geometry,
        weights=blocks_test$pop,
        weight_ids=blocks_test$GEOID10,
      allow_unmatched_weights="drop",
      verbose=FALSE
    )
  )
  expect_equal(nrow(cw), 3)
  expect_equal(sum(cw$weight), 888)
  expect_true(all(cw$geom.id == "05-03"))
})

test_that("crosswalk_geoms", {

  divs_05_19 <- divs_test_2019 %>% filter(substr(warddiv, 1, 2)=="05")
  divs_05_1911 <- divs_test_201911 %>% filter(substr(warddiv, 1, 2)=="05")

  cw <- crosswalk_geoms(
    divs_05_19$geometry,
    divs_05_1911$geometry,
    weight_pts=blocks_test$geometry,
    weights=blocks_test$pop,
    x_id=divs_05_19$warddiv,
    y_id=divs_05_1911$warddiv,
    allow_unmatched_weights="distance",
    verbose=FALSE
  )

  fractional <- cw %>% filter(from_x_to_y < 1)

  expect_equal(nrow(fractional), 8)

  get_val <- function(xgeom, ygeom, var){
    var <- ensym(var)
    fractional %>%
      filter(geom.id.x == xgeom, geom.id.y == ygeom) %>%
      with(eval(var))
  }

  expect_equal(
    get_val("05-01", "05-01", from_x_to_y),
    434 / (434 + 661)
  )
  expect_equal(
    get_val("05-01", "05-01", from_y_to_x),
    1
  )
  expect_equal(
    get_val("05-24", "05-35", from_y_to_x),
    1855 / 1880
  )
  expect_equal(
    get_val( "05-24", "05-35", from_y_to_x),
    1855 / 1880
  )
  expect_equal(
    get_val( "05-24", "05-35", from_x_to_y),
    1855 / (1855 + 1587 + 393)
  )
})


test_that("crosswalk_geoms null ids", {

  divs_05_19 <- divs_test_2019 %>% filter(substr(warddiv, 1, 2)=="05") %>% arrange(warddiv)
  divs_05_1911 <- divs_test_201911 %>% filter(substr(warddiv, 1, 2)=="05") %>% arrange(warddiv)

  cw <- crosswalk_geoms(
    divs_05_19$geometry,
    divs_05_1911$geometry,
    weight_pts=blocks_test$geometry,
    weights=blocks_test$pop,
    x_id=NULL,
    y_id=NULL,
    allow_unmatched_weights="distance",
    verbose=FALSE
  )

  fractional <- cw %>% filter(from_x_to_y < 1)

  expect_equal(nrow(fractional), 8)

  get_val <- function(xgeom, ygeom, var){
    var <- ensym(var)
    fractional %>%
      filter(geom.id.x == xgeom, geom.id.y == ygeom) %>%
      with(eval(var))
  }

  expect_equal(
    get_val(1, 1, from_x_to_y),
    434 / (434 + 661)
  )
  expect_equal(
    get_val(1, 1, from_y_to_x),
    1
  )
  expect_equal(
    get_val(24, 35, from_y_to_x),
    1855 / 1880
  )
  expect_equal(
    get_val(24, 35, from_y_to_x),
    1855 / 1880
  )
  expect_equal(
    get_val(24, 35, from_x_to_y),
    1855 / (1855 + 1587 + 393)
  )
})

