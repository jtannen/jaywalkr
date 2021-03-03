library(jaywalkr)

divs_test_2019 <- readRDS("divs_test_2019.RDS")
divs_test_201911 <- readRDS("divs_test_201911.RDS")

test_that("crosswalk_area", {
  res <- crosswalk_geoms_area(
    divs_test_2019$geometry,
    divs_test_201911$geometry,
    divs_test_2019$warddiv,
    divs_test_201911$warddiv
  )

  fetch_val <- function(id.x, id.y, val){
    res %>%
      filter(id.x==!!id.x, id.y==!!id.y) %>%
      with(eval(ensym(val)))
  }

  expect_equal(
    fetch_val("05-01", "05-01", from_x_to_y),
    0.79568,
    tolerance=1e-5
  )
  expect_equal(
    fetch_val("05-01", "05-01", from_y_to_x),
    1.0,
    tolerance=1e-5
  )

  expect_equal(
    fetch_val("05-02", "05-02", from_x_to_y),
    1.0,
    tolerance=1e-5
  )

  expect_equal(
    fetch_val("05-24", "05-35", from_x_to_y),
    0.3214023,
    tolerance=1e-5
  )
  expect_equal(
    fetch_val("05-24", "05-35", from_y_to_x),
    0.7826613,
    tolerance=1e-5
  )
})

test_that("crosswalk_area no ids", {
  divs_test_2019 <- arrange(divs_test_2019, warddiv)
  divs_test_201911 <- arrange(divs_test_201911, warddiv)

  res <- crosswalk_geoms_area(
    divs_test_2019$geometry,
    divs_test_201911$geometry
  )

  fetch_val <- function(id.x, id.y, val){
    res %>%
      filter(id.x==!!id.x, id.y==!!id.y) %>%
      with(eval(ensym(val)))
  }

  expect_equal(
    fetch_val(1, 1, from_x_to_y),
    0.79568,
    tolerance=1e-5
  )
  expect_equal(
    fetch_val(1, 1, from_y_to_x),
    1.0,
    tolerance=1e-5
  )


  expect_equal(
    fetch_val(2, 2, from_x_to_y),
    1.0,
    tolerance=1e-5
  )

  expect_equal(
    fetch_val(24, 35, from_x_to_y),
    0.3214023,
    tolerance=1e-5
  )
  expect_equal(
    fetch_val(24, 35, from_y_to_x),
    0.7826613,
    tolerance=1e-5
  )
})

test_that("crosswalk_area error", {
  divs_test_0501 <- divs_test_2019 %>% filter(warddiv %in% c("05-01"))
  expect_error(
    crosswalk_geoms_area(
      divs_test_0501$geometry,
      divs_test_201911$geometry,
      divs_test_0501$warddiv,
      divs_test_201911$warddiv
    )
  )
})

test_that("crosswalk_area subset with drop", {
  divs_test_0501 <- divs_test_2019 %>% filter(warddiv %in% c("05-01"))
  res <- crosswalk_geoms_area(
      divs_test_0501$geometry,
      divs_test_201911$geometry,
      divs_test_0501$warddiv,
      divs_test_201911$warddiv,
      allow_unmatched_weights = "allow"
    )
  fetch_val <- function(id.x, id.y, val){
    res %>%
      filter(id.x==!!id.x, id.y==!!id.y) %>%
      with(eval(ensym(val)))
  }

  expect_equal(
    fetch_val("05-01", "05-01", from_x_to_y),
    0.79568,
    tolerance=1e-5
  )
  expect_equal(
    fetch_val("05-01", "05-01", from_y_to_x),
    1.0,
    tolerance=1e-5
  )
  expect_true(all(res$id.x == "05-01"))
  expect_equal(as.character(res$id.y), c("05-01", "05-18", "05-37"))
  expect_equal(sum(res$from_x_to_y), 1)
})

