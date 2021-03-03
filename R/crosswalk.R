
crosswalk_geoms_to_pts <- function(
  geom,
  weight_pts,
  weights,
  geom_ids=NULL,
  weight_ids=NULL,
  allow_unmatched_weights=c("error", "drop", "distance"),
  verbose=TRUE
){
  allow_unmatched_weights <- match.arg(allow_unmatched_weights)
  crosswalk_geoms_to_pts_validate(weight_pts, weights, weight_ids, geom, geom_ids)

  res <- map_points_to_intersection(geom, weight_pts, weights)
  if(any(is.na(res$geom.id))){
    res <- correct_missing(res, allow_unmatched_weights, weight_pts, geom)
  }

  if(verbose){
    print("sum of matched weights:")
    print(sum(res$weight))
    print("sum of all weights:")
    print(sum(weights))
  }

  res <- res %>% select(geom.id, weight.id, weight)
  if(!is.null(geom_ids)) res$geom.id <- geom_ids[res$geom.id]
  if(!is.null(weight_ids)) res$weight.id <- weight_ids[res$weight.id]

  return(res)
}

crosswalk_geoms_to_pts_validate <- function(weight_pts, weights, weight_ids, geom, geom_ids){
  assertthat::assert_that(class(weight_pts)[1] == "sfc_POINT")
  assertthat::assert_that(length(weights) == length(weight_pts))

  if(!is.null(weight_ids)){
    assertthat::assert_that(length(weight_ids) == length(weight_pts))
  }
  if(!is.null(geom_ids)){
    assertthat::assert_that(length(geom_ids) == length(geom))
  }
}

map_points_to_intersection <- function(geom, weight_pts, weights){
  res <- data.frame(
    weight.id = 1:length(weight_pts),
    weight = weights
  )

  weights_to_geom <- sf::st_intersects(weight_pts, geom)

  if(any(sapply(weights_to_geom, length) > 1)){
    stop("Weights were mapped to multiple geom. geom should be non-overlapping.")
  }

  res$geom.id <- sapply(weights_to_geom, function(x) ifelse(is.null(x), NA, x))
  res
}

correct_missing <- function(res, allow_unmatched_weights, weight_pts, geom){
  missing <- is.na(res$geom.id)

  unmatched_txt <- sprintf(
    "%s / %s weights were unmatched.",
    sum(missing),
    length(missing)
  )

  if(allow_unmatched_weights == "distance"){

    warning(paste(unmatched_txt, "Falling back to distance."))

    rematched <- weight_pts[missing] %>%
      sf::st_distance(geom) %>%
      apply(1, which.min)

    res$geom.id[missing] <- rematched

  } else if (allow_unmatched_weights == "error") {

    stop(paste(unmatched_txt, "Consider setting allow_unmatched_weights."))

  } else if(allow_unmatched_weights == "drop"){

    warning(paste(unmatched_txt, "Those will be dropped."))
    res <- res[!missing, ]

  } else stop("Bad allow_unmatched_weights")

  return(res)
}


crosswalk_from_weights <- function(
  x_weights,
  y_weights,
  x_weight_id="weight.id",
  y_weight_id="weight.id",
  x_geom_id="geom.id",
  y_geom_id="geom.id",
  weight_col="weight"
){

  if(is.null(x_geom_id)){
    x_geom_id <- "geom.id"
    if(!"geom.id" %in% names(x_weights)){
      x_weights$geom.id <- 1:nrow(x_weights)
    }
  }

  if(is.null(y_geom_id)){
    y_geom_id <- "geom.id"
    if(!"geom.id" %in% names(y_weights)){
      y_weights$geom.id <- 1:nrow(y_weights)
    }
  }

  rename_col <- function(df, old, new){
    names(df)[names(df) == old] <- new
    df
  }

  if(x_geom_id == y_geom_id){
    x_weights <- x_weights %>% rename_col(x_geom_id, paste0(x_geom_id, ".x"))
    y_weights <- y_weights %>% rename_col(y_geom_id, paste0(y_geom_id, ".y"))
    x_geom_id <- paste0(x_geom_id, ".x")
    y_geom_id <- paste0(y_geom_id, ".y")
  }

  named_vec <- function(name, x) {names(x) <- name; x}
  res <- left_join(
    x_weights[ , c(x_geom_id, x_weight_id, weight_col)],
    y_weights[ , c(y_geom_id, y_weight_id)],
    by=named_vec(x_weight_id, y_weight_id)
  )

  res <- res %>%
    group_by(!!sym(x_geom_id), !!sym(y_geom_id)) %>%
    summarise(weight=sum(!!sym(weight_col))) %>%
    group_by(!!sym(x_geom_id)) %>%
    mutate(from_x_to_y = weight / sum(weight)) %>%
    group_by(!!sym(y_geom_id)) %>%
    mutate(from_y_to_x= weight / sum(weight)) %>%
    ungroup()

  return(res)
}

crosswalk_geoms <- function(
  x_geom,
  y_geom,
  weight_pts,
  weights,
  x_id=NULL,
  y_id=NULL,
  allow_unmatched_weights=c("error", "drop", "distance"),
  verbose=TRUE
){
  if(verbose) print("Matching x to weights.")
  x_weights <- crosswalk_geoms_to_pts(
    x_geom,
    weight_pts,
    weights,
    geom_ids=x_id,
    allow_unmatched_weights=allow_unmatched_weights,
    verbose=verbose
  ) %>% filter(weight > 0)

  if(verbose) print("Matching y to weights.")
  y_weights <- crosswalk_geoms_to_pts(
    y_geom,
    weight_pts,
    weights,
    geom_ids=y_id,
    allow_unmatched_weights=allow_unmatched_weights,
    verbose=verbose
  )%>% filter(weight > 0)

  if(verbose) print("Crosswalking.")
  res <- crosswalk_from_weights(x_weights, y_weights)
  return(res)
}
