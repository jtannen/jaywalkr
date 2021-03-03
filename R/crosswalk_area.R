
library(dplyr)

crosswalk_geoms_area <- function(
  x_geom,
  y_geom,
  x_id=NULL,
  y_id=NULL,
  allow_unmatched_weights=c("error", "allow"),
  verbose=TRUE,
  tol=1e4
){
  allow_unmatched_weights <- match.arg(allow_unmatched_weights)

  if(is.null(x_id)) x_id <- seq_along(x_geom)
  if(is.null(y_id)) y_id <- seq_along(y_geom)

  res <- sf::st_intersection(
    sf::st_sf(data.frame(geom=x_geom, id.x=x_id)),
    sf::st_sf(data.frame(geom=y_geom, id.y=y_id))
  )

  res$area <- sf::st_area(res)
  res <- res[as.numeric(res$area) > 0, ]
  res <- as.data.frame(res) %>% select(-geometry)

  if(allow_unmatched_weights == "error"){
    validate_loss(x_geom, x_id, res, tol, id.x)
    validate_loss(y_geom, y_id, res, tol, id.y)
  }

  res <- res %>%
    group_by(id.x) %>%
    mutate(from_x_to_y = as.numeric(area / sum(area))) %>%
    group_by(id.y) %>%
    mutate(from_y_to_x = as.numeric(area / sum(area)))

  return(res)
}

validate_loss <- function(geom, id, res, tol, res_id){
  res_id <- enquo(res_id)

  orig_area <- data.frame(id=id, area=sf::st_area(geom))
  new_area <- res %>% group_by(!!res_id) %>% summarise(area=sum(area))

  lost_area <- left_join(orig_area, new_area, by=c(id=rlang::as_name(res_id))) %>%
    mutate(diff = area.x - area.y)

  invalid <- abs(lost_area$diff) / lost_area$area.x > units::as_units(tol, "1")
  invalid <- invalid | is.na(invalid)

  if(any(invalid)){
      stop(
        sprintf(
          "Area was lost. geoms %s changed more than tolerance.",
          paste(which(invalid), collapse=", ")
        )
      )
  }
}
