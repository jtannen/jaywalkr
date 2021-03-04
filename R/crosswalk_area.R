requireNamespace("dplyr")

#' Crosswalk two `sf` geoms, using area.
#'
#' @param x_geom `sf` polygons to crosswalk from.
#' @param y_geom `sf` polygons to crosswalk to.
#' @param x_id A vector of ids associated with `x_geom`. If `NULL`, ids are `1:N`.
#' @param y_id A vector of ids associated with `y_geom`. If `NULL`, ids are `1:N`.
#' @param allow_unmatched_weights How to handle cases where geoms don't perfectly overlap.
#' @param verbose Print informative meessages.
#' @param tol A tolerance used if `allow_unmatched_weights` is "error".
#'
#' @return A dataframe with columns `x.id`, `y.id`, `area`, `from_x_to_y`, `from_y_to_x`.
#' If x_id or y_id are NULL, the id columns are integers refering to indices of the geoms.
#' `area` is the area of the intersection. `from_x_to_y` is the area in
#' the intersection divided by the area of `x_geom` as a whole. It should be
#' used to proportionally allocate values from `x_geom` to `y_geom`s.
#'
#' `allow_unmatched_weights` handles unmatched geoms that are not 1:1. If "error",
#' then an error is raised whenever the total area for a given geom in the final crosswalk
#' divided by the original area of the geom is not within `tol` of 1.
#'
#' @examples
#' \dontrun{
#'
#' data("divs_2019")
#' data("divs_201911")
#' cw <- crosswalk_geoms_area(
#'   divs_2019$geometry,
#'   divs_201911$geometry,
#'   x_id=divs_2019$warddiv,
#'   y_id=divs_201911$warddiv
#' )
#' }
#' @export
crosswalk_geoms_area <- function(
  x_geom,
  y_geom,
  x_id=NULL,
  y_id=NULL,
  allow_unmatched_weights=c("error", "allow"),
  verbose=TRUE,
  tol=1e-4
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
