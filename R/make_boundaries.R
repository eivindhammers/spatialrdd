
make_buffers <- function(obj, bandwidth, idvar, check_valid = FALSE, main = NULL) {
  sf::sf_use_s2(FALSE)
  
  crs_orig <- sf::st_crs(obj)
  
  if (!"sf" %in% class(obj)) {
    stop(paste0("'", deparse(substitute(obj)), "'", " is not an sf-object."))
  }
  
  if (is.null(bandwidth)) {
    stop("Bandwidth must be supplied (in meters)")
  }
  
  if (is.null(idvar)) {
    stop(paste0("Please provide the variable containing the unique ID for your polygons."))
  }
  
  if (check_valid == TRUE) {
    valid <- sf::st_is_valid(obj)
    if (any(valid) == FALSE) {
      warning(paste0("'", deparse(substitute(obj)), "'",
                     " is not valid. Consider running sf::st_make_valid(", deparse(substitute(obj)), ")."))
    }
  }
  
  if (sf::st_is_longlat(obj)) {
    warning(paste0("'", deparse(substitute(obj)), "'",
                   " is lat/lon, and is automatically converted to UTM (zone given by average longitude).\n", 
                   "Consider converting to projected CRS if UTM does not work for you."))
    centroids <- suppressWarnings(sf::st_centroid(obj)) |> 
      sf::st_coordinates()
    mean_lon <- mean(centroids)
    
    zone <- floor((mean_lon + 180) / 6) + 1
    utm_crs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +type=crs")
    obj <- sf::st_transform(obj, utm_crs)
    
    if (!is.null(main)) {
      main_poly <- obj |> 
        dplyr::select(all_of(idvar)) |> 
        dplyr::filter(!!rlang::sym(idvar) == main)
      
      poly <- obj |> 
        dplyr::select(all_of(idvar)) |> 
        sf::st_filter(main_poly, .predicate = sf::st_touches) |> 
        dplyr::bind_rows(main_poly)
      
      borders_main <- main_poly |> 
        sf::st_cast("MULTILINESTRING") |> 
        # Keep border segments within 1 meter from neighbor
        sf::st_intersection(poly |> 
                              dplyr::filter(!!rlang::sym(idvar) != main) |> 
                              sf::st_buffer(1) |> 
                              dplyr::rename("{idvar}_right" := idvar))
      
      borders_nb <- poly |> 
        dplyr::filter(!!rlang::sym(idvar) != main) |> 
        sf::st_cast("MULTILINESTRING") |> 
        # Keep border segments within 1 meter from main
        sf::st_intersection(main_poly |> 
                              sf::st_buffer(1) |> 
                              dplyr::rename("{idvar}_right" := idvar)) |> 
        dplyr::group_by(!!idvar) |> 
        dplyr::mutate(segment_id = dplyr::row_number()) |> 
        dplyr::ungroup()
      
      borders_buf <- dplyr::bind_rows(borders_nb, borders_main) |> 
        # Make border buffer of 'bandwidth' width
        sf::st_buffer(bandwidth) |>
        # Remove overlapping buffers on neighbor side by intersecting with country polygons, segment by segment
        sf::st_intersection(poly |> dplyr::rename("IDVAR_right" := idvar), by_feature = TRUE) |>
        # Keep only intersections between segment and correct neighbor country
        dplyr::filter(!!rlang::sym(idvar) == IDVAR_right) |> 
        dplyr::select(-IDVAR_right) |> 
        sf::st_transform(crs_orig)
      
    } else {
      borders_buf <- obj |> 
        dplyr::select(all_of(idvar)) |> 
        sf::st_cast("MULTILINESTRING") |> 
        sf::st_buffer(bandwidth) |> 
        sf::st_intersection(obj |> dplyr::rename("IDVAR_right" := idvar), by_feature = TRUE) |>
        dplyr::filter(!!rlang::sym(idvar) == IDVAR_right) |> 
        dplyr::select(-IDVAR_right) |> 
        sf::st_transform(crs_orig)
    }
    
    return(borders_buf)
  }
}
