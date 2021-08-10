#' Get netcdf data from files
#'
#' Retrieve MOMMA input variables and replace fill values with NA.
#'
#' @param swot_file string path to swot_file
#' @param sos_file string path to sos_file
#' @param reach_id integer unique reach identifier
#'
#' @return list matrix of reach data (both valid and invalid)
get_input_data <- function(swot_file, sos_file, reach_id) {

  # Open files for reading and get data
  swot_input <- RNetCDF::open.nc(swot_file)
  sos_input <- RNetCDF::open.nc(sos_file)

  # Track nt
  nt <- RNetCDF::var.get.nc(swot_input, "nt")

  # Data
  reach_grp = RNetCDF::grp.inq.nc(swot_input, "reach")$self
  width <- RNetCDF::var.get.nc(reach_grp, "width")
  wse <- RNetCDF::var.get.nc(reach_grp, "wse")
  slope2 <- RNetCDF::var.get.nc(reach_grp, "slope2")

  # Bankfull depth
  reach_grp = RNetCDF::grp.inq.nc(sos_input, "reaches")$self
  reach_ids <- RNetCDF::var.get.nc(reach_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  db <- RNetCDF::var.get.nc(reach_grp, "logDb_hat")[index]
  db <- exp(db)

  # Close files
  RNetCDF::close.nc(swot_input)
  RNetCDF::close.nc(sos_input)

  # Check validity of observation data
  obs_data <- check_observations(width, wse, slope2, dim(nt))
  if (length(obs_data) == 0) { return(list(valid = FALSE, reach_id = reach_id, nt = nt)) }

  # Create a list of data with reach identifier
  ## TODO mbl, Qb, Qmean_prior -> use real data
  return(list(valid = TRUE, reach_id = reach_id, nt = nt,
              width = obs_data$width, slope2 = obs_data$slope2,
              wse = obs_data$wse, db = db,
              mbl = 16800, Qb = 15900, Qm = 6625,
              invalid_time = obs_data$invalid_time))
}

#' Checks if observation data is valid.
#'
#' @param width vector
#' @param wse vector
#' @param slope2 vector
#' @param nt integer
#'
#' @return list of valid observations or an empty list if there are none
check_observations <- function(width, wse, slope2, nt) {
  # Test for negative data
  width[width < 0] <- NA
  slope2[slope2 < 0] <- NA

  # Track invalid time
  invalid_width <- which(is.na(width))
  invalid_wse <- which(is.na(wse))
  invalid_slope2 <- which(is.na(slope2))
  invalid_time <- sort(unique(c(invalid_width, invalid_wse, invalid_slope2)))

  # Keep valid data from width, wse, and slope2
  valid_time <- !c(1:nt) %in% invalid_time
  width <- width[valid_time]
  wse <- wse[valid_time]
  slope2 <- slope2[valid_time]

  # Return a list of valid or invalid observation data
  if (length(width) < 5 || length(wse) < 5 || length(slope2) < 5) { return(vector(mode = "list")) }
  else { return(list(width = width, wse = wse, slope2 = slope2, invalid_time = invalid_time)) }
}
