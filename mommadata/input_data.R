#' Get netcdf data from files
#'
#' Retrieve MOMMA input variables and replace fill values with NA.
#'
#' @param swot_file string path to swot_file
#' @param sos_file string path to sos_file
#' @return list matrix of reach data (both valid and invalid)
get_input_data <- function(swot_file, sos_file) {

  # Open files for reading and get data
  swot_input <- ncdf4::nc_open(swot_file)
  sos_input <- ncdf4::nc_open(sos_file)

  # Track reachid and nt
  reachid <- ncdf4::ncatt_get(sos_input, 0)$reach_id[[1]]
  nt <- ncdf4::ncvar_get(swot_input, "nt")

  # Check global attribute for reach validity and return empty list if invalid
  valid <- ncdf4::ncatt_get(sos_input, 0)$valid[[1]]
  if (valid == 0) { return(list(valid = FALSE, reachid = reachid, nt = nt)) }

  # width
  width <- ncdf4::ncvar_get(swot_input, "reach/width")
  width_fill <- ncdf4::ncatt_get(swot_input, "reach/width", "_FillValue")
  width[width == width_fill$value] <- NA
  width = t(width)

  # wse
  wse <- ncdf4::ncvar_get(swot_input, "reach/wse")
  wse_fill <- ncdf4::ncatt_get(swot_input, "reach/wse", "_FillValue")
  wse[wse == wse_fill$value] <- NA
  wse = t(wse)

  # slope2
  slope2 <- ncdf4::ncvar_get(swot_input, "reach/slope2")
  slope_fill <- ncdf4::ncatt_get(swot_input, "reach/slope2", "_FillValue")
  slope2[slope2 == slope_fill$value] <- NA
  slope2 = t(slope2)

  # Bankfull depth
  logDb <- ncdf4::ncvar_get(sos_input, "reach/logDb_hat")
  logDb_fill <- ncdf4::ncatt_get(sos_input, "reach/logDb_hat", "_FillValue")
  logDb[logDb == logDb_fill$value] <- NA
  if (!is.na(logDb)) logDb <- 10^logDb

  # Check validity of observation data
  obs_data <- check_observations(width, wse, slope2)
  if (length(obs_data) == 0) { return(list(valid = FALSE, reachid = reachid, nt = nt)) }

  # Close files
  ncdf4::nc_close(swot_input)
  ncdf4::nc_close(sos_input)

  # Create a list of data with reach identifier
  ## TODO mbl, Qb, Qmean_prior -> use real data
  return(list(valid = TRUE, reachid = reachid, nt = nt,
              width = obs_data$width, slope2 = obs_data$slope2,
              wse = obs_data$wse, Db = logDb,
              mbl = 16800, Qb = 15900, Qm = 6625,
              invalid_time = obs_data$invalid_time))

}

#' Checks if observation data is valid.
#'
#' @param width vector
#' @param wse vector
#' @param slope2 vector
#' @return list of valid observations or an empty list if there are none
check_observations <- function(width, wse, slope2) {
  # Test for negative data
  width[width < 0] <- NA
  slope2[slope2 < 0] <- NA

  # Get invalid width indexes and remove from observation data; test if enough data is present
  invalid_width <- which(is.na(width))
  width <- width[!is.na(width)]
  wse <- wse[!is.na(width)]
  slope2 <- slope2[!is.na(width)]
  if (length(width) < 5) { return(vector(mode = "list")) }

  # Get invalid wse indexes and remove from observation data; test if enough data is present
  invalid_wse <- which(is.na(wse))
  wse <- wse[!is.na(wse)]
  width <- width[!is.na(wse)]
  slope2 <- slope2[!is.na(wse)]
  if (length(wse) < 5) { return(vector(mode = "list")) }

  # Get invalid slope2 indexes and remove from observation data; test if enough data is present
  invalid_slope2 <- which(is.na(slope2))
  slope2 <- slope2[!is.na(slope2)]
  wse <- wse[!is.na(slope2)]
  width <- width[!is.na(slope2)]
  if (length(slope2) < 5) { return(vector(mode = "list")) }

  # Concatenate lists of invalid indexes
  invalid_time <- unique(c(invalid_width, invalid_wse, invalid_slope2))

  # Return list of valid observation data
  return(list(width = width, wse = wse, slope2 = slope2, invalid_time = invalid_time))

}
