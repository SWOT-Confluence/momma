#' Get netcdf data from files
#'
#' Retrieve MOMMA input variables and replace fill values with NA.
#'
#' @param swot_file string path to swot_file
#' @param sos_file string path to sos_file
#' @param reach_id integer unique reach identifier
#'
#' @return list matrix of reach data (both valid and invalid)
get_input_data <- function(swot_file, sos_file, reach_id, min_nobs, constrained) {

  Qgage <- NA

  # Open files for reading and get data
  swot_input <- RNetCDF::open.nc(swot_file)
  sos_input <- RNetCDF::open.nc(sos_file)

  # Track nt
  nt <- RNetCDF::var.get.nc(swot_input, "nt")

  # Data
  reach_grp = RNetCDF::grp.inq.nc(swot_input, "reach")$self

  obs_times <- RNetCDF::var.get.nc(reach_grp, "time_str")
  width <- RNetCDF::var.get.nc(reach_grp, "width")
  wse <- RNetCDF::var.get.nc(reach_grp, "wse")
  slope2 <- RNetCDF::var.get.nc(reach_grp, "slope2")

  # Bankfull depth
  reach_grp <- RNetCDF::grp.inq.nc(sos_input, "reaches")$self
  reach_ids <- RNetCDF::var.get.nc(reach_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  gbp_grp <- RNetCDF::grp.inq.nc(sos_input, "gbpriors/reach")$self
  db <- exp(RNetCDF::var.get.nc(gbp_grp, "logDb_hat")[index])
  model_grp <- RNetCDF::grp.inq.nc(sos_input, "model")$self
  Qm <- RNetCDF::var.get.nc(model_grp, "mean_q")[index]
  Qb <- RNetCDF::var.get.nc(model_grp, "two_year_return_q")[index]

  # Gauges
  if (constrained){

  
#   # Find out what gauge groups there are with the top level sos variable
#   gauge_groups = get variable in sos 
    all_gauge_groups = RNetCDF::att.get.nc(sos_input, "NC_GLOBAL", "gauge_agency")
    all_gauge_groups = strsplit(all_gauge_groups, ";")[[1]]
    all_gauge_groups = all_gauge_groups[all_gauge_groups != "SWOT_SHAQ"]

    for (gauge_group in all_gauge_groups){
      gauge_grp <- RNetCDF::grp.inq.nc(sos_input, gauge_group)$self
      reach_group_name = paste0(gauge_group, "_reach_id")
      reach_ids_in_group = RNetCDF::var.get.nc(gauge_grp, reach_group_name)
      
      gauge_index = which(reach_ids_in_group==reach_id, arr.ind=TRUE)
      if (length(gauge_index)!=0){
        break
      }
    }

    if (length(gauge_index)!=0){
      gauge_times = RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_qt"))[,gauge_index]

      find_index <- function(x) {
      index <- which(gauge_times == x)
      if (length(index) == 0) {
        return(NA)  # Return NA if not found
      } else {
        return(index)  # Return the index if found
      }
    }

    # Function to convert a date string to days since 2000-01-01
    date_to_days <- function(date_str) {
      # Convert the input string to a Date object
      date_obj <- as.Date(date_str, format = "%Y-%m-%d")
      
      # Reference date: 2000-01-01
      reference_date <- as.Date("0001-01-01")
      
      # Calculate the difference in days
      days_since_2000 <- as.numeric(difftime(date_obj, reference_date, units = "days"))
      
      return(days_since_2000)
    }


    # Example usage
    date_str <- obs_times[1]
    days_since_year_1 <- date_to_days(date_str)

    # # Apply the function to each element of the list
    result_list <- lapply(obs_times, date_to_days)

    # Apply the function to each element in obs_times
    indices <- sapply(result_list, find_index)

    Qgage <- RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_q"))[,gauge_index][indices]
    }
  }

# 
  # at this point Qgage should be the daily gauge mesaurments for the reach and have the same dimensions as the observations
  
  # Close files
  RNetCDF::close.nc(swot_input)
  RNetCDF::close.nc(sos_input)

  # Check validity of observation data
  obs_data <- check_observations(width, wse, slope2, dim(nt), min_nobs, Qgage)
  if (length(obs_data) == 0) { return(list(valid = FALSE, reach_id = reach_id, nt = nt, obs_times=obs_times)) }

  # Create a list of data with reach identifier
  ## TODO mbl, Qb, Qmean_prior -> use real data
  return(list(valid = TRUE, reach_id = reach_id, nt = nt,
              width = obs_data$width, slope2 = obs_data$slope2,
              wse = obs_data$wse, db = db,
              mbl = 16800, Qb = Qb, Qm = Qm,
              invalid_time = obs_data$invalid_time, obs_times=obs_times))
  }

#' Checks if observation data is valid.
#'
#' @param width vector
#' @param wse vector
#' @param slope2 vector
#' @param nt integer
#'
#' @return list of valid observations or an empty list if there are none
check_observations <- function(width, wse, slope2, nt, min_nobs, Qgage) {
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
  Qgage <- Qgage[valid_time]

  # Return a list of valid or invalid observation data
  if (length(width) < min_nobs || length(wse) < min_nobs || length(slope2) < min_nobs) { 
    return(vector(mode = "list"))
  }  else { 
    return(list(width = width, wse = wse, slope2 = slope2, invalid_time = invalid_time, Qgage = Qgage)) 
  }
}