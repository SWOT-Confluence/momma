#' Get netcdf data from files
#'
#' Retrieve MOMMA input variables and replace fill values with NA.
#'
#' @param swot_file string path to swot_file
#' @param sos_file string path to sos_file
#' @param reach_id integer unique reach identifier
#'
#' @return list matrix of reach data (both valid and invalid)
get_input_data <- function(swot_file, sos_file, reach_id, reach_id_v16, min_nobs, constrained) {

  Qgage <- NA

  # Open files for reading and get data
  swot_input <- RNetCDF::open.nc(swot_file)
  sos_input <- RNetCDF::open.nc(sos_file)

  # Track nt
  nt <- RNetCDF::var.get.nc(swot_input, "nt")
  nx <- RNetCDF::var.get.nc(swot_input, "nx")

  # Data
  reach_grp <- RNetCDF::grp.inq.nc(swot_input, "reach")$self
  node_grp <- RNetCDF::grp.inq.nc(swot_input, "node")$self

  node_ids <- RNetCDF::var.get.nc(node_grp, "node_id")

  obs_times_all <- RNetCDF::var.get.nc(node_grp, "time_str")
  width_all <- RNetCDF::var.get.nc(node_grp, "width")
  wse_all <- RNetCDF::var.get.nc(node_grp, "wse")
  slope2_all <- RNetCDF::var.get.nc(node_grp, "slope2")
    
  obs_times <- RNetCDF::var.get.nc(reach_grp, "time_str")
  width <- RNetCDF::var.get.nc(reach_grp, "width")
  wse <- RNetCDF::var.get.nc(reach_grp, "wse")
  slope2 <- RNetCDF::var.get.nc(reach_grp, "slope2")

  # Bankfull depth
  reach_grp <- RNetCDF::grp.inq.nc(sos_input, "reaches")$self
  reach_ids <- RNetCDF::var.get.nc(reach_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  gbp_grp <- RNetCDF::grp.inq.nc(sos_input, "gbpriors/node")$self
  db <- exp(RNetCDF::var.get.nc(gbp_grp, "logDb_hat")[index])
  model_grp <- RNetCDF::grp.inq.nc(sos_input, "model")$self
  Qm <- RNetCDF::var.get.nc(model_grp, "mean_q")[index]
  Qb <- RNetCDF::var.get.nc(model_grp, "two_year_return_q")[index]

  # monthly prior
  Qmon <- RNetCDF::var.get.nc(model_grp, "monthly_q")[ , index]
  # print(dim(Qmon))
  # print(index)
  # cat("\nORIGINAL:", Qmon, "\n")
  if (all(is.na(Qmon))) {
      Qmon[] <- Qm
  } else {
      Qmon[is.na(Qmon)] <- mean(Qmon, na.rm = TRUE)
  }
  # cat("CHANGE NA:", Qmon, "\n")

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
      
      # gauge_index = which(reach_ids_in_group==reach_id, arr.ind=TRUE)
      gauge_index = which(reach_ids_in_group==reach_id_v16, arr.ind=TRUE) # 버전 이슈로 수정!!!!!!!!!!!!!!!!
      if (length(gauge_index)!=0){
        break
      }
    }

    # if (length(gauge_index)!=0){
      gauge_times = RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_qt"))[, gauge_index]
      # gauge_times = RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_qt"))[gauge_index,] # 여기 인덱스 위치 수정!!!!!!!!

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
        # "2023-07-27T11:20:21Z" --> "2023-07-27"
        date_str <- substr(date_str, 1, 10)
        
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

      # Apply the function to each element of the list
      result_list <- lapply(obs_times, date_to_days)

      # Apply the function to each element in obs_times
      indices <- sapply(result_list, find_index)

      Qgage <- RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_q"))[,gauge_index][indices]
      # gauge_q <- RNetCDF::var.get.nc(gauge_grp, paste0(gauge_group, "_q"))[gauge_index, ] # 여기 인덱스 위치 수정!!!!!!!!
      # Qgage <- gauge_q[indices]
      
    # }

    # cat("\n===== Qgage =====\n", Qgage, "\n======================\n")
  }


#   # at this point Qgage should be the daily gauge mesaurments for the reach and have the same dimensions as the observations
  
  # Close files
  RNetCDF::close.nc(swot_input)
  RNetCDF::close.nc(sos_input)

  obs_list <- vector("list", nx)
  for (ix in seq_len(nx)) {
      
      node_id <- node_ids[ix]
      index <- which(node_ids==node_id, arr.ind=TRUE)
      
      width <- width_all[, index]
      wse <- wse_all[, index]
      slope2 <- slope2_all[, index]
      
      # Check validity of observation data
      obs_data <- check_observations(width, wse, slope2, length(nt), min_nobs, Qgage)
      
      if (length(obs_data) == 0) { 
          obs_list[[ix]] <- list(valid = FALSE, 
                                 reach_id = reach_id, 
                                 node_id = node_id, 
                                 nt = nt, 
                                 obs_times=obs_times) 
      } else { 
          obs_list[[ix]] <- list(valid = TRUE, 
                                 reach_id = reach_id, 
                                 node_id = node_id, 
                                 nt = nt,
                                 width = obs_data$width, 
                                 slope2 = obs_data$slope2,
                                 wse = obs_data$wse, 
                                 db = db,
                                 mbl = 16800, 
                                 Qb = Qb, 
                                 Qm = Qm,
                                 Qmon = Qmon,
                                 invalid_time = obs_data$invalid_time, 
                                 obs_times=obs_times) 
      }
  }

    
    return(obs_list)
}