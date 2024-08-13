#' Write netCDF
#'
#' Writes netCDF of posterior data to output directory labelled with reach
#' identifier.
#'
#' @param reach_data list of reach data (observations)
#' @param momma_list list of posterior data
#' @param output_dir string path to output directory
write_netcdf <- function(reach_data, momma_list, output_dir) {

  # Prep data for writing
  if (reach_data$valid == TRUE) {
    # Convert momma_list data data.frame into a named list
    momma_list$data <- convert_to_list(momma_list$data)
    # Concatenate invalid nodes back into valid posterior reach data
    momma_list <- concatenate_invalid(momma_list, reach_data$invalid_time)
    # Replace invalid output with NA
    momma_list <- replace_invalid(momma_list)
  }
  # Determine if output has made the reach invalid
  reach_data$valid <- is_valid(momma_list, length(reach_data$nt))

  # Create NetCDF file
  nc_file <- paste(output_dir, paste0(reach_data$reach_id, "_momma.nc"), sep=.Platform$file.sep)
  nc_out <- RNetCDF::create.nc(nc_file, format="netcdf4")

  # Global attributes
  if (reach_data$valid == TRUE) valid = 1 else valid = 0
  RNetCDF::att.put.nc(nc_out, "NC_GLOBAL", "valid", "NC_INT", valid)
  RNetCDF::att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_INT64", reach_data$reach_id)
  RNetCDF::att.put.nc(nc_out, "NC_GLOBAL", "time_str", "NC_STRING", reach_data$obs_times)
  # Dimensions
  RNetCDF::dim.def.nc(nc_out, "nt", length(reach_data$nt))
  RNetCDF::var.def.nc(nc_out, "nt", "NC_INT", "nt")
  RNetCDF::att.put.nc(nc_out, "nt", "units", "NC_STRING", "time")
  RNetCDF::var.put.nc(nc_out, "nt", reach_data$nt)

  # Write data
  write_data(nc_out, momma_list)

  # Close nc file
  RNetCDF::close.nc(nc_out)
}


write_data <- function(nc_out, momma_list) {

  fill = -999999999999

  # Data variable
  RNetCDF::var.def.nc(nc_out, "stage", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "stage", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "stage", as.numeric(momma_list$data$stage))

  RNetCDF::var.def.nc(nc_out, "width", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "width", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "width", as.numeric(momma_list$data$width))

  RNetCDF::var.def.nc(nc_out, "slope", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "slope", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "slope", as.numeric(momma_list$data$slope))

  RNetCDF::var.def.nc(nc_out, "Qgage", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "Qgage", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qgage", as.numeric(momma_list$data$Qgage))

  RNetCDF::var.def.nc(nc_out, "seg", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "seg", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "seg", as.numeric(momma_list$data$seg))

  RNetCDF::var.def.nc(nc_out, "n", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "n", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "n", as.numeric(momma_list$data$n))

  RNetCDF::var.def.nc(nc_out, "nb", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "nb", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "nb", as.numeric(momma_list$data$nb))

  RNetCDF::var.def.nc(nc_out, "x", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "x", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "x", as.numeric(momma_list$data$x))

  RNetCDF::var.def.nc(nc_out, "Y", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "Y", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Y", as.numeric(momma_list$data$Y))

  RNetCDF::var.def.nc(nc_out, "v", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "v", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "v", as.numeric(momma_list$data$v))

  RNetCDF::var.def.nc(nc_out, "Q", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "Q", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Q", as.numeric(momma_list$data$Q))

  RNetCDF::var.def.nc(nc_out, "Q_constrained", "NC_DOUBLE", "nt")
  RNetCDF::att.put.nc(nc_out, "Q_constrained", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Q_constrained", as.numeric(momma_list$data$Q.constrained))

  # Output variables
  RNetCDF::var.def.nc(nc_out, "gage_constrained", "NC_DOUBLE", NA)

  RNetCDF::att.put.nc(nc_out, "gage_constrained", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "gage_constrained", as.numeric(momma_list$output$gage_constrained))

  RNetCDF::var.def.nc(nc_out, "input_Qm_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_Qm_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_Qm_prior", as.numeric(momma_list$output$input_Qm_prior))

  RNetCDF::var.def.nc(nc_out, "input_Qb_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_Qb_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_Qb_prior", as.numeric(momma_list$output$input_Qb_prior))

  RNetCDF::var.def.nc(nc_out, "input_Yb_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_Yb_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_Yb_prior", as.numeric(momma_list$output$input_Yb_prior))

  RNetCDF::var.def.nc(nc_out, "input_known_ezf", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_ezf", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_ezf", as.numeric(momma_list$output$input_known_ezf))

  RNetCDF::var.def.nc(nc_out, "input_known_bkfl_stage", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_bkfl_stage", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_bkfl_stage", as.numeric(momma_list$output$input_known_bkfl_stage))

  RNetCDF::var.def.nc(nc_out, "input_known_nb_seg1", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_nb_seg1", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_nb_seg1", as.numeric(momma_list$output$input_known_nb_seg1))

  RNetCDF::var.def.nc(nc_out, "input_known_x_seg1", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_x_seg1", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_x_seg1", as.numeric(momma_list$output$input_known_nb_seg1))

  RNetCDF::var.def.nc(nc_out, "Qgage_constrained_nb_seg1", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qgage_constrained_nb_seg1", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qgage_constrained_nb_seg1", as.numeric(momma_list$output$Qgage_constrained_nb_seg1))

  RNetCDF::var.def.nc(nc_out, "Qgage_constrained_x_seg1", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qgage_constrained_x_seg1", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qgage_constrained_x_seg1", as.numeric(momma_list$output$Qgage_constrained_x_seg1))

  RNetCDF::var.def.nc(nc_out, "input_known_nb_seg2", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_nb_seg2", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_nb_seg2", as.numeric(momma_list$output$input_known_nb_seg2))

  RNetCDF::var.def.nc(nc_out, "input_known_x_seg2", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "input_known_x_seg2", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "input_known_x_seg2", as.numeric(momma_list$output$input_known_x_seg2))

  RNetCDF::var.def.nc(nc_out, "Qgage_constrained_nb_seg2", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qgage_constrained_nb_seg2", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qgage_constrained_nb_seg2", as.numeric(momma_list$output$Qgage_constrained_nb_seg2))

  RNetCDF::var.def.nc(nc_out, "Qgage_constrained_x_seg2", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qgage_constrained_x_seg2", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qgage_constrained_x_seg2", as.numeric(momma_list$output$Qgage_constrained_x_seg2))

  RNetCDF::var.def.nc(nc_out, "n_bkfl_Qb_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "n_bkfl_Qb_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "n_bkfl_Qb_prior", as.numeric(momma_list$output$n_bkfl_Qb_prior))

  RNetCDF::var.def.nc(nc_out, "n_bkfl_slope", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "n_bkfl_slope", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "n_bkfl_slope", as.numeric(momma_list$output$n_bkfl_slope))

  RNetCDF::var.def.nc(nc_out, "vel_bkfl_Qb_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "vel_bkfl_Qb_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "vel_bkfl_Qb_prior", as.numeric(momma_list$output$vel_bkfl_Qb_prior))

  RNetCDF::var.def.nc(nc_out, "Froude_bkfl_diag_Smean", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Froude_bkfl_diag_Smean", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Froude_bkfl_diag_Smean", as.numeric(momma_list$output$Froude_bkfl_diag_Smean))

  RNetCDF::var.def.nc(nc_out, "width_bkfl_solved_obs", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "width_bkfl_solved_obs", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "width_bkfl_solved_obs", as.numeric(momma_list$output$width_bkfl_solved_obs))

  RNetCDF::var.def.nc(nc_out, "depth_bkfl_solved_obs", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "depth_bkfl_solved_obs", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "depth_bkfl_solved_obs", as.numeric(momma_list$output$depth_bkfl_solved_obs))

  RNetCDF::var.def.nc(nc_out, "depth_bkfl_diag_Wb_Smean", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "depth_bkfl_diag_Wb_Smean", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "depth_bkfl_diag_Wb_Smean", as.numeric(momma_list$output$depth_bkfl_diag_Wb_Smean))

  RNetCDF::var.def.nc(nc_out, "zero_flow_stage", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "zero_flow_stage", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "zero_flow_stage", as.numeric(momma_list$output$zero_flow_stage))

  RNetCDF::var.def.nc(nc_out, "bankfull_stage", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "bankfull_stage", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "bankfull_stage", as.numeric(momma_list$output$bankfull_stage))

  RNetCDF::var.def.nc(nc_out, "Qmean_prior", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qmean_prior", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qmean_prior", as.numeric(momma_list$output$Qmean_prior))

  RNetCDF::var.def.nc(nc_out, "Qmean_momma", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qmean_momma", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qmean_momma", as.numeric(momma_list$output$Qmean_momma))
# width_stage_corr
  RNetCDF::var.def.nc(nc_out, "Qmean_momma.constrained", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "Qmean_momma.constrained", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "Qmean_momma.constrained", as.numeric(momma_list$output$Qmean_momma.constrained))


  RNetCDF::var.def.nc(nc_out, "width_stage_corr", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc_out, "width_stage_corr", "_FillValue", "NC_DOUBLE", fill)
  RNetCDF::var.put.nc(nc_out, "width_stage_corr", as.numeric(momma_list$output$width_stage_corr))
}

#' Convert data.frame parameter to a named list
#'
#' @param df data.frame to convert to a list
#'
#' @return named list of data.frame contents
convert_to_list <- function(df) {
  l <- list()
  for (i in 1:ncol(df)) {
    l[[i]] <- df[,i]
  }
  names(l) <- colnames(df)
  return(l)
}

#' Insert NA values back into MOMMA list vectors to account for invalid
#' nodes.
#'
#' @param momma_list list of posterior data vectors
#' @param invalid_time list of invalid time indexes
#'
#' @return list of MOMMA data with NA in place of invalid nodes
concatenate_invalid <- function(momma_list, invalid_time) {

  # Time-level data
  for (index in invalid_time) {
    momma_list$data$stage <- append(momma_list$data$stage, NA, after = index - 1)
    momma_list$data$width <- append(momma_list$data$width, NA, after = index - 1)
    momma_list$data$slope <- append(momma_list$data$slope, NA, after = index - 1)
    momma_list$data$seg <- append(momma_list$data$seg, NA, after = index - 1)
    momma_list$data$n <- append(momma_list$data$n, NA, after = index - 1)
    momma_list$data$Y <- append(momma_list$data$Y, NA, after = index - 1)
    momma_list$data$v <- append(momma_list$data$v, NA, after = index - 1)
    momma_list$data$Q <- append(momma_list$data$Q, NA, after = index - 1)
    momma_list$data$Qgage <- append(momma_list$data$Qgage, NA, after = index - 1)
    momma_list$data$Q.constrained <- append(momma_list$data$Q.constrained, NA, after = index - 1)
    momma_list$data$width_stage_corr <- append(momma_list$data$width_stage_corr, NA, after = index - 1)
  }

  return(momma_list)

}

#' Replace invalid NaN values with NA for n, Y, v, and Q.
#'
#' @param momma_list list of data and diagnostic results
replace_invalid <- function(momma_list) {
  momma_list$data$n[is.nan(momma_list$data$n)] <- NA
  momma_list$data$Y[is.nan(momma_list$data$Y)] <- NA
  momma_list$data$v[is.nan(momma_list$data$v)] <- NA
  momma_list$data$Q[is.nan(momma_list$data$Q)] <- NA
  momma_list$data$Qgage[is.nan(momma_list$data$Qgage)] <- NA
  momma_list$data$Q.constrained[is.nan(momma_list$data$Q.constrained)] <- NA
  momma_list$data$width_stage_corr[is.nan(momma_list$data$width_stage_corr)] <- NA
  return(momma_list)
}

#' Determine if output stored in momma_list is valid.
#'
#' @param momma_list list of data
#' @param nt integer time steps
is_valid <- function(momma_list, nt) {
  if (sum(is.na(momma_list$data$n)) == nt || sum(is.na(momma_list$data$Y)) == nt
      || sum(is.na(momma_list$data$v)) == nt || sum(is.na(momma_list$data$Q)) == nt) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
