#' Write netCDF
#'
#' Writes netCDF of posterior data to output directory labelled with reach
#' identifier.
#'
#' @param reach_data list of reach data (observations)
#' @param momma_list list of posterior data
#' @param output_dir string path to output directory
write_netcdf <- function(reach_data, momma_list, output_dir) {

  # Define dimension
  nt_dim <- ncdf4::ncdim_def(name = "nt", units = "", vals = reach_data$nt, create_dimvar=TRUE)

  # Create list of variables
  var_list <- create_vars(reach_data$reachid, nt_dim)

  # Create netcdf
  nc_out <- create_nc_file(reach_data$reachid, var_list, output_dir)

  # Concatenate invalid nodes back into valid posterior reach data
  if (reach_data$valid == TRUE) {
    momma_list <- concatenate_invalid(momma_list, reach_data$invalid_time)
    # Replace invalid output with NA
    momma_list <- replace_invalid(momma_list)

  }

  # Determine if output has made the reach invalid
  reach_data$valid <- is_valid(momma_list, length(reach_data$nt))

  # Write valid and reach id global attributes and posteriors to netcdf
  if (reach_data$valid == TRUE) valid <- 1 else valid <- 0
  ncdf4::ncatt_put(nc_out, 0, "valid", valid)
  ncdf4::ncatt_put(nc_out, 0, "reachid", reach_data$reachid)
  write_vars(var_list, momma_list, nc_out)

  # Close nc file
  ncdf4::nc_close(nc_out)
}

#' Create a list of netcdf4 variables
#'
#' Create list of data and diagnostic netcdf variables grouped by reachid.
#'
#' @param reachid string reach identifier
#' @param nt_dim ncdim
create_vars <- function(reachid, nt_dim) {

  # Data variable
  stage <- ncdf4::ncvar_def(paste(reachid, "stage", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  width <- ncdf4::ncvar_def(paste(reachid, "width", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  slope <- ncdf4::ncvar_def(paste(reachid, "slope", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  Qgage <- ncdf4::ncvar_def(paste(reachid, "Qgage", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  seg <- ncdf4::ncvar_def(paste(reachid, "seg",  sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  n <- ncdf4::ncvar_def(paste(reachid, "n", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  Y <- ncdf4::ncvar_def(paste(reachid, "Y", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  v <- ncdf4::ncvar_def(paste(reachid, "v", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  Q <- ncdf4::ncvar_def(paste(reachid, "Q", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  Q.constrained <- ncdf4::ncvar_def(paste(reachid, "Q_constrained", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")

  # Output variables
  gage_constrained <- ncdf4::ncvar_def(paste(reachid, "gage_constrained", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_MBL_prior <- ncdf4::ncvar_def(paste(reachid, "input_MBL_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_Qm_prior <- ncdf4::ncvar_def(paste(reachid, "input_Qm_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_Qb_prior <- ncdf4::ncvar_def(paste(reachid, "input_Qb_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_Yb_prior <- ncdf4::ncvar_def(paste(reachid, "input_Yb_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_ezf <- ncdf4::ncvar_def(paste(reachid, "input_known_ezf", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_bkfl_stage <- ncdf4::ncvar_def(paste(reachid, "input_known_bkfl_stage", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_nb_seg1 <- ncdf4::ncvar_def(paste(reachid, "input_known_nb_seg1", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_x_seg1 <- ncdf4::ncvar_def(paste(reachid, "input_known_x_seg1", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qgage_constrained_nb_seg1 <- ncdf4::ncvar_def(paste(reachid, "Qgage_constrained_nb_seg1", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qgage_constrained_x_seg1 <- ncdf4::ncvar_def(paste(reachid, "Qgage_constrained_x_seg1", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_nb_seg2 <- ncdf4::ncvar_def(paste(reachid, "input_known_nb_seg2", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  input_known_x_seg2 <- ncdf4::ncvar_def(paste(reachid, "input_known_x_seg2", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qgage_constrained_nb_seg2 <- ncdf4::ncvar_def(paste(reachid, "Qgage_constrained_nb_seg2", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qgage_constrained_x_seg2 <- ncdf4::ncvar_def(paste(reachid, "Qgage_constrained_x_seg2", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  n_bkfl_Qb_prior <- ncdf4::ncvar_def(paste(reachid, "n_bkfl_Qb_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  n_bkfl_final_used <- ncdf4::ncvar_def(paste(reachid, "n_bkfl_final_used", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  vel_bkfl_Qb_prior <- ncdf4::ncvar_def(paste(reachid, "vel_bkfl_Qb_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  vel_bkfl_diag_MBL <- ncdf4::ncvar_def(paste(reachid, "vel_bkfl_diag_MBL", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Froude_bkfl_diag_Smean <- ncdf4::ncvar_def(paste(reachid, "Froude_bkfl_diag_Smean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  width_bkfl_empirical <- ncdf4::ncvar_def(paste(reachid, "width_bkfl_empirical", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  width_bkfl_solved_obs <- ncdf4::ncvar_def(paste(reachid, "width_bkfl_solved_obs", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  depth_bkfl_solved_obs <- ncdf4::ncvar_def(paste(reachid, "depth_bkfl_solved_obs", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  depth_bkfl_diag_MBL <- ncdf4::ncvar_def(paste(reachid, "depth_bkfl_diag_MBL", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  depth_bkfl_diag_Wb_Smean <- ncdf4::ncvar_def(paste(reachid, "depth_bkfl_diag_Wb_Smean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  zero_flow_stage <- ncdf4::ncvar_def(paste(reachid, "zero_flow_stage", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  bankfull_stage <- ncdf4::ncvar_def(paste(reachid, "bankfull_stage", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qmean_prior <- ncdf4::ncvar_def(paste(reachid, "Qmean_prior", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qmean_momma <- ncdf4::ncvar_def(paste(reachid, "Qmean_momma", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  Qmean_momma.constrained <- ncdf4::ncvar_def(paste(reachid, "Qmean_momma.constrained", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")

  # List of all variables
  return(list(stage = stage, width = width, slope = slope, Qgage = Qgage,
              seg = seg, n = n, Y = Y, v = v, Q = Q,
              Q.constrained = Q.constrained, gage_constrained = gage_constrained,
              input_MBL_prior = input_MBL_prior, input_Qm_prior = input_Qm_prior,
              input_Qb_prior = input_Qb_prior, input_Yb_prior = input_Yb_prior,
              input_known_ezf = input_known_ezf,
              input_known_bkfl_stage = input_known_bkfl_stage,
              input_known_nb_seg1 = input_known_nb_seg1,
              input_known_x_seg1 = input_known_x_seg1,
              Qgage_constrained_nb_seg1 = Qgage_constrained_nb_seg1,
              Qgage_constrained_x_seg1 = Qgage_constrained_x_seg1,
              input_known_nb_seg2 = input_known_nb_seg2,
              input_known_x_seg2 = input_known_x_seg2,
              Qgage_constrained_nb_seg2 = Qgage_constrained_nb_seg2,
              Qgage_constrained_x_seg2 = Qgage_constrained_x_seg2,
              n_bkfl_Qb_prior = n_bkfl_Qb_prior,
              n_bkfl_final_used = n_bkfl_final_used,
              vel_bkfl_Qb_prior = vel_bkfl_Qb_prior,
              vel_bkfl_diag_MBL = vel_bkfl_diag_MBL,
              Froude_bkfl_diag_Smean = Froude_bkfl_diag_Smean,
              width_bkfl_empirical = width_bkfl_empirical,
              width_bkfl_solved_obs = width_bkfl_solved_obs,
              depth_bkfl_solved_obs = depth_bkfl_solved_obs,
              depth_bkfl_diag_MBL = depth_bkfl_diag_MBL,
              depth_bkfl_diag_Wb_Smean = depth_bkfl_diag_Wb_Smean,
              zero_flow_stage = zero_flow_stage,
              bankfull_stage = bankfull_stage,
              Qmean_prior = Qmean_prior,
              Qmean_momma = Qmean_momma,
              Qmean_momma.constrained = Qmean_momma.constrained))
}

#
#' Create netCDF output file
#'
#' @param reachid string reach identifier
#' @param var_list list ncvar
#' @param output_dir character
#'
#' @return ncdf4
create_nc_file <- function(reachid, var_list, output_dir) {

  nc_filename <- paste(reachid, "_momma.nc", sep = '')
  nc_file <- paste(output_dir, nc_filename, sep=.Platform$file.sep)
  return(ncdf4::nc_create(nc_file, var_list, force_v4 = TRUE))
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
    momma_list$data$stage <- append(momma_list$stage, NA, after = index - 1)
    momma_list$data$width <- append(momma_list$width, NA, after = index - 1)
    momma_list$data$slope <- append(momma_list$slope, NA, after = index - 1)
    momma_list$data$seg <- append(momma_list$seg, NA, after = index - 1)
    momma_list$data$n <- append(momma_list$n, NA, after = index - 1)
    momma_list$data$Y <- append(momma_list$Y, NA, after = index - 1)
    momma_list$data$v <- append(momma_list$v, NA, after = index - 1)
    momma_list$data$Q <- append(momma_list$Q, NA, after = index - 1)
    momma_list$data$Qgage <- append(momma_list$Qgage, NA, after = index - 1)
    momma_list$data$Q.constrained <- append(momma_list$data$Q.constrained, NA, after = index - 1)
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

#' Write netcdf variables
#'
#' Write data and diagnostic variables to netcdf file.
#'
#' @param var_list list of ncvar
#' @param momma_list list of posterior data
#' @param nc_out ncdf4
write_vars <- function(var_list, momma_list, nc_out) {

  ncdf4::ncvar_put(nc_out, var_list$stage, momma_list$data$stage)
  ncdf4::ncvar_put(nc_out, var_list$width, momma_list$data$width)
  ncdf4::ncvar_put(nc_out, var_list$slope, momma_list$data$slope)
  ncdf4::ncvar_put(nc_out, var_list$seg, momma_list$data$seg)
  ncdf4::ncvar_put(nc_out, var_list$n, momma_list$data$n)
  ncdf4::ncvar_put(nc_out, var_list$Y, momma_list$data$Y)
  ncdf4::ncvar_put(nc_out, var_list$v, momma_list$data$v)
  ncdf4::ncvar_put(nc_out, var_list$Q, momma_list$data$Q)
  ncdf4::ncvar_put(nc_out, var_list$Qgage, momma_list$data$Qgage)
  ncdf4::ncvar_put(nc_out, var_list$Q.constrained, momma_list$data$Q.constrained)

  ncdf4::ncvar_put(nc_out, var_list$gage_constrained, momma_list$output$gage_constrained)
  ncdf4::ncvar_put(nc_out, var_list$input_MBL_prior, momma_list$output$input_MBL_prior)
  ncdf4::ncvar_put(nc_out, var_list$input_Qm_prior, momma_list$output$input_Qm_prior)
  ncdf4::ncvar_put(nc_out, var_list$input_Qb_prior, momma_list$output$input_Qb_prior)
  ncdf4::ncvar_put(nc_out, var_list$input_Yb_prior, momma_list$output$input_Yb_prior)
  ncdf4::ncvar_put(nc_out, var_list$input_known_ezf, momma_list$output$input_known_ezf)
  ncdf4::ncvar_put(nc_out, var_list$input_known_bkfl_stage, momma_list$output$input_known_bkfl_stage)
  ncdf4::ncvar_put(nc_out, var_list$input_known_nb_seg1, momma_list$output$input_known_nb_seg1)
  ncdf4::ncvar_put(nc_out, var_list$input_known_x_seg1, momma_list$output$input_known_x_seg1)
  ncdf4::ncvar_put(nc_out, var_list$Qgage_constrained_nb_seg1, momma_list$output$Qgage_constrained_nb_seg1)
  ncdf4::ncvar_put(nc_out, var_list$Qgage_constrained_x_seg1, momma_list$output$Qgage_constrained_x_seg1)
  ncdf4::ncvar_put(nc_out, var_list$input_known_nb_seg2, momma_list$output$input_known_nb_seg2)
  ncdf4::ncvar_put(nc_out, var_list$input_known_x_seg2, momma_list$output$input_known_x_seg2)
  ncdf4::ncvar_put(nc_out, var_list$Qgage_constrained_nb_seg2, momma_list$output$Qgage_constrained_nb_seg2)
  ncdf4::ncvar_put(nc_out, var_list$Qgage_constrained_x_seg2, momma_list$output$Qgage_constrained_x_seg2)
  ncdf4::ncvar_put(nc_out, var_list$n_bkfl_Qb_prior, momma_list$output$n_bkfl_Qb_prior)
  ncdf4::ncvar_put(nc_out, var_list$n_bkfl_final_used, momma_list$output$n_bkfl_final_used)
  ncdf4::ncvar_put(nc_out, var_list$vel_bkfl_Qb_prior, momma_list$output$vel_bkfl_Qb_prior)
  ncdf4::ncvar_put(nc_out, var_list$vel_bkfl_diag_MBL, momma_list$output$vel_bkfl_diag_MBL)
  ncdf4::ncvar_put(nc_out, var_list$Froude_bkfl_diag_Smean, momma_list$output$Froude_bkfl_diag_Smean)
  ncdf4::ncvar_put(nc_out, var_list$width_bkfl_empirical, momma_list$output$width_bkfl_empirical)
  ncdf4::ncvar_put(nc_out, var_list$width_bkfl_solved_obs, momma_list$output$width_bkfl_solved_obs)
  ncdf4::ncvar_put(nc_out, var_list$depth_bkfl_solved_obs, momma_list$output$depth_bkfl_solved_obs)
  ncdf4::ncvar_put(nc_out, var_list$depth_bkfl_diag_MBL, momma_list$output$depth_bkfl_diag_MBL)
  ncdf4::ncvar_put(nc_out, var_list$depth_bkfl_diag_Wb_Smean, momma_list$output$depth_bkfl_diag_Wb_Smean)
  ncdf4::ncvar_put(nc_out, var_list$zero_flow_stage, momma_list$output$zero_flow_stage)
  ncdf4::ncvar_put(nc_out, var_list$bankfull_stage, momma_list$output$bankfull_stage)
  ncdf4::ncvar_put(nc_out, var_list$Qmean_prior, momma_list$output$Qmean_prior)
  ncdf4::ncvar_put(nc_out, var_list$Qmean_momma, momma_list$output$Qmean_momma)
  ncdf4::ncvar_put(nc_out, var_list$Qmean_momma.constrained, momma_list$output$Qmean_momma.constrained)

}

