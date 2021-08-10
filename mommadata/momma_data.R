#' Run MOMMA
#'
#' Write output from MOMMA execution on each reach data input.
#'
#' Commandline arguments (optional):
#' name of txt file which contains reach identifiers on each line
run_momma <- function() {

  # Load needed functions
  source("/app/mommadata/input_data.R")
  source("/app/mommadata/output_data.R")
  source("/app/mommadata/momma/function.find.rating.break.R")
  source("/app/mommadata/momma/function.find.zero.flow.stage.R")
  source("/app/mommadata/momma/function.constrain.momma.nb.x.R")
  source("/app/mommadata/momma/function.MOMMA.confluence.swot.v3.constrain.option.R")
  
  # I/O directories
  input_dir <- file.path("/mnt", "data", "input")
  output_dir <- file.path("/mnt", "data", "output")

  # Identify reach files to process
  args <- R.utils::commandArgs(trailingOnly = TRUE)
  reach_file <- ifelse(is.null(args), "reaches.txt", args[1])
  io_data <- get_reach_files(file.path(input_dir, reach_file))

  # Get SWOT and SoS input data
  reach_data <- get_input_data(swot_file = file.path(input_dir, "swot", io_data$swot_file),
                               sos_file = file.path(input_dir, "sos", io_data$sos_file))

  # Create empty placeholder list
  momma_results <- create_momma_list(length(reach_data$nt))

  # Run MOMMA on valid input reach data
  if (reach_data$valid == TRUE) {
    momma_results <- momma(stage = reach_data$wse,
                           width = reach_data$width,
                           slope = reach_data$slope2,
                           MBL_prior = reach_data$mbl,
                           Qb_prior = reach_data$Qb,
                           Qm_prior = reach_data$Qm,
                           Yb_prior = reach_data$Db)
  }

  # Write posteriors to netCDF
  write_netcdf(reach_data, momma_results, output_dir)

}

#' Identify reach and locate SWOT and SoS files.
#'
#' @return list of swot file and sos file
get_reach_files <- function(reach_txt){
  # Get reach identifier from array environment variable
  line_no = strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
  reach_df = read.table(reach_txt, header = FALSE, sep = "\n")
  reach = reach_df[line_no,]
  return(list(swot_file = paste(reach, "_SWOT.nc", sep = ''),
              sos_file = paste(reach, "_SOS.nc", sep = '')))
}

#' Create placeholder MOMMA list that will be used to store results.
#'
#' @param nt number of time steps
#'
#' @return list of data and diagnostics
create_momma_list <- function(nt) {
  # Create empty vector placeholder
  nt_vector <- rep(NA, nt)

  # Create empty data list
  data = list(stage = nt_vector,
              width = nt_vector,
              slope = nt_vector,
              Qgage = nt_vector,
              seg = nt_vector,
              n = nt_vector,
              Y = nt_vector,
              v = nt_vector,
              Q = nt_vector,
              Q.constrained = nt_vector)

  # Create empty diagnostics list
  output = list(gage_constrained = NA,
                input_MBL_prior = NA,
                input_Qm_prior = NA,
                input_Qb_prior = NA,
                input_Yb_prior = NA,
                input_known_ezf = NA,
                input_known_bkfl_stage = NA,
                input_known_nb_seg1 = NA,
                input_known_x_seg1 = NA,
                Qgage_constrained_nb_seg1 = NA,
                Qgage_constrained_x_seg1 = NA,
                input_known_nb_seg2 = NA,
                input_known_x_seg2 = NA,
                Qgage_constrained_nb_seg2 = NA,
                Qgage_constrained_x_seg2 = NA,
                n_bkfl_Qb_prior = NA,
                n_bkfl_final_used = NA,
                vel_bkfl_Qb_prior = NA,
                vel_bkfl_diag_MBL = NA,
                Froude_bkfl_diag_Smean = NA,
                width_bkfl_empirical = NA,
                width_bkfl_solved_obs = NA,
                depth_bkfl_solved_obs = NA,
                depth_bkfl_diag_MBL = NA,
                depth_bkfl_diag_Wb_Smean = NA,
                zero_flow_stage = NA,
                bankfull_stage = NA,
                Qmean_prior = NA,
                Qmean_momma = NA,
                Qmean_momma.constrained = NA)

  # Return placeholder list
  return(list(data = data, output = output))
}

run_momma()