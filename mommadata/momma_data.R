# Local
source("/app/mommadata/input_data.R")
source("/app/mommadata/output_data.R")
source("/app/mommadata/momma/function.find.rating.break.R")
source("/app/mommadata/momma/function.find.zero.flow.stage.R")
source("/app/mommadata/momma/function.constrain.momma.nb.x.R")
source("/app/mommadata/momma/function.calibrate.Qmean.R")
source("/app/mommadata/momma/function.MOMMA.confluence.swot.v3.2.R")

#' Identify reach and locate SWOT and SoS files.
#'
#' @param input_dir string path to input directory
#' @param reaches_json name of json reach file
#'
#' @return list of swot file and sos file
get_reach_files <- function(input_dir, reaches_json){
  # Get reach identifier from array environment variable
  index = strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
  json_data <- rjson::fromJSON(file=reaches_json)[[index]]
  return(list(reach_id = json_data$reach_id,
              swot_file = file.path(input_dir, "swot", json_data$swot),
              sos_file = file.path(input_dir, "sos", json_data$sos)))
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
              nb = nt_vector,
              x = nt_vector,
              Y = nt_vector,
              v = nt_vector,
              Q = nt_vector,
              Q.constrained = nt_vector)
  
  # Create empty diagnostics list
  output = list(gage_constrained = NA,
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
                n_bkfl_slope = NA,
                vel_bkfl_Qb_prior = NA,
                Froude_bkfl_diag_Smean = NA,
                width_bkfl_solved_obs = NA,
                depth_bkfl_solved_obs = NA,
                depth_bkfl_diag_Wb_Smean = NA,
                zero_flow_stage = NA,
                bankfull_stage = NA,
                width_stage_corr = NA,
                Qmean_prior = NA,
                Qmean_momma = NA,
                Qmean_momma.constrained = NA)
  
  # Return placeholder list
  return(list(data = data, output = output))
}

#' Run MOMMA
#'
#' Write output from MOMMA execution on each reach data input.
#'
#' Commandline arguments (optional):
#' name of txt file which contains reach identifiers on each line
run_momma <- function() {
  
  # I/O directories
  input_dir <- file.path("/mnt", "data", "input")
  output_dir <- file.path("/mnt", "data", "output")
  
  # Identify reach files to process
  args <- R.utils::commandArgs(trailingOnly = TRUE)
  # reach_file <- ifelse(is.null(args), "reaches.json", args[1])

  

  if (length(args)>=1){
      reach_file = file.path(input_dir, paste('reaches_',strtoi(args[1]),'.json', sep = ""))
  } else{
      reach_file = file.path(input_dir, 'reaches.json')
  }

  print("reach")
  print(reach_file)
  io_data <- get_reach_files(input_dir, reach_file)
  print(as.character(io_data$reach_id))
  # Get SWOT and SoS input data
  reach_data <- get_input_data(swot_file = io_data$swot_file,
                               sos_file = io_data$sos_file,
                               reach_id = io_data$reach_id)
  
  # Create empty placeholder list
  momma_results <- create_momma_list(length(reach_data$nt))
  
  # Run MOMMA on valid input reach data
  if (reach_data$valid == TRUE) {
    momma_results <- momma(stage = reach_data$wse,
                           width = reach_data$width,
                           slope = reach_data$slope2,
                           Qb_prior = reach_data$Qb,
                           Qm_prior = reach_data$Qm,
                           Yb_prior = reach_data$db)
  }
  
  # Write posteriors to netCDF
  write_netcdf(reach_data, momma_results, output_dir)
}

run_momma()
