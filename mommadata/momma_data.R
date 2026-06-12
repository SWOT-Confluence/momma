# Imports
library(reticulate)
library(optparse)

library(purrr)
library(dplyr)
library(hydroGOF)

# Local
source("/app/mommadata/input_data.R")
source("/app/mommadata/output_data.R")
source("/app/mommadata/momma/function.find.rating.break.R")
source("/app/mommadata/momma/function.find.zero.flow.stage.newfunH.R")
source("/app/mommadata/momma/function.constrain.momma.nb.x.R")
source("/app/mommadata/momma/function.calibrate.Qmonthly.optim.R")
source("/app/mommadata/momma/function.MOMMA.node.Qmonthly.R")

# example local deployment
#sudo docker run -v /mnt/input/:/mnt/data/input -v /mnt/flpe/momma:/mnt/data/output -v ~/.aws:/root/.aws momma -r /mnt/data/input/reaches.json -m 3 -b confluence-sos/unconstrained/0000 -i 5

PYTHON_EXE = "/usr/bin/python3"
PYTHON_FILE = "/app/sos_read/sos_read.py"
TMP_PATH = "/tmp"

#' Identify reach and locate SWOT and SoS files.
#'
#' Download SoS file to TEMP_PATH and return full path to download.
#'
#' @param input_dir string path to input directory
#' @param reaches_json name of json reach file
#'
#' @return list of swot file and sos file
get_reach_files <- function(input_dir, reaches_json, index, bucket_key){

  # Get reach data from index
  json_data <- rjson::fromJSON(file=file.path(input_dir, reaches_json))[[index]]

  if (bucket_key != "") {
    # Download the SoS file and reference the file path
    use_python(PYTHON_EXE)
    source_python(PYTHON_FILE)

    sos_filepath <- file.path(TMP_PATH, json_data$sos)
    download_sos(bucket_key, sos_filepath)
    reach_list <- list(reach_id = json_data$reach_id,
                      swot_file = file.path(input_dir, "swot", json_data$swot),
                      sos_file = sos_filepath)
  } else {
    reach_list <- list(reach_id = json_data$reach_id,
                      swot_file = file.path(input_dir, "swot", json_data$swot),
                      sos_file = file.path(input_dir, "sos", json_data$sos))
  }

  return(reach_list)

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


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


robust_reach_mean <- function(x, min_n = 3, ratio_limit = 5) {
  x <- x[is.finite(x)]

  if (length(x) < min_n) return(NA_real_)

  med <- median(x, na.rm = TRUE)

  if (med <= 0) {
    mad_val <- mad(x, center = med, constant = 1, na.rm = TRUE)
    if (mad_val == 0) return(mean(x, na.rm = TRUE))
    keep <- abs(x - med) <= 3 * mad_val
  } else {
    keep <- x <= ratio_limit * med
  }

  x_keep <- x[keep]

  if (length(x_keep) < min_n) return(NA_real_)

  mean(x_keep, na.rm = TRUE)
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

  option_list <- list(
    make_option(c("-i", "--index"), type = "integer", default = -256, help = "Index to run on"),
    make_option(c("-b", "--bucket_key"), type = "character", default = "", help = "Bucket key to find the sos"),
    make_option(c("-r", "--reaches_json"), type = "character", default = NULL, help = "Name of reaches.json"),
    make_option(c("-m", "--min_nobs"), type = "character", default = NULL, help = "Minimum number of observations for a reach to have to be considered valid"),
    make_option(c("-c", "--constrained"), action = "store_true", default = FALSE, help = "Indicate constrained run")
  )

  opt_parser <- OptionParser(option_list = option_list)
  opts <- parse_args(opt_parser)
  bucket_key <- opts$bucket_key
  reaches_json <- opts$reaches_json
  min_nobs <- as.numeric(opts$min_nobs)
  constrained <- opts$constrained

  # Parse index
  index <- opts$index

  # Check if we are running via env variable...
  if (index == -256){
    index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX"))
  }

  index <- index + 1    # Add 1 to AWS 0-based index


  print(paste("bucket_key: ", bucket_key))
  print(paste("index: ", index))
  print(paste("reaches_json: ", reaches_json))
  print(paste("min_nobs: ", min_nobs))
  print(paste("constrained: ", constrained))

  io_data <- get_reach_files(input_dir, reaches_json, index, bucket_key)
  print(paste("reach_id: ", io_data$reach_id))
  print(paste("swot_file: ", io_data$swot_file))
  print(paste("sos_file: ", io_data$sos_file))

  # Get SWOT and SoS input data
  reach_data <- get_input_data(swot_file = io_data$swot_file,
                               sos_file = io_data$sos_file,
                               reach_id = io_data$reach_id,
                               min_nobs = min_nobs,
                               constrained = constrained)

  # Create empty placeholder list
  momma_results <- create_momma_list(length(reach_data$nt))

  # shape parameter, r 
  shape.param <- 0.8 
  # r <= 1, convex shape
  # r == 2, parabolic shape
  # r => 10, rectangular shape
    

  # Run MOMMA on valid input reach data
  momma_results_node <- vector("list", length(reach_data))
  for (i in 1:length(reach_data)) {
    if (reach_data[[i]]$valid == TRUE) {
        print('running momma')
        momma_results_node[[i]] <- momma(node_id = reach_data[[i]]$node_id,
                                         stage = reach_data[[i]]$wse,
                                         width = reach_data[[i]]$width,
                                         slope = reach_data[[i]]$slope2,
                                         shape.param = shape.param,
                                         Qb_prior = reach_data[[i]]$Qb,
                                         Qm_prior = reach_data[[i]]$Qm,
                                         Qmon_prior = reach_data[[i]]$Qmon,
                                         Yb_prior = reach_data[[i]]$db,
                                         obs_times = reach_data[[i]]$obs_times,
                                         invalid_time = reach_data[[i]]$invalid_time, 
                                         Qgage = reach_data[[i]]$Qgage,
                                         constrain = constrained)

        valid_times <- reach_data[[i]]$obs_times[-reach_data[[i]]$invalid_time]
        valid_dates <- sub("T.*", "", valid_times)
        momma_results_node[[i]]$data$date <- valid_dates

    }else{
        print('decided not to run')
        momma_results_node[[i]] <- NULL
    }
  }

  # =========================================================================
  # Average reach_data inputs across nodes by date
  # =========================================================================
  valid_reach_data <- keep(reach_data, ~ isTRUE(.x$valid))

  if (length(valid_reach_data) == 0) {
      return(NULL)
  }

  # base structure
  template <- valid_reach_data[[1]]
  n_time <- length(template$nt)

  reach_node_full <- map_dfr(valid_reach_data, function(x) {

      # NA vector
      wse_full <- rep(NA_real_, n_time)
      width_full <- rep(NA_real_, n_time)
      slope2_full <- rep(NA_real_, n_time)
    
      valid_idx <- setdiff(seq_len(n_time), x$invalid_time)
    
      wse_full[valid_idx] <- as.numeric(x$wse)
      width_full[valid_idx] <- as.numeric(x$width)
      slope2_full[valid_idx] <- as.numeric(x$slope2)
    
      tibble(
        nt = template$nt,
        time_str = template$obs_times,
        wse = wse_full,
        width = width_full,
        slope2 = slope2_full
      )
  })

  reach_input_avg_df <- reach_node_full %>%
    group_by(nt, time_str) %>%
    summarise(
        n_wse = sum(!is.na(wse)),
        n_width = sum(!is.na(width)),
        n_slope2 = sum(!is.na(slope2)),

        wse = if (n_wse >= 3) mean(wse, na.rm = TRUE) else NA_real_,
        width = if (n_width >= 3) mean(width, na.rm = TRUE) else NA_real_,
        slope2 = if (n_slope2 >= 3) mean(slope2, na.rm = TRUE) else NA_real_,

        .groups = "drop"
    )

  reach_data_avg <- template

  reach_data_avg$wse <- reach_input_avg_df$wse
  reach_data_avg$width <- reach_input_avg_df$width
  reach_data_avg$slope2 <- reach_input_avg_df$slope2

  reach_data_avg$nt <- reach_input_avg_df$nt
  reach_data_avg$time_str <- reach_input_avg_df$time_str
  reach_data_avg$obs_times <- reach_input_avg_df$time_str

  reach_data_avg$invalid_time <- which(
      is.na(reach_data_avg$wse) |
      is.na(reach_data_avg$width) |
      is.na(reach_data_avg$slope2)
  )

  # check valid time
  reach_data_avg$valid <- length(reach_data_avg$invalid_time) < length(reach_data_avg$nt)

  reach_df <- tibble(time_str = reach_data_avg$time_str,
                     date = suppressWarnings(as.Date(ifelse(time_str == "no_data", NA, sub("T.*", "", time_str)))),
                     nt = reach_data_avg$nt,
                     wse = reach_data_avg$wse,
                     width = reach_data_avg$width,
                     slope = reach_data_avg$slope2
                    )
        
  
  valid_momma <- momma_results_node[!map_lgl(momma_results_node, is.null)]
        
  if (length(valid_momma) == 0) {
      mean_by_date2 <- tibble(
      date = as.Date(character()),
      seg = numeric(),
      Qgage = numeric(),
      n = numeric(),
      Y = numeric(),
      v = numeric(),
      Q = numeric(),
      Q.constrained = numeric()
      )
  } else {
      all_node_data <- map_dfr(valid_momma, ~ .x$data)

  mean_by_date <- all_node_data %>%
    group_by(date) %>%
    summarise(
      seg = Mode(seg),
      across(
        c(stage, width, slope, Qgage, nb, x, n, Y, v, Q.constrained),
        ~ {
          x <- as.numeric(.x)
          if (sum(!is.na(x)) >= 3) mean(x, na.rm = TRUE) else NA_real_
        }
      ),
      Q = robust_reach_mean(Q, min_n = 3, ratio_limit = 5),
      .groups = "drop"
    )

  mean_by_date2 <- mean_by_date %>%
    mutate(date = as.Date(date)) %>%
    select(-width, -slope)
  }

        

  result <- left_join(reach_df,
                      mean_by_date2,
                      by = "date"
                     )


  reach_data_avg[c("node_id")] <- NULL

        
  final_result <- list(
      data = data.frame(stage = as.numeric(result$wse),
                        width = as.numeric(result$width),
                        slope = as.numeric(result$slope),
                        Qgage = as.numeric(result$Qgage),
                        seg = as.numeric(result$seg),
                        n = as.numeric(result$n),
                        nb = as.numeric(result$nb),
                        x = as.numeric(result$x),
                        Y = as.numeric(result$Y),
                        v = as.numeric(result$v),
                        Q = as.numeric(result$Q),
                        Q.constrained = as.numeric(result$Q.constrained)),
      # ******************** TODO: All variables are currently set to NA and need to be updated later ********************
      output = setNames(
          as.list(rep(NA_real_, 27)),
          c("gage_constrained", "input_Qm_prior", "input_Qb_prior", "input_Yb_prior",
              "input_known_ezf", "input_known_bkfl_stage",
              "input_known_nb_seg1", "input_known_x_seg1",
              "Qgage_constrained_nb_seg1", "Qgage_constrained_x_seg1",
              "input_known_nb_seg2", "input_known_x_seg2",
              "Qgage_constrained_nb_seg2", "Qgage_constrained_x_seg2",
              "n_bkfl_Qb_prior", "n_bkfl_slope", "vel_bkfl_Qb_prior",
              "Froude_bkfl_diag_Smean", "width_bkfl_solved_obs",
              "depth_bkfl_solved_obs", "depth_bkfl_diag_Wb_Smean",
              "zero_flow_stage", "bankfull_stage", "Qmean_prior",
              "Qmean_momma", "Qmean_momma.constrained", "width_stage_corr"
            )
      )
  )

  # Write posteriors to netCDF
  write_netcdf(reach_data_avg, final_result, output_dir)
}


run_momma()
