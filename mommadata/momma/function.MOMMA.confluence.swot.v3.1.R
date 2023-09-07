# Modified Optimized Manning Method Algorithm (MOMMA)
# Authors: RWDudley, DMBjerklie
# v3.1 October 2022
# MOMMA configured for CONFLUENCE application
# Uses input data (SWOT observations)
#   observed stages (stage, vector)
#   observed widths (width, vector)
#   observed slopes (slope, vector)
# For gage constrained product:
#   gaged flows (Qgage, vector corresponding in time to SWOT observations)
# Uses priors
#   Mean discharge (Qmean), m3/s
#   Bankfull discharge (Qb_prior), m3/s
#   Bankfull depth (Yb_prior), m
# Uses channel shape parameter to assume shape of unobserved channel geometry
# Uses the elevation of zero flow known_ezf,(i.e. channel thalweg) if known or otherwise fixed; default is unknown (NA)
# Uses the bankfull elevation, known_bkfl_stage, if known or otherwise fixed; default is unknown (NA)
# known_bkfl_stage is still subjected to minimum data requirements for the reach
# known_nb_seg[n] and known_x_seg[n] are flow law params if they have already been calibrated and are now known
# constrain: Boolean whether to calibrate to gage data for gage-constrained product
#
momma <- function(stage, width, slope, Qgage = NA, Qm_prior, Qb_prior,
                  Yb_prior = NA, known_ezf = NA, known_bkfl_stage = NA,
                  known_nb_seg1 = NA, known_x_seg1 = NA,
                  known_nb_seg2 = NA, known_x_seg2 = NA,
                  constrain = FALSE){
  #browser()
  #--Functions called-----------------------
  # source("function.find.rating.break.R")
  # source("function.constrain.momma.nb.x.R")
  # source("function.find.zero.flow.stage.R")
  # library('hydroGOF')
  #-----------------------------------------
  
  #--Global variables/settings------------------------------------
  min_nobs <- 5 # minimum number of observations required to entertain making flow calculations
  min_nobs_mean <- 10 # minimum number of observations required to coerce mean of estimated flows to match Qmean_prior
  shape_param <- 2 # channel shape parameter. 2 = parabolic
  # REQUIRE minimum number of obs and delta stage to look for breakpoints
  n.min = 7 # minimum number of obs required to look for breakpoint in stage-width relation
  stage.range.min = 1.5 # meter; minimum range in observed stage required to look for breakpoint in stage-width relation
  # REQUIRE minimum number of obs and delta stage to retain a stage-width relation segment
  n.min.seg = 3 # number of obs in the segment
  stage.range.min.seg = stage.range.min / 2 # meter; range in stage to retain a stage-width relation segment
  breakpoint.method <- "wd.ratio" # method to identify possible bankfull stage
  #---------------------------------------------------------------

  # initialize diagnostic values----------------------
  # compute diagnostic values
  Frbd <- NA # empirical bankfull Froude number, diagnostic
  Ybd_Wb_Smean <- NA # empirical bankfull depth, diagnostic, m
  nb_seg1 <- NA; nb_seg2 <- NA; x_seg1 <- NA; x_seg2 <- NA # gage-constrained params
  #---------------------------------------------------
  
  # Form the dataframe, df
  if (is.na(Qgage[1]) & length(Qgage) == 1){# if there are no in situ gage data
    df <- data.frame(stage, width, slope)
    df$Qgage <- NA
  }else{
    df <- data.frame(stage, width, slope, Qgage)
  }

  # omit NA cases among obs
  df <- df[which(!is.na(df$stage)),]
  df <- df[which(!is.na(df$width)),]
  df <- df[which(!is.na(df$slope)),]

    # omit NaN cases among obs
  df <- df[which(!is.nan(df$stage)),]
  df <- df[which(!is.nan(df$width)),]
  df <- df[which(!is.nan(df$slope)),]

  print("df after things")
  print(df)

  # add columns to receive computations
  df$seg <- 1 # stage-width relation segment number; 1 = below bankfull; 2 = above bankfull
  df$n <- NA # Manning's n
  df$Y <- NA # average depth (m)
  df$v <- NA # average velocity (m/s)
  df$Q <- NA # discharge
  df$Q.constrained <- NA

  # initialize the function return package
  diag <- list(gage_constrained = constrain,
               input_Qm_prior = Qm_prior,
               input_Qb_prior = Qb_prior,
               input_Yb_prior = Yb_prior,
               input_known_ezf = known_ezf,
               input_known_bkfl_stage = known_bkfl_stage,
               input_known_nb_seg1 = known_nb_seg1,
               input_known_x_seg1 = known_x_seg1,
               Qgage_constrained_nb_seg1 = nb_seg1,
               Qgage_constrained_x_seg1 = x_seg1,
               input_known_nb_seg2 = known_nb_seg2,
               input_known_x_seg2 = known_x_seg2,
               Qgage_constrained_nb_seg2 = nb_seg2,
               Qgage_constrained_x_seg2 = x_seg2,
               n_bkfl_Qb_prior = NA,
               n_bkfl_slope = NA,
               vel_bkfl_Qb_prior = NA,
               Froude_bkfl_diag_Smean = NA,
               width_bkfl_solved_obs = NA,
               depth_bkfl_solved_obs = NA,
               depth_bkfl_diag_Wb_Smean = NA,
               zero_flow_stage = NA,
               bankfull_stage = NA,
               Qmean_prior = Qm_prior,
               Qmean_momma = NA,
               Qmean_momma.constrained = NA)

  # retain only positive slopes
  df <- df[which(df$slope > 0),]
  print("processing df")
  print(df)
  # if inadequate data return NA results
  if (nrow(df) < min_nobs | is.null(nrow(df))) {
    cat("INADEQUATE DATA\n")# message print to screen
    pkg <- list(data = df, output = diag)
    return(pkg)
  }

  #browser()
  # if stage and width are not constant, test correlation between them
  if (sd(df$stage) != 0 & sd(df$width) != 0){
    # check that stage and width are positively correlated
    cr <- cor.test(df$stage, df$width, method = 'pearson')
    # if negative correlation return NA results
    # enforces stage and width must increase together
    if (cr$estimate < 0) {
      cat("STAGE-WIDTH DATA NEGATIVELY CORRELATED\n")# message print to screen
      pkg <- list(data = df, output = diag)
      return(pkg)
    }
  }

  # find min and max stage values (in meters)
  stage.min <- min(df$stage)
  stage.max <- max(df$stage)
  stage.range <- stage.max - stage.min

  # compute mean observed slope
  Smean <- mean(df$slope)

  # channel shape coefficient b based on shape param
  b <- 1 - (1 / (1 + shape_param))

  # --------------------------------------------------
  # Determine Bankfull Stage
  # find a bankfull breakpoint in the relation between w^2 and stage
  # If a suitable bankfull breakpoint cannnot be identified, the max obs stage 
  # is assumed to be bankfull stage
  bkfl_stage <- NA
  nsegs <- 1

  if (stage.range > stage.range.min){

    if (is.na(known_bkfl_stage)){
      if (nrow(df) >= n.min & (stage.range > stage.range.min)){
        bkfl_stage <- find.rating.break(df$width, df$stage, shape.param,
                                             method = breakpoint.method)
      }else{
        bkfl_stage <- stage.max
      }
    }else{
      bkfl_stage <- known_bkfl_stage
    }

    # Check whether the break results in segments that don't meet minimums
    # if not, don't use the breakpoint
    if (!is.na(bkfl_stage)){
      seg1 <- df[which((df$stage >= stage.min) & (df$stage <= bkfl_stage)),]
      seg2 <- df[which((df$stage >= bkfl_stage) & (df$stage <= stage.max)),]
      if (nrow(seg1) < n.min.seg | nrow(seg2) < n.min.seg |
          (bkfl_stage - stage.min) < stage.range.min.seg |
          (stage.max - bkfl_stage) < stage.range.min.seg){
        bkfl_stage <- stage.max
      }else{
        # label the above-bankfull segment '2' if a satisfactory bankfull 
        # breakpoint is found
        df$seg[which(df$stage > bkfl_stage)] <- 2
        nsegs <- 2
      }
    }

  }else{
    bkfl_stage <- stage.max
  }
  
  # Determine the corresponding bankfull width from obs
  if (sd(df$stage) == 0){ # if all observed stages are constant
    Wb_obs <- max(df$width[which(df$stage == bkfl_stage)])
  }else{
    apx <- approx(x = df$stage, y = df$width, xout = bkfl_stage)
    Wb_obs <- apx$y
  }
  # --------------------------------------------------
  
  # --------------------------------------------------
  # Determine the zero.stage (EZF)
  if (is.na(known_ezf)){
    # Empirically estimate mean bankfull depth from Bjerklie 2007
    Yb_upper95 <- round(0.10 * (Wb_obs ^ 0.43) * (Smean ^ (-0.28)), 2)
    # Bjerklie, D.M., 2007. Estimating the bankfull velocity and discharge for 
    # rivers using remotely sensed river morphology information. 
    # J. Hydrol. 341 (3–4), 144–155.
    # use Yb_upper95 to limit the depth of the EZF estimate (no deeper than)
    zero.stage.floor <- bkfl_stage - Yb_upper95 / b
    
    if (sd(df$width) == 0){ # special case if width is constant
      # If width is constant (a canal), zero.stage is set to zero.stage.floo
      zero.stage <- zero.stage.floor
      Wb <- df$width[1]
    }else{
      # function find.zero.stage makes an estimate of the stage of zero flow 
      # via extrapolation of the width^2 - elevation relation
      zero.stage <- find.zero.stage(df, nsegs)
      # enforce zero.stage.floor
      zero.stage <- max(c(zero.stage, zero.stage.floor))
      Wb <- Wb_obs
    }
    # Backstop: enforce upper limit for EZF
    # must be at least 1 meter deeper than lowest observed stage
    if (round(zero.stage, 2) >= round(stage.min - 1.0, 2)){
      zero.stage <- round(stage.min, 2) - 1.0
    }
    
  }else{# if EZF (zero.h) is known from SoS
    zero.stage <- known_ezf
    Wb <- Wb_obs
  }
  
  # zero.stage now known. Compute mean bankfull depth
  Yb <- b * (bkfl_stage - zero.stage)
  # --------------------------------------------------
  
  # Bankfull velocity
  Vb_derived_from_Qb_prior <- Qb_prior / (Wb * Yb) # m/s
  print("bankful velocity")
  print("qb")
  print(Qb_prior)
  print("wb")
  print(Wb)
  print("yb")
  print(Yb)
  # Bankfull Mannings n for all obs
  nb <- (Yb ^ (2/3) * Smean ^ 0.5) / Vb_derived_from_Qb_prior
  nb_prior <- nb

  print("computing bankful manning n")
  print("y")
  print(Yb)
  print("smean")
  print(Smean)
  print("vbder")
  print(Vb_derived_from_Qb_prior)


  # estimate nb value from empirical formula using slope
  # https://il.water.usgs.gov/proj/nvalues/equations.shtml?equation=09-bray1
  nb_slope <- 0.094 * (Smean ^ (1 / 6)) # Bray and Davar equation
  # take the mean of the two
  nb_mean <- (nb_prior + nb_slope) / 2
  # enforce min/max
  nb_minmax <- max(0.002, min(0.20, nb_mean))
  # assign to dataframe
  df$nb <- nb_minmax

  # Compute mean depths for all obs
  df$Y <- (df$stage - zero.stage) * b # m

  # assign default value for exponent x
  df$x <- 1.0

  # compute Manning's n  
  df$n <- round(df$nb * ((stage.max - zero.stage)/(df$stage - zero.stage)) ^ df$x, 4)
  print("computing mannings n")
  print("nb")
  print(df$nb)
  print("df$stage")
  print(df$stage)
  print("zero")
  print(zero.stage)
  print("max")
  print(stage.max)

  # enforce minimum n-value to avoid n values approaching zero
  df$n[which(df$n < 0.002)] <- 0.002

  # compute mean velocities for all obs
  df$v <- (df$Y ^ (2/3) * df$slope ^ 0.5) / df$n # m/s
  print("computing mean velocity")
  print("slope")
  print(df$slope)
  print("n")
  print(df$n)


  # compute discharges for all obs
  df$Q <- df$width * df$Y * df$v # m3/s
  print("computing q")
  print("w")
  print(df$width)
  print("y")
  print(df$Y)
  print("v")
  print(df$v)
  print("----")

  # If enough observations are available, enforce MOMMA computations to
  # align with Qmean prior
  if (nrow(df) >= min_nobs_mean){
    
    #### 1. Calibrate using nb ####
    nb_tests <- seq(0.002, 0.200, 0.001)
    print(df$Q)
    print("q")
    print(Qm_prior)
    print("qmprior")
    Qdiff_obj <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)
    nb_obj <- df$nb[1]
    
    for (nbt in nb_tests){
      # compute Manning's n
      df$n <- round(nbt * ((stage.max - zero.stage)/(df$stage - zero.stage)) ^ df$x, 4)
      # enforce minimum n-value to avoid n values approaching zero
      df$n[which(df$n < 0.002)] <- 0.002
      # compute mean velocities for all obs
      df$v <- (df$Y ^ (2/3) * df$slope ^ 0.5) / df$n # m/s
      # compute discharges for all obs
      df$Q <- df$width * df$Y * df$v # m3/s
      Qdiff <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)
      print(Qdiff)
      print(Qdiff_obj)
      print("one")
      if (!(is.na(Qdiff) | is.na(Qdiff_obj))){
        if (Qdiff < Qdiff_obj){
          Qdiff_obj <- Qdiff
          nb_obj <- nbt
        }
      }

    }# nbt in nb_tests
    # assign best nb to dataframe
    df$nb <- nb_obj
    
    #### 2. Fine tune calibration using x ####
    x_tests <- seq(-1, 2, 0.01)
    Qdiff_obj <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)
    x_obj <- df$x[1]
    
    for (x in x_tests){
      # compute Manning's n
      df$n <- round(df$nb * ((stage.max - zero.stage)/(df$stage - zero.stage)) ^ x, 4)
      # enforce minimum n-value to avoid n values approaching zero
      df$n[which(df$n < 0.002)] <- 0.002
      # compute mean velocities for all obs
      df$v <- (df$Y ^ (2/3) * df$slope ^ 0.5) / df$n # m/s
      # compute discharges for all obs
      df$Q <- df$width * df$Y * df$v # m3/s
      Qdiff <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)
      print("two")
      if (Qdiff < Qdiff_obj){
        Qdiff_obj <- Qdiff
        x_obj <- x
      }
    }# x in x_tests
    # assign best x to dataframe
    df$x <- x_obj

    # compute Manning's n
    df$n <- round(df$nb * ((stage.max - zero.stage)/(df$stage - zero.stage)) ^ df$x, 4)
    # enforce minimum n-value to avoid n values approaching zero
    df$n[which(df$n < 0.002)] <- 0.002
    
    # compute mean velocities for all obs
    df$v <- (df$Y ^ (2/3) * df$slope ^ 0.5) / df$n # m/s

    # compute discharges for all obs
    df$Q <- df$width * df$Y * df$v # m3/s

  }# if enough obs, adjust x to match Qmean

  ###########################################################
  # If constrain = TRUE and Qgage has coincident daily gage-observed flows
  # then use the 2-parameter calibration approach for MOMMA
  if (constrain){

    df$nb <- NA
    df$x <- NA
    mo <- c(NA,NA)

    for (s in 1:nsegs){# segment; s == 1 below bankfull; s == 2 above bankfull

      cdf <- df[(which(df$seg == s)),]
      cdf <- cdf[-which(is.na(cdf$Qgage)),]

      if (nrow(cdf > 2)){# need 3 or more flows to calibrate to
        if (s == 1 & !is.na(known_nb_seg1) & !is.na(known_x_seg1)){
          mo[1] <- known_nb_seg1
          mo[2] <- known_x_seg1
        }else if (s == 2 & !is.na(known_nb_seg2) & !is.na(known_x_seg2)){
          mo[1] <- known_nb_seg2
          mo[2] <- known_x_seg2
        }else{
          mo <- constrain.momma.nb.x(flows = cdf$Qgage, elevations = cdf$stage,
                                     widths = cdf$width, slopes = cdf$slope,
                                     zeroQ.stage = zero.stage,
                                     maxDepth.stage = stage.max,
                                     shape.param = shape_param)
        }

        df$nb[which(df$seg == s)] <- mo[1]
        df$x[which(df$seg == s)] <- mo[2]
        if (s == 1){nb_seg1 <- mo[1]; x_seg1 <- mo[2]}
        if (s == 2){nb_seg2 <- mo[1]; x_seg2 <- mo[2]}

      }# if enough gaged flows available to constrain with
    }# segment; s == 1 below bankfull; s == 2 above bankfull

    # compute calibrated/constrained flows
    # compute the constrained Mannings n values using the flow parameters
    df$n.constrained <- round(df$nb * ((stage.max - zero.stage)/(df$stage - zero.stage)) ^ df$x, 4)
    # enforce minimum n-value to avoid n values approaching zero
    df$n.constrained[which(df$n.constrained < 0.002)] <- 0.002
    # compute constrained mean velocities with derived Mannings n
    df$v.constrained <- round(df$Y ^ (2/3) * df$slope ^ 0.5 / df$n.constrained, 3)
    # compute flows with widths, depths, and constrained velocities
    df$Q.constrained <- signif(df$width * df$Y * df$v.constrained, 3)

  }# constrained flow-calibration option using coincident daily gage-observed Qs
  ##################################################################

  # Compute diagnostic values and assemble with MOMMA computed values
  # Diagnostic variables below are computed to serve as bases of comparison
  # to algorithm outputs; a kind of 'reality check'
  Frbd <- 2.85 * Smean ^ 0.31 # Bankfull Froude number derived from mean slope
  # Bankfull mean depth derived from bankfull width and mean slope
  Ybd_Wb_Smean <- 0.08 * Wb ^ 0.34 * Smean ^ -0.24 # m 

  if (!constrain){df$Q.constrained <- NA}
  # assemble into a list for the function return package
  diag <- list(gage_constrained = constrain,
               input_Qm_prior = Qm_prior,
               input_Qb_prior = Qb_prior,
               input_Yb_prior = Yb_prior,
               input_known_ezf = known_ezf,
               input_known_bkfl_stage = known_bkfl_stage,
               input_known_nb_seg1 = known_nb_seg1,
               input_known_x_seg1 = known_x_seg1,
               Qgage_constrained_nb_seg1 = nb_seg1,
               Qgage_constrained_x_seg1 = x_seg1,
               input_known_nb_seg2 = known_nb_seg2,
               input_known_x_seg2 = known_x_seg2,
               Qgage_constrained_nb_seg2 = nb_seg2,
               Qgage_constrained_x_seg2 = x_seg2,
               n_bkfl_Qb_prior = nb_prior,
               n_bkfl_slope = nb_slope,
               vel_bkfl_Qb_prior = Vb_derived_from_Qb_prior,
               Froude_bkfl_diag_Smean = Frbd,
               width_bkfl_solved_obs = Wb_obs,
               depth_bkfl_solved_obs = Yb,
               depth_bkfl_diag_Wb_Smean = Ybd_Wb_Smean,
               zero_flow_stage = zero.stage,
               bankfull_stage = bkfl_stage,
               Qmean_prior = Qm_prior,
               Qmean_momma = signif(mean(df$Q), 3),
               Qmean_momma.constrained = signif(mean(df$Q.constrained), 3))

  # attach flow computations and diagnostics and return the package
  pkg <- list(data = df, output = diag)

  return(pkg)
}















