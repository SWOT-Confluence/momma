# Modified Optimized Manning Method Algorithm (MOMMA)
# Authors: RWDudley, DMBjerklie
# v3 Constrain Option: June 2021
# MOMMA configured for SWORD/CONFLUENCE application
# Uses input data (SWOT observations)
#   observed stages (stage, vector)
#   observed widths (width, vector)
#   observed slopes (slope, vector)
# FOr gage constrained product:
#   gaged flows (Qgage, vector corresponding to SWOT observations)
# Uses priors
#   Meander bend length (MBL), m - unclear whether this will actually be available as a prior
#   Mean discharge (Qmean), m3/s
#   Bankfull discharge (Qb_prior), m3/s
#   Bankfull depth (Yb_prior), m
# Uses channel shape parameter to assume shape of unobserved channel geometry
# Uses the elevation of zero flow known_ezf,(aka channel thalweg) if known or otherwise fixed; default is unknown (NA)
# Uses the bankfull elevation, known_bkfl_stage, if known or otherwise fixed; default is unknown (NA)
# known_bkfl_stage is still subjected to minimum data requirements for the reach
# known_nb_seg[n] and known_x_seg[n] are flow law params if they have already been calibrated and are now known
# constrain: Boolean whether to calibrate to gage data for gage-constrained product
#
momma <- function(stage, width, slope, Qgage = NA, MBL_prior = NA, Qm_prior, Qb_prior,
                  Yb_prior = NA, known_ezf = NA, known_bkfl_stage = NA,
                  known_nb_seg1 = NA, known_x_seg1 = NA,
                  known_nb_seg2 = NA, known_x_seg2 = NA,
                  constrain = FALSE){
  #browser()
  #--functions called---
  # source("function.find.rating.break.R")
  # source("function.constrain.momma.nb.x.R")
  library('hydroGOF')
  #source("function.find.zero.flow.stage.R")
  #--global variables/settings---
  min_nobs <- 5 # minimum number of observations required to entertain making flow calculations
  min_nobs_mean <- 10 # minimum number of observations required to coerce mean of estimated flows to match Qmean_prior
  shape_param <- 2 # channel shape parameter. 2 = parabolic
  # REQUIRE minimum number of obs and delta stage to look for breakpoints
  n.min = 7 # minimum number of obs required to look for breakpoint in stage-width relation
  stage.range.min = 1.5 # meter; minimum range in observed stage required to look for breakpoint in stage-width relation
  # REQUIRE minimum number of obs and delta stage to retain a stage-width relation segment
  n.min.seg = 3 # number of obs in the segment
  stage.range.min.seg = stage.range.min / 2 # meter; range in stage to retain a stage-width relation segment
  bkfl_prior_stage_diff_threshold <- 1.5 # m, if the prior-derived bankfull stage differs from the
  # data-derived value by more than this, use the prior-derived value
  breakpoint.method <- "wd.ratio" # method to identify possible bankfull stage
  #------

  # initialize diagnostic values
  # compute diagnostic values
  Vbd_MBL <- NA # bankfull velocity, diagnostic, derived from Meander Bend Length, m/s
  Frbd <- NA # bankfull Froude number, diagnostic
  Ybd_MBL <- NA # bankfull depth number 1, diagnostic, m
  Ybd_Wb_Smean <- NA # bankfull depth number 2, diagnostic, m
  nb_seg1 <- NA; nb_seg2 <- NA; x_seg1 <- NA; x_seg2 <- NA # gage-constrained params
  #browser()
  if (is.na(Qgage[1]) & length(Qgage) == 1){
    df <- data.frame(stage, width, slope)
    df$Qgage <- NA
  }else{
    df <- data.frame(stage, width, slope, Qgage)
  }

  df <- df[which(!is.na(df$stage)),]
  df <- df[which(!is.na(df$width)),]
  df <- df[which(!is.na(df$slope)),]

  # add columns to receive computations
  df$seg <- 1 # stage-width relation segment number; 1 = below bankfull; 2 = above bankfull
  df$n <- NA # Manning's n
  df$Y <- NA # average depth (m)
  df$v <- NA # average velocity (m/s)
  df$Q <- NA # discharge
  df$Q.constrained <- NA

  # initialize the function return package
  diag <- list(gage_constrained = constrain,
               input_MBL_prior = MBL_prior,
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
               n_bkfl_final_used = NA,
               vel_bkfl_Qb_prior = NA,
               vel_bkfl_diag_MBL = Vbd_MBL,
               Froude_bkfl_diag_Smean = Frbd,
               width_bkfl_empirical = NA,
               width_bkfl_solved_obs = NA,
               depth_bkfl_solved_obs = NA,
               depth_bkfl_diag_MBL = Ybd_MBL,
               depth_bkfl_diag_Wb_Smean = Ybd_Wb_Smean,
               zero_flow_stage = NA,
               bankfull_stage = NA,
               Qmean_prior = Qm_prior,
               Qmean_momma = NA,
               Qmean_momma.constrained = NA)

  # retain only positive slopes
  df <- df[which(df$slope > 0),]

  # if inadequate data return NA results
  if (nrow(df) < min_nobs) {
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
      cat("INADEQUATE DATA\n")# message print to screen
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

  # find a bankfull breakpoint in the relation between w^2 and stage
  # If a bankfull breakpoint is not identified, the max obs stage is assumed to be bankfull stage
  bkfl_stage <- NA
  nsegs <- 1

  if (stage.range > stage.range.min){

    if (is.na(known_bkfl_stage)){
      if (nrow(df) >= n.min & (stage.range > stage.range.min)){
        bkfl_stage <- find.rating.break(df$width, df$stage, shape.param,
                                             method = breakpoint.method)
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
        bkfl_stage <- NA
      }
    }

    # label the above-bankfull segment '2' if a bankfull breakpoint is found
    if (!is.na(bkfl_stage)){
      df$seg[which(df$stage > bkfl_stage)] <- 2
      nsegs <- 2
    }

  }# seek bankfull stage

  #browser()

  #----compare obs-derived bkfl_stage to the prior bankfull info provided---

  # Estimate the zero.stage
  # version 1 method used this function: zero.stage <- find.zero.stage(df,
  # stage.min, stage.max, nsegs) to make an initial estimate of the stage of
  # zero flow via extrapolation of the width - elevation relation
  # Current version uses empirical relations to estimate bankfull width (Wb)
  # depending on what priors are known (meander bend length, MBL_prior; or
  # mean flow, Qm_prior). The estimated Wb is then used to estimate bankfull
  # depth (Yb) using eq. 20 from Bjerklie 2007
  # Yb is in turn used to estimate the stage of zero flow based on
  # the bankfull stage or max observed stage
  if (is.na(known_ezf)){
    if (sd(df$width) == 0){ # special case if width is constant
      Wb <- df$width[1]
    }else if (!is.na(MBL_prior)){
      Wb <- (MBL_prior / 10.2) ^ (1 / 1.12) # equation 23 Bjerklie 2007
    }else{
      Wb <- 8.27 * Qm_prior ^ (1 / 1.71) # first equation in Table 3 of
      # USGS Prof Paper 1242; units are metric
    }

    # bankfull with used with mean slope to estimate bankfull depth
    Yb <- 0.08 * Wb ^ 0.39 * (Smean ^ -0.24) # equation 20 from Bjerklie 2007

    # Bankfull depth used with channel shape parameter and bankfull stage or
    # max stage to estimate zero-flow stage
    if (is.na(bkfl_stage)){
      zero.stage <- stage.max - Yb / b
    }else{
      zero.stage <- bkfl_stage - Yb / b
    }
    if (zero.stage >= stage.min){zero.stage <- stage.min - 1.0}# backstop

  }else{
    zero.stage <- known_ezf
    Wb <- NA
    if (is.na(bkfl_stage)){
      Yb <- b * (stage.max - zero.stage)
    }else{
      Yb <- b * (bkfl_stage - zero.stage)
    }
  }

  # Bankfull stage estimate (Hb) as a function of bankfull depth and zero stage
  Hb <- Yb / b + zero.stage # bankfull stage, m; should be equal to stage.max or bkfl_stage
  # if the bankfull stage is specified by known_bkfl_stage then enforce it
  if (!is.na(known_bkfl_stage)){Hb <- known_bkfl_stage}

  # Check whether Hb meets minimum requirements
  # if not, flag as unusable
  Hb_usable <- TRUE
  seg1 <- df[which((df$stage >= stage.min) & (df$stage <= Hb)),]
  seg2 <- df[which((df$stage >= Hb) & (df$stage <= stage.max)),]
  if (nrow(seg1) < n.min.seg | nrow(seg2) < n.min.seg |
      (Hb - stage.min) < stage.range.min.seg |
      (stage.max - Hb) < stage.range.min.seg){
    Hb_usable <- FALSE
  }
  # Do comparisons
  if (is.na(bkfl_stage) & (Hb < stage.max) & Hb_usable){
    bkfl_stage <- Hb
    df$seg <- 1
    df$seg[which(df$stage > bkfl_stage)] <- 2
    nsegs <- 2
  }

  if ((abs(bkfl_stage - Hb) > bkfl_prior_stage_diff_threshold) & Hb_usable){
    bkfl_stage <- Hb
    df$seg <- 1
    df$seg[which(df$stage > bkfl_stage)] <- 2
    nsegs <- 2
  }

  # after all computations and checks, if bankfull stage is still NA at this point
  # then set it to max obs stage
  if (is.na(bkfl_stage)){bkfl_stage <- stage.max}

  # Determine the corresponding bankfull width from obs
  if (sd(df$stage) == 0){ # if all observed stages are constant
    Wb_obs <- max(df$width[which(df$stage == bkfl_stage)])
  }else{
    apx <- approx(x = df$stage, y = df$width, xout = bkfl_stage)
    Wb_obs <- apx$y
  }

  #------------------

  # FOR DEBUG/TESTING: plot "rating" with breakpoints
  # plot(df$width^2, df$stage, pch = 20,
  #      xlab = "Width^2, in square meters",
  #      ylab = "Stage, in meters")
  # abline(bkfl_stage,0)

  # Bankfull velocity
  Vb_derived_from_Qb_prior <- Qb_prior / (Wb_obs * Yb) # m/s

  # Bankfull Mannings n for all obs
  nb <- (Yb ^ (2/3) * Smean ^ 0.5) / Vb_derived_from_Qb_prior
  nb_prior <- nb

  # Compute mean depths for all obs
  df$Y <- (df$stage - zero.stage) * b # m

  # compute Manning's n
  #browser()
  df$n[which(df$seg == 1)] <- nb * (1 + log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 1)] - zero.stage))
  df$n[which(df$seg == 2)] <- nb * (1 - log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 2)] - zero.stage))

  # compute mean velocities for all obs
  df$v <- (df$stage ^ (2/3) * df$slope ^ 0.5) / df$n # m/s

  # compute discharges for all obs
  df$Q <- df$width * df$Y * df$v # m3/s

  # If enough observations are available, enforce MOMMA computations to
  # align with Qmean
  #browser()
  if (nrow(df) >= min_nobs_mean){
    nb_tests <- seq(0.008, 0.18, 0.001)
    Qdiff_obj <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)
    nb_obj <- nb

    for (nb in nb_tests){
      # compute Manning's n
      df$n[which(df$seg == 1)] <- nb * (1 + log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 1)] - zero.stage))
      df$n[which(df$seg == 2)] <- nb * (1 - log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 2)] - zero.stage))

      # compute mean velocities for all obs
      df$v <- (df$stage ^ (2/3) * df$slope ^ 0.5) / df$n # m/s

      # compute discharges for all obs
      df$Q <- df$width * df$Y * df$v # m3/s

      Qdiff <- abs(mean(df$Q, na.rm=TRUE) - Qm_prior)

      if (Qdiff < Qdiff_obj){
        Qdiff_obj <- Qdiff
        nb_obj <- nb
      }
    }# nb in nb_tests

    nb <- nb_obj

    # compute Manning's n
    df$n[which(df$seg == 1)] <- nb * (1 + log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 1)] - zero.stage))
    df$n[which(df$seg == 2)] <- nb * (1 - log10(bkfl_stage - zero.stage) / (df$stage[which(df$seg == 2)] - zero.stage))

    # compute mean velocities for all obs
    df$v <- (df$stage ^ (2/3) * df$slope ^ 0.5) / df$n # m/s

    # compute discharges for all obs
    df$Q <- df$width * df$Y * df$v # m3/s

  }# if enough obs, adjust nb to match Qmean

  ###########################################################
  # if constrain = TRUE and Qgage has gage-observed flows to work with
  # then use the calibration approach for MOMMA
  if (constrain){

    df$nb <- NA
    df$x <- NA
    mo <- c(NA,NA)

    for (s in 1:nsegs){

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
    df$n.constrained[which(df$n.constrained < 0.001)] <- 0.001
    # compute constrained mean velocities with derived Mannings n
    df$v.constrained <- round(df$Y ^ (2/3) * df$slope ^ 0.5 / df$n.constrained, 3)
    # compute flows with widths, depths, and constrained velocities
    df$Q.constrained <- signif(df$width * df$Y * df$v.constrained, 4)

  }# constrained flow-calibration option
  ##################################################################

  # Compute diagnostic values and assemble with MOMMA computed values
  # Diagnostic variables below are computed to serve as bases of comparison
  # to algorithm outputs; a kind of 'reality check'
  Vbd_MBL <- 1.37 * Smean ^ 0.31 * MBL_prior ^ 0.32 # m/s, bankfull velocity derived from mean slope and meander length
  Frbd <- 2.85 * Smean ^ 0.31 # Bankfull Froude number derived from mean slope
  Ybd_MBL <- (Vbd_MBL / Frbd) ^ 2 / 9.81 # m, Bankfull mean depth derived from bankfull velocity (above) and bankfull Froude number (above)
  Ybd_Wb_Smean <- 0.08 * Wb_obs ^ 0.34 * Smean ^ -0.24 # m, Bankfull mean depth derived from bankfull width and mean slope

  if (!constrain){df$Q.constrained <- NA}

  # assemble into a list for the function return package
  diag <- list(gage_constrained = constrain,
               input_MBL_prior = MBL_prior,
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
               n_bkfl_final_used = nb,
               vel_bkfl_Qb_prior = Vb_derived_from_Qb_prior,
               vel_bkfl_diag_MBL = Vbd_MBL,
               Froude_bkfl_diag_Smean = Frbd,
               width_bkfl_empirical = Wb,
               width_bkfl_solved_obs = Wb_obs,
               depth_bkfl_solved_obs = Yb,
               depth_bkfl_diag_MBL = Ybd_MBL,
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















