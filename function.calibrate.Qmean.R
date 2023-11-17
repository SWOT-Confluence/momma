# Function to calibrate MOMMA estimates to Qmean prior

calibrate.qmean.prior <- function(dframe, Qmean_prior, h.max, zero.h,
                                  nb.min = 0.01, nb.max = 0.2,
                                  exp.x.min = -1, exp.x.max = 2.0){
  #browser()

  nb_tests <- seq(nb.min, nb.max, 0.001)
  nb_objective <- dframe$nb[1]
  x_tests <- seq(exp.x.min, exp.x.max, 0.01)
  x_objective <- dframe$x[1]
  Qdiff_obj <- abs(mean(dframe$Q, na.rm=TRUE) - Qmean_prior)

  for (nbt in nb_tests){
    for (x in x_tests){

      # compute Manning's n
      dframe$n <- round(nbt * ((h.max - zero.h)/(dframe$stage - zero.h)) ^ x, 4)
      # enforce min/max n-values
      dframe$n[which(dframe$n < nb.min)] <- nb.min
      dframe$n[which(dframe$n > nb.max)] <- nb.max
      # compute mean velocities for all obs
      dframe$v <- (dframe$Y ^ (2/3) * dframe$slope ^ 0.5) / dframe$n # m/s
      # compute discharges for all obs
      dframe$Q <- dframe$width * dframe$Y * dframe$v # m3/s
      Qdiff <- abs(mean(dframe$Q, na.rm=TRUE) - Qmean_prior)

      if (is.na(Qdiff) | is.na(Qdiff_obj) ){# if bad data result in NA
          msg <- "BAD DATA. Qmean calibration fail."
          return(msg)
        }else{
          if (Qdiff < Qdiff_obj){
            Qdiff_obj <- Qdiff
            nb_objective <- nbt
            x_objective <- x
          }
        }

    }# x in x_tests
  }# nbt in nb_tests

  # assign best nb to dataframe
  dframe$nb <- nb_objective
  # assign best x to dataframe
  dframe$x <- x_objective

  # compute final calibrated values
  # compute Manning's n
  dframe$n <- round(dframe$nb * ((stage.max - zero.stage)/(dframe$stage - zero.stage)) ^ dframe$x, 4)
  # enforce min/max n-values
  dframe$n[which(dframe$n < resist.min)] <- resist.min
  dframe$n[which(dframe$n > resist.max)] <- resist.max
  # compute mean velocities for all obs
  dframe$v <- (dframe$Y ^ (2/3) * dframe$slope ^ 0.5) / dframe$n # m/s
  # compute discharges for all obs
  dframe$Q <- dframe$width * dframe$Y * dframe$v # m3/s

  return(dframe)

}# function
