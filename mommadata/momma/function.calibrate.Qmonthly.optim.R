# Function to calibrate MOMMA estimates to Qmean prior

calibrate.qmean.prior <- function(dframe, Qmean_prior, 
                                  Qmon_prior, obs_times, invalid_time,
                                  h.max, zero.h,
                                  nb.min = 0.01, nb.max = 0.2,
                                  exp.x.min = -1, exp.x.max = 2.0) {

  # cat("Monthly Prior: ", Qmon_prior, "\n")

  valid_index <- !(seq_along(obs_times) %in% invalid_time)
  valid_times <- obs_times[valid_index]
  # cat("Times: ", valid_times, "\n")

  dframe$date <- as.POSIXct(valid_times, tz = "UTC")
  dframe$month <- as.integer(format(dframe$date, "%m"))

  dframe$Qmon_target <- Qmon_prior[dframe$month]

  # print(dframe)
    
  # ----- Method: Nelder-Mead -------------------------------------------------------------------

  if (any((dframe$stage - zero.h) <= 0)) {
    cat("BAD DATA. stage - zero.h <= 0.\n")

    dframe$n  <- NA
    dframe$v  <- NA
    dframe$Q  <- NA
    
    
    return(dframe)      
  }

    
  # === Objective Function ===  
  objective_fn <- function(params) {
      nb_test <- params[1]
      x_test <- params[2]

      # give penalty
      if (nb_test < nb.min | nb_test > nb.max) return(10^9)
      if (x_test < exp.x.min | x_test > exp.x.max) return(10^9)

      # compute Manning's n
      n <- nb_test * ((h.max - zero.h) / (dframe$stage - zero.h)) ^ x_test
      if (any(!is.finite(n))) return(10^12)
      # enforce min/max n-values
      n <- pmax(pmin(n, nb.max), nb.min)
      # compute mean velocities for all obs
      v <- (dframe$Y^(2/3) * dframe$slope^0.5) / n

      # compute discharges for all obs
      Qest <- dframe$width * dframe$Y * v

      
      # # ===== 1. Qmean =====
      # Qest_mean <- mean(Qest, na.rm = TRUE)

      # if (is.na(Qest_mean)) return(10^9)

      # return((Qest_mean - Qmean_prior)^2)


      
      # ===== 2. Qmonthly =====
      # # case1 ----------------------------------
      # Qmon_target <- dframe$Qmon_target

      # ok <- is.finite(Qest) & is.finite(Qmon_target)

      # if (sum(ok) == 0) return(10^9)

      # return(mean((Qest[ok] - Qmon_target[ok])^2)) #Mean Squared Error

      
      # case2 ----------------------------------
      tmp <- data.frame(month = dframe$month, Qest = Qest)

      monthly_mean <- aggregate(Qest ~ month, data = tmp, FUN = mean, na.rm = TRUE)

      monthly_mean$Qprior <- Qmon_prior[monthly_mean$month]

      return(mean((monthly_mean$Qest - monthly_mean$Qprior)^2))
    
  }


  # === Optimize nb and x ===  
  result <- optim(
      par = c(dframe$nb[1], dframe$x[1]),
      fn = objective_fn,
      method = "Nelder-Mead",   # choose optimization method
      control = list(maxit = 1000)
  )
  # cat("convergence:", result$convergence, "\n")
  # cat("message:", result$message, "\n")
  # cat("par:", result$par, "\n")
  # cat("value:", result$value, "\n")

  if (result$convergence != 0 & result$convergence != 1) {
    cat("BAD DATA. Qmean calibration fail.\n")

    dframe$n  <- NA
    dframe$v  <- NA
    dframe$Q  <- NA
    
    return(dframe)
  }
  
  nb_objective <- result$par[1]
  x_objective  <- result$par[2]
  

  # === Compute Final Result ===  
  dframe$nb <- nb_objective
  dframe$x <- x_objective
  dframe$n <- round(nb_objective * ((h.max - zero.h) / (dframe$stage - zero.h)) ^ x_objective, 4)
  dframe$n <- pmax(pmin(dframe$n, nb.max), nb.min)
    
  dframe$v  <- (dframe$Y ^ (2/3) * dframe$slope ^ 0.5) / dframe$n
  dframe$Q  <- dframe$width * dframe$Y * dframe$v
  
  return(dframe)
}
