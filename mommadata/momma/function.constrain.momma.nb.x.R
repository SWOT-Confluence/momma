# Function to solve nb and x parameters for MOMMA under constrained gage conditions

constrain.momma.nb.x <- function(flows, elevations, widths, slopes, zeroQ.stage, maxDepth.stage, siteName,
                          shape.param = 2, nb.range = c(0.01, 0.2), x.range = c(-1, 2), max.iter = 3) {

  library(hydroGOF)
  #browser()

  nobs <- length(flows) # number of observed flows being used for calibration

  b <- 1 - (1 / (1 + shape.param)) # channel shape coefficient

  max.stage <- maxDepth.stage

  dim.rand <- 5000

  # init calibration values
  obj.func <- 10^6
  x.calib <- -1
  nb.calib <- 0.05

  for (iter in 1:max.iter){
    #browser()
    nb <- runif(dim.rand, min(nb.range), max(nb.range))
    x <- runif(dim.rand, min(x.range), max(x.range))

    df <- array(data = NA, dim = c(dim.rand, 8, nobs))

    for (i in 1:nobs){
      df[, 1, i] <- nb
      df[, 2, i] <- x
      n <- nb * ((max.stage - zeroQ.stage)/(elevations[i] - zeroQ.stage)) ^ x
      df[, 3, i] <- n
      Y <- (elevations[i] - zeroQ.stage) * b
      v <- Y ^ 0.67 * slopes[i] ^ 0.5 / n # velocity, m/s
      df[, 4, i] <- v
      Q.hat <- widths[i] * Y * v # m3/s
      df[, 5, i] <- Q.hat
      df[, 6, i] <- Q.hat - flows[i]# residual from obs
      df[, 7, i] <- (Q.hat - flows[i]) ^ 2# residual squared
      Fr <- v / sqrt(9.81 * Y)
      df[, 8, i] <- Fr# Froude numbers
    }# i

    eval.df <- data.frame(matrix(nrow=length(df[, 1, 1]), ncol = 6))

    colnames(eval.df) <- c("nrmse", "pbias", "NSE", "nb", "x", "color")

    for (i in 1:length(df[, 1, 1])){
      eval.df[i,1] <- ((sum(df[i, 7, ]) / (nobs - 1)) ^ 0.5) / mean(flows) * 100 # RMSE = sum of the squared residuals
      # divide by n-1, and take the square root; normalized to the mean so divide by the mean x 100% for percent
      # https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
      eval.df[i,2] <- (sum(df[i, 6, ]) / sum(flows)) * 100 # percent bias = 100 * [sum(residuals) / sum(obs) ]
      eval.df[i,3] <- (1 - NSE(sim = df[i, 5, ], obs = flows)) * 100 # converted to an inverse percent
      eval.df[i,4] <- df[i, 1, 1]
      eval.df[i,5] <- df[i, 2, 1]
    }#i

    # objective to minimize
    eval.df$obj.func <- (eval.df$nrmse ^ 2 + eval.df$pbias ^ 2 + eval.df$NSE ^ 2) ^ 0.5

    # find the params yielding minimum objective function value
    nb.new <- signif(eval.df$nb[which.min(eval.df$obj.func)],3)
    x.new <- signif(eval.df$x[which.min(eval.df$obj.func)],4)

    obj.func.new <- signif(eval.df$obj.func[which.min(eval.df$obj.func)],5)

    if (obj.func.new < obj.func){
      obj.func <- obj.func.new
      x.calib <- x.new
      nb.calib <- nb.new
      cat("min(obj.function) =",obj.func,"nb =",nb.calib,"x =",x.calib,"iter =",iter,"\n")

    }

    scal.nb <- (0.6 * nb.calib)
    scal.x <- 0.3

    nb.range = c(nb.calib - scal.nb, nb.calib + scal.nb)
    x.range = c(x.calib - scal.x, x.calib + scal.x)


  }# iter


  nbx <- c(NA, NA, NA, NA, NA)
  nbx[1] <- nb.calib
  nbx[2] <- x.calib
  nbx[3] <- shape.param
  nbx[4] <- zeroQ.stage
  nbx[5] <- maxDepth.stage

  return(nbx)

}# function

