# Function to estimate the stage at which zero flow occurs

find.zero.stage <- function(df, nsegs, shape.param, Wb) {
  #browser()
  r <- shape.param
    
  if (nsegs > 1) {
    df2 <- df[which(df$seg == 1),]
    df2$width_r <- (df2$width)^r
    df2$cal_width <- (df2$width / Wb)^r
    lb <- lm(df2$stage ~ df2$cal_width)

    zero.h <- round(as.numeric(lb$coefficients[1]), 2) # get the intercept of the line
  } else {
      if (nrow(df) > 9){
        # if enough obs, use the lower half of the stage obs
        df2 <- df[which(df$stage <= quantile(df$stage, probs = 0.50)),]
      } else {
        df2 <- df
      }
    df2$width_r <- (df2$width)^r
    df2$cal_width <- (df2$width / Wb)^r
    lb <- lm(df2$stage ~ df2$cal_width)

    zero.h <- round(as.numeric(lb$coefficients[1]), 2) # get the intercept of the line
  }
  
  # make sure all "stage - zero.h" are positive
  if (any((df$stage - zero.h) <= 0, na.rm = TRUE)) {
      zero.h <- min(df$stage, na.rm = TRUE) - 1
      # cat("BAD H0. H0 = minH - 1\n")
  }

  return(zero.h)    
  
}
