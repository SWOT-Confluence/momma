# Function to estimate the stage at which zero flow occurs

find.zero.stage <- function(df, nsegs) {
  #browser()
  if (nsegs > 1) {
    df2 <- df[which(df$seg == 1),]
    lb <- lm(df2$stage ~ df2$width ^ 2)
    zero.h <- round(as.numeric(lb$coefficients[1]), 2)# get the intercept of the line
  }else{
    if (nrow(df) > 9){# if enough obs, use the lower half of the stage obs
      df2 <- df[which(df$stage <= quantile(df$stage, probs = 0.50)),]
    }else{
      df2 <- df
    }
    lb <- lm(df2$stage ~ df2$width ^ 2)
    zero.h <- round(as.numeric(lb$coefficients[1]), 2)# get the intercept of the line
  }
  
  return(zero.h)    
  
}# function

