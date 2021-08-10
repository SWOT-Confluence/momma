# Function to estimate the stage at which zero flow occurs

find.zero.stage <- function(df, stage.min, stage.max, nsegs) {
  #browser()
  if (nsegs > 1) {
    df2 <- df[which(df$seg == 1),]
    lb <- lm(df2$stage ~ df2$width ^ 2)
    zero.h <- round(as.numeric(lb$coefficients[1]), 2)# get the intercept of the line
  }else{
    df2 <- df[which(df$stage <= quantile(df$stage, probs = 0.50)),]
    lb <- lm(df2$stage ~ df2$width ^ 2)
    zero.h <- round(as.numeric(lb$coefficients[1]), 2)# get the intercept of the line
  }
  
  if (zero.h >= stage.min){zero.h <- stage.min - 0.5}# backstop

  return(zero.h)    

}# function

