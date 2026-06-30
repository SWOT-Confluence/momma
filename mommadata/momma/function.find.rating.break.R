
find.rating.break <- function(widths, stages, method = "wd.ratio"){
# methods: wd.ratio or max.resid
  
  #========================
  # smooth out and increase sampling density using linear interpolation
  np = 100
  xd <- data.frame(stages, widths)
  if (nrow(xd) >= 100){np = nrow(xd)}
  xd <- xd[order(xd$stages),]
  xdf <- data.frame(matrix(vector(), np, 2, dimnames=list(c(), c("stages", "widths"))), stringsAsFactors=F)
  
  xapprox <- approx(x=xd$stages, y=xd$widths, method="linear", n = np) 
  
  xdf$stages <- xapprox$x
  xdf$widths <- xapprox$y
  
  rm(xd, xapprox)

  colnames(xdf) <- c("h", "w")
  # set up an arbitrary datum for depth for purposes of width/depth ratio
  xdf$d <- xdf$h - min(xdf$h) + 1
  
  xdf <- xdf[order(xdf$h),]
  
  if (sd(xdf$w) == 0){method <- "wd.ratio"}
  
  #----------------------------------------------
  # METHOD: Maximum residual from a min-max line
  if (method == "max.resid"){
    thresh <- 1.5 # meters; residual threshold to identify a breakpoint
    xdf$w2 <- xdf$w ^ 2
    m.slope <- (max(xdf$h) - min(xdf$h)) / (max(xdf$w2) - min(xdf$w2))
    b.intcp <- min(xdf$h) - m.slope * min(xdf$w2)
    xdf$hhat <- m.slope * xdf$w2 + b.intcp
    xdf$resid <- abs(xdf$hhat - xdf$h)
    
    break.point <- ifelse(xdf$h[which.max(xdf$resid)] >= thresh, xdf$h[which.max(xdf$resid)], NA)
    
    return(break.point)
  
  }
  #----------------------------------------------
  # METHOD: Minimum Width-Depth Ratio
  if (method == "wd.ratio"){
    xdf$wd.ratio <- xdf$w / xdf$d

    break.point <- xdf$h[which.min(xdf$wd.ratio)]
    
    return(break.point)
    #----------------------------------------------
  }
}

