# from https://www.nber.org/cycles.html
.nber_dates <- data.frame(
  start = as.Date(c(
    "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01")),
  stop = as.Date(c(
    "1980-07-29", "1982-11-29", "1991-03-29", "2001-11-29", "2009-06-29")))

library(lubridate)
.nber_dates <- within(.nber_dates, {
  start_y <- year(start) + (month(start) - 1L) / 12 + (day(start) - 1L) / 365
  stop_y  <- year(stop) + (month(stop) - 1L) / 12 + (day(stop) - 1L) / 365
})

nber_poly <- function(is_dates = FALSE, very_ligth_gray = FALSE){
  if(is_dates){
    ysa <- .nber_dates$start
    yso <- .nber_dates$stop

  } else {
    ysa <- .nber_dates$start_y
    yso <- .nber_dates$stop_y
    
  }
  
  ylim <- par("usr")[3:4]
  for(i in 1:nrow(.nber_dates))
    rect(ysa[i], ylim[1], yso[i], ylim[2], 
         col = rgb(0, 0, 0, if(very_ligth_gray) .05 else .1), 
         border = NA)
  
  invisible(list(ylim, par("usr"), .nber_dates))
}
