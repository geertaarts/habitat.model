
# simple functions to define trips

# function: get time interval between subsequent fixes
calc.d.time <- function(dt) { 
  ndt <- length(dt)
  if ( ndt ==1) {
    out <- NA
  } else {out <- c(abs(as.vector(difftime(time1=dt[1:(ndt-1)],
                                          time2=dt[2:ndt], 
                                          units='mins'))), NA) 
  }
  return(out)
}

# function: get distance between subsequent fixes
calc.d.dist <- function(lat, lon) {
  ndt <- length(lat)
  if ( ndt==1 ) {
    out <- NA
  } else {
    out <- c(argosfilter::distanceTrack(lat=lat, lon=lon), NA)
  }
  return(out)
}

# function: colony departure
col.dep <- function(in_range) {
  if (is.logical(in_range)) {
    out <- c(paste0(as.numeric(in_range[1:(length(in_range)-1)]), as.numeric(in_range[2:(length(in_range))]))=="10", NA)
  } 
  if (is.numeric(in_range)) {  
    out <- c(paste0(in_range[1:(length(in_range)-1)], in_range[2:(length(in_range))])=="10", NA)
  }
  return(out)
}

# function: colony arrival
col.arr <- function(in_range) {
  if (is.logical(in_range)) {
    out <- c(NA, paste0(as.numeric(in_range[1:(length(in_range)-1)]), as.numeric(in_range[2:(length(in_range))]))=="01")
  } 
  if (is.numeric(in_range)) {  
    out <- c(NA, paste0(in_range[1:(length(in_range)-1)], in_range[2:(length(in_range))])=="01")
  }
  return(out)
}

# function: which time gaps are larger than a threshold value?
dt.gap <- function(date_time, max.gap) { # max gap in minutes
  dt <- calc.d.time(date_time)
  dt <- c(NA, dt[-length(dt)])
  t.gap <- dt > max.gap
  return(t.gap)
}

# function: create trip index: split track between colony departures
trips.index <- function(dep, gap) {
  ri <- 1:length(dep)
  gap[1] <- FALSE
  br <- c(which(dep | gap)-1, length(dep))
  if ( br[1]!=1 ) {
    br <- c(1, br)
  }
  if (br[1]==0) {
    br <- br[-1]
  }
  br <- unique(br)
  ct <- cut(ri, breaks=br, include.lowest = TRUE, labels=1:(length(br)-1))
  return(ct)
}

# function: time since colony departure
time.since.departure <- function(time, inrange) {
  coldep <- col.dep(inrange) # colony departure
  coldep[length(coldep)] <- FALSE
  # includes departure
  if ( any(coldep) ) {
    out <- as.vector(difftime(
      time1=time,
      time2=first(time[coldep==TRUE]),
      units="mins"))
    out[inrange==1] <- 0
  } else {
    out <- rep(NA, length(time))
  }
  return(out)
}

# function: assess whether the trip is complete (=starts and ends in colony) or otherwise
completeness <- function(inrange) {
  cd <- any(col.dep(inrange), na.rm=TRUE) # departure
  ca <- any(col.arr(inrange), na.rm=TRUE) # arrival
  if (ca & cd) { out <- "both" }
  if (ca & !cd) { out <- "start" }
  if (!ca & cd) { out <- "end" }
  if (!ca & !cd) { out <- "none" }
  return(out)
}

# function: make index for subsequent similar values that are 0 (e.g. in_range==0, thus bird outside the colony)
make.index.out <- function(v, val=0) {
  # v <- c(0,0,0,1,1,0,1,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,0,1)
  if (is.logical(v)) { v <- as.numeric(v) }
  if (is.logical(val)) { val <- as.numeric(val)}
  h <- rle(v)
  x <- rep(h$values, h$lengths)
  y <- rep(1:length(h$values), h$length)
  y[x!=val] <- NA
  y[!is.na(y)] <- rep(1:length(h$values[h$values==val]), h$length[h$values==val])
  return(y)
}
