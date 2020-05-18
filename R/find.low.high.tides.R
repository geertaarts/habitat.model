
### FUNCTION: calculate timesteps ####
steptime <- function(tt, ID=NULL, units='mins') {
  #tt <- st$date_time
  #ID <- st$tripnm
  dt <- round(difftime(tt[2:length(tt)], tt[1:(length(tt)-1)], units=units),0)
  sameID <- ID[2:length(ID)]!=ID[1:(length(ID)-1)]
  dt[sameID] <- NA
  dt <- c(dt, dt[is.na(dt)][1])
  return(dt)
}

delta.time <- function(tt, ID=NULL, units='mins') {
  
}

### FUNCTION: find high tides ####
find.high.tides <- function(date, time, wh) {
  # data is tidal data per 10 minutes
  # date=date
  # time=time
  # wh=water level relative to mean sea level in meters
  tid.site <- data.frame(date_time=as.POSIXct(strptime(paste(date, time), format='%d-%m-%Y %H:%M:%S', tz='UTC')), 
                         height=wh)
  # fix time zone so that it really is UTC... Bloody hell fucking time zone stuff
  tid.site$date_time <- tid.site$date_time-60*60
  
  tid.site$num.date <- as.numeric(tid.site$date_time)
  tid.site$height <- tid.site$height+runif(n=nrow(tid.site), min=0, max=0.1) # add some random noise to avoid the same value twice
  tid.site <- tid.site[!is.na(tid.site$date_time),]
  #cat('Smoothing measurements...\n')
  tid.site$height.rav <- zoo::rollmean(tid.site$height, k=7, align='center', na.pad=TRUE) #predict(loess(height~num.date, data=tid.site, span=0.15))
  
  # make sure date/time are ordered...
  tid.site <- tid.site[order(tid.site$date_time),]
  
  # extract high tides
  htid <- tid.site[pracma::findpeaks(tid.site$height.rav, 
                                     thresh=0.5, 
                                     minpeakdistance = 70)[,2],]
  
  # remove NAs in datetime
  htid <- htid[!is.na(htid$date_time),]

  # sort
  htid <- htid[order(htid$date_time),]

  # remove errors (positive heights for valleys and negative for peaks)
  htid <- htid[htid$height>0,]

  # calculate inter-tide duration
  htid$tstep <- as.vector(steptime(tt=htid$date_time))

  # fix rownames
  row.names(htid) <- 1:nrow(htid)

  # median time steps
  htid.median <- median(htid$tstep, na.rm=TRUE)

  # there are two categories of time intervals:
  # a) when one tide is missing: tstep between 1290 and 1690
  # b) when two tides are missing: tstep of 2070 and 3020 
  # c) when three tides are missing: tstep of 
  
  # for very long tsteps, impute high tides if missing
  # sometimes there is a second round required... not sure why...
  for (x in 1:10) { # iterate maximum of 10 times to fix missing/short intervals
    if ( any(htid$tstep[!is.na(htid$tstep)]>1000) ) {
      htid <- impute.missing.tides(tid.data=tid.site, tides=htid, tides.med=htid.median, high=TRUE)
    }
    if ( any(htid$tstep[!is.na(htid$tstep)]<500) ) {
      htid <- remove.short.tides(tides=htid, high=TRUE, keep='extreme')
    }
    if ( all(between(htid$tstep[!is.na(htid$tstep)],500,1000))  ) {
      break()
    }
  }
  return(htid)
}
  
### FUNCTION: impute missing tides ####
# helper function for find.high.tides
impute.missing.tides <- function(tid.data, tides, tides.median, high=TRUE) {
  for (t in 1:nrow(tides)) {
    #progress.bar(p=t, n=length(which(tides$tstep > 1000)))
    if ( length(which(tides$tstep > 1000)) > 0 ) {
      this.tide <- which(tides$tstep > 1000)[1]
      n.missed <- floor(tides$tstep[this.tide]/tides.median)-1
      if (n.missed==0) {
        n.missed <- 1
      }
      # select data around missed peak(s)
      tid.t <- tid.data[tid.data$date_time > (tides$date_time[(this.tide)]+9*60*60) & 
                          tid.data$date_time < (tides$date_time[(this.tide+1)]-9*60*60),]
      # for low tides, invert water levels
      if (high==FALSE) {
        tid.t$height <- -1*tid.t$height
      }
      # find these peaks and add to list
      add.this <- tid.t[pracma::findpeaks(tid.t$height, 
                                          npeaks=n.missed)[,2],]
      
      # combine with data, order, fix rownames and re-calculate tstep
      tides <- rbind(tides[,colnames(add.this)], add.this)
      tides <- tides[order(tides$date_time),]
      row.names(tides) <- 1:nrow(tides)
      tides$tstep <- as.vector(steptime(tt=tides$date_time))
    } else {
      break()
    }
  }
  return(tides)
}

### FUNCTION: remove tides with short intervals ####
# helper function for find.high.tides
# remove short tides, much shorter than the median
remove.short.tides <- function(tides, high=TRUE, keep=c('extreme', 'first')) {
  #cat(paste0('There are ', length(which(tides$tstep< 500)),' tides with intervals shorter than 500 minutes. Fixing this...\n'))
  for (t in 1:nrow(tides)) {
    if ( length(which(tides$tstep < 500))>0 ) { # check if there are still any short ones
      this.tide <- which(tides$tstep< 500)[1]
      if (keep=='extreme') {
        if (high==TRUE) {
          remove.this <- (this.tide:(this.tide+1))[which.max(tides$height[this.tide:(this.tide+1)])]
        } else {
          remove.this <- (this.tide:(this.tide+1))[which.min(tides$height[this.tide:(this.tide+1)])]
        }
        # remove this record:
        tides <- tides[-remove.this,]
      }
      if (keep=='first') {
        # remove the second record
        tides <- tides[-(this.tide+1),]
      }

      # re-order, etc, re-calculate inter-tide duration
      tides <- tides[order(tides$date_time),]
      row.names(tides) <- 1:nrow(tides)
      tides$tstep <- as.vector(steptime(tt=tides$date_time))
    } else {
      #cat('Done!\n')
      break()
    }
  }
  return(tides)
}



### TRASH... but note: a smart way to extract both HIGH and LOW tides
# tid.site$height.rav <- zoo::rollmean(tid.site$height, k=7, align='center', na.pad=TRUE) #predict(loess(height~num.date, data=tid.site, span=0.15))
# 
# # FIND BOTH LOW AND HIGH TIDES, using abs(height)
# atid <- tid.site[pracma::findpeaks(abs(tid.site$height.rav), 
#                                    thresh=0.5,
#                                    minpeakdistance = 30)[,2],]
# atid <- atid[order(atid$date_time),]
# atid$tstep <- as.vector(steptime(tt=atid$date_time))
# my.hist(atid$tstep)
# 
# # median interval
# med.interval <- median(atid$tstep, na.rm=TRUE)
# 
# # first fix missing tides
# add.these <- list()
# for (t in 1:length(which(atid$tstep>600))) { # 600 is just above the 'split' visible in an histogram of tstep (max of reasonable values is 550)
#   progress.bar(p=t, n=length(which(atid$tstep>600)))
#   this.tide <- which(atid$tstep > 600)[t]
#   #n.missed <- round(atid$tstep[this.tide]/med.interval)-1
#   #if (n.missed==0) { n.missed <- 1 }
#   # select data around missed peak(s)
#   t0 <- atid$date_time[(this.tide)]
#   t1 <- atid$date_time[(this.tide+1)]
#   tid.t <- tid.site[tid.site$date_time > t0 + 3.5*60*60 & # +3.5*60*60
#                       tid.site$date_time < t1 - 3.5*60*60,]
#   add.this <- tid.t[pracma::findpeaks(abs(tid.t$height.rav), 
#                                       zero='-',
#                                       minpeakdistance = 30)[,2],]
#   add.this <- add.this[order(add.this$date_time),]
#   #add.this$tstep <- as.vector(steptime(tt=add.this$date_time))
#   #add.this <- add.this[between(add.this$tstep,300,600),]
#   #plot(tid.t$date_time, tid.t$height, type='l')
#   add.these[[t]] <- add.this
# }
# #table(unlist(lapply(add.these, nrow))==0)
# 
# # combine with atid and recalc stuff
# atid <- rbind(atid[,colnames(add.these[[1]])], do.call('rbind', add.these))
# atid <- atid[order(atid$date_time),]
# atid$tstep <- as.vector(steptime(tt=atid$date_time))
# 
# my.hist(atid$tstep)
# range(atid$tstep, na.rm=TRUE)
# 
# # determine low/high tide
# atid$tide <- ifelse(atid$height > 0, 'high', 'low') # simple classification
# # check for same values in sequence (checks with previous tide)
# same.tide <- c(FALSE, atid$tide[2:nrow(atid)]==atid$tide[1:(nrow(atid)-1)])
# table(same.tide) # 51
# 
# for (t in which(same.tide==TRUE)) {
#   #t <- which(same.tide==TRUE)[1]
#   atid[(t-2):(t+2),]
#   t0 <- atid$date_time[(t-2)]
#   t1 <- atid$date_time[(t+2)]
#   tid.t <- tid.site[between(tid.site$date_time, t0, t1), ]
#   plot(tid.t[,c('date_time','height.rav')], type='l')
#   abline(v=atid$date_time, col=2)
#   
# }
# 
# # for low atid, invert water levels
# 
# if (  ) {
#   tid.t$height <- -1*tid.t$height
# }
# # find these peaks and add to list
# add.this <- tid.t[pracma::findpeaks(tid.t$height, 
#                                     npeaks=n.missed)[,2],]
# 
# # combine with data, order, fix rownames and re-calculate tstep
# atid <- rbind(atid[,colnames(add.this)], add.this)
# atid <- atid[order(atid$date_time),]
# row.names(atid) <- 1:nrow(atid)
# atid$tstep <- as.vector(steptime(tt=atid$date_time))
# }
# 
# }
# length(which(atid$tstep>600))
# this.tide <- which(atid$tstep>600)[1]
# t0 <- atid$date_time[this.tide]
# t1 <- atid$date_time[(this.tide+1)]
# plot(tid.site[between(tid.site$date_time, t0, t1),c('date_time', 'height')], type='l')
# abline(v=atid$date_time, col=2)
# 
# tid.this <- tid.site[between(tid.site$date_time, t0+3*60*60, t1-3*60*60), c('date_time', 'height')]
# add.this <- tid.this[pracma::findpeaks(abs(tid.this$height), 
#                                        npeaks=1)[,2],]
# abline(v=add.this$date_time, col=3)
# 
# # then label them...
# 
# plot(tid.site$date_time[rr], abs(tid.site$height[rr]), type='l')
# abline(v=atid$date_time, col=3)
# 
# my.hist(atid$tstep)
# 
# 
# 
# # remove NAs in datetime
# htid <- htid[!is.na(htid$date_time),]
# ltid <- ltid[!is.na(ltid$date_time),]
# 
# # sort
# htid <- htid[order(htid$date_time),]
# ltid <- ltid[order(ltid$date_time),]
# 
# # remove errors (positive heights for valleys and negative for peaks)
# htid <- htid[htid$height>0,]
# ltid <- ltid[ltid$height<0,]
# 
# # calculate inter-tide duration
# htid$tstep <- as.vector(steptime(tt=htid$date_time))
# ltid$tstep <- as.vector(steptime(tt=ltid$date_time))
# 
# # fix rownames
# row.names(htid) <- 1:nrow(htid)
# row.names(ltid) <- 1:nrow(ltid)
# 
# # median time steps
# htid.median <- median(htid$tstep, na.rm=TRUE)
# ltid.median <- median(ltid$tstep, na.rm=TRUE)
# 
# # there are two categories of time intervals:
# # a) when one tide is missing: tstep between 1290 and 1690
# # b) when two tides are missing: tstep of 2070 and 3020 
# # c) when three tides are missing: tstep of 
# 
# # for very long tsteps, impute high tides if missing
# # sometimes there is a second round required... not sure why...
# for (x in 1:10) {
#   cat("+")
#   if ( any(htid$tstep[!is.na(htid$tstep)]>1000) ) {
#     htid <- impute.missing.tides(tid.data=tid.site, tides=htid, tides.med=htid.median, high=TRUE)
#   }
#   if ( any(htid$tstep[!is.na(htid$tstep)]<500) ) {
#     htid <- remove.short.tides(tides=htid, high=TRUE, keep='extreme')
#   }
#   if ( all(between(htid$tstep[!is.na(htid$tstep)],500,1000))  ) {
#     break()
#   }
# }
# # same for low tides
# for (x in 1:10) {
#   cat("+")
#   if ( any(ltid$tstep[!is.na(ltid$tstep)]>1000) ) {
#     ltid <- impute.missing.tides(tid.data=tid.site, tides=ltid, tides.med=ltid.median, high=FALSE)
#   }
#   if ( any(ltid$tstep[!is.na(ltid$tstep)]<500) ) {
#     ltid <- remove.short.tides(tides=ltid, high=FALSE, keep='extreme')
#   }
#   if ( all(between(ltid$tstep[!is.na(ltid$tstep)],500,1000))  ) {
#     break()
#   }
# }
# 
# t0 <- ltid$date_time[1130]-24*60*60
# t1 <- ltid$date_time[1130]+24*60*60
# plot(tid.site[between(tid.site$date_time, t0, t1) , c('date_time', 'height')], type='l')
# abline(v=ltid$date_time, col=2)
# abline(v=htid$date_time, col=3)
# 
# ltid.sel <- ltid[as.POSIXlt(ltid$date_time)$mon %in% c(4:6),]
# 
# # check histogram of timestep
# if (plot==TRUE) {
#   par(mfrow=c(1,2), mar=c(4,5,1,1))
#   my.hist(htid$tstep, main='high tide', xlab='time intervals (mins)') 
#   my.hist(ltid$tstep, main='low tide', xlab='time intervals (mins)')
#   readline('ok?')
# }
# 
# # select output
# if (out=='both') {
#   out <- list(ltid, htid)
# }
# if (out=='high') {
#   out <- htid
# }
# if (out=='low') {
#   out <- ltid
# }
# return(out)
# }
