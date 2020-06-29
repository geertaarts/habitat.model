
### function to sample random points from the range of individuals
### + a buffer around it and - predefined areas (e.g. land)

# work.dir <- "/home/rob/Documents/github/sandwich_tern/"
# gis.dir <- '/home/rob/Documents/GISdata/'
# git.dir <- '/home/rob/Documents/github/'
# fig.dir <- paste0(work.dir, 'figures/')
# dat.dir <- paste0(work.dir, 'data/')
# sav.dir <- paste0(work.dir, 'data/')
# big.dir <- paste0(git.dir,'bigdata/')
# load(paste0(dat.dir, "deltares.rdata"), verbose=TRUE)
# load(file=paste0(big.dir, "dataHMM_20200410.rdata"), verbose=TRUE)
# 

random_locations <- function(trks, npoints=10, excl=TRUE, buf=10000, shp=NULL, common.crs) {
  # trks <- st
  # npoints <- 10
  # buf = 10000
  # shp = coast
  # common.crs = my.proj$UTM31
  
  # trks = tracks of several individuals or class trk: it should include a column either 
  #     id or ID that identifies individuals. Random points are selected from 
  #     a convex hull around the animals' relocations
  # npoints = number of random points per original animal relocation
  # buf = the buffer (in meters) around the convex hull
  # shp = (list of) shapefile(s) of areas that should be EXCLUDED
  # keep.time = logical indicating whether or not the same date/time labels as the original data should be included
  
  # libraries
  require(sp)
  require(dismo)
  require(rgeos)
  
  # to-do: 
  # - now accepts amt class but should work with simple dataframes
  # - now generates random locations ignoring date/time but would be nice
  # - ...
  
  # ID stuff
  ID.column <- colnames(trks)[which(colnames(trks) %in% c('id', 'ID'))]
  ID <- as.character(droplevels(factor(trks[,ID.column])))
  IDs <- unique(ID)
  IDl <- length(IDs)
  
  # check class
  if ('data.frame' %in% class(trks)) {
    trks <- data.frame(trks)
    trks[,ID.column] <- droplevels(factor(trks[,ID.column]))
    trks.l  <- split(trks, f=trks[,ID.column])
  }
  if ('tbl_df' %in% class(trks)) {
    trks.l <- lapply(trks$dat_clean, function(d) {
      d <- data.frame(d)
      d <- rename.colums(x, 'x_', 'x')
      d <- rename.colums(x, 'y_', 'y')
      d <- rename.colums(x, 't_', 't')
      return(d)
    })
  }
  
  # make list and run through all individuals
  rp.list <- vector('list', length=IDl)
  message('Running through individuals...\n')
  for (indi in 1:IDl) {
    message("+")
    # calculate convexx hull, as spatialpointsdataframe
    mch0 <- dismo::convHull(trks.l[[indi]][,c('x','y')])
    mch1 <- mch0@polygons
    if ( is.na(proj4string(mch1)) & !is.null(common.crs) )  {
      proj4string(mch1) <- CRS(common.crs)
      } else { cat('Please provide a crs.\n') }
    
    # add buffer
    mch2 <- rgeos::gBuffer(mch1, width=buf)
    
    # substract shapefile(s) [either a list of shapefiles or a single shapefile]
    if ( !is.null(shp) ) {
      if (is.list(shp)) {
        for (sh in 1:length(shp)) {
          if (excl[sh]) {
            mch3 <- rgeos::gDifference(mch2, shp[[sh]])
          } else {
            mch3 <- rgeos::gIntersection(mch2, shp[[sh]])
          }
        }
      } else {
        if (excl) {
          mch3 <- rgeos::gDifference(mch2, shp)
        } else {
          mch3 <- rgeos::gIntersection(mch2, shp)
        }
      }
    
    # generate random points 
    mch.rp <- sp::spsample(mch3, n=npoints*nrow(trks.l[[indi]][,c('x','y')]), "random")
    } else {
      mch.rp <- sp::spsample(mch2, n=npoints*nrow(trks.l[[indi]][,c('x','y')]), "random")
    }
    
    
    # and put coordinates in list
    rp.list[[indi]] <- data.frame(sp::coordinates(mch.rp))
  }
  message('...finished\n')
  # unlist the stuff
  rp.df <- do.call('rbind', rp.list)
  
  # make ID column
  rp.df$id <- rep(IDs, times=unlist(lapply(rp.list, nrow)))
  
  # return the shizl
  return(rp.df)
}
