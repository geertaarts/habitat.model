
# requires coast and ncp.utm shapefiles
utmmap <- function(study.area=NULL, incl.ncp=FALSE, ...) {
  if ( !exists('coast') ) {
    load('data/NL_coast.rdata')
  }
  if ( !exists('ncp.utm') & incl.ncp==TRUE) {
    load('data/NCP.rdata')
  }
  
  # study area can be defined as a abbreviation of an area: Voordelta, Netherlands, North Sea, NCP, Waddensea
  # alternatively, one can provide the bbox of a spatial object, or a list containing xlim and ylim, respectively
  if ( length(study.area)==1 ) {
    if (study.area=='VD' | is.null(study.area)) { # Voordelta
      xlim=c(435282.8, 628135.7)
      ylim=c(5650715, 5905695)
    }
    if (study.area=='NL'){ # Netherlands (and Belgium)
      xlim <- c(514953.5, 680000)
      ylim <- c(5689000, 5930000)
    }
    if (study.area=='NS'){ # North Sea
      xlim <- c(0, 680000)
      ylim <- c(5689000,  6000000)
    }
    if (study.area=='NCP'){ # Dutch Continenal Shelf
      xlim <- c(464167,  744405.2) ## change!
      ylim <- c(5691582,  6179910) ## change!
    }
    if (study.area=='WS') { # Waddensea
      xlim <- c(608000, 793000)
      ylim <- c(5850000, 5938000 )
    }
  } else {
    if ( all(dim(study.area)==c(2,2))) {
      xlim <- study.area[1,]
      ylim <- study.area[2,]
    } 
    if ( is.list(study.area) ) {
      xlim <- study.area[[1]]
      ylim <- study.area[[2]]
    }
  }

  # plot the wicked map
  plot(coast, col='lightgrey', border='lightgrey', xlim=xlim, ylim=ylim, ...)
  if (incl.ncp==TRUE) { plot(ncp.utm, border='lightgrey', add=TRUE) }
  box()
}
