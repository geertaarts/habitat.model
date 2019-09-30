
# requires coast and ncp.utm shapefiles
utmmap <- function(study.area=NULL, incl.ncp=FALSE, ...) {
  if ( !exists('coast') ) {
    load('data/NL_coast.rdata')
  }
  if ( !exists('ncp.utm') & incl.ncp==TRUE) {
    load('data/NCP.rdata')
  }
  
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
  
  par(mar=rep(1,4))
  plot(coast, col='lightgrey', border='lightgrey', xlim=xlim, ylim=ylim, ...)
  if (incl.ncp==TRUE) { plot(ncp.utm, border='lightgrey', add=TRUE) }
  box()
  par(mar=rep(4,4,2,2))
}
