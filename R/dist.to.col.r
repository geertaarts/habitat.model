
### function to calculate the distance over-sea to a colony or haulout
### based on https://github.com/geertaarts/seal_habitat/blob/master/seal_book/_05-Calculate_distance.Rmd

dist.to.col <- function(gr, # grid, as spatial object
                        site, # colony location, as spatial object
                        plot=FALSE) {
  require(spdep)
  require(sp)
  require(igraph)
  require(raster)
  
  # gr is grid of class spatialPoints ; this can have an irregular shape or holes
  # col is colony/haulout location: must be one location!

  # check if all are in the same coordinate system...
  if ( proj4string(gr)!=proj4string(site) ) {
    cat('Error: please provide all input to the same CRS.\n')
    break()
  }
  
  # node coordinates
  nodes <- as.data.frame(coordinates(gr))

  # k nearest neighbours
  near <- spdep::knearneigh(x=gr, k=36, longlat = NULL, RANN=TRUE)
  
  # Calculate the links  
  links <- data.frame(start_id=rep(1:near$np, each=near$k), end_id=c(t(near$nn)))
  links$newcost <- sqrt((nodes$x[links$start_id]-nodes$x[links$end_id])^2 + (nodes$y[links$start_id]-nodes$y[links$end_id])^2)
  
  # Create network graph    
  netwrk <- igraph::graph.data.frame(links, directed=FALSE)
 
  # which node is closest to the colony location?
  first.node <- which.min(raster::pointDistance(p1=site, p2=nodes, lonlat=FALSE))

  # Calculate shortest paths
  dists <- igraph::shortest.paths(netwrk,
                                 v=as.character(first.node), # CORRECT?
                                 weights=igraph::E(netwrk)$newcost)  
  
  # quick plot
  if ( plot==TRUE ) {
      dev.new()
      utmmap('NCP', TRUE)
      #points(gr, pch="+")
      points(gr, 
             col=rainbow(100)[round((dists-min(dists))/(max(dists)-min(dists)),2)*100], 
             pch=16)
      points(site, pch='*', col=1, cex=3)
  }
  return(dists)
}


### another way to do it. Avoid crossing over-land
#library(gdistance)
#pC.atsea <- st.xy[is.na(over(st.xy, coast)[,1]),]
#pC <- coordinates(st.xy)
#pC.atsea <- pC.atsea[is.na(over(pC.atsea, gr0)[,1]),]

#huh <- raster::extract(gr0, pC.atsea)

#plot(pC.atsea[huh==0 & !is.na(huh),]) # on land
#plot(pC.atsea[huh==1 & !is.na(huh),]) # at sea

#pC.atsea <- pC.atsea[!is.na(huh) & huh==1,]

#gr.st.rs <- raster(x=gr.st[,1], y=gr.st[,2], z=NA)

#geoDist <- pointDistance(pC, longlat=FALSE)
#geoDist <- as.dist(geoDist)

#tr <- transition(gr0, mean, directions=8)
#trC <- geoCorrection(tr, "c", scl=TRUE)
#trR <- geoCorrection(tr, "r", scl=TRUE)

#cosDist <- costDistance(trC, pC.atsea)
#resDist <- commuteDistance(trR, pC.atsea)
#cor(genDist,geoDist)

#Europe <- raster(system.file("external/Europe.grd", package="gdistance"))
#str(Europe)
#image(Europe)
