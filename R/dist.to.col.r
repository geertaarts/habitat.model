
### function to calculate the distance over-sea to a colony or haulout
### based on https://github.com/geertaarts/seal_habitat/blob/master/seal_book/_05-Calculate_distance.Rmd

#st.xy <- st
#coordinates(st.xy) <- XY
#proj4string(st.xy) <- my.proj$UTM31

# 
#
#

dist.to.col <- function(gr, # grid, as spatial object
                        site, # colony location, as spatial object
                        trk # tracking data, also as spatial object
                        ) {
  require(spdep)
  require(sp)
  require(igraph)
  
  # gr is grid
  # col is colony/haulout location: must be one location!
  # tracking data for animals from this colony/haulout
  
  #gr <- create.grid(x=ncp.utm, cellsize=10000)
  #site <- col.utm[col.utm@data$colony=='Scheelhoek',]
  #trk <- st.xy
  
  nodes <- as.data.frame(coordinates(gr))
  
  # first get shortest distance from colony (on land) to sea: the closest point on the grid
  startpoint <- gr[unname(apply(rgeos::gDistance(gr, site, byid=TRUE), 1, which.min)),]
  
  # k nearest neighbours
  near <- spdep::knearneigh(x=gr, k=36, longlat = NULL, RANN=TRUE)
  
  # Calculate the links  
  links <- data.frame(start_id=rep(1:near$np, each=near$k), end_id=c(t(near$nn)))
  links$newcost <- sqrt((nodes$x[links$start_id]-nodes$x[links$end_id])^2 + (nodes$y[links$start_id]-nodes$y[links$end_id])^2)
  
  # Create network graph    
  netwrk <- igraph::graph.data.frame(links, directed=FALSE)
 
  # Calculate shortest paths
  dists <- igraph::shortest.paths(netwrk,
                                 v=as.character(nrow(nodes)), # CORRECT?
                                 weights=igraph::E(netwrk)$newcost)  
  
  # quick plot
  dev.new()
  utmmap('NCP', TRUE)
  #points(gr, pch="+")
  points(site, pch=16, col=2)
  points(gr, 
         col=rainbow(100)[round((dists-min(dists))/(max(dists)-min(dists)),2)*100], 
         pch=16)
  
  
  return()
  
}


# Create network graph  
g2 <- graph.data.frame(links, directed=FALSE)

# Calculate shortest paths
tmp3 <- shortest.paths(g2,v='85000',weights=E(g2)$newcost) # change v to the gr200 or gr1000 locations were the haulout is located
tmp3 <- shortest.paths(g2,v='10',weights=E(g2)$newcost)  
tmp3[tmp3>100000]<-100000
kleur<-rainbow(120)[round(tmp3/max(tmp3)*100)+1]
plot(nodes$x,nodes$y,col=kleur,pch=20,lwd=2)


### voorbeeldcode Geert


# example nodes
ndim=round(sqrt(10000))
npoints=ndim^2
nodes<-expand.grid(x=1:ndim,y=1:ndim)
# remove some parts
nodes <- nodes[sample(x=1:nrow(nodes), size=1000),]

# Real data 
names(gr1000)<-tolower(names(gr1000))
nodes<-rbind(gr1000[,c("x","y")], gr200[,c("x","y")])

# nearest neighbours
near<-spdep::knearneigh(cbind(nodes$x,nodes$y), k=36, longlat = NULL, RANN=TRUE)

# Calculate the links 
links<-data.frame(start_id=rep(1:near$np,each=near$k),end_id=c(t(near$nn)))
links$newcost<-sqrt((nodes$x[links$start_id]-nodes$x[links$end_id])^2 + (nodes$y[links$start_id]-nodes$y[links$end_id])^2)

# Create network graph 
g2 <- igraph::graph.data.frame(links, directed=FALSE)

# Calculate shortest paths
tmp3 <- igraph::shortest.paths(g2,v='2',weights=igraph::E(g2)$newcost)

# Calculate kleur 
kleur<-rainbow(101)[round(tmp3/max(tmp3)*100)+1]
plot(nodes$x,nodes$y,col=kleur, asp=1)


png('/Users/robvb/Desktop/ST_quickmap.png', res=150, units='in', width=7, height=7)
stmap('NL')
points(st[,XY], pch="+", cex=0.4)

dev.off()