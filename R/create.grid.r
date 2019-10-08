
### function to create a grid the size of an input spatial object
# x <- ncp.utm
create.grid <- function(x, crop.shp=NULL, cut.shp=NULL, cellsize=1000) { # cellsize in meters if x is in UTM
  xfl <- 10^(nchar(cellsize)-1)
  bx <- x@bbox
  bx[,1] <- floor(bx[,1]/xfl)*xfl
  bx[,2] <- ceiling(bx[,2]/xfl)*xfl
  gr.out <- expand.grid(x=seq(bx[1,1], bx[1,2], cellsize), 
                        y=seq(bx[2,1], bx[2,2], cellsize))
  coordinates(gr.out) <- c('x','y')
  proj4string(gr.out) <- x@proj4string
  
  if (!is.null(crop.shp)) {
    gr.out <- gr.out[!is.na(over(gr.out, crop.shp)[,1]),]
  }
  if (!is.null(cut.shp)) {
    gr.out <- gr.out[is.na(over(gr.out, cut.shp)[,1]),]
  }
  return(gr.out)
}
