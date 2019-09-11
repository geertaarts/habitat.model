xy.project.na<-function(dataframe,CRSold,CRSnew,xold,yold,xnew,ynew)
{
  require(sp)
  xy<-data.frame(dataframe[,xold],dataframe[,yold])
  No.NAs<-which(!rowSums(is.na(xy))>0)
  xy<-xy[No.NAs,]
  coordinates(xy)<-names(xy)
  proj4string(xy) <- CRS(CRSold)
  xy<-as.data.frame(spTransform(xy, CRS(CRSnew)))
   dataframe[No.NAs,xnew]<-xy[,1]
  dataframe[No.NAs,ynew]<-xy[,2]
  return(dataframe)
}

