% Please edit documentation in R/S3.R
\name{xy.project.na}
\alias{xy.project.na}
\title{Add projected coordinates to a dataframe which contains NAs}
\usage{
function(dataframe,CRSold,CRSnew,xold,yold,xnew,ynew)
}
\arguments{
\item{dataframe}{A data.frame containing coordinates with column names specified in xold and yold}
\item{CRSold}{The current CRS projection, see example}
\item{CRSnew}{The new CRS projection, see example}
\item{xold}{name of column containing x-coordinate}
\item{yold}{name of column containing y-coordinate}
\item{xnew}{name of column of projected x-coordinate}
\item{ynew}{name of column of projected y-coordinate}
}
\value{
The class of \code{x} is printed as a side-effect.
}
\description{
Function to change projection of a data.frame
}
\examples{
d<-data.frame(lon=c(3,4,NA,5),lat=c(51,NA,52,53))
xy.project.NA(dataframe=d,
              CRSold="+proj=longlat +datum=WGS84",
              CRSnew="+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              xold="lon",yold="lat",xnew="xutm",ynew="yutm")
              }
