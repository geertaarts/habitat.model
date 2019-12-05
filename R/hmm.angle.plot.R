
hmm.angle.plot <- function(mod, data, cols=c("black","royalblue","firebrick2")) {
  # https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-custom-plots.pdf
  nstate <- ncol(mod$mle$step)
  
  # Estimated turning angle parameters
  angleMean <- mod$mle$angle["mean",]
  angleCon <- mod$mle$angle["concentration",]
  
  # Grid of step length values, to plot densities
  anglegrid <- seq(-pi, pi,length=1000)
  
  hist.dat <- hist(data$angle, 
                   breaks=seq(-pi, pi, length=15), 
                   plot=FALSE)
  
  my.hist(data$angle, 
          breaks=seq(-pi, pi, length=15),
          xlab="Turning angle (radians)", 
          freq=FALSE, xlim=c(-pi, pi),
          xaxt='n',
          ylim=c(0, 1.3*max(hist.dat$density))
          )
  axis(side=1, at=c(-pi, -pi/2, 0, pi/2, pi), labels=expression(-pi, -pi/2, 0, pi/2, pi))
  
  for (s in 1:nstate) {
    points(anglegrid, CircStats::dvm(anglegrid, mu=angleMean[s], kappa=angleCon[s])/max(hist.dat$density),
           col=cols[s], type="l")
  }
  legend('topright', bty='n', inset=0.05, legend=paste('state',1:nstate), lwd=1.4, col=cols)
}
