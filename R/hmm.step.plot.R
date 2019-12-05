
hmm.step.plot <- function(mod, data, cols=c("black","royalblue","firebrick2")) {
  # https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-custom-plots.pdf
  nstate <- ncol(mod$mle$step)
  
  # Estimated step length parameters
  stepMean <- mod$mle$step["mean",]
  stepSD <- mod$mle$step["sd",]
  
  # step shape and rate
  stepShape <- stepMean^2/stepSD^2
  stepRate <- stepMean/stepSD^2
  
  stepMean <- stepShape
  stepSD <- 1/stepRate
  
  # Grid of step length values, to plot densities
  stepgrid<-seq(min(data$step,na.rm=TRUE),
                max(data$step,na.rm=TRUE),
                length=1000)
  
  my.hist(data$step, probability=TRUE, 
          xlab='Step length(m)', 
          breaks=seq(0, max(data$step,na.rm=TRUE), length=22)
          )
  
  for (s in 1:nstate) {
    points(stepgrid, dgamma(stepgrid, shape=stepShape[s], rate=stepRate[s]),
           col= cols[s], type="l")
  }
  legend('topright', bty='n', inset=0.02, legend=paste('state',1:nstate), lwd=1.4, col=cols)
}
