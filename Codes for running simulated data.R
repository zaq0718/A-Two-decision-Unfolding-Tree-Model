library(R2WinBUGS)
GGUMTree <- function () {
  for (i in 1:I) {
    theta[i,1:2] ~ dmnorm(mu[1:2], I_cov[1:2,1:2])
    
    for (j in 1:J) {
      # Process I
      num.p1[i,j,1] <- 1
      num.p1[i,j,2] <- exp(alpha[j]*(1*(theta[i,1]-delta[j]) - tau[j]))
      num.p1[i,j,3] <- exp(alpha[j]*(2*(theta[i,1]-delta[j]) - tau[j]))
      num.p1[i,j,4] <- exp(alpha[j]*(3*(theta[i,1]-delta[j])))
      denom.p1[i,j] <- sum(num.p1[i,j,])
      
      prob.1[i,j,1] <- (num.p1[i,j,1]+num.p1[i,j,4])/denom.p1[i,j]	# Disagree
      prob.1[i,j,2] <- (num.p1[i,j,2]+num.p1[i,j,3])/denom.p1[i,j]	# Agree
      
      # Process II
      
      logit(prob.2[i,j]) <- beta[j]*(theta[i,2] - xi[j])
      
      P[i,j,1] <- prob.1[i,j,1]*prob.2[i,j]							# Strongly disagree
      P[i,j,2] <- prob.1[i,j,1]*(1-prob.2[i,j])						# Disagree
      P[i,j,3] <- prob.1[i,j,2]*(1-prob.2[i,j])						# Agree
      P[i,j,4] <- prob.1[i,j,2]*prob.2[i,j]							# Strongly agree
      
      r[i,j] ~ dcat(P[i,j,])
    }
  }
  
  rho ~ dunif(-1, 1)
  mu[1] <- 0
  mu[2] <- 0
  Cov[1,1] <- 1
  Cov[1,2] <- rho
  Cov[2,1] <- rho
  Cov[2,2] <- 1
  I_cov[1:2, 1:2] <- inverse(Cov[1:2, 1:2])
  
  for (j in 1:J) {
    # Process I
    alpha[j] ~ dlnorm(0, 4)
    delta[j] ~ dnorm(0, 0.25)
    tau[j] ~ dnorm(-2, 0.25)
    
    # Process II
    beta[j] ~ dlnorm(0, 4)
    xi[j] ~ dnorm(0, 0.25)
  }
}

init_p <- function() {
  list(alpha=rep(1, J), delta=rep(0, J), tau=rep(-2, J), beta=rep(1, J), xi=rep(0, J), rho=0.2)}

parameters <- c("alpha", "delta", "tau", "beta", "xi", "rho", "theta")

setwd("P:/Research/GGUM/JBES/N = 1000")
for (i in 6:20) {
  outpath <- paste("Raw data/data_", formatC(i, format="d", digit=2, flag="0"), ".txt", sep="")
  r <- data.matrix(read.table(outpath, header=FALSE))
  I <- nrow(r)
  J <- ncol(r)
  fdata <- list("r", "I", "J")
  
  output <- bugs(fdata, init_p, parameters, GGUMTree, n.chains=1, n.iter=10000, n.burnin=5000, n.thin=1,bugs.directory = "C:/WinBUGS14")
  pd <- paste("pD =", output$pD)
  dic <- paste("DIC =", output$DIC)
  
  outpath <- paste("Output/output_", formatC(i, format="d", digit=2, flag="0"), ".txt", sep="")
  write.table(output$summary, outpath, sep="\t")
  write(c(pd, dic), outpath, sep="\t", append=T)
}
quit("no")
