###The number of items :11
###The number of responses for an item: 5
###Item paramters:alpha, beta,delta,tau,xi; tau is a 2*11 matirx; alpha, beta, delta and xi are vectors 
###Person paramters:theta, and eta;theta is a latent triat, eta is an extrem response style 
###Five response probabilities:P, num; P and num are arrarys
###Expected scores:expected; expected is a 11*people matrix
###Papers: Thissen-Roe &Thissen (2013) and Roberts et al (2000) 
alpha<-c(0.469 ,0.344, 1.191 ,1.159, 1.155 ,0.944 ,3.473 ,1.625, 1.721 ,0.133, 1.109)
beta<-c(0.769 ,0.944, 1.489 ,1.609 ,1.838, 1.425 ,1.777, 1.723 ,1.412, 0.948 ,0.950)
delta<-c(-0.902 ,-0.134, -1.014 ,-0.163,  0.065 ,-1.190 ,-0.845, -1.377, -0.911 ,-0.672 ,-0.719)
tau<-matrix(c(0.966,-0.952,-1.059,0.571,-0.564,-1.666,-1.214,-1.882,-1.056,-0.315,-0.176,
              -0.312,-1.097,-1.261,-0.112,-0.748,-1.373,-0.932,-1.654,-0.973,-1.239,-0.094),ncol = 2,
            nrow=11,byrow = FALSE)
xi<-c(1.220, 1.314 ,0.887 ,1.115 ,1.007, 0.933, 0.728 ,0.986, 1.084, 1.599 ,0.864)
theta_1<-seq(-3,3,by=0.1)
theta<-rep(theta_1,each=61)
eta_1<-seq(-3,3,by=0.1)
eta<-rep(eta_1,times=61)

people<-length(theta)  ##The number of people
itemnum<-length(alpha) ## The number of items 
P<-array(NA, c(people, itemnum, 5))
num<-array(NA, c(people, itemnum,6))
denom<-D<-matrix(NA,nrow=people,ncol=itemnum)

for (i in 1:people) 
{
  for (j in 1:itemnum) 
  {
    num[i,j,1] <- 1
    num[i,j,2] <- exp(alpha[j]*(1*(theta[i]-delta[j]) - tau[j,1]))
    num[i,j,3] <- exp(alpha[j]*(2*(theta[i]-delta[j]) - tau[j,1] - tau[j,2]))
    num[i,j,4] <- exp(alpha[j]*(3*(theta[i]-delta[j]) - tau[j,1] - tau[j,2]))
    num[i,j,5] <- exp(alpha[j]*(4*(theta[i]-delta[j]) - tau[j,1]))
    num[i,j,6] <- exp(alpha[j]*(5*(theta[i]-delta[j])))
    denom[i,j] <- sum(num[i,j,])
    
    D[i,j] <- exp(beta[j]*(eta[i] - xi[j]))/(1+exp(beta[j]*(eta[i] - xi[j])))  ### A GGUM part 
    
    P[i,j,1] <- ((num[i,j,1]+num[i,j,6])/denom[i,j])*D[i,j]
    P[i,j,2] <- ((num[i,j,1]+num[i,j,6])/denom[i,j])*(1-D[i,j])
    P[i,j,3] <- (num[i,j,2]+num[i,j,5])/denom[i,j]
    P[i,j,4] <- ((num[i,j,3]+num[i,j,4])/denom[i,j])*(1-D[i,j])
    P[i,j,5] <- ((num[i,j,3]+num[i,j,4])/denom[i,j])*D[i,j]
  }
}

expected<-NULL
expected<-1*(P[,,1])+2*(P[,,2])+3*(P[,,3])+4*(P[,,4])+5*P[,,5]  ### Expected probabilities 

###Plot a 2-dimensional item response surface
library(scatterplot3d)
final<-cbind(theta,eta,expected)
colnames(final)<-c("theta","eta","I1","I2","I3","I4","I5","I6","I7","I8","I9","I10","I11")
par(mfrow=c(2,3))
for(i in 9:13)  ### Plot I7 to I11
{ 
  scatterplot3d(final[,1],final[,2],final[,i],highlight.3d = T,xlim=c(-3,3),ylim=c(-3,3),zlim=c(1,5),
                xlab=expression(theta),ylab="",zlab=expression(paste("E","(","X",")")),main=paste("Item",i-2),
                grid=FALSE)
  dims <- par("usr")
  x <- dims[1]+ 0.9*diff(dims[1:2])
  y <- dims[3]+ 0.1*diff(dims[1:2])
  text(x,y,expression(gamma),srt=45)
}


plot_ly(as.data.frame(final), x = final[,1], y =final[,2], z = final[,9]) %>%
  add_markers()%>%layout(scene=list(xaxis = list(range = c(-3,3)),yaxis = list(range = c(-3,3)),
                                    zaxis=list(range = c(1,5))))
