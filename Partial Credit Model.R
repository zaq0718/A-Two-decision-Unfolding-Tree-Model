####Partial Credit Model####
People<-10 ## The number of people
Item<-2    ## The number of items
theta<-rnorm(People,0,1)
category<-3 ## The number of category 
beta<-matrix(cbind(rnorm((category-1),0,1),rnorm((category-1),0,1)),ncol=(category-1),nrow=Item,byrow = T) 
##beta is a matrix with (category-1)*Item
denom<-pr0<-pr1<-pr2<-matrix(NA,People,Item)  ## a matrix with the number of people*Item 
P<-array(NA,c(People,Item,category))          ## an arrary with three categories
RR<-matrix(runif((People*Item),0,1),ncol=2,nrow = 10) ## simulate random probability 
rr<-NULL                                              ## obsered responses 
for (i in 1:People){
  for (j in 1:Item){ 
    pr0[i,j] <- 1
    pr1[i,j] <- exp((theta[i]-beta[j,1]))
    pr2[i,j] <- pr1[i,j] +exp((theta[i]-beta[j,2]))
    denom[i,j]<-sum(pr0[i,j]+ pr1[i,j]+pr2[i,j])
    P[i,j,1]<- pr0[i,j]/denom[i,j]
    P[i,j,2]<-pr1[i,j]/denom[i,j]
    P[i,j,3]<-pr2[i,j]/denom[i,j]
  }
}

r1<-ceiling(RR-P[,,1])
r2<-ceiling(RR-(P[,,1]+P[,,2]))
r3<-ceiling(RR-(P[,,1]+P[,,2]+P[,,3]))
rr<-r1+r2+r3