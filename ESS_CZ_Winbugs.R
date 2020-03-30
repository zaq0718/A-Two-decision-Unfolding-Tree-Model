ibrary(foreign)
library(dplyr)
library(tidyverse)
library(R2WinBUGS)
####1. Data Management:
rm(list=ls())
setwd("~/Dropbox/Future Research/Response Style/Data/ESS")
data<-read.spss("ESS8e01_1.sav",use.value.labels =F,to.data.frame = T)
CZ<-subset(data,data$cntry=="CZ") ##Select "CZ"
CZ<-CZ%>%select(c(dfincac, smdfslv, sbstrec, sbprvpv, sbeqsoc, sbbsntx, sblazy, sblwcoa, uentrjb, lbenent, bennent))     ##Select 11 items for welfare-state  attitudes
CZ%>%map(function(x) table(x))##Check 7,8,9 missing data
CZ%>%map(function(x) sum(is.na(x))/nrow(CZ))##Missing rate
NO_missing<-CZ[complete.cases(CZ),]##Remove a row with missing data
lapply(NO_missing,function(x) sum(is.na(x))/nrow(NO_missing))##Make sure that there is no missing data

####2.Winbugs
itemnum<-11
people<-1789
response<-as.matrix(NO_missing,nrow=people,ncol=ncol(itemnum))
CZ_data<-list("people","itemnum","response")
inits_1<-function(){
  list(alpha=alpha,delta=delta,beta=beta,tau=tau,beta=beta,xi=xi,
       theta=c(rep(0,people)),eta=c(rep(0,people)))
}

setwd("~/Dropbox/Future Research/Response Style/Data/ESS/Winbugs/New Models_CZ")
schools.sim <- bugs(CZ_data, inits, model.file = "Model_CZ_new.txt",
                    parameters = c("alpha", "delta", "theta","tau","beta","eta","xi"),
                    n.chains = 1, n.iter = 3000,
                    bugs.directory = "C:/WinBUGS14/",codaPkg=TRUE)

####3. Extracted parameters from Winbugs
output<-read.bugs(schools.sim)
estimate<-summary(output)
Paramters<-as.data.frame(round(estimate[[1]],digits = 3))

alpha<-Paramters[grep("alpha",rownames(Paramters)),1]
beta<-Paramters[grep("beta",rownames(Paramters)),1]
delta<-Paramters[grep("delta",rownames(Paramters)),1]
tau.1<-Paramters[grep("tau",rownames(Paramters)),1]
tau<-matrix(tau.1,nrow=11,ncol=2,byrow=T)
theta<-Paramters[grep("theta",rownames(Paramters)),1]
xi<-Paramters[grep("xi",rownames(Paramters)),1]
eta<-Paramters[grep("^eta",rownames(Paramters)),1]
