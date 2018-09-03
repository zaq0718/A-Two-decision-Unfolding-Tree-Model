library(foreign)
library(GGUM)
library(dplyr)
setwd("~\\Dropbox\\Future Research\\Response Style\\Data\\ESS")
data<-read.spss("ESS8e01_1.sav",use.value.labels =F,to.data.frame = T)
CNT<-c(levels(data$cntry))

#### 1.Data managment 
cleaning<-function(data_name,cnt)
{
if(is.character(cnt)==TRUE)
  {
  country<-subset(data_name,data_name$cntry==cnt) 
  country_WF<-country%>%select(c(dfincac, smdfslv, sbstrec, sbprvpv, sbeqsoc, sbbsntx, sblazy, sblwcoa, uentrjb, lbenent, bennent))##Select 11 items for Welfare attitudes
  NO_missing<-country_WF[complete.cases(country_WF),]
  Recode_data<-as.data.frame(sapply(NO_missing,function(x) recode(x,'1'="0",'2'="0",'3'="1",'4'="2",'5'="2")))
  country<-sapply(Recode_data, function(x) as.numeric(as.character(x)))
  }
else
  {
  print("country is not a character")
  }
return(country)
}

all<-list()
for(i in 1:length(CNT))
{
  
  all[[i]]<-cleaning(data_name=data,cnt=CNT[i])
}
names(all)<-CNT

#### 2.GGUM Analysis
analysis<-function(path,data,num_c)
{
setwd(path)
fit_GGUM<-GGUM(data,num_c)
Model_fit_GGUM<-MODFIT(fit_GGUM)
Theta<-Theta.EAP(fit_GGUM,precision = 3) 
for(i in 1:ncol(data))
{
  file_name = paste("ICC_GUMM", i, ".png", sep="")
  tiff(file_name)
  print(plotICC(fit_GGUM,Theta,items=i,quiet = TRUE))
  dev.off()
  
}
}

CH<-analysis(path="~\\Dropbox\\Future Research\\Response Style\\Data\\ESS\\CH",
             data = all[["CH"]],num_c=2) ## an example
