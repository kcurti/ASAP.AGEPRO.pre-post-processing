#code to ID significant change points in a time series and code to do arima model
rm(list=ls())

run.no <- '9'

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2023.Management.Track')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')

#run.dir="C:\\State Space 2021\\Mackerel WHAM\\BaseASAP"
asapname=paste("RUN",run.no,sep='')
save.place=output.dir

ssb.rho=0
asap=dget(paste(run.dir,paste0(asapname,".RDAT"),sep="\\"))
recruits=asap$N.age[2:nrow(asap$N.age),1] #drop first so lag with ssb correct

ssb=asap$SSB[1:(length(asap$SSB)-1)] #drop last
RperS=recruits/ssb
RperS=data.frame("RperS"=RperS,"Year"=names(RperS))
#RperS=RperS[-c((nrow(RperS)-1):nrow(RperS)),] #don't use most recent 2 recruitment values due to uncertainty  #RperS[RperS$Year < 2020,] 


library(changepoint)
#Recruits per SSSB
id.changept=cpt.mean(RperS$RperS,method="SegNeigh",penalty="AIC",Q=4)
chpts=cpts(id.changept)
#summary(id.changept)
windows()
plot(id.changept,xaxt="n",ylab="R/SSB",xlab="",type="o")
axis(side=1,at=seq(1:nrow(RperS)),labels=RperS$Year)
abline(v=c(chpts),lty=2)
pt.labels=as.numeric(min(RperS$Year))+chpts
text(chpts+1,max(RperS$RperS),labels=pt.labels,srt=90)
text(chpts-1,max(RperS$RperS)-2,labels=pt.labels-1,srt=90)
# savePlot(paste(save.place,"ChangePts_RperS.jpg",sep="\\"))

#Recruits
id.changept.rec=cpt.mean(recruits[-c((length(recruits)-1):length(recruits))],method="SegNeigh",penalty="AIC",Q=3)
chpts.rec=cpts(id.changept.rec)#[2]
#summary(id.changept.rec)
plot(id.changept.rec,xaxt="n",ylab="Recruits",xlab="",type="o")
axis(side=1,at=seq(1:nrow(RperS)),labels=RperS$Year)
abline(v=c(chpts.rec),lty=2)
pt.labels.rec=as.numeric(min(RperS$Year))+chpts.rec
text(chpts.rec+1,max(recruits),labels=pt.labels.rec,srt=90)
text(chpts.rec-1,max(recruits),labels=pt.labels.rec-1,srt=90)
# savePlot(paste(save.place,"ChangePts_Recruits.jpg",sep="\\"))
# graphics.off()


#############################################ARIMA###############
recruits.ar=data.frame("R"=asap$N.age[,1]) #
recruits.ar$logR=log(recruits.ar$R)
recruits.ar$Year=as.numeric(row.names(recruits.ar))

ar.fxn=function(year.cut=NULL,recruits.ar=NULL,term.rec=recruits[length(recruits)],rho=ssb.rho){
dat=recruits.ar[recruits.ar$Year>year.cut,]
dat$et=dat$logR-mean(dat$logR)
ar=arima(dat$et,order=c(1,0,0),include.mean = F) #The et have been differenced from the mean, centering on zero and so no intercept needed. Models are the same anyway.
stdev.logR=sqrt(var(dat$logR)) #std dev of logR needed for agepro
#calc "last residual" needed for agepro
term.rec.adj=term.rec*(1/(rho+1))
e.zero=log(term.rec.adj)-mean(dat$logR)

return(list("ar"=ar,"stdev"=stdev.logR,"e.zero"=e.zero,"mean.logR"=mean(dat$logR)))
} #end ar function

allrec.ar=ar.fxn(year.cut=1968,recruits.ar=recruits.ar)
#recent.rec.ar=ar.fxn(year.cut=2012,recruits.ar=recruits.ar)
#changept.ar=ar.fxn(year.cut=1991,recruits.ar=recruits.ar)
library(lmtest)
coeftest(allrec.ar$ar)

exp(allrec.ar$e.zero+allrec.ar$mean.logR)
recruits[length(recruits)]*(1/(ssb.rho+1))

#############################################R/SSB analysis
RperS=cbind(RperS,ssb)
windows()
regress=lm(RperS~ssb,data=RperS)
plot(RperS$ssb,RperS$RperS,ylab="R/SSB",xlab="SSB")
plot(RperS$ssb/1000,RperS$RperS,ylab="R/SSB",xlab="SSB (thous mt)")
#abline(regress)

#default unfished From ASAP Run4
get_a_b_fxn=function(SSB0=378078,R0=226137,steepness=NULL){
  alpha=((4*steepness)*(SSB0/(SSB0/R0)))/(5*steepness-1)
  beta=(SSB0*(1-steepness))/(5*steepness-1)
  return(data.frame("alpha"=alpha,"beta"=beta))
}
#0.6 to 0.9 are 20th and 80th percentiles of steepness for mackerel (Myers et al. 1999 CJFAS)
Six=get_a_b_fxn(steepness=0.6)
Nine=get_a_b_fxn(steepness=0.9)
RperS$Pt6=(Six$alpha*RperS$ssb)/(Six$beta+RperS$ssb)
RperS$Pt9=(Nine$alpha*RperS$ssb)/(Nine$beta+RperS$ssb)
lines(supsmu(RperS$ssb,(RperS$Pt6/RperS$ssb)),col="black",type="l",lty=2,lwd=2)
lines(supsmu(RperS$ssb,(RperS$Pt9/RperS$ssb)),col="black",type="l",lty=3,lwd=2)
legend(x="topright",legend=c("Steepness 0.6","Steepness 0.9"), lty=c(2,3),lwd=c(2,2))
# savePlot(paste(save.place,"R_SSB.jpg",sep="\\"))
# graphics.off()



####### My additions ######
#Recruits per SSB
RperS.sort <- RperS[order(RperS$ssb), ]

id.changept=cpt.mean(RperS.sort$RperS,method="SegNeigh",penalty="AIC",Q=3)
chpts=cpts(id.changept)
nchpts <- length(chpts)


#summary(id.changept)
windows()
# plot(x=RperS$ssb, id.changept,ylab="R/SSB",xlab="SSB",type="o")
plot(id.changept,ylab="R/SSB",xlab="SSB",xaxt="n",type="o")
abline(v=c(chpts),lty=2)
# text(chpts+1,max(RperS$RperS),labels=pt.labels,srt=90)
# text(chpts-1,max(RperS$RperS)-2,labels=pt.labels-1,srt=90)
# savePlot(paste(save.place,"ChangePts_RperS_SSB.jpg",sep="\\"))


block.vec <- NULL
for (i in 1:(nchpts+1))
{
  if(i == 1) {
    lb <- 1
  } else {
    lb <- chpts[i-1]+1
  }
  if(i==(nchpts+1)) {
    ub <- nrow(RperS.sort)
  } else {
    ub <- chpts[i]
  }
  print(lb)
  print(ub)
  block.vec <- c(block.vec, rep(i, length(lb:ub)))
}


RperS.sort$block <- block.vec
RperS.sort$obs <- 1:nrow(RperS.sort)

RperS.list <- split(RperS.sort, RperS.sort$block)

CP.RperS.vals <- unlist(lapply(RperS.list, function(x) {mean(x$RperS)}))
ssb.lb <- unlist(lapply(RperS.list, function(x) {min(x$ssb)}))
  ssb.lb[1] <- 0
ssb.ub <- unlist(lapply(RperS.list, function(x) {max(x$ssb)}))


windows()
plot(RperS.sort$ssb/1000,RperS.sort$RperS,ylab="R/SSB",xlab="SSB (thous mt)")
abline(v=ssb.lb[2:(length(chpts)+1)]/1000,lty=2)
for (i in 1:(nchpts+1))
{
  ssb.line <- c(ssb.lb[i]/1000,ssb.ub[i]/1000)
  rssb.line <- rep(CP.RperS.vals[i],2)
  lines(ssb.line, rssb.line, col='red', lty=1, lwd=2)
}

  
  rep(NA,length(chpts))
SSB.lb <- 
SSB.ub <- 

for (i in 1:length(chpts))
{
  if(i == 1) {
    lb <- 1
  } else {
    lb <- chpts[i-1]+1
  }
  
  if(i==length(chpts)) {
    ub <- nrow(RperS.sort)
  } else {
    ub <- chpts[i]
  }

  CP.RperS.vals[i] <- mean(RperS.sort[lb:ub,'RperS'])
}




