# ASAP.packages <- c('devtools','dplyr','Hmisc','officer','plotMCMC','plotrix','readr','reshape2','tseries')
# install.packages(ASAP.packages)
# devtools::install_github("cmlegault/ASAPplots")


##################################################################


library(ASAPplots)
library(dplyr)


rm(list=ls())
ls()

run.no <- '4'
retro <- 'y'
mcmc <- 'n'

wd <- paste('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling/Run',run.no,sep='')

asap.name <- paste('Run',run.no,sep='')


if(retro=='y')  {
  wd <- file.path(paste('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling/Run',run.no,sep=''),'retro')
  asap.name <- paste('Run',run.no,'.RETRO',sep='')
}
if(mcmc=='y')  {
  wd <- file.path(paste('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling/Run',run.no,sep=''),'mcmc.2000.it.1000.thin')
  asap.name <- paste('Run',run.no,'.MCMC',sep='')
}


PlotASAP(wd, asap.name)



