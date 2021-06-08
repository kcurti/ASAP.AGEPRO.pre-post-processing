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

current.assess.dir <- c('//net.nefsc.noaa.gov/home0/kcurti/Mackerel/Modeling/2021.Management.Track')
base.wd <- file.path(current.assess.dir,paste('Run',run.no,sep=''))
wd <- base.wd

asap.name <- paste('Run',run.no,sep='')


if(retro=='y')  {
  wd <- file.path(base.wd,'retro')
  asap.name <- paste('Run',run.no,'.RETRO',sep='')
}
if(mcmc=='y')  {
  wd <- file.path(base.wd,'mcmc.2000.it.1000.thin')
  asap.name <- paste('Run',run.no,'.MCMC',sep='')
}


PlotASAP(wd, asap.name)



