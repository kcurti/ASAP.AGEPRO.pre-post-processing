# ASAP.packages <- c('dplyr','Hmisc','officer','plotMCMC','plotrix','readr','reshape2','tseries')
# install.packages(ASAP.packages)
# devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)


##################################################################


library(ASAPplots)
library(dplyr)


rm(list=ls())
ls()

run.no <- '9'
retro <- 'n'
mcmc <- 'n'

# current.assess.dir <- c('//nefscfile/Atlantic_Mackerel/Kiersten_Curti/Modeling/2021.Management.Track')
# current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/Work/Mackerel/2021.MT.Modeling')
current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/Work/Mackerel/2023.Management.Track')
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


#############################


rdat <- paste0(asap.name, ".rdat")
asap <- dget(paste0(wd, "\\", rdat))


# Compare recruitment to median
cbind.data.frame(asap$SR.resids$year, asap$SR.resids$recruits > median(asap$SR.resids$recruits))

# Modified recruitment figure for ppt
windows(height=6, width=5)
par(mfrow = c(2, 1), mar = c(3, 3, 1.5, 2))
years <- asap$SR.resids[, 1]
recr <- asap$SR.resids[, 2]
plot(years, rep(median(asap$SR.resids$recruits),length(years)), type = "l", col = "#114466", 
     lty = 2, lwd = 2, #xlab = "Year", ylab = "Recruits ", 
     ylim = c(0, 1.1 * max(recr)))
lines(years, recr, col = "grey35", lwd = 2)
points(years, recr, pch = 19)
yrs.sub <- 1980:tail(years,1)
recr.sub <- asap$SR.resids[asap$SR.resids$year%in%yrs.sub, 2]
plot(yrs.sub, rep(median(asap$SR.resids$recruits),length(yrs.sub)), type = "l", col = "#114466", 
     lty = 2, lwd = 2, #xlab = "Year", ylab = "Recruits ", 
     ylim = c(0, 1.1 * max(recr.sub)))
lines(yrs.sub, recr.sub, col = "grey35", lwd = 2)
points(yrs.sub, recr.sub, pch = 19)


