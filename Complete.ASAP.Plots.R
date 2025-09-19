# ASAP.packages <- c('dplyr','Hmisc','officer','plotMCMC','plotrix','readr','reshape2','tseries')
# install.packages(ASAP.packages)
# # devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)
# pak::pkg_install("cmlegault/ASAPplots")

install.packages("devtools")
devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)

##################################################################


library(ASAPplots)
library(dplyr)


rm(list=ls())
ls()

run.no <- '8'
retro <- 'n'
mcmc <- 'y'

current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/2025.Management.Track')
# current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/Work/Scup/ASAP')
base.wd <- file.path(current.assess.dir,paste('Run',run.no,sep=''))
wd <- base.wd


if(retro=='n' && mcmc=='n')  {
  wd <- file.path(base.wd)
  asap.name <- paste('Run',run.no,sep='')
}
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



##### Rect #####

wd <- file.path(base.wd)
asap.name <- paste('Run',run.no,sep='')

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
yrs.sub <- 1995:tail(years,1)
recr.sub <- asap$SR.resids[asap$SR.resids$year%in%yrs.sub, 2]
plot(yrs.sub, rep(median(asap$SR.resids$recruits),length(yrs.sub)), type = "l", col = "#114466", 
     lty = 2, lwd = 2, #xlab = "Year", ylab = "Recruits ", 
     ylim = c(0, 1.1 * max(recr.sub)))
lines(yrs.sub, recr.sub, col = "grey35", lwd = 2)
points(yrs.sub, recr.sub, pch = 19)


### Retro

# if (retro.flag == TRUE) {
#   windows()
#   pdf(file = paste0(od, pdf.name, ".RETRO.PLOTS.pdf"), 
#       onefile = TRUE)
#   PlotRetroWrapper(wd, asap.name, asap, save.plots, od, 
#                    plotf)
#   dev.off()
#   graphics.off()
# }


##### MCMC #####

# 
# if (asap$options$do.mcmc > 0) {
#   windows()
#   pdf(file = paste0(od, pdf.name, ".MCMC.PLOTS.pdf"), onefile = T)
#   PlotMCMC(wd, asap.name, asap, mcmc.burn, mcmc.thin, save.plots, 
#            od, plotf)
#   dev.off()
#   graphics.off()


library(tidyverse)

wd <- file.path(base.wd,'mcmc.2000.it.1000.thin')
asap.name <- paste('Run',run.no,'.MCMC',sep='')

rdat <- paste0(asap.name, ".rdat")
asap <- dget(paste0(wd, "\\", rdat))


### SSB

ssb.mcmc <- read_csv(file.path(wd, "plots", paste0("ssb.90pi_Run",run.no,".MCMC.csv")))

years <- ssb.mcmc$years
nyears <- length(years)

windows(width=5, height=7)
  plot(years, ssb.mcmc$'5th', type = "l", col = "grey35", lwd = 2, 
       xlab = "Year", ylab = "", ylim = c(0, 1.03 * max(ssb.mcmc[,'95th'])), axes = F)
  axis(side = 1, at = years[seq(1, nyears, by = 2)], labels = years[seq(1, 
                                                                        nyears, by = 2)], las = 2)
  axis(side = 2, at = pretty(seq(0, 1.01 * max(ssb.mcmc[,'95th'])), 
                             n = 10), labels = format(pretty(seq(0, 1.01 * max(ssb.mcmc[,'95th'])), 
                                                             n = 10), scientific = T), las = 1)
  axis(side = 4, at = pretty(seq(0, 1.01 * max(ssb.mcmc[,'95th'])), n = 10), labels=NA, las = 1)
  box()
  lines(years, ssb.mcmc$'95th', col = "grey35", lwd = 2)
  lines(years, ssb.mcmc$'Median', col = "orangered1", lwd = 2)
  lines(years, asap$SSB, col = "green3", lwd = 1)
  points(years, asap$SSB, col = "green3", pch = 17, cex = 0.7)
  legend("topleft", horiz = T, legend = c("5th, 95th", "Median", 
                                          "Point Est."), col = c("grey35", "orangered1", "green3"), lwd = c(2, 
                                                                                                     2, 2), lty = c(1, 1, 1), cex = 0.85, pch = c(1, 1, 17), pt.cex = c(0, 0, 1))
  
  
### Full F
  
f.mcmc <- read_csv(file.path(wd, "plots", paste0("Full.F.90pi_Run",run.no,".MCMC.csv")))

years <- f.mcmc$years
nyears <- length(years)

full.f <- apply(asap$F.age, 1, max)

windows(width=5, height=7)
plot(years, f.mcmc$'5th', type = "l", col = "grey35", lwd = 2, 
     xlab = "Year", ylab = "Full F", ylim = c(0, 1.1 * max(f.mcmc[,'95th'])), axes = F)
axis(side = 1, at = years[seq(1, nyears, by = 2)], labels = years[seq(1, 
                                                                      nyears, by = 2)], las = 2)
axis(side = 2, at = pretty(seq(0, 1.01 * max(f.mcmc$'95th'), by = 0.1), 
                           n = 10), labels = pretty(seq(0, 1.01 * max(f.mcmc$'95th'), 
                                                        by = 0.1), n = 10), las = 1)
axis(side = 4, at = pretty(seq(0, 1.01 * max(f.mcmc$'95th'), by = 0.1), n = 10), labels = NA, las = 1)
box()
lines(years, f.mcmc$'95th', col = "grey35", lwd = 2)
lines(years, f.mcmc$'Median', col = "orangered1", lwd = 2)
lines(years, full.f, col = "green3", lwd = 1)
points(years, full.f, col = "green3", pch = 17, cex = 0.7)
legend("topleft", horiz = T, legend = c("5th, 95th", "Median", 
                                        "Point Est."), col = c("grey35", "orangered1", "green3"), lwd = c(2, 
                                                                                                   2, 2), lty = c(1, 1, 1), cex = 0.85, pch = c(1, 1, 17), 
       pt.cex = c(0, 0, 1))



plot(years, , type = "l", col = "grey35", lwd = 2, 
     xlab = "Year", ylab = "", ylim = c(0, 1.03 * max(f.mcmc[,'95th'])), axes = F)
axis(side = 1, at = years[seq(1, nyears, by = 2)], labels = years[seq(1, 
                                                                      nyears, by = 2)], las = 2)
axis(side = 2, at = pretty(seq(0, 1.01 * max(f.mcmc[,'95th'])), 
                           n = 10), labels = format(pretty(seq(0, 1.01 * max(f.mcmc[,'95th'])), 
                                                           n = 10), scientific = T), las = 1)
axis(side = 4, at = pretty(seq(0, 1.01 * max(f.mcmc[,'95th'])), n = 10), labels=NA, las = 1)
box()
mtext(side = 2, text = "SSB", outer = T)
lines(years, f.mcmc$'5th', col = "grey35", lwd = 2)
lines(years, f.mcmc$'95th', col = "grey35", lwd = 2)
lines(years, f.mcmc$'Median', col = "orangered1", lwd = 2)
lines(years, full.f, col = "green3", lwd = 1)
points(years, full.f, col = "green3", pch = 17, cex = 0.7)
legend("topleft", horiz = T, legend = c("5th, 95th", "Median", 
                                        "Point Est."), col = c("grey35", "orangered1", "green3"), lwd = c(2, 
                                                                                                          2, 2), lty = c(1, 1, 1), cex = 0.85, pch = c(1, 1, 17), 
       pt.cex = c(0, 0, 1))  
  

##### R/SSB

wd <- file.path(base.wd)
asap.name <- paste('Run',run.no,sep='')

rdat <- paste0(asap.name, ".rdat")
asap <- dget(paste0(wd, "\\", rdat))

windows(height=7, width=5)  
par(mfrow = c(1, 1))
ssb <- asap$SSB
recr <- asap$N.age[, 1]
years <- seq(asap$parms$styr, asap$parms$endyr)
nyears <- length(years)
SR <- matrix(NA, (nyears - 1), 4)
SR[, 1] <- years[1:(nyears - 1)]
SR[, 2] <- ssb[1:(nyears - 1)]
SR[, 3] <- recr[2:nyears]
SR[, 4] <- SR[, 3]/SR[, 2]
yr.text = substr(SR[, 1], 3, 4)
npts <- length(years) - 1

plot(SR[, 1], SR[, 4], type = "o", col = "black", pch = 16, 
     xlab = "Year", ylab = "R/SSB")
abline(h = mean(SR[, 4]), lty = 2)
  

windows(height=7, width=5)  
par(mfrow = c(1, 1))

plot(SR[, 2]/1000, SR[, 4], type = "p", col = "black", pch = 16, 
     xlab = "SSB (thous mt)", ylab = "R/SSB")

  