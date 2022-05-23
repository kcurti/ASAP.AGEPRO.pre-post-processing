#######
### This program reads in the short-term projection outputs and frequency histograms and cdfs of population estimates in a particular year
#######


rm(list=ls())
ls()

library(tidyverse)

# Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
save.fig <- 'y'
fig.type <- 'tif'

# Projection details

pstar.yr <- "2032.2nditer"
rect.name <- 'Rect.2Stanza.90545mt' #   'Rect.2009' # 
f.name <- 'FMSY' # 'F12' # 
CV <- 150

### Frebuild
# proj.dir.name <- paste('rebuilding/Updated.Projections.March2022', 'F.rebuild', sep='/')
# proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), toupper(f.name), sep='.')

### PStar
proj.dir.name <- paste('rebuilding/Updated.Projections.March2022/PStar', rect.name, f.name, paste(2020,pstar.yr,sep="-"), sep='/')
#proj.dir.name <- paste('rebuilding/Additional.Projections.May2022/PStar.Constant.Catch.Combo', "CC.6316mt", paste(2020,pstar.yr,sep="-"), sep='/')
proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), paste("CV",CV,sep=""), toupper(f.name), sep='.')




######## ######## ########



### set directories and the agepro file name (proj.fname) based on above inputs

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))

proj.master.folder <- paste('projections',proj.dir.name, sep='.')
proj.master.dir <- file.path(run.dir, proj.master.folder)

# proj.dir <- file.path(proj.master.dir, rect.name, f.name)
proj.dir <- proj.master.dir

setwd(proj.dir)



### Read in rdat file

rdat <- dget(paste(proj.fname, 'RDAT', sep='.'))

proj.fyr <- rdat$genparms$startyear
proj.lyr <- rdat$genparms$endyear
proj.yrs <- as.character(proj.fyr:proj.lyr)



### Read in AGEPRO auxillary files

# SSB (xx3)
ssb <- read.table(paste(proj.fname,'xx3',sep='.'))
  colnames(ssb) <- proj.yrs
ssb.median <- apply(ssb,2,median)

# Fmult (xx9)
fmult <- read.table(paste(proj.fname,'xx9',sep='.'))
  colnames(fmult) <- proj.yrs
fmult.median <- apply(fmult,2,median)

# Recruitment (xx2)
rect <- read.table(paste(proj.fname,'xx2',sep='.'))
  colnames(rect) <- proj.yrs
rect.median <- apply(rect,2,median)



### Create histogram plot for each variable

plot.yr <- '2032'
ssb.brp <- 181090

windows()
ssb.hist <- ssb %>%
  ggplot(aes(x=!!sym(plot.yr))) + 
  geom_histogram(color="black", fill="white", bins=100) + 
  geom_vline(xintercept=ssb.brp, color="blue", size=0.9, linetype="dashed") + 
  geom_vline(xintercept=ssb.median[plot.yr], color="red", size=0.9, linetype="dashed") + 
  xlab(paste("SSB in", plot.yr)) + 
  ylab("Frequency")
ssb.hist 
if(save.fig=='y') { savePlot(file.path(proj.dir, paste(f.name, rect.name, 'ssb.histogram',plot.yr,fig.type,sep='.')), type=fig.type) }

windows()
ssb.hist.2 <- ssb %>%
  ggplot(aes(x=!!sym(plot.yr))) + 
  geom_histogram(color="black", fill="white", bins=100) + 
  annotate(geom = "vline", 
           x = c(ssb.brp, ssb.median[plot.yr]),
           xintercept = c(ssb.brp, ssb.median[plot.yr]), 
           linetype = c("dashed", "dashed"),
           color = c("blue","red"),
           size = rep(0.9, 2)
           ) + 
  annotate(geom = "text",
           x = rep(210000, 2),
           y = c(10000,10500),
           angle = 0,
           hjust = 0,
           label = c("SSB40%", paste("Median SSB in",plot.yr)),
           color = c("blue", "red")) + 
  xlab(paste("SSB in", plot.yr)) + 
  ylab("Frequency")
ssb.hist.2 
if(save.fig=='y') { savePlot(file.path(proj.dir, paste(f.name, rect.name, 'ssb.histogram.2',plot.yr,fig.type,sep='.')), type=fig.type) }

ssb.cdf <- ssb %>%
  ggplot(aes(x=!!sym(plot.yr))) + 
  stat_ecdf(geom="step", pad=FALSE) + 
  geom_vline(xintercept=ssb.brp, color="blue", size=0.9, linetype="dashed") + 
  xlab(paste("SSB in", plot.yr)) + 
  ylab("Cumulative probability")
ssb.cdf



### Save image and export CSV file

save.image(file.path(proj.dir, paste(f.name, rect.name, 'Projection.histogram.summary.RDATA', sep='.')))



####################################################################



