#######
### This program reads in the short-term projection outputs and frequency histograms and cdfs of population estimates in a particular year
#######


rm(list=ls())
ls()

library(tidyverse)

# Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')


# Projection details

# pstar.yr <- "2032.2nd.iter"
rect.name <- 'Rect.2Stanza.90545mt' #   'Rect.2009' # 
f.name <- 'F12' # 'FMSY' # 
CV <- 150
proj.dir.name <- paste('rebuilding/Updated.Projections.March2022', 'F.rebuild', sep='/')
proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), toupper(f.name), sep='.')
# proj.dir.name <- paste('rebuilding/Additional.Projections.May2022/PStar.Constant.Catch.Combo', "CC.6316mt", paste(2020,pstar.yr,sep="-"), sep='/')
# proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), paste("CV",CV,sep=""), toupper(f.name), sep='.')




######## ######## ########



### set directories and the agepro file name (proj.fname) based on above inputs

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))

proj.master.folder <- paste('projections',proj.dir.name, sep='.')
proj.master.dir <- file.path(run.dir, proj.master.folder)

proj.dir <- file.path(proj.master.dir, rect.name, f.name)
# proj.dir <- proj.master.dir

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
ssb.CI <- apply(ssb,2,function(x){quantile(x, quant.values) })

# Fmult (xx9)
fmult <- read.table(paste(proj.fname,'xx9',sep='.'))
  colnames(fmult) <- proj.yrs
fmult.median <- apply(fmult,2,median)
fmult.CI <- apply(fmult,2,function(x){quantile(x, quant.values) })

# Recruitment (xx2)
rect <- read.table(paste(proj.fname,'xx2',sep='.'))
  colnames(rect) <- proj.yrs
rect.median <- apply(rect,2,median)
rect.CI <- apply(rect,2,function(x){quantile(x, quant.values) })



### Create histogram plot for each variable

plot.col <- '2032'
ssb.brp <- 181090

ssb.hist <- ssb %>%
  ggplot(aes(x=!!sym(plot.col))) + 
  geom_histogram(color="black", fill="white", bins=100) + 
  geom_vline(xintercept=ssb.brp, color="blue", size=0.9, linetype="dashed") + 
  xlab(paste("SSB in", plot.col)) + 
  ylab("Count")
ssb.hist 

ssb.cdf <- ssb %>%
  ggplot(aes(x=!!sym(plot.col))) + 
  stat_ecdf(geom="step", pad=FALSE) + 
  geom_vline(xintercept=ssb.brp, color="blue", size=0.9, linetype="dashed") + 
  xlab(paste("SSB in", plot.col)) + 
  ylab("Cumulative probability")
ssb.cdf



### Save image and export CSV file

save.image(file.path(proj.dir, paste(f.name, rect.name, 'Projection.histogram.summary.RDATA', sep='.')))



####################################################################



