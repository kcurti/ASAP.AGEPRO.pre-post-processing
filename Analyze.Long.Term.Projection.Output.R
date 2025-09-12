#######
### This program reads in the projection outputs and extracts the median as well as 90th and 95th percentiles.
### The program then calculates the average of these outputs for the last nyr years
#######


rm(list=ls())
ls()


# Run details
run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')

# Projection details
proj.run <- 'Rect.1975.Onward'
proj.fname <- 'projections.100.YR.100.SIMS'
proj.fyr <- 2023
proj.lyr <- 2124
nyr.avg <- 10


########


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir,'projections.brps',proj.run)
# setwd(proj.dir)

proj.yrs <- as.character(proj.fyr:proj.lyr)


### Read in AGEPRO rdat and auxillary files

# RDAT
rdat <- dget(file.path(proj.dir, paste0(proj.fname, ".RDAT")))

# SSB (xx3)
ssb <- read.table(file.path(proj.dir, paste(proj.fname,'xx3',sep='.')))
  colnames(ssb) <- proj.yrs
ssb.median <- apply(ssb,2,median)
ssb.CI <- apply(ssb,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })
ssb.stdev <- apply(ssb,2,sd)
ssb.mean <- apply(ssb,2,mean)
ssb.cv <- ssb.stdev / ssb.mean

# MSY: Combined catch (xx6)
msy <- read.table(file.path(proj.dir, paste(proj.fname,'xx6',sep='.')))
  colnames(msy) <- proj.yrs
msy.median <- apply(msy,2,median)
msy.CI <- apply(msy,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })
msy.stdev <- apply(msy,2,sd)
msy.mean <- apply(msy,2,mean)
msy.cv <- msy.stdev / msy.mean

# Fmult (xx9)
fmult <- read.table(file.path(proj.dir, paste(proj.fname,'xx9',sep='.')))
  colnames(fmult) <- proj.yrs
fmult.median <- apply(fmult,2,median)
fmult.CI <- apply(fmult,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Stock Biomass (xx4)
biomass <- read.table(file.path(proj.dir, paste(proj.fname,'xx4',sep='.')))
  colnames(biomass) <- proj.yrs
biomass.median <- apply(biomass,2,median)
biomass.CI <- apply(biomass,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Recruitment (xx2)
rect <- read.table(file.path(proj.dir, paste(proj.fname,'xx2',sep='.')))
colnames(rect) <- proj.yrs
rect.median <- apply(rect,2,median)
rect.CI <- apply(rect,2,function(x){quantile(x, c(0.025, 0.05, 0.95, 0.975)) })



  # rect <- read.table(paste(proj.fname,'xx2',sep='.'))


### Calculate average reference points for last nyrs

avg.yrs <- tail(proj.yrs, nyr.avg)

# SSB
ssb.brp <- mean(ssb.median[avg.yrs])
ssb.brp.CI <- rowMeans(ssb.CI[,avg.yrs])
ssb.brp.CV <- mean(ssb.cv[avg.yrs])

# MSY
msy.brp <- mean(msy.median[avg.yrs])
msy.brp.CI <- rowMeans(msy.CI[,avg.yrs])
msy.brp.CV <- mean(msy.cv[avg.yrs])

# Fmult
fmult.brp <- mean(fmult.median[avg.yrs])
fmult.brp.CI <- rowMeans(fmult.CI[,avg.yrs])

# Stock biomass
biomass.brp <- mean(biomass.median[avg.yrs])
biomass.brp.CI <- rowMeans(biomass.CI[,avg.yrs])

# Rect 
rect.at.brp <- mean(rect.median[avg.yrs])


# Summary table
summary.table <- data.frame(matrix(NA,nrow=4, ncol=3))
  row.names(summary.table) <- c('FMSY proxy', 'SSBMSY proxy','BMSY proxy', 'MSY proxy')
  colnames(summary.table) <- c('Estimate','5th percentile', '95th percentile')

summary.table['FMSY proxy',] <- c(round(fmult.brp,2), round(fmult.brp.CI,2)[c('5%','95%')])
summary.table['SSBMSY proxy',] <- c(round(ssb.brp,0), round(ssb.brp.CI,0)[c('5%','95%')])
summary.table['BMSY proxy',] <- c(round(biomass.brp,0), round(biomass.brp.CI,0)[c('5%','95%')])
summary.table['MSY proxy',] <- c(round(msy.brp,0), round(msy.brp.CI,0)[c('5%','95%')])


### Save image and export CSV file

rm(list=ls()[ls()%in%c('ssb','msy','fmult','biomass','rect')]) 
save.image(file.path(proj.dir, 'Projection.summary.RDATA'))

write.csv(summary.table,file.path(proj.dir, 'Reference.point.summary.csv'))



####################################################################



rm(list=ls())
ls()

run.no <- '9'
proj.run <- 'Rect.1975.Onward'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir,'projections.brps',proj.run)
setwd(proj.dir)

load('Projection.summary.RDATA')


