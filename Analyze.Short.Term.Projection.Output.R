#######
### This program reads in the short-term projection outputs and extracts the median as well as 90th and 95th percentiles.
#######


rm(list=ls())
ls()


# Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

# Projection details
proj.master.folder <- 'projections.2year'
proj.run <- 'Rect.2009.Onward'
proj.fname <- 'PROJECTIONS.2YEAR.100.SIMS.FMSY.PROXY.RECT.2009'
proj.fyr <- 2020
proj.lyr <- 2023


######## ######## ########


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.master.dir <- file.path(run.dir, proj.master.folder)
proj.dir <- file.path(proj.master.dir,proj.run)
setwd(proj.dir)

proj.yrs <- as.character(proj.fyr:proj.lyr)



### Read in AGEPRO auxillary files

# SSB (xx3)
ssb <- read.table(paste(proj.fname,'xx3',sep='.'))
  colnames(ssb) <- proj.yrs
ssb.median <- apply(ssb,2,median)
ssb.CI <- apply(ssb,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Combined catch (xx6)
catch <- read.table(paste(proj.fname,'xx6',sep='.'))
  colnames(catch) <- proj.yrs
catch.median <- apply(catch,2,median)
catch.CI <- apply(catch,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Fmult (xx9)
fmult <- read.table(paste(proj.fname,'xx9',sep='.'))
  colnames(fmult) <- proj.yrs
fmult.median <- apply(fmult,2,median)
fmult.CI <- apply(fmult,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Stock Biomass (Jan-1) (xx4)
biomass <- read.table(paste(proj.fname,'xx4',sep='.'))
  colnames(biomass) <- proj.yrs
biomass.median <- apply(biomass,2,median)
biomass.CI <- apply(biomass,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })

# Recruitment (xx2)
rect <- read.table(paste(proj.fname,'xx2',sep='.'))
  colnames(rect) <- proj.yrs
rect.median <- apply(rect,2,median)
rect.CI <- apply(rect,2,function(x){quantile(x,c(0.025, 0.05, 0.95, 0.975)) })



### Create summary tables for each variable

ests <- c('Median', '5th Percentile', '95th Percentile')  
summary.template <- data.frame(matrix(NA,nrow=3, ncol=(length(proj.yrs)+2)))
  colnames(summary.template) <- c(rep(NA,2),proj.yrs)
  rownames(summary.template) <- ests
summary.template[,2] <- ests

create.summary.table <- function(var.name, var.label, unit.scalar)
{
  # var.name <- 'ssb'  
  # var.label <- toupper(var.name)
  # unit.scalar <- 1
  summary.table <- summary.template
  summary.table[1,1] <- var.label
  summary.table['Median',proj.yrs]          <- round(get(paste(var.name,'median',sep='.'))/unit.scalar, 0)
  summary.table['5th Percentile',proj.yrs]  <- round(get(paste(var.name,'CI',sep='.'))['5%',]/unit.scalar, 0)
  summary.table['95th Percentile',proj.yrs] <- round(get(paste(var.name,'CI',sep='.'))['95%',]/unit.scalar, 0)
  summary.table
}

ssb.table   <- create.summary.table('ssb', 'SSB (mt)', 1)
catch.table <- create.summary.table('catch', 'Catch (mt)', 1)
f.table     <- create.summary.table('fmult','F', 1)
biomass.table <- create.summary.table('biomass','January 1 biomass (mt)', 1)
rect.table  <- create.summary.table('rect','Recruitment (000s)', 1000)

combined.table <- rbind.data.frame(ssb.table, rect.table, biomass.table, catch.table)


### Save image and export CSV file

rm(ssb, catch, fmult, biomass, rect)
save.image(file.path(proj.dir, 'Projection.summary.RDATA'))

write.csv(combined.table,file.path(proj.dir, 'Short.term.projection.summary.csv'), na='')



####################################################################



rm(list=ls())
ls()

# Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

# Projection details
proj.master.folder <- 'projections.3year'
proj.run <- 'Rect.1975.Onward'

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.master.dir <- file.path(run.dir, proj.master.folder)
proj.dir <- file.path(proj.master.dir,proj.run)

load(file.path(proj.dir, 'Projection.summary.RDATA'))


