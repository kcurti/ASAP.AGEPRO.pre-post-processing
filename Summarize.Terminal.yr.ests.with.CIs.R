#######
#
# Builds off of workspace created in Summarize.ASAP.Model.estimates.R
# Takes the MCMC results and summarizes the 90% CIs
# Estimate.type specifies whether to use the ASAP point estimates or the MCMC medians for the final estimates; The code always uses the MCMC 90% CIs
# Program used to be called Terminal.yr.ests.and.CIs.R during 2017 benchmark
#
#######


rm(list=ls())
ls()


run.no <- '4'
mcmc.folder.name <- 'mcmc.2000.it.1000.thin'

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

estimate.type <- 'point.est'   # 'median' or 'point.est'


#######################


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')
mcmc.dir <- file.path(run.dir, mcmc.folder.name, 'plots') 

load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.RDATA',sep='')) )


### Medians and PI's from base run
ssb.ests <- read.csv(file.path(mcmc.dir, paste('ssb.90pi_Run',run.no,'.MCMC.csv',sep='')))
 rownames(ssb.ests) <- ssb.ests$years

f.ests <- read.csv(file.path(mcmc.dir, paste('Freport.90pi_Run',run.no,'.MCMC.csv',sep='')))
 rownames(f.ests) <- f.ests$years
 
biomass.ests <- read.csv(file.path(mcmc.dir, paste('Jan1.B.90pi_Run',run.no,'.MCMC.csv',sep='')))
 rownames(biomass.ests) <- biomass.ests$years

NAA.ests.lyr <- read.table(file.path(run.dir, mcmc.folder.name, paste('Run',run.no,'.MCMC.BSN',sep='')), sep=" ") 
NAA.ests.lyr <- NAA.ests.lyr[,!apply(is.na(NAA.ests.lyr), 2, all)]

median.annual.ests <- cbind.data.frame(ssb.ests[,'Median',drop=FALSE], biomass.ests[,'Median',drop=FALSE], f.ests[,'Median',drop=FALSE])
  colnames(median.annual.ests) <-   c('SSB',                          'January 1 B',                      'F')
  
  
### Terminal year estimates with CIs
terminal.yr.ests <- data.frame(matrix(NA,4,3)) 
  rownames(terminal.yr.ests) <- c('SSB.mt', 'Jan1B.mt', 'Rect.thous', 'Avg.F')
  colnames(terminal.yr.ests) <- c('Estimate','5th percentile','95th percentile')

if(estimate.type == 'median')
{
  terminal.yr.ests['SSB.mt',  ] <- ssb.ests[    as.character(lyr),c('Median','X5th','X95th')]
  terminal.yr.ests['Jan1B.mt',] <- biomass.ests[as.character(lyr),c('Median','X5th','X95th')]
  terminal.yr.ests['Avg.F',   ] <- round(f.ests[as.character(lyr),c('Median','X5th','X95th')], 3)
  terminal.yr.ests['Rect.thous',] <- quantile(NAA.ests.lyr[,1], c(0.5, 0.05, 0.95))
}

if(estimate.type == 'point.est')
{
  terminal.yr.ests['SSB.mt',  ] <- c( annual.ests[as.character(lyr),'SSB'],         ssb.ests[    as.character(lyr),c('X5th','X95th')] )       
  terminal.yr.ests['Jan1B.mt',] <- c( annual.ests[as.character(lyr),'January 1 B'], biomass.ests[as.character(lyr),c('X5th','X95th')] )
  terminal.yr.ests['Avg.F',   ] <- c( annual.ests[as.character(lyr),'F'],           round(f.ests[as.character(lyr),c('X5th','X95th')], 3) )           
  terminal.yr.ests['Rect.thous',] <- c( annual.ests[as.character(lyr),'Rect'], quantile(NAA.ests.lyr[,1], c(0.05, 0.95)) )
}
  

### Formatted table
terminal.yr.ests.formatted <- terminal.yr.ests
  rownames(terminal.yr.ests.formatted) <- c('Spawning stock biomass (mt)', 'January 1 biomass (mt)', 'Recruitment (000s)', 'Average F (ages 6-9)')


### Save and output
save.image( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )
write.csv(terminal.yr.ests.formatted, file.path(output.dir, paste('Run',run.no,'.Terminal.yr.estimates.with.CIs.csv',sep='')) )
write.csv(median.annual.ests, file.path(output.dir, paste('Run',run.no,'.Annual.Median.Estimates.From.MCMC.csv',sep='')) )



##########################################



rm(list=ls())
ls()

run.no <- '4'

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )

