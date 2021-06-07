# Create Catch and Status Table for summary document


rm(list=ls())
ls()

run.no <- '4'

net.dir <- c('//net.nefsc.noaa.gov/home0/kcurti')
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

estimate.type <- 'point.est'   # 'median' or 'point.est'
                               #  i.e the median MCMC estimates or the ASAP point estimates 


#######################


### Load ASAP output

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )

# Specify years
table.yrs <- as.character((lyr-9):lyr)
avg.yrs <- as.character(fyr:lyr)

# Obtain model estimates for table
if(estimate.type == 'point.est') {model.ests <- annual.ests}
if(estimate.type == 'median')    {model.ests <- median.annual.ests}

# Create table to be filled
status <- data.frame(matrix(NA,nrow=9,ncol=14))
  status[,1] <- c(
                  'Commercial landings',
                  'Commercial discards',
                  'Recreational catch',
                  'Canadian catch',
                  'Catch used in assessment',
                  'NA',
                  'Spawning stock biomass',
                  'Recruitment (age 1)',
                  'Fully selected F' ) 
  rownames(status) <- c(
                  'comm.land',
                  'comm.disc',
                  'rec.catch',
                  'can.catch',
                  'assess.catch',
                  'blank',
                  'ssb',
                  'rect',
                  'ffull' ) 
  colnames(status) <- c(
                  'Year',
                  table.yrs,
                  'Min', 'Max', 'Mean' )


# Fill table with ASAP output        
status['ssb',table.yrs]   <- model.ests[table.yrs,'SSB']
status['rect',table.yrs]  <- round(annual.ests[table.yrs,'Rect'] / 1000, 1)
status['ffull',table.yrs] <- round(model.ests[table.yrs,'F'], 2)
status['assess.catch',table.yrs] <- round(asap$catch.obs[,table.yrs],0)

status['ssb',c('Min','Max')]  <- range(model.ests[avg.yrs,'SSB'])
status['ssb',c('Mean')]       <- mean (model.ests[avg.yrs,'SSB'])

status['rect',c('Min','Max')] <- round( range(annual.ests[table.yrs,'Rect'] / 1000), 1)
status['rect',c('Mean')] <-      round( mean(annual.ests[table.yrs,'Rect']  / 1000), 1)

status['ffull',c('Min','Max')] <- round( range(model.ests[avg.yrs,'F']), 2)
status['ffull',c('Mean')]      <- round( mean( model.ests[avg.yrs,'F']), 2)

status['assess.catch',c('Min','Max')] <- round( range(asap$catch.obs[,avg.yrs]), 0)
status['assess.catch',c('Mean')]      <- round( mean( asap$catch.obs[,avg.yrs]), 0)


### Load total catch workspace to fill in remaining rows of status table

totc.dir <- file.path(net.dir, 'Mackerel/Fishery-dependent.data/Total.catch/', paste(fyr,lyr,sep='-'))
totc.env <- new.env()
load( file.path(totc.dir, paste('Total.Catch',fyr,lyr,'RDATA',sep='.')), envir=totc.env )
  
status['comm.land',table.yrs] <- round(totc.env$totc.final[table.yrs,'US.Commercial'],0)
status['comm.disc',table.yrs] <- round(totc.env$totc.final[table.yrs,'US.Comm.discards'],0)  
status['rec.catch',table.yrs] <- round(totc.env$totc.final[table.yrs,'US.Recreational'],0)  
status['can.catch',table.yrs] <- round(totc.env$totc.final[table.yrs,'Canada'],0)    
  
status['comm.land',c('Min','Max')] <- round( range(totc.env$totc.final[avg.yrs,'US.Commercial']) ,0)
status['comm.land',c('Mean')]      <- round( mean( totc.env$totc.final[avg.yrs,'US.Commercial']) ,0)

status['comm.disc',c('Min','Max')] <- round( range(totc.env$totc.final[avg.yrs,'US.Comm.discards'], na.rm=TRUE) ,0)
status['comm.disc',c('Mean')]      <- round( mean( totc.env$totc.final[avg.yrs,'US.Comm.discards'], na.rm=TRUE) ,0)
  
status['rec.catch',c('Min','Max')] <- round( range(totc.env$totc.final[avg.yrs,'US.Recreational'], na.rm=TRUE) ,0)
status['rec.catch',c('Mean')]      <- round( mean( totc.env$totc.final[avg.yrs,'US.Recreational'], na.rm=TRUE) ,0)

status['can.catch',c('Min','Max')] <- round( range(totc.env$totc.final[avg.yrs,'Canada']) ,0)
status['can.catch',c('Mean')]      <- round( mean( totc.env$totc.final[avg.yrs,'Canada']) ,0)


### Save outputs
save.image(file.path(output.dir,'Summary.Catch.Status.Table.RDATA'))
write.csv(status, file.path(output.dir,'Summary.Catch.Status.Table.csv'), row.names=FALSE, na='')


