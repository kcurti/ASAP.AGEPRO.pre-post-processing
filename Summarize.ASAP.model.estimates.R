#######
#
# Takes base ASAP output (RDAT file) and summarizes SSB, B, F, FAA, NAA, selectivity and terminal year estimates
# Program used to be called Create.tables.for.final.ASAP.model.R during 2017 benchmark
# May 2021: Changed Fmult to Freport to make sure correct F is extracted when have two fleets
#
#######


rm(list=ls())
ls()

run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')


##########################################


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
asap.name <- paste('Run',run.no,sep='')
rdat <- paste0(asap.name, ".RDAT")
asap <- dget(file.path(run.dir, rdat))

fyr <- asap$parms$styr
lyr <- asap$parms$endyr
nyr <- asap$parms$nyears
yrs <- fyr:lyr
nages <- asap$parms$nages


output.dir <- file.path(run.dir,'outputs')
  if(!dir.exists(output.dir)) {dir.create(output.dir)}


# Check units that NAA indeed in same units as inputted CAA (which in this case was thousands of fish)
# caa.fyr should be close to the inputted CAA in the first year
faa.fyr <- asap$fleet.FAA$FAA.directed.fleet1[as.character(fyr),]
naa.fyr <- asap$N.age[as.character(fyr),]
caa.fyr <- round(faa.fyr*naa.fyr,2)


# Import ASAP summary file
asap.summary <- read.csv(file.path(run.dir, "plots", paste('ASAP_summary_', asap.name, '.csv', sep="")))


# Annual B and F estimates
annual.ests <-             cbind(asap.summary[,'SSB',drop=FALSE], as.matrix(asap$tot.jan1.B), asap.summary[,'Recr',drop=FALSE], asap.summary[,'Freport',drop=FALSE])
  colnames(annual.ests) <- c(                  'SSB',                          'January 1 B',               'Rect',                           'F')
  rownames(annual.ests) <- as.character(yrs)
annual.ests[,c('SSB', 'January 1 B', 'Rect')] <- round(annual.ests[,c('SSB', 'January 1 B', 'Rect')], 2)
annual.ests[,'F'] <- round(annual.ests[,'F'], 3)   
  

# Fishery selectivity
fishery.sel <- unique(asap$fleet.sel.mats$sel.m.fleet1)


# Output csv files for assessment tables
write.csv(round(asap$N.age/1000,2) , file.path(output.dir, paste('Run',run.no,'.N.age.millions.csv',sep='')) )
write.csv(round(asap$F.age,3) , file.path(output.dir, paste('Run',run.no,'.F.age.csv',sep='')) )
write.csv(annual.ests , file.path(output.dir, paste('Run',run.no,'.Annual.estimates.csv',sep='')) )
write.csv(round(t(fishery.sel),2) , file.path(output.dir, paste('Run',run.no,'.Fishery.selectivity.csv',sep='')) )

 
# Save final workspace
save.image( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.RDATA',sep='')) )



##########################################



rm(list=ls())
ls()

run.no <- '4'

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
run.dir <- paste('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling/Run',run.no,sep='')
output.dir <- file.path(run.dir,'outputs')
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.RDATA',sep='')) )

