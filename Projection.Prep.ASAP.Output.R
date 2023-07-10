#############
#
### Get ASAP data ready for projections
#
#############


rm(list=ls())
ls()

run.no <- '9'
avg.nyrs <- 5

current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/Work/Mackerel/2023.Management.Track')


#############


### Input and organize data
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
asap.name <- paste('Run',run.no,sep='')

rdat <- paste0(asap.name, ".rdat")
# setwd(run.dir)
asap <- dget(file.path(run.dir, rdat))

yrs <- rownames(asap$N.age)
avg.yrs <- tail(yrs, avg.nyrs)
lyr <- as.integer(tail(yrs,1))


### Function to calculate age-specific CVs for a year x age matrix
calc.cv.for.yr.age.mat <- function(input)
{
  # input <- asap$WAA.mats$WAA.jan1[avg.yrs,]
  input.avg <- colMeans(input)
  input.stdev <- apply(input, 2, sd)
  input.cv <- round(input.stdev / input.avg, 2)

  list(avg=input.avg, stdev=input.stdev, cv=input.cv)
}


### Apply CV function to yr x age matrices
jan1.waa.summary  <- calc.cv.for.yr.age.mat(asap$WAA.mats$WAA.jan1[avg.yrs,])
ssb.waa.summary   <- calc.cv.for.yr.age.mat(asap$WAA.mats$WAA.ssb[avg.yrs,])
catch.waa.summary <- calc.cv.for.yr.age.mat(asap$WAA.mats$WAA.catch.fleet1[avg.yrs,])
maturity.summary  <- calc.cv.for.yr.age.mat(asap$maturity[avg.yrs,]) 


if(asap$parms$nfleets==2)
{
fleet2.catch.waa.summary <- calc.cv.for.yr.age.mat(asap$WAA.mats$WAA.catch.fleet2[avg.yrs,])
}


### Determine CV for fishery selectivity estimates; Read in estimates from std file
asap.std <- read.table(file.path(run.dir, paste0(asap.name, ".std")), header = F, skip = 1, sep = "")
 names(asap.std) <- c("index", "name", "value", "stdev")
fish.sel.params <- asap.std[substr(asap.std$name, 1, 10) == 'sel_params',]
 
fish.sel.summary <- fish.sel.params[,c('name','value','stdev')]
fish.sel.summary$CV <- round(fish.sel.summary$stdev / fish.sel.summary$value, 2)
fish.sel.summary <- as.data.frame(t(fish.sel.summary))
  
  
### Recruitment
recruits <- asap$N.age[,'1',drop=FALSE]
recruits.75onward <- recruits[as.character(1975:lyr),,drop=FALSE]
# recruits.76onward <- recruits[as.character(1976:lyr),,drop=FALSE]


### F
f.brp <- asap$Fref
f.lyr <- round(tail(asap$F.report,1),2)


### Save workspace and output csv files
proj.dir <- file.path(run.dir,'projections.brps')
if(!dir.exists(proj.dir)) {dir.create(proj.dir)}

save.image(file.path(proj.dir, 'ASAP.Data.For.Projections.RDATA'))

write.csv(do.call(rbind, jan1.waa.summary),   file.path(proj.dir, 'Jan1.WAA.for.projections.csv'))
write.csv(do.call(rbind, ssb.waa.summary),    file.path(proj.dir, 'SSB.WAA.for.projections.csv'))
write.csv(do.call(rbind, catch.waa.summary),  file.path(proj.dir, 'Catch.WAA.for.projections.csv'))
write.csv(do.call(rbind, maturity.summary),   file.path(proj.dir, 'Maturity.for.projections.csv'))
write.csv(fish.sel.summary,   file.path(proj.dir, 'Fishery.sel.for.projections.csv'))
write.csv(recruits.75onward,  file.path(proj.dir, 'Rect.for.projections.csv'))



#######################################



rm(list=ls())
ls()

run.no <- '7'
current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/Work/Mackerel/2023.Management.Track')

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir,'projections.brps')
load(file.path(proj.dir, 'ASAP.Data.For.Projections.RDATA'))


