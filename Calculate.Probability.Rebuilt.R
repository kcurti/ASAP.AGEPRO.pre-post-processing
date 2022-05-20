### Code to calculate the probability that the stock is rebuilt in the last year of the projection


rm(list=ls())
ls()


### Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))


### Import BRPs
outputs.dir <- file.path(run.dir, "outputs")
load(file.path(outputs.dir, "Comparison.with.BRPs.RDATA"))
ssb.brp


### Projection details
rect.name <- 'Rect.2Stanza.90545mt' #   'Rect.2009' #  
f.name <- 'FMSY' #   'F13' #
CV <- 150


### Runs to determine Frebuild
# proj.dir.name <- paste('rebuilding/Updated.Projections.March2022', 'F.rebuild', sep='/')
# proj.master.folder <- paste('projections',proj.dir.name, sep='.')
# proj.master.dir <- file.path(run.dir, proj.master.folder)
# proj.dir <- file.path(proj.master.dir, rect.name, f.name)

### P* runs
proj.dir.name <- paste('rebuilding/Additional.Projections.May2022', 'PStar.Constant.Catch.Combo', sep='/')
proj.master.folder <- paste('projections',proj.dir.name, sep='.')
proj.master.dir <- file.path(run.dir, proj.master.folder)
# proj.dir <- file.path(proj.master.dir, f.name, "2020-2032.2nd.Iter")
proj.dir <- file.path(proj.master.dir, "CC.6316mt", "2020-2032.2nd.Iter")


load(file.path(proj.dir, paste(f.name, rect.name, 'Projection.summary.RDATA', sep='.')))


### Extract SSB in projection lyr
ssb <- read.table(file.path(proj.dir, paste(proj.fname,'xx3',sep='.')))
  colnames(ssb) <- proj.yrs
ssb.lyr <- ssb[,as.character(proj.lyr)]


### Calculate probability rebuilt
ssb.prob.rebuilt <- round( length(ssb.lyr[ssb.lyr>ssb.brp]) / length(ssb.lyr) , 3)
ssb.prob.rebuilt <- data.frame(ssb.prob.rebuilt)
  rownames(ssb.prob.rebuilt) <- as.character(proj.lyr)
ssb.prob.rebuilt


### Save workspace and export
rm(ssb)
save.image(file.path(proj.dir, paste(f.name, rect.name, 'Probability.Rebuilt.RDATA', sep='.')))

write.csv(ssb.prob.rebuilt, file.path(proj.dir, paste(f.name, rect.name, 'Probability.Rebuilt.in', proj.lyr, "csv", sep='.')))


