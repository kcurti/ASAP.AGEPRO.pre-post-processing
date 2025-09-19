### Code to calculate the probability that the stock is rebuilt in the last year of the projection


rm(list=ls())
ls()


### Run details
run.no <- '8'
current.assess.dir <- c('C:/Users/Kiersten.Curti/Desktop/2025.Management.Track')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))


### Import BRPs
outputs.dir <- file.path(run.dir, "outputs")
load(file.path(outputs.dir, "Comparison.with.BRPs.RDATA"))
ssb.brp


### Projection details
rect.name <- 'Rect.2Stanza' #   'Rect.2009' #  
f.name <- 'F20' #   'F13' #


### Runs to determine Frebuild
proj.master.folder <- c('projections.short.term')
proj.master.dir <- file.path(run.dir, proj.master.folder)
# proj.dir <- file.path(proj.master.dir)
proj.dir <- file.path(proj.master.dir, f.name)


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



