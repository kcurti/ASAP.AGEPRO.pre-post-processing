

rm(list=ls())
ls()


# Save figure?
save.fig <- 'y'


# ASAP details
run.no <- '4'


net.dir <- '//net.nefsc.noaa.gov/home0/kcurti'
# Modeling directory
modeling.dir <- file.path(net.dir, 'Mackerel/Modeling')
# Current assessment name
current.assess <- '2021.Management.Track'
  names(current.assess) <- 'MT.2021'

# Vector of previous assessment names to include (that are not in the historical assessment spreadsheet); These names correspond to the modeling folder name
prev.assess <- '2017.Benchmark'
  names(prev.assess) <- 'Bench.2017'
# Vector of final runs corresponding to each previous assessment
final.runs <- prev.assess
  final.runs['Bench.2017'] <- 'Run118'

# Directory for current assessment
  current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
 
   
########################################################


### Current assessment model
run.wd <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.wd,'outputs')
current.assess.env <- new.env()
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')), envir=current.assess.env )
# current.ests <- current.assess.env$annual.ests
current.ests <- vector('list',3)
  names(current.ests) <- c('SSB','F','Rect')
current.ests[['SSB']] <- current.assess.env$ssb.ests   
current.ests[['F']]   <- current.assess.env$f.ests
current.ests[['Rect']] <- data.frame(current.assess.env$annual.ests[,'Rect',drop=FALSE])
current.yrs <- rownames(current.ests[[1]])


### Previous assessment models that I completed
prev.ests <- vector('list',length(prev.assess))
  names(prev.ests) <- names(prev.assess)
for (assess.name in names(prev.ests))
{
  if(assess.name == 'Bench.2017')
  {
    assess.dir <- file.path(modeling.dir, prev.assess[assess.name], 'First.Year.1968', final.runs[assess.name])
  }  else {
    assess.dir <- file.path(modeling.dir, prev.assess[assess.name], final.runs[assess.name]) 
  }
  orig <- read.csv(file.path(assess.dir, 'outputs', paste(final.runs[assess.name], 'Annual.estimates.csv', sep='.')), row.names=1)
  prev.ests[[assess.name]] <- orig[current.yrs,]
    rownames(prev.ests[[assess.name]]) <- current.yrs
}


### Historical assessment results
retro.dir <- file.path(modeling.dir, 'Historical.Assessment.Estimates')
hist.ests.mat <- read.csv(file.path(retro.dir,'Previous.Retro.Adjusted.Stock.Estimates.csv'))
  rownames(hist.ests.mat) <- as.character(hist.ests.mat$YEAR)
# Select years corresponding to the current assessment  
hist.ests.mat <- hist.ests.mat[current.yrs,] 
  rownames(hist.ests.mat) <- current.yrs
# Convert to a list
hist.assess <- c('2009.TRAC', '2005.Benchmark')
  names(hist.assess) <- c('TRAC','SAW42')
hist.ests <- vector('list',length(hist.assess))
  names(hist.ests) <- names(hist.assess)
for(assess.name in names(hist.ests))
{
  # assess.name <- names(hist.ests[1])
  hist.ests[[assess.name]] <- hist.ests.mat[,grepl(assess.name, colnames(hist.ests.mat))]
    colnames(hist.ests[[assess.name]]) <- do.call(rbind, strsplit(colnames(hist.ests[[assess.name]]), '\\.'))[,1]
}


### Color list
color.list <- c("magenta3", "limegreen", "midnightblue", "gold2", "steelblue1", "magenta3", "limegreen", "midnightblue", "gold2", "steelblue1")


 
### Functions to plot F, SSB and Recruitment

plot.F <- function(include.saw42, write.xlab)
{
  # include.saw42 <- FALSE, write.xlab <- TRUE
 
  current.ests.var <- current.ests[['F']]
  all.prev.ests.var <- lapply(c(prev.ests, hist.ests), function(x) {data.frame(x[,'F',drop=FALSE])})
    if(!include.saw42) {all.prev.ests.var <- all.prev.ests.var[!names(all.prev.ests.var)=='SAW42']}

  ymax <- max(c(current.ests.var$X95th, unlist(all.prev.ests.var)), na.rm=TRUE)

  plot(current.yrs, current.ests.var$Median, ylim=c(0,ymax), axes=FALSE, xlab='', ylab='', type='l', col='black', cex=0.6, lwd=2)
  polygon(x = c(current.yrs, rev(current.yrs)),
          y = c(current.ests.var$X5th, rev(current.ests.var$X95th)),
          col =  adjustcolor("darkslategrey", alpha.f = 0.20), border = NA)
  for (i in 1:length(all.prev.ests.var))
  {
    assess.data <- all.prev.ests.var[[i]]
    lines(as.integer(rownames(assess.data)), assess.data[,'F'], col=color.list[i])
  }
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.1, padj = 0.25)
  axis(side=1, at=axTicks(1), labels=write.xlab, cex.axis=1.1, padj = -0.25)
  box()
  mtext('Fishing mortality', side=2, line=2.7, cex=0.8) 
} 


plot.SSB <- function(include.saw42, write.xlab)
{
  # include.saw42 <- FALSE, write.xlab <- TRUE; 
  
  current.ests.var <- current.ests[['SSB']]/1000
  all.prev.ests.var <- lapply(c(prev.ests, hist.ests), function(x) {data.frame(x[,'SSB',drop=FALSE])/1000})
  if(!include.saw42) {all.prev.ests.var <- all.prev.ests.var[!names(all.prev.ests.var)=='SAW42']}
  
  ymax <- max(c(current.ests.var$X95th, unlist(all.prev.ests.var)), na.rm=TRUE)
  
  plot(current.yrs, current.ests.var$Median, ylim=c(0,ymax), axes=FALSE, xlab='', ylab='', type='l', col='black', cex=0.6, lwd=2)
  polygon(x = c(current.yrs, rev(current.yrs)),
          y = c(current.ests.var$X5th, rev(current.ests.var$X95th)),
          col =  adjustcolor("darkslategrey", alpha.f = 0.20), border = NA)
  for (i in 1:length(all.prev.ests.var))
  {
    assess.data <- all.prev.ests.var[[i]]
    lines(as.integer(rownames(assess.data)), assess.data[,'SSB'], col=color.list[i])
  }
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.1, padj = 0.25)
  axis(side=1, at=axTicks(1), labels=write.xlab, cex.axis=1.1, padj = -0.25)
  box()
  mtext('SSB (000s mt)', side=2, line=2.7, cex=0.8) 
} 


plot.Rect <- function(include.saw42, write.xlab)
{
  current.ests.var <- current.ests[['Rect']] / 1000
  prev.ests.var <- lapply(c(prev.ests), function(x) {data.frame(x[,'Rect',drop=FALSE]/1000)})
  hist.ests.var <- lapply(c(hist.ests), function(x) {data.frame(x[,'Rect',drop=FALSE])})
    if(!include.saw42) {hist.ests.var <- hist.ests.var[!names(hist.ests.var)=='SAW42']}
  all.prev.ests.var <- c(prev.ests.var, hist.ests.var)

  ymax <- max(c(current.ests.var$X95th, unlist(all.prev.ests.var)), na.rm=TRUE)

  plot(current.yrs, current.ests.var$Rect, ylim=c(0,ymax), axes=FALSE, xlab='', ylab='', type='l', col='black', cex=0.6, lwd=2)
  for (i in 1:length(all.prev.ests.var))
  {
    assess.data <- all.prev.ests.var[[i]]
    lines(as.integer(rownames(assess.data)), assess.data[,'Rect'], col=color.list[i])
  }
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.1, padj = 0.25)
  axis(side=1, at=axTicks(1), labels=write.xlab, cex.axis=1.1, padj = -0.25)
  box()
  mtext('Recruitment (millions)', side=2, line=2.7, cex=0.8) 
}



### Create Historical Retrospective plot
include.saw42 <- FALSE
assess.name.labels <- c(current.assess, prev.assess, hist.assess)
assess.name.labels <- if(!include.saw42) {assess.name.labels[(!names(assess.name.labels)%in%'SAW42')]}

windows(height=8.0, width=5.0)
par(mfcol=c(3,1))
par(mar=c(0.5, 2.7, 1.3, 1) +0.1);  par(oma=c(4.5,2.2,0.5,0)) # Horizontal y-axis
plot.SSB(include.saw42, write.xlab <- FALSE)
legend('topright', assess.name.labels, bty="n", col=c('black',color.list[1:(length(assess.name.labels)-1)]),cex=1.0,lwd=2) 
plot.F(include.saw42, write.xlab <- FALSE)
plot.Rect(include.saw42, write.xlab <- TRUE)
mtext('Year', side=1, line=3, cex=0.8) 
# Cannot save as wmf due to use of polygon
if(save.fig=='y') {savePlot(file.path(output.dir,'Historical.retrospective.png'),type='png')}



### Save final workspace
save.image(file.path(output.dir,'Historical.retrospective.comparison.RDATA'))



#########################



rm(list=ls())
ls()


