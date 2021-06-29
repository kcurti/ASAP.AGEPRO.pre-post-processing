#######
#
# New code to compare current assessment and projections with those from the previous assessment
#
#######

### Load workspace

rm(list=ls())
ls()

run.no <- '4'
proj.nyr.name <- '2year' 
rdata.name <- 'Rect.scenarios.with.Fmsy.proxy'

modeling.dir <- '//net.nefsc.noaa.gov/home0/kcurti/Mackerel/Modeling'
current.assess.dir <- file.path(modeling.dir, '2021.Management.Track')

prev.assess.dir <- file.path(modeling.dir, '2017.Benchmark/First.Year.1968/Run118')


# Load projection summary
proj.master.folder <- paste('projections',proj.nyr.name, sep='.')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
base.proj.dir <- file.path(run.dir, proj.master.folder)

setwd(base.proj.dir)
load(paste(rdata.name, 'Proj.Summary.RDATA', sep='.'))


# Load historical retrospective 
hist.env <- new.env()
output.dir <- file.path(run.dir,'outputs')
load(file.path(output.dir,'Historical.retrospective.comparison.RDATA'), envir=hist.env)
ls(hist.env)

# Load previous assessment short-term projections and mcmc estimates
prev.assess.env <- new.env()
load(file.path(prev.assess.dir,'projections.rebuilding/Simulations.100/F237','Projection.summary.RDATA'), envir=prev.assess.env)
ls(prev.assess.env)
prev.mcmc <- read.csv(file.path(prev.assess.dir, 'mcmc.2000.it.1000.thin/plots','ssb.90pi_Run118.MCMC.csv') )
  rownames(prev.mcmc) <- prev.mcmc$years

# Previous projected and estimated SSB
prev.proj.lyr <- min(proj.lyr,max(as.numeric(colnames(prev.assess.env$ssb.table)),na.rm=TRUE))
prev.proj <- prev.assess.env$ssb.table[,as.character(2017:prev.proj.lyr),drop=FALSE]
  rownames(prev.proj)[1] <- 'Estimate'
prev.ests <- hist.env$prev.ests$Bench.2017[as.character(1968:2016),'SSB',drop=FALSE]
  colnames(prev.ests) <- 'Estimate'
#prev.ests.proj <- rbind(prev.ests, t(prev.proj))


### For a given variable, merge base run estimates (either point estimates from ASAP output or MCMC medians) with MCMC CIs and short-term projections 
#plot.historical.retro.with.projections <- function(var.name, yaxis.label, plot.fyr, legend.location)
#{
var.name <- 'ssb'; yaxis.label <- 'SSB (mt)'; plot.fyr <- 2000
  
  # Get ASAP and MCMC estimates
  var.asap <- model.ests[,var.name,drop=FALSE]
  colnames(var.asap) <- 'Estimate'
  if(var.name %in% c('ssb','biomass'))  { 
    mcmc <- get(paste(var.name,'ests',sep='.'))[,c('X5th','X95th')] 
  }  else {
    mcmc <- cbind(var.asap,var.asap)
    colnames(mcmc) <- c('X5th','X95th')
  }
  
  merged.asap <- merge(var.asap[as.character(plot.fyr:lyr),,drop=FALSE], mcmc[as.character(plot.fyr:lyr),], by="row.names",all=TRUE)
  rownames(merged.asap) <- merged.asap$'Row.names'
  colnames(merged.asap)[colnames(merged.asap)=='Row.names'] <- 'Year'
  
  prev.ests.plot.data <- cbind(prev.ests[as.character(plot.fyr:tail(rownames(prev.ests),1)),,drop=FALSE], 
                               prev.mcmc[as.character(plot.fyr:tail(rownames(prev.ests),1)),c('X5th','X95th')] )
  prev.lyr <- tail(rownames(prev.ests),1)
  prev.proj.plot.data <- cbind(as.matrix(rep(prev.ests[prev.lyr,],3)),prev.proj)
  # prev.proj.plot.data <- cbind(t(prev.ests[prev.lyr,]),prev.proj['Estimate',,drop=FALSE])
    colnames(prev.proj.plot.data)[1] <- prev.lyr
  
  # Get projection estimates
  proj.series <- get(paste(var.name,'proj',sep='.'))   
  
  # Determine figure xlim, ylim
  asap.max <- max(merged.asap[,!colnames(merged.asap)=='Year'])  
  proj.max <- max(do.call(rbind,proj.series)) 
  prev.max <- max(c(max(prev.ests.plot.data), max(prev.proj.plot.data)))
  y.lim <- c(0, max(c(asap.max, proj.max, prev.max)) )
  x.lim <- c(plot.fyr,max(proj.lyr, prev.proj.lyr))
  
  # Plot
  windows(width=6,height=5)
  par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
  
  # Plot previous point estimates
  plot( as.integer(rownames(prev.ests.plot.data)), prev.ests.plot.data[,'Estimate'], lty=1, col='orange', lwd=2,xlim=x.lim, ylim=y.lim, axes=FALSE, type="l"  ) 
  # Previous MCMC
  lines(rownames(prev.ests.plot.data), prev.ests.plot.data[,'X5th'], col='orange', lty=2, lwd=2)
  lines(rownames(prev.ests.plot.data), prev.ests.plot.data[,'X95th'], col='orange', lty=2, lwd=2)
  
  # Previous projections
  lines(as.integer(colnames(prev.proj.plot.data)), prev.proj.plot.data['Estimate',], col='darkgreen', lty=1, lwd=2)
  lines(as.integer(colnames(prev.proj.plot.data)), prev.proj.plot.data['5th Percentile',], col='darkgreen', lty=2, lwd=1)
  lines(as.integer(colnames(prev.proj.plot.data)), prev.proj.plot.data['95th Percentile',], col='darkgreen', lty=2, lwd=1)
  
  
  # ASAP point estimates and MCMC
  #plot ( merged.asap$Year, merged.asap[,'Estimate'], lwd=2, col='black', xlim=x.lim, ylim=y.lim, axes=FALSE, type="l" )
  lines ( merged.asap$Year, merged.asap[,'Estimate'], lwd=2, col='black')
  lines( merged.asap$Year, merged.asap[,'X5th'], lty=2, col='black', lwd=2 ) 
  lines( merged.asap$Year, merged.asap[,'X95th'], lty=2, col='black', lwd=2 ) 
  
  # Plot previous point estimates
  #lines( as.integer(rownames(prev.ests.plot.data)), prev.ests.plot.data[,'Estimate'], lty=1, col='orange', lwd=2 ) 
  
  # Current projections to plot
  plot.proj.list <- proj.run.list['1975 onward']
  
  # Projections
  for (i in 1:length(plot.proj.list))
  {
    # i <- 1
    proj.run <- proj.run.list[i]
    
    x.vec <- as.numeric(c(lyr,proj.yrs))
    y.vec <- cbind(merged.asap[as.character(lyr),'Estimate'], as.vector(proj.series[[proj.run]]['Median',]))
    lines(x.vec, y.vec, col=color.list[i], lty=1, lwd=2)
    
    l.ci <- cbind(merged.asap[as.character(lyr),'X5th'], proj.series[[proj.run]]['5th Percentile',])
    lines(x.vec, l.ci, col=color.list[i], lty=2, lwd=1)
    
    u.ci <- cbind(merged.asap[as.character(lyr),'X95th'], proj.series[[proj.run]]['95th Percentile',])
    lines(x.vec, u.ci, col=color.list[i], lty=2, lwd=1)
  }
  
  
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
  axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
  box()
  mtext(side=1, 'Year', line=0, outer=TRUE, cex=0.9)
  mtext(side=2, yaxis.label, line=0, outer=TRUE, cex=0.9)
  
  abline(h = (ssb.brp), lty=2)
  text(x=2007, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
  if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('SSB.projections',f.name,'fyr',plot.fyr,'with.historical.wmf',sep='.'))) }
  




