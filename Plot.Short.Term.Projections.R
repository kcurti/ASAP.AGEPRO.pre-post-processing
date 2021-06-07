#######
#
### This program plots stock history with short-term projections
#
#######



rm(list=ls())
ls()


# Run details
run.no <- '4'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
estimate.type <- 'point.est'   # 'median' or 'point.est'
                               #  i.e the median MCMC estimates or the ASAP point estimates 


# Projection details
brp.run <- 'Rect.1975.Onward'

proj.fyr <- 2020
proj.lyr <- 2023

rect.scenarios <- c('Rect.1975', 'Rect.1999', 'Rect.2009')
proj.master.folder <- 'projections.2year'
proj.run.list   <- paste(rect.scenarios, 'Onward', sep='.')
  # Names are used for the figures legend
  names(proj.run.list) <- apply(do.call(rbind, strsplit(proj.run.list, "\\.")), 1, function(x) {paste(x[2:3], collapse=' ')})
proj.fname.list <- paste('PROJECTIONS.2YEAR.100.SIMS.FMSY.PROXY', toupper(rect.scenarios), sep='.')


# Figure details
save.fig <- 'y'
color.list <- c("magenta3", "limegreen", "steelblue1", "gold2", "midnightblue", "purple", 'seagreen')



################################################################



run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')

proj.yrs <- as.character(proj.fyr:proj.lyr)
n.proj <- length(proj.run.list)


### BRPs
brp.env <-new.env()
load( file.path(run.dir, 'projections.brps', brp.run, 'Projection.summary.RDATA'), envir=brp.env )
ssb.brp <- brp.env$ssb.brp 
f.brp <- brp.env$fmult.brp
biomass.brp <- brp.env$biomass.brp
catch.brp <- brp.env$msy.brp


### Base run and MCMC results
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )

# Model estimates
if(estimate.type == 'point.est')
{
  model.ests <- cbind.data.frame(        annual.ests[,c('SSB','January 1 B')], t(asap$catch.obs) )
}
if(estimate.type == 'median')
{
  model.ests <- cbind.data.frame( median.annual.ests[,c('SSB','January 1 B')], t(asap$catch.obs) )
}
colnames(model.ests) <- c('ssb','biomass','catch')



### Create template list for projections
proj.template <- vector('list',n.proj)
  names(proj.template) <- proj.run.list
  
# Base projection directory
base.proj.dir <- file.path(run.dir, proj.master.folder)


### Fill template for ssb, January 1 biomass and catch
ssb.proj     <- proj.template
biomass.proj <- proj.template
catch.proj   <- proj.template

for (proj.run in proj.run.list)
{
  # proj.run <- proj.run.list[1]
  print(proj.run)
  proj.env <- new.env()
  proj.dir <- file.path(base.proj.dir, proj.run)
  load( file.path(proj.dir, 'Projection.summary.RDATA'), envir=proj.env )
  
  ssb.proj[[proj.run]] <- proj.env$ssb.table[,proj.yrs]
  biomass.proj[[proj.run]] <- proj.env$biomass.table[,proj.yrs]
  catch.proj[[proj.run]] <- proj.env$catch.table[,proj.yrs]

  rm(proj.env, proj.dir)
}


### For a given variable, merge base run estimates (either point estimates from ASAP output or MCMC medians) with MCMC CIs and short-term projections 
plot.short.term.projections <- function(var.name, yaxis.label, plot.fyr, legend.location)
{
  # var.name <- 'ssb'; yaxis.label <- 'SSB (mt)'; plot.fyr <- 2000

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
    
  # Get projection estimates
  proj.series <- get(paste(var.name,'proj',sep='.'))   
  
  # Determine figure xlim, ylim
  asap.max <- max(merged.asap[,!colnames(merged.asap)=='Year'])  
  proj.max <- max(do.call(rbind,proj.series))  
  y.lim <- c(0, max(c(asap.max, proj.max)) )
  x.lim <- c(plot.fyr,proj.lyr)
    
  # Plot
  windows(width=6,height=5)
  par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
  
  # ASAP point estimates and MCMC
  plot ( merged.asap$Year, merged.asap[,'Estimate'], lwd=2, col='black', xlim=x.lim, ylim=y.lim, axes=FALSE, type="l" )
  lines( merged.asap$Year, merged.asap[,'X5th'], lty=2, col='black', lwd=2 ) 
  lines( merged.asap$Year, merged.asap[,'X95th'], lty=2, col='black', lwd=2 ) 

  # Projections
  for (i in 1:n.proj)
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
  legend(legend.location, names(proj.run.list), bty="n", col=color.list[1:n.proj],cex=0.8,lwd=2) 
}


### SSB
plot.fyr <- fyr
plot.short.term.projections('ssb',  'Spawning stock biomass (mt)', plot.fyr, 'topright')
  # SSBmsy
  abline(h = (ssb.brp), lty=2)
  text(x=fyr-3, y=(ssb.brp-50000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('ssb.projections.fyr',plot.fyr,'wmf',sep='.'))) }

plot.fyr <- 2005
plot.short.term.projections('ssb',  'Spawning stock biomass (mt)', plot.fyr, 'topleft')
  # SSBmsy
  abline(h = (ssb.brp), lty=2)
  text(x=2005, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('ssb.projections.fyr',plot.fyr,'wmf',sep='.'))) }


### Biomass
plot.fyr <- fyr
plot.short.term.projections('biomass',  'January 1 biomass (mt)', plot.fyr, 'topright')
  # Bmsy
  abline(h = (biomass.brp), lty=2)
  text(x=(fyr-2), y=(biomass.brp-90000), labels=bquote('B'['MSY PROXY'] ~ '=' ~ .(format(round((biomass.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('biomass.projections.fyr',plot.fyr,'wmf',sep='.'))) }

plot.fyr <- 2005
plot.short.term.projections('biomass',  'January 1 biomass (mt)', plot.fyr, 'topleft')
  # Bmsy
  abline(h = (biomass.brp), lty=2)
  text(x=(2007), y=(biomass.brp+15000), labels=bquote('B'['MSY PROXY'] ~ '=' ~ .(format(round((biomass.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('biomass.projections.fyr',plot.fyr,'wmf',sep='.'))) }

  
### Catch
plot.fyr <- fyr
plot.short.term.projections('catch',  'Catch (mt)', plot.fyr, 'topright')
  # MSY
  abline(h = (catch.brp), lty=2)
  text(x=(fyr-2), y=(catch.brp-30000), labels=bquote('MSY'['PROXY'] ~ '=' ~ .(format(round((catch.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('catch.projections.fyr',plot.fyr,'wmf',sep='.'))) }

plot.fyr <- 2005
plot.short.term.projections('catch',  'Catch (mt)', plot.fyr, 'topright')
  # MSY
  abline(h = (catch.brp), lty=2)
  text(x=(2004.5), y=(catch.brp-5000), labels=bquote('MSY'['PROXY'] ~ '=' ~ .(format(round((catch.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('catch.projections.fyr',plot.fyr,'wmf',sep='.'))) }

  
### Save workspace
setwd(base.proj.dir)
save.image('Summary.All.Projections.RDATA')



##############################################



### Load workspace

rm(list=ls())
ls()



