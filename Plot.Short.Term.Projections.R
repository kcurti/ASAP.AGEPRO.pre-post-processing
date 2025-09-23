#######
#
### This program plots stock history with short-term projections
#
#######



rm(list=ls())
ls()


# Run details
run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')

estimate.type <- 'point.est'   # 'median' or 'point.est'
#  i.e the median MCMC estimates or the ASAP point estimates 


# Projection details

proj.folder.name <- 'short.term' 
# rect.scenarios <- c('Rect.1975', 'Rect.1999', 'Rect.2009')
# f.name <- 'F.zero' # 'Fmsy.proxy' 'F.zero'

nsim <- 100
proj.fyr <- 2025
proj.lyr <- 2032


# Final workspace name
rdata.name <- 'Rebuilding.projections'


# Figure details
save.fig <- 'y'
fig.type <- 'png'
color.list <- c("magenta3", "limegreen", "steelblue1", "gold2", "blue", "purple", 'seagreen')



################################################################



proj.master.folder <- paste('projections',proj.folder.name, sep='.')

proj.name.list <- c(
  'Fmsy proxy'
)

proj.path.list <- c(
  'F20'
)
names(proj.path.list) <- proj.name.list


proj.file.list <- c(
  'F20.Rect.2Stanza'
)
names(proj.file.list) <- proj.name.list


n.proj <- length(proj.name.list)


# proj.run.list   <- paste(rect.scenarios, 'Onward', sep='.')
#   # Names are used for the figures legend
#   names(proj.run.list) <- paste(do.call(rbind, strsplit(rect.scenarios, "\\."))[,2], 'Onward')
# n.proj <- length(proj.run.list)

# proj.fname.list <- paste('PROJECTIONS', toupper(proj.folder.name), nsim, 'SIMS', toupper(rect.scenarios), toupper(f.name), sep='.')

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')

proj.yrs <- as.character(proj.fyr:proj.lyr)


### BRPs
brp.env <- new.env()
load( file.path(output.dir, 'Comparison.with.BRPs.RDATA'), envir=brp.env )
ssb.brp <- brp.env$ssb.brp 
f.brp <- brp.env$fmult.brp
biomass.brp <- brp.env$biomass.brp
catch.brp <- brp.env$msy.brp
rect.at.brp <- brp.env$rect.at.brp


### Base run and MCMC results
asap.env <- new.env()
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')), envir=asap.env )
fyr <- asap.env$fyr
lyr <- asap.env$lyr

# Model estimates
if(estimate.type == 'point.est')
{
  model.ests <- cbind.data.frame( asap.env$annual.ests[,c('SSB','Rect','F')], 
                                  t(asap.env$asap$catch.obs) )
}
if(estimate.type == 'median')
{
  model.ests <- cbind.data.frame( asap.env$median.annual.ests[,c('SSB','Rect','F')],
                                  t(asap.env$asap$catch.obs) )
}
colnames(model.ests) <- c('ssb','rect', 'f', 'catch')
# CIs
mcmc.ests <- asap.env$mcmc.ests
colnames(mcmc.ests) <- c('year','ssb','ssb.lo','ssb.hi',
                         'rect','rect.lo','rect.hi',
                         'f','f.lo','f.hi')
rownames(mcmc.ests) <- mcmc.ests$year


### Create template list for projections
proj.template <- vector('list',n.proj)
names(proj.template) <- proj.name.list

# Base projection directory
base.proj.dir <- file.path(run.dir, proj.master.folder)


### Fill template for ssb, January 1 biomass, catch, F and recruitment
ssb.proj     <- proj.template
biomass.proj <- proj.template
catch.proj   <- proj.template
f.proj       <- proj.template
rect.proj    <- proj.template

prob.rebuilt <- proj.template

for (proj.run in proj.name.list)
{
  # proj.run <- proj.name.list[1]
  print(proj.run)
  proj.env <- new.env()
  proj.dir <- file.path(base.proj.dir, proj.path.list[proj.run])
  load( file.path(proj.dir, paste(proj.file.list[proj.run],'Projection.summary.RDATA',sep=".")), envir=proj.env )
  
  ssb.proj[[proj.run]]     <- proj.env$ssb.table[,proj.yrs]
  catch.proj[[proj.run]]   <- proj.env$catch.table[,proj.yrs]
  f.proj[[proj.run]]       <- proj.env$f.table[,proj.yrs]
  rect.proj[[proj.run]]    <- proj.env$rect.table[,proj.yrs]
  
  rebuilt.env <- new.env()
  load( file.path(proj.dir, paste(proj.file.list[proj.run],'Probability.Rebuilt.RDATA',sep=".")), envir=rebuilt.env )
  prob.rebuilt[[proj.run]] <- rebuilt.env$ssb.prob.rebuilt
  
  rm(rebuilt.env, proj.env, proj.dir)
}


### For a given variable, merge base run estimates (either point estimates from ASAP output or MCMC medians) with MCMC CIs and short-term projections 
plot.short.term.projections <- function(var.name, yaxis.label, plot.fyr, legend.location)
{
  # var.name <- 'ssb';   yaxis.label <- 'SSB (mt)';            plot.fyr <- 2000;  legend.location <- 'topright'
  # var.name <- 'rect';  yaxis.label <- 'Recruitment (000s)';  plot.fyr <- 2000;  legend.location <- 'topright'
  
  # Get final estimates
  var.asap <- model.ests[,var.name,drop=FALSE]
    colnames(var.asap) <- 'Estimate'
  # Get CIs    
  if(var.name != 'catch')  
  {
    mcmc.var.list <- paste(var.name,c('lo','hi'),sep='.')
    mcmc <- mcmc.ests[,mcmc.var.list]
    colnames(mcmc) <- c('X5th','X95th')
  }  else {
    mcmc <- cbind(var.asap,var.asap)
    colnames(mcmc) <- c('X5th','X95th')
  }
  
  # Merge  
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
  windows(width=5.5,height=4.5)
  par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
  
  # ASAP point estimates and MCMC
  plot ( merged.asap$Year, merged.asap[,'Estimate'], lwd=2, col='black', xlim=x.lim, ylim=y.lim, axes=FALSE, type="l" )
  lines( merged.asap$Year, merged.asap[,'X5th'], lty=2, col='black', lwd=2 ) 
  lines( merged.asap$Year, merged.asap[,'X95th'], lty=2, col='black', lwd=2 ) 
  
  # Projections
  for (i in 1:n.proj)
  {
    # i <- 1
    proj.run <- proj.name.list[i]
    
    x.vec <- as.numeric(c(lyr,proj.yrs))
    y.vec <- cbind(merged.asap[as.character(lyr),'Estimate'], proj.series[[proj.run]]['Median',])
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
  legend(legend.location, proj.name.list, bty="n", col=color.list[1:n.proj],cex=0.8,lwd=2) 
}


### SSB
plot.fyr <- 2010
plot.short.term.projections('ssb',  'Spawning stock biomass (mt)', plot.fyr, 'topleft')
# SSBmsy
  abline(h = (ssb.brp), lty=2)
  text(x=plot.fyr, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('ssb.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }

plot.fyr <- 1968
plot.short.term.projections('ssb',  'Spawning stock biomass (mt)', plot.fyr, 'topleft')
# SSBmsy
  abline(h = (ssb.brp), lty=2)
  text(x=plot.fyr, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('ssb.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }

### Catch
#plot.fyr <- 2017
plot.short.term.projections('catch',  'Catch (mt)', plot.fyr, 'topright')
# MSY
  abline(h = (catch.brp), lty=2)
  text(x=(plot.fyr+0.5), y=(catch.brp-2000), labels=bquote('MSY'['PROXY'] ~ '=' ~ .(format(round((catch.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('catch.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }


### F
#plot.fyr <- 2017
plot.short.term.projections('f',  'F', plot.fyr, 'topright')
# FMSY
  abline(h = (f.brp), lty=2)
  text(x=(plot.fyr), y=(f.brp-0.1), labels=bquote('FMSY'['PROXY'] ~ '=' ~ .(format(round((f.brp),2),big.mark=','))), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('f.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }

# plot.fyr <- 2017
# plot.short.term.projections('f',  'F', plot.fyr, 'topright')
# # FMSY
#   abline(h = (f.brp), lty=2)
#   text(x=(plot.fyr+10), y=(f.brp+0.05), labels=bquote('FMSY'['PROXY'] ~ '=' ~ .(format(round((f.brp),2),big.mark=','))), cex= 0.8, pos=4)
# if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('f.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }


### Rect
plot.fyr <- 1975
plot.short.term.projections('rect',  'Recruitment (000s)', plot.fyr, 'topleft')
plot.ref <- rect.at.brp/1000
  abline(h = (plot.ref), lty=2)
  text(x=(plot.fyr+6.3), y=(plot.ref+20000), labels=bquote('Long-term (100-yr) median recruitment' ~ '=' ~ .(format(round(plot.ref,0),big.mark=','))), cex= 0.8, pos=4)
if(save.fig=='y') { savePlot(file.path(base.proj.dir, paste('rect.projections.fyr',plot.fyr,fig.type,sep='.')), type=fig.type) }



### Create terminal year summary tables    
rect.proj.lyr <- do.call(rbind, lapply(rect.proj, function(x) {x['Median',as.character(proj.lyr),drop=FALSE]}))
  colnames(rect.proj.lyr) <- 'Rect.2032'

ssb.proj.lyr <- do.call(rbind, lapply(ssb.proj, function(x) {x['Median',as.character(proj.lyr),drop=FALSE]}))
prob.rebuilt.mat <- do.call(rbind, prob.rebuilt)
ssb.combined <- cbind.data.frame(ssb.proj.lyr, prob.rebuilt.mat)
  colnames(ssb.combined) <- c('SSB.2032', 'Prob.rebuilt')

catch.proj.lyr <- do.call(rbind, lapply(catch.proj, function(x) {x['Median',as.character(proj.lyr),drop=FALSE]}))
  colnames(catch.proj.lyr) <- 'Catch.2032'
  
  
  
### Save workspace
save.image(file.path(base.proj.dir, paste(rdata.name, 'Proj.Summary.RDATA', sep='.')))

write.csv(rect.proj.lyr, file.path(base.proj.dir, 'Median.Rect.proj.lyr.csv'))
write.csv(ssb.combined, file.path(base.proj.dir, 'Median.SSB.proj.lyr.csv'))
write.csv(catch.proj.lyr, file.path(base.proj.dir, 'Median.Catch.proj.lyr.csv'))



##############################################


# 
# ### Load workspace
# 
# rm(list=ls())
# ls()
# 
# run.no <- '4'
# proj.folder.name <- 'rebuilding/Updated.Projections.March2022' 
# rdata.name <- 'Rebuilding.projections.March2022'
# 
# current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')
# run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
# 
# proj.master.folder <- paste('projections',proj.folder.name, sep='.')
# base.proj.dir <- file.path(run.dir, proj.master.folder)
# 
# setwd(base.proj.dir)
# load(paste(rdata.name, 'Proj.Summary.RDATA', sep='.'))
# 
# 
# ########## Plot two alternatives under final consideration ###########
# 
# 
# proj.name.list <- c(
#   'Rect 2 Stanza, 60% prob rebuild',
#   'Rect 2 stanza, P* at Fmsy'
# )
# 
# n.proj <- length(proj.name.list)


##########################

#### Second figure retry in ggplot: Projections only to new model terminal year

library(tidyverse)

# var.name <- 'ssb';     yaxis.label <- 'SSB (mt)';            plot.fyr <- 1980;  legend.location <- 'topright'
# var.name <- 'catch';   yaxis.label <- 'Catch (mt)';          plot.fyr <- 1980;  legend.location <- 'topright'
var.name <- 'rect';  yaxis.label <- 'Recruitment (000s)';  plot.fyr <- 1980;  legend.location <- 'topright'


# Get final estimates
var.asap <- model.ests[,var.name,drop=FALSE]
colnames(var.asap) <- 'Estimate'
# Get CIs    
if(var.name != 'catch')  
{
  mcmc.var.list <- paste(var.name,c('lo','hi'),sep='.')
  mcmc <- mcmc.ests[,mcmc.var.list]
  colnames(mcmc) <- c('X5th','X95th')
}  else {
  mcmc <- cbind(var.asap,var.asap)
  colnames(mcmc) <- c('X5th','X95th')
}

# Merge  
merged.asap <- merge(var.asap[as.character(plot.fyr:lyr),,drop=FALSE], mcmc[as.character(plot.fyr:lyr),], by="row.names",all=TRUE)
rownames(merged.asap) <- merged.asap$'Row.names'
colnames(merged.asap)[colnames(merged.asap)=='Row.names'] <- 'Year'

# Get projection estimates
proj.series <- get(paste(var.name,'proj',sep='.'))   

# # Determine figure xlim, ylim
# asap.max <- max(merged.asap[,!colnames(merged.asap)=='Year'])  
# proj.max <- max(do.call(rbind,proj.series))  
# y.lim <- c(0, max(c(asap.max, proj.max)) )
# x.lim <- c(plot.fyr,proj.lyr)


tidy.merged.asap <- tibble(merged.asap) %>%
  rename(Percentile5 = X5th,
         Percentile95 = X95th,
         Median = Estimate) %>%
  mutate(Year = as.integer(Year),
         Estimate='ASAP')

proj.series.tmp <- lapply(proj.series, 
                          function (x) {
                            as_tibble(t(x)) %>%
                              rename(Percentile5 = '5th Percentile',
                                     Percentile40 = '40th Percentile',
                                     #Percentile75 = '75th Percentile',
                                     Percentile95 = '95th Percentile') %>%
                              mutate(Year = as.integer(colnames(x))) %>%
                              bind_rows(., tidy.merged.asap%>%filter(Year==lyr))
                          })
tidy.proj.series <- bind_rows(proj.series.tmp, .id="Estimate")


plot.data <- bind_rows(tidy.merged.asap, tidy.proj.series)

windows(width=8, height=6)

plot.data %>% 
  filter(Year >=plot.fyr) %>%
  ggplot(aes(Year, Median)) +
  theme(text = element_text(size=13)) +
  geom_line(aes(color=Estimate)) +
  geom_ribbon(aes(ymin = Percentile5, ymax = Percentile95, fill=Estimate), alpha = 0.2, linetype='blank') +
  theme(legend.position = "none") +
  
  ### SSB
  # ylab("SSB (mt)") + 
  # geom_hline(yintercept=ssb.brp, lty=2) +
  # annotate(
  #   "text",
  #   x=2015,
  #   y=ssb.brp+10000,
  #   label=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt')
  # )  
  
  # ### Catch
  # ylab("Catch (mt)") + 
  # geom_hline(yintercept=catch.brp, lty=2) +
  # annotate(
  # "text",
  # x=2015,
  # y=catch.brp+-2000,
  # label=bquote('MSY'['PROXY'] ~ '=' ~ .(format(round((catch.brp),0),big.mark=',')) ~ 'mt')
  # )

### Rect
ylab("Recruitment (000s)") + 
  geom_hline(yintercept=rect.at.brp/1000, lty=2) +
  annotate(
    "text",
    x=plot.fyr+12,
    y=(0),
    label=bquote('Median recruitment' ~ '=' ~ .(format(round((rect.at.brp/1000),0),big.mark=',')))
  )


# if(save.fig=='y') { ggsave(file.path(base.proj.dir, paste('GGplot.projected.SSB.ests','fyr',plot.fyr,'png',sep='.'))) }
# if(save.fig=='y') { ggsave(file.path(base.proj.dir, paste('GGplot.projected.Catch.ests','fyr',plot.fyr,'png',sep='.'))) }
if(save.fig=='y') { ggsave(file.path(base.proj.dir, paste('GGplot.projected.Rect.ests','fyr',plot.fyr,'png',sep='.'))) }


pivot_longer(tibble(ssb.proj$`Fmsy proxy`["Median",]), cols=all_of(colnames(ssb.proj$`Fmsy proxy`)), names_to="Year", values_to="SSB")
