#######
#
# New code to compare current assessment and projections with those from the previous assessment
#
#######

### Load workspace

rm(list=ls())
ls()

save.fig <- 'n'

### Current run information

run.no <- '8'
proj.name <- 'short.term' 
F.name <- "F20"
proj.fname <- paste('F20.Rect.2Stanza', 'Projection.Summary.RDATA', sep='.')

### Set Model directory
source("~/R/Directory_Paths.R")
desktop.dir <- 'C:/Users/kiersten.curti/Desktop'
modeling.dir <- file.path(mack.net.dir, 'Kiersten_Curti/Modeling')

### Current run directories
current.assess.dir <- file.path(desktop.dir, '2025.Management.Track')
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir, paste('projections',proj.name,sep='.'), F.name)

### Previous run directories and file names
prev.assess.dir <- file.path(modeling.dir, '2023.Management.Track')
prev.run.dir <- file.path(prev.assess.dir, 'Run9')
prev.proj.dir <- file.path(prev.run.dir,'projections.rebuilding/Base/F11_UpdatedWithRealizedCatch')
prev.proj.fname <- 'F11.Rect.2Stanza.Projection.Summary.RDATA'
# prev.proj.dir <- file.path(prev.run.dir,'projections.rebuilding/Age2.35Percent.AvgABC.F07/AvgABC_3200mt_UpdatedWithRealizedCatch')
# prev.proj.fname <- 'F07.Rect.2Stanza.Projection.summary.RDATA'


### Load current model estimates (via historical retrospective workspace)
# For current model, just need hist.env$current.ests.  This object uses point or MCMC estimates, as specified in Plot.Historical.Retrospective, and then combined with MCMC CIs
hist.env <- new.env()
output.dir <- file.path(run.dir, 'outputs')
load(file.path(output.dir,'Historical.retrospective.comparison.RDATA'), envir=hist.env)
ls(hist.env)
current.ests <- hist.env$current.ests
model.yrs <- as.integer(rownames(current.ests[[1]]))
model.lyr <- tail(model.yrs, 1)

### Load current projection summary
proj.env <- new.env()
load(file.path(proj.dir, proj.fname), envir=proj.env)
ls(proj.env)
proj.fyr <- proj.env$proj.fyr
proj.lyr <- proj.env$proj.lyr
proj.yrs <- proj.fyr:proj.lyr
proj.catch <- proj.env$catch.table
proj.ssb   <- proj.env$ssb.table
proj.f     <- proj.env$f.table
proj.rect  <- proj.env$rect.table

### Load current reference points
brp.env <- new.env()
load(file.path(output.dir,'Comparison.with.BRPs.RDATA'), envir=brp.env)
ssb.brp <- brp.env$ssb.brp


### Load previous model estimates
prev.assess.env <- new.env()
load(file.path(prev.run.dir, 'outputs', 'Historical.retrospective.comparison.RDATA'), envir = prev.assess.env)
ls(prev.assess.env)
prev.ests <- prev.assess.env$current.ests
prev.model.yrs <- as.integer(rownames(prev.ests[[1]]))
prev.model.lyr <- tail(prev.model.yrs, 1)


### Load previous short-term projection summary
prev.proj.env <- new.env()
load(file.path(prev.proj.dir,prev.proj.fname), envir=prev.proj.env)
ls(prev.proj.env)
prev.proj.fyr <- prev.proj.env$proj.fyr
prev.proj.lyr <- prev.proj.env$proj.lyr
prev.proj.yrs <- prev.proj.fyr:prev.proj.lyr
prev.proj.catch <- prev.proj.env$catch.table
prev.proj.ssb   <- prev.proj.env$ssb.table
prev.proj.f     <- prev.proj.env$f.table
prev.proj.rect  <- prev.proj.env$rect.table



### For a given variable, merge base run estimates and short-term projections 
#plot.historical.retro.with.projections <- function(var.name, yaxis.label, plot.fyr, legend.location)
#{
var.name <- 'SSB'; yaxis.label <- 'SSB (mt)'; plot.fyr <- 2000

# Get current model estimates and brp
current.model <- current.ests[[var.name]]
  colnames(current.model) <- c('Estimate','95low','95hi')
current.brp <- get(paste(tolower(var.name),'brp',sep='.'))

# Get previous model estimates  
prev.model <- prev.ests[[var.name]]
  colnames(prev.model) <- c('Estimate','95low','95hi')

# Get current projection estimates
current.proj <- get(paste('proj',tolower(var.name),sep='.'))[c('Median', '5th Percentile', '95th Percentile'), as.character(proj.yrs)]
# Add last model year
current.proj <- cbind.data.frame(t(current.model[as.character(model.lyr),,drop=FALSE]),current.proj)

# Get previous projection estimates
prev.proj <- get(paste('prev.proj',tolower(var.name),sep='.'))[c('Median', '5th Percentile', '95th Percentile'), as.character(prev.proj.yrs)]
# Add last model year
prev.proj <- cbind.data.frame(t(prev.model[as.character(prev.model.lyr),,drop=FALSE]),prev.proj)



### Necessary objects for figure
current.model
prev.model
current.proj
prev.proj


#### First figure: Full projections

# Determine figure xlim, ylim
ymax <- max(c(max(current.model[as.character(plot.fyr: model.lyr),]),
              max(prev.model[as.character(plot.fyr: prev.model.lyr),]),
              max(current.proj),
              max(prev.proj)
            ))
xmax <- max(c(proj.yrs, prev.proj.yrs))

y.lim <- c(0, ymax)
x.lim <- c(plot.fyr, xmax)

# Plot
windows(width=6,height=5)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))


# Plot previous model estimates
plot( prev.model.yrs, prev.model[,'Estimate'], xlim=x.lim, ylim=y.lim, axes=FALSE, type="l", lty=1, col='orange', lwd=2 ) 
lines(prev.model.yrs, prev.model[,'95low'], lty=2, col='orange', lwd=1)
lines(prev.model.yrs, prev.model[,'95hi'],  lty=2, col='orange', lwd=1)

# Previous projections
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['Estimate',], lty=1, col='darkgreen', lwd=2)  
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['95low',],    lty=2, col='darkgreen', lwd=1)  
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['95hi',],     lty=2, col='darkgreen', lwd=1)  
  
# Plot current model estimates  
lines(model.yrs, current.model[,'Estimate'], lty=1, col='black', lwd=2 ) 
lines(model.yrs, current.model[,'95low'],    lty=2, col='black', lwd=2)
lines(model.yrs, current.model[,'95hi'],     lty=2, col='black', lwd=2)

# Current projections
lines(c(model.lyr, proj.yrs), current.proj['Estimate',], lty=1, col='magenta', lwd=2)  
lines(c(model.lyr, proj.yrs), current.proj['95low',],    lty=2, col='magenta', lwd=1)  
lines(c(model.lyr, proj.yrs), current.proj['95hi',],     lty=2, col='magenta', lwd=1)  

# Axes, etc
axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
box()
mtext(side=1, 'Year', line=0, outer=TRUE, cex=0.9)
mtext(side=2, yaxis.label, line=0, outer=TRUE, cex=0.9)
  
abline(h = (ssb.brp), lty=2)
text(x=2007, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)

if(save.fig=='y') { savePlot(file.path(proj.dir, paste('SSB.projections',F.name,'fyr',plot.fyr,'with.historical.wmf',sep='.'))) }
  


#### Second figure: Projections only to new model terminal year

# Determine figure xlim, ylim
ymax <- max(c(max(current.model[as.character(plot.fyr: model.lyr),]),
              max(prev.model[as.character(plot.fyr: prev.model.lyr),]),
              # max(current.proj),
              max(prev.proj[as.character(prev.model.lyr:model.lyr)])
))

y.lim <- c(0, ymax)
x.lim <- c(plot.fyr, model.lyr)

# Plot
windows(width=6,height=5)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))


# Plot previous model estimates
plot( prev.model.yrs, prev.model[,'Estimate'], xlim=x.lim, ylim=y.lim, axes=FALSE, type="l", lty=1, col='orange', lwd=2 ) 
lines(prev.model.yrs, prev.model[,'95low'], lty=2, col='orange', lwd=1)
lines(prev.model.yrs, prev.model[,'95hi'],  lty=2, col='orange', lwd=1)

# Previous projections
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['Estimate',], lty=1, col='darkgreen', lwd=2)  
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['95low',],    lty=2, col='darkgreen', lwd=1)  
lines(c(prev.model.lyr, prev.proj.yrs), prev.proj['95hi',],     lty=2, col='darkgreen', lwd=1)  

# Plot current model estimates  
lines(model.yrs, current.model[,'Estimate'], lty=1, col='black', lwd=2 ) 
lines(model.yrs, current.model[,'95low'],    lty=2, col='black', lwd=2)
lines(model.yrs, current.model[,'95hi'],     lty=2, col='black', lwd=2)

# Current projections
# lines(c(model.lyr, proj.yrs), current.proj['Estimate',], lty=1, col='magenta', lwd=2)  
# lines(c(model.lyr, proj.yrs), current.proj['95low',],    lty=2, col='magenta', lwd=1)  
# lines(c(model.lyr, proj.yrs), current.proj['95hi',],     lty=2, col='magenta', lwd=1)  

# Axes, etc
axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
box()
mtext(side=1, 'Year', line=0, outer=TRUE, cex=0.9)
mtext(side=2, yaxis.label, line=0, outer=TRUE, cex=0.9)

abline(h = (ssb.brp), lty=2)
text(x=2007, y=(ssb.brp+10000), labels=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)

if(save.fig=='y') { savePlot(file.path(proj.dir, paste('SSB.ests','fyr',plot.fyr,'with.historical.sub.wmf',sep='.'))) }




#### Second figure retry in ggplot: Projections only to new model terminal year

library(tidyverse)

tidy.current.model <- tibble(current.model) %>%
  mutate(Year = as.integer(rownames(current.model))) %>%
  rename(low95 = '95low',
         hi95  = '95hi') %>%
  mutate(Estimates = '2025 MT')

tidy.prev.model <-tibble(prev.model) %>%
  mutate(Year = as.integer(rownames(prev.model))) %>%
  rename(low95 = '95low',
         hi95  = '95hi') %>%
  mutate(Estimates = '2023 MT')

tidy.prev.proj <- tibble(Estimate=as.numeric(prev.proj['Estimate',]),
                         low95=as.numeric(prev.proj['95low',]),
                         hi95=as.numeric(prev.proj['95hi',]),
                         Year=as.integer(colnames(prev.proj)),
                         Estimates = '2023 MT proj'
                         )
  
plot.data <- bind_rows(tidy.prev.model, tidy.prev.proj) %>%
  bind_rows(., tidy.current.model)

windows(width=9, height=6)
# Define the custom color and fill palettes
# custom_colors <- c("#7fc97f", "#beaed4", "#fdc086")
# custom_fills <- c("#7fc97f", "#beaed4", "#fdc086")

plot.data %>% 
  filter(Year %in% 2000:2024) %>%
  ggplot(aes(Year, Estimate)) +
  theme(text = element_text(size=13)) +
  geom_line(aes(color=Estimates)) +
  geom_ribbon(aes(ymin = low95, ymax = hi95, fill=Estimates), alpha = 0.2, linetype='blank') +

  #   # Manually set the color palette for the lines
  # scale_color_manual(values = custom_colors) +
  # # Manually set the fill palette for the ribbons
  # scale_fill_manual(values = custom_fills) +

  ylab("SSB (mt)") + 
  geom_hline(yintercept=ssb.brp, lty=2) +
  annotate(
    "text",
    x=2015,
    y=ssb.brp+10000,
    label=bquote('SSB'['MSY PROXY'] ~ '=' ~ 'SSB'['40%'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt')
  )
if(save.fig=='y') { ggsave(file.path(proj.dir, paste('GGplot.SSB.ests','fyr',plot.fyr,'with.historical.sub.png',sep='.'))) }
# if(save.fig=='y') { ggsave(file.path(proj.dir, paste('GGplot.SSB.ests','fyr',plot.fyr,'with.historical.sub.reduced.2022.rect.png',sep='.'))) }


save.image(file.path(proj.dir, "Historical.Retro.With.Projections.RDATA"))


