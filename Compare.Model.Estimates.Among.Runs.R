### Code to compare results of several runs, including bridge runs


rm(list=ls())
ls()

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')
lyr <- 2024

save.fig <- 'y'
fig.type <- 'png'


#########################################


comp.dir <- file.path(current.assess.dir, 'Run.Comparsions')
if(!dir.exists(comp.dir)) {dir.create(comp.dir)}

  
### Color list
# color.list <- c("magenta3", "limegreen", "midnightblue", "gold2", "steelblue1", "magenta3", "limegreen", "midnightblue", "gold2", "steelblue1")


library(RColorBrewer)
color.list <- c('magenta3','#7fc97f','steelblue1','#ffff99','#386cb0','#f0027f','#bf5b17')
# display.brewer.all()



prepare.run.ests <- function(run.list) {
  # run.list <- c(9,2:7)
  #   names(run.list) <- c('Final','Initial','Updated maturity','Updated WAA','Updated Catch','Updated egg index','Updated trawl survey indices')
  
  run.ests <- list()
  
  ### Loop over runs to import data
  for (run.no in run.list)
  {
    # run.no <- 2
    run.dir <- file.path(current.assess.dir, paste('run',run.no,sep=''))
    asap.name <- paste('Run',run.no,sep='')
    # Import ASAP summary file
    asap.summary <- read.csv(file.path(run.dir, "plots", paste('ASAP_summary_', asap.name, '.csv', sep="")))
    run.ests[[as.character(run.no)]] <- asap.summary
  }
  run.ests  
}



plot.run.comparison <- function(var.name, var.label.name, plot.fyr, line.width, legend.loc, legend.type)
{
  
  # var.name <- 'SSB'; var.label.name <- 'SSB (mt)'; plot.fyr <- 1968; legend.loc='topright'; legend.type='ordered'
  
  # Variable list to extract
  var.list <- c(var.name, paste(var.name,c('95_lo','95_hi'),sep='_'))
  
  # Data to plot
  plot.data <- lapply(run.ests, function(x) {
    x1 <- x[x$Year%in%(plot.fyr:lyr), c('Year',var.list)]
    colnames(x1) <- c('Year','Estimate','Lo','Hi')
    x1  
  })
  
  # Determine axis bounds
  plot.data.mat <- do.call(rbind, plot.data)
  xmax <- max(plot.data.mat$Year)
  ymax <- max(plot.data.mat$Hi)
  
  # Plot
  windows()
  plot(plot.data[[1]]$Year, plot.data[[1]]$Estimate, ylim=c(0,ymax), axes=FALSE, xlab='', ylab='', type='l', col='black', cex=0.6, lwd=(line.width))
  polygon(x = c(plot.data[[1]]$Year, rev(plot.data[[1]]$Year)),
          y = c(plot.data[[1]]$Lo, rev(plot.data[[1]]$Hi)),
          col =  adjustcolor("darkslategrey", alpha.f = 0.2), border = NA)
  for (i in 2:length(run.list))
  {
    color.id <- i-1
    lines(plot.data[[i]]$Year, plot.data[[i]]$Estimate, col=color.list[color.id], lwd=line.width)
  }
  # Repeat final run
  lines(plot.data[[1]]$Year, plot.data[[1]]$Estimate, col='black', lwd=(line.width))
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.1, padj = 0.25)
  axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=1.1, padj = -0.25)
  box()
  mtext('Year', side=1, line=2.7, cex=1.0) 
  mtext(var.label.name, side=2, line=2.7, cex=1.0) 

  if(legend.type=='final.first') {legend(legend.loc, names(run.list), bty="n", col=c('black',color.list[1:(length(run.list)-1)]),cex=0.9,lwd=2) }
  if(legend.type=='ordered') {legend(legend.loc, c(names(run.list[2:length(run.list)]),names(run.list)[1]) , bty="n", col=c(color.list[1:(length(run.list)-1)], 'black'),cex=0.9,lwd=2) }
}


#############################


run.list <- c(7,8)
  names(run.list) <- c('Base','Added years')
comp.name <- 'Runs7-8'
# run.list <- c("4a","4b","4c")
#   names(run.list) <- c('Updated catch','Catch and WAA','Fix WAA Outlier')
# comp.name <- 'Runs4abc'

run.ests <- prepare.run.ests(run.list=run.list)  

plot.fyr <- 1968  
plot.run.comparison(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


plot.fyr <- 2010  
plot.run.comparison(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


plot.fyr <- 2000  
plot.run.comparison(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.run.comparison(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='final.first')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }



#########################


run.list <- c(6,2:5)
names(run.list) <- c('Updated egg index','Initial','Updated maturity','Updated WAA','Updated Catch')
comp.name <- 'Runs2-6'

run.ests <- prepare.run.ests(run.list=run.list)  

plot.fyr <- 1968  
plot.run.estimates(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.fyr <- 1975  
plot.run.estimates(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


plot.fyr <- 2000  
plot.run.estimates(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


#########################


run.list <- c(9,6,7)
names(run.list) <- c('Final (adjusted egg index weighting)','Updated Canadian data','Updated trawl survey indices')
comp.name <- 'Runs6.7.9'

run.ests <- prepare.run.ests(run.list=run.list)  

plot.fyr <- 1968  
plot.run.estimates(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.fyr <- 1975  
plot.run.estimates(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


plot.fyr <- 2000  
plot.run.estimates(var.name='SSB', var.label.name='SSB (mt)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'SSB', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Freport', var.label.name='Fishing mortality', plot.fyr=plot.fyr, line.width=2, legend.loc='topleft', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'F', fig.type,sep='.')), type=fig.type) }

plot.run.estimates(var.name='Recr', var.label.name='Recruitment (000s)', plot.fyr=plot.fyr, line.width=2, legend.loc='topright', legend.type='ordered')
if(save.fig=='y') { savePlot(file.path(comp.dir, paste(comp.name,'fyr', plot.fyr,'Rect', fig.type,sep='.')), type=fig.type) }


