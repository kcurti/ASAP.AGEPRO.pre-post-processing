################
#
# May 2021: Combining Compare.Terminal.yr.ests.to.BRPs with Compare.Terminal.yr.ests.to.BRPs.Phase.Plot.with.CIs.R by putting the code for both phase plots in one program
#
################


rm(list=ls())
ls()

run.no <- '4'
proj.run <- 'Rect.1975.Onward'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling')

plot.data.name <- 'median.annual.ests' # 'median.annual.ests' or 'annual.ests'

save.fig <- 'y'



###########################



run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir,'projections.brps',proj.run)
output.dir <- file.path(run.dir,'outputs')

load( file.path(proj.dir, 'Projection.summary.RDATA') )
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )

# term.yr.cis <- read.csv( file.path(output.dir,paste('Run',run.no,'.Terminal.yr.estimates.with.CIs.csv',sep='')), row.names=1)
fmort.name <- rownames(terminal.yr.ests)[4]



### Annual estimates to use in the plot (specified above as annual.ests or median.annual.ests)
plot.data <- as.matrix(get(plot.data.name))

# Fyr and Lyr SSB and F estimates for plot labels
ssb.label.ests <- plot.data[as.character(c(fyr,lyr)),'SSB'] / 1000
f.label.ests   <- plot.data[as.character(c(fyr,lyr)),'F']
#ssb.tmt.label.ests <- ssb.label.ests / 1000

# First year estimates
fyr.ssb.tmt <- plot.data[as.character(fyr),'SSB']/1000
fyr.f   <- plot.data[as.character(fyr),'F']

# Terminal year estimates
lyr.ssb.tmt <- plot.data[as.character(lyr),'SSB']/1000
lyr.f   <- plot.data[as.character(lyr),'F']



### BRPs
ssb.brp
f.brp <- fmult.brp



### Create phase plots

# Standard phase plot

windows(width=5,height=4)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
plot((plot.data[,'SSB']/1000), plot.data[,'F'], type='o', cex=1.0, pch=16, col='black', axes=FALSE, xlab='', ylab='')
axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
box()
mtext(side=1,'Spawning stock biomass (000s mt)', line=0, outer=TRUE, cex=0.9)
mtext(side=2,'Fishing mortality', line=0, outer=TRUE, cex=0.9)
# First year values
lines(fyr.ssb.tmt, fyr.f, type='p',col='red', pch=16, cex=1.1)
# Last year values
lines(lyr.ssb.tmt, lyr.f, type='p',col='red', pch=16, cex=1.1)
# Labels for first and last year values
text(x=ssb.label.ests, y=f.label.ests, labels=names(f.label.ests), cex= 1.0, pos=1, font=2, col='red')
# Fmsy
abline(h = f.brp, lty=2)
# 1/2 SSBmsy
abline(v = (0.5*ssb.brp/1000), lty=2)
# SSBmsy
abline(v = (ssb.brp/1000), lty=2)
# BRP labels
# 1/2 SSBmsy
text(x=(0.5*ssb.brp/1000), y=2, labels=bquote('0.5*SSB'['40%'] ~ '= 0.5*SSB'['MSY PROXY'] ~ '=' ~ .(format(round((ssb.brp/2),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
# SSBmsy
text(x=(ssb.brp/1000), y=1.7, labels=bquote('SSB'['40%'] ~ '= SSB'['MSY PROXY'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
# Fmsy
text(x=700, y=f.brp, labels=bquote('F'['40%'] ~ '= F'['MSY PROXY'] ~ '=' ~ .(f.brp)), cex= 0.8, pos=3)

if(save.fig == 'y'){savePlot(file.path(output.dir, 'Phase.plot.wmf'))}



# Phase plot with CIs

# CIs for last year values
lyr.ssb.tmt.uci <- terminal.yr.ests['SSB.mt', '95th percentile'] / 1000
lyr.ssb.tmt.lci <- terminal.yr.ests['SSB.mt', '5th percentile']  / 1000

lyr.f.uci <- terminal.yr.ests[fmort.name, '95th percentile']
lyr.f.lci <- terminal.yr.ests[fmort.name, '5th percentile']

windows(width=5,height=4)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
plot((plot.data[,'SSB']/1000), plot.data[,'F'], type='o', cex=1.0, pch=16, col='black', axes=FALSE, xlab='', ylab='')
  axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
  box()
mtext(side=1,'Spawning stock biomass (000s mt)', line=0, outer=TRUE, cex=0.9)
mtext(side=2,'Fishing mortality', line=0, outer=TRUE, cex=0.9)
# First year values
lines(fyr.ssb.tmt, fyr.f, type='p',col='red', pch=16, cex=1.1)
# Last year values
lines(lyr.ssb.tmt, lyr.f, type='p',col='red', pch=16, cex=1.1)
# Last year CIs in F
arrows(lyr.ssb.tmt, lyr.f, lyr.ssb.tmt, lyr.f.uci, length=0.05, angle=90, code=3, col='red')
arrows(lyr.ssb.tmt, lyr.f, lyr.ssb.tmt, lyr.f.lci, length=0.05, angle=90, code=3, col='red')
# Last year CIs in SSB
arrows(lyr.ssb.tmt, lyr.f, lyr.ssb.tmt.lci, lyr.f, length=0.05, angle=90, code=3, col='red')
arrows(lyr.ssb.tmt, lyr.f, lyr.ssb.tmt.uci, lyr.f, length=0.05, angle=90, code=3, col='red')

# Labels for first and last year values
text(x=ssb.label.ests, y=f.label.ests, labels=names(f.label.ests), cex= 1.0, pos=1, font=2, col='red')
# Fmsy
abline(h = f.brp, lty=2)
# 1/2 SSBmsy
abline(v = (0.5*ssb.brp/1000), lty=2)
# SSBmsy
abline(v = (ssb.brp/1000), lty=2)
# BRP labels
# 1/2 SSBmsy
text(x=(0.5*ssb.brp/1000), y=2, labels=bquote('0.5*SSB'['40%'] ~ '= 0.5*SSB'['MSY PROXY'] ~ '=' ~ .(format(round((ssb.brp/2),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
# SSBmsy
text(x=(ssb.brp/1000), y=1.7, labels=bquote('SSB'['40%'] ~ '= SSB'['MSY PROXY'] ~ '=' ~ .(format(round((ssb.brp),0),big.mark=',')) ~ 'mt'), cex= 0.8, pos=4)
# Fmsy
text(x=700, y=f.brp, labels=bquote('F'['40%'] ~ '= F'['MSY PROXY'] ~ '=' ~ .(f.brp)), cex= 0.8, pos=3)

if(save.fig == 'y'){savePlot(file.path(output.dir, 'Phase.plot.with.cis.wmf'))}



### Calculate current F and SSB and percentage of reference points

current.ssb <- plot.data[as.character(lyr),'SSB']
current.f   <- plot.data[as.character(lyr),'F']

prop.ssb.brp <- current.ssb/ssb.brp
prop.f.brp <- current.f/f.brp



### Save workspace
save.image(file.path(output.dir, 'Comparison.with.BRPs.RDATA'))



####################################################



rm(list=ls())
ls()

run.no <- '4'

run.dir <- paste('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling/Run',run.no,sep='')
output.dir <- file.path(run.dir,'outputs')

load(file.path(output.dir, 'Comparison.with.BRPs.RDATA'))


