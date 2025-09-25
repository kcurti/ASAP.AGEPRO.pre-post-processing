#######
#
# Builds off of workspace created in Summarize.Terminal.yr.ests.with.CIs.R
# Takes terminal year estimates and mohns rho estimates from the restrospective analysis to calculate rho adjusted values
# Compares the rho adjusted values to the terminal year estimates with CIs to determine if a retro adjustment is needed
#
#######


rm(list=ls())
ls()


run.no <- '8'
save.fig <- TRUE
fig.type <- 'png'

current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')


#######################


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))

output.dir <- file.path(run.dir,'outputs')
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) )


### Extract terminal year estimates
term.yr.ssb <- unlist(terminal.yr.ests['SSB.mt',])
term.yr.f   <- unlist(terminal.yr.ests['Avg.F',])


### Import rho values
retro.dir <- file.path(run.dir, 'retro','plots')
retro.rho.values <- read.csv(file.path(retro.dir, paste('Retro.rho.values_Run',run.no,'.RETRO_000.csv',sep='')), row.names=1)


### Function to calculate rho-adjusted values using Mohn's rho
calc.rho.adj.ests <- function(orig.ests, rho)
{
  adj.ests <- (1/(1+rho))*orig.ests
  adj.ests
}


### Calculate adjusted SSB and F
mohns.rho.ssb <- retro.rho.values['Mohn.rho','ssb.rho']
mohns.rho.f <-   retro.rho.values['Mohn.rho','f.rho']

rho.adj.ssb <- calc.rho.adj.ests(term.yr.ssb, mohns.rho.ssb)
rho.adj.f   <- calc.rho.adj.ests(term.yr.f,   mohns.rho.f)
rho.adj.ests <- rbind.data.frame('SSB.mt'=t(rho.adj.ssb), 'Avg.F'=t(round(rho.adj.f,3)))


### Convert SSB to tmt for plotting
term.yr.ssb.tmt <- term.yr.ssb/1000
rho.adj.ssb.tmt <- rho.adj.ssb/1000


### Plot original and rho-adjusted F and SSB values with their confidence intervals
ssb.max <- max(cbind(term.yr.ssb.tmt, rho.adj.ssb.tmt))
f.max   <- max(cbind(term.yr.f, rho.adj.f))

windows(width=5,height=4)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
plot(term.yr.ssb.tmt['Estimate'], term.yr.f['Estimate'], axes=FALSE, xlim=c(0,ssb.max), ylim=c(0,f.max), xlab='', ylab='', pch=16)
arrows(term.yr.ssb.tmt['5th percentile'],  term.yr.f['Estimate'],
       term.yr.ssb.tmt['95th percentile'], term.yr.f['Estimate'], length=0.05, angle=90, code=3, col='black')
arrows(term.yr.ssb.tmt['Estimate'],  term.yr.f['5th percentile'],
       term.yr.ssb.tmt['Estimate'],  term.yr.f['95th percentile'], length=0.05, angle=90, code=3, col='black')
lines(rho.adj.ssb.tmt['Estimate'], rho.adj.f['Estimate'], type='p', pch=16, col='red')
# arrows(rho.adj.ssb.tmt['5th percentile'],  rho.adj.f['Estimate'],
#        rho.adj.ssb.tmt['95th percentile'], rho.adj.f['Estimate'], length=0.05, angle=90, code=3, col='black', lty=2)
# arrows(rho.adj.ssb.tmt['Estimate'],  rho.adj.f['5th percentile'],
#        rho.adj.ssb.tmt['Estimate'],  rho.adj.f['95th percentile'], length=0.05, angle=90, code=3, col='black', lty=2)
axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
box()
mtext(side=1,"Spawning stock biomass (000s mt)", line=0, outer=TRUE, cex=0.9)
mtext(side=2,'Fishing mortality', line=0, outer=TRUE, cex=0.9)
if(save.fig) {savePlot(file=file.path(output.dir, paste("Terminal.yr.and.Retro.adj.values.with.CIs",fig.type,sep=".")),type=fig.type)}
if(save.fig) {savePlot(file=file.path(output.dir, paste("Terminal.yr.and.Retro.adj.values.with.CIs","wmf",sep=".")),type="wmf")}


### Confirm whether retro adjustment is needed based on terminal year CIs and retro adjusted values
retro.adj.needed <- FALSE
if(
  rho.adj.ssb.tmt['Estimate'] < term.yr.ssb.tmt['5th percentile']  | 
  rho.adj.ssb.tmt['Estimate'] > term.yr.ssb.tmt['95th percentile']
  ) {retro.adj.needed <- TRUE}
if(
  rho.adj.f['Estimate'] < rho.adj.f['5th percentile']  | 
  rho.adj.f['Estimate'] > rho.adj.f['95th percentile']
) {retro.adj.needed <- TRUE}
retro.adj.needed


### If retro adjustment is needed, output retro adjusted values
if(retro.adj.needed==TRUE)
{
  rho.adj.ests.formatted <- rho.adj.ests
    rownames(rho.adj.ests.formatted) <- c('Spawning stock biomass (mt)', 'Average F (ages 6-9)')
  write.csv(rho.adj.ests.formatted, file.path(output.dir, paste('Run',run.no,'.Rho.adj.terminal.yr.ests.csv',sep='')) )
}


### Save final workspace
save.image( file.path(output.dir, paste('Run',run.no,'.Retrospective.Analysis.RDATA',sep='')) )



##########################################



rm(list=ls())
ls()


run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
output.dir <- file.path(run.dir,'outputs')
load( file.path(output.dir, paste('Run',run.no,'.Retrospective.Analysis.RDATA',sep='')) )




