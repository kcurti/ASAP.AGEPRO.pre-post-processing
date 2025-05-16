############################################
# The following program assumes that the base/original run has already been completed with MCMC output
############################################


rm(list=ls())
ls()


### Paths to directories with original ASAP dat files and original ASAP dat file names
  # orig.fname represents the base name of the dat file used to run the MCMC
base.dir <- c('C:/Users/Kiersten.Curti/Desktop/Herring/Run26_2017')
orig.dir <- file.path(base.dir, 'mcmc')
orig.fname <- c('RUN26_2017.MCMC') 


### Path to directory where all runs completed in this program call will be saved
run.dir <- file.path(base.dir,'Remove_One_Survey')


### Path to directory with ASAP3 executable
asap.dir <- c('C:/NFT/ASAPV30')


### Code directory
code.dir <- c('C:/Users/Kiersten.Curti/Desktop/Herring')


### Figure type for summary plots
fig.type <- 'wmf'
save.plots <- T


################################# End of specifications ##################################################


# Source R programs with the read and write functions
source(paste(code.dir,'Read.ASAP3.dat.file.R', sep='/'))
source(paste(code.dir,'Write.ASAP3.dat.file.R',  sep='/'))

  
############## Read in results from orig run ############## 

orig.rdatfile <- paste(orig.fname,'RDAT',sep='.')
orig.rdat <- dget(file.path(orig.dir,orig.rdatfile))



############## Create run directory ############## 

if(!dir.exists(run.dir))  {
  dir.create(run.dir)
} 


############## Create dat files and run ASAP ##############

  
### Read in and organize original ASAP dat file
orig.datfile <- paste(orig.fname,'DAT',sep='.')
asap <- read.asap3.dat.file(datf=file.path(orig.dir,orig.datfile))
orig.dat <- asap$dat
comments <- asap$comments
CAA_mats  <- asap$fleet.names
  names(CAA_mats) <- as.character(1:length(CAA_mats))
IAA_mats  <- asap$survey.names
  names(IAA_mats) <- as.character(1:length(IAA_mats))

nages <- orig.dat$n_ages
fyr  <- orig.dat$year1
nyrs <- orig.dat$n_years
  yrs <- as.character(seq(from=fyr, length=nyrs, by=1))
  
IAA_used <- IAA_mats[orig.dat$use_index==1]
IAA.obj.names <- gsub("[[:space:]]", "", IAA_used)

convergence <- rep(NA,length(IAA_used))    
  names(convergence) <- IAA.obj.names

  
### Loop over surveys to sequentially turn each one off, run ASAP and read in resulting rdat file
for (survey in IAA_used)
{
  
  # survey <- 'Shrimp'
  survey.num.char <- names(IAA_used)[IAA_used==survey]
  survey.num <- as.numeric(survey.num.char)
  survey.name <- IAA.obj.names[survey.num.char]
  
  # Create survey directory if does not already exist
  survey.dir <- file.path(run.dir,survey)
  if(!dir.exists(survey.dir))  {
    dir.create(survey.dir)
  } 
  
  # Create copy of dat file for modification  
  adj.dat <- orig.dat  

  # Modify survey indicators and data matrix
  adj.dat$use_index[survey.num] <- 0
  adj.dat$use_index_acomp[survey.num] <- 0
  adj.dat$IAA_mats[[survey.num]][,4:(4+nages)] <- 0

  # Write new ASAP dat file
  dat.obj <- list( dat=adj.dat, comments=comments, fleet.names=CAA_mats, survey.names=IAA_mats )
  dat.fname <- paste( survey.name, 'dat', sep='.' )
  write.asap3.dat.file( fname = file.path(survey.dir, dat.fname),  dat.object = dat.obj,  header.text = paste(orig.datfile, 'but without', survey))
    
  # Copy and paste ASAP executable into run directory if not already there
  if(!file.exists( file.path(survey.dir, 'asap3.exe') ))  {
    file.copy( from=file.path(asap.dir,'asap3.exe'), to=file.path(survey.dir,'asap3.exe'), copy.date=TRUE)
  }   

  # Run ADMB (using a dat file that has a different name than the executable)
  setwd(survey.dir)
  assign( 'cmd.output',
          shell(paste('ASAP3.exe -ind', dat.fname, ">", 'asap3.out'), intern = TRUE)
  )

  # If converged, read in rdat file
  if (is.null(attributes(get('cmd.output'))) ) 
  {
    assign( paste(survey.name,'rdat',sep='.'), dget(file.path(survey.dir,'asap3.rdat')) )
    convergence[survey.name] <-TRUE
  }  else {
    convergence[survey.name] <-FALSE
  }
  
} # End of survey loop


### Save workspace progress
rm(survey.num, survey, survey.name, adj.dat, dat.obj, dat.fname, cmd.output)
save.image(file.path(run.dir,'Intermediate.Summary.RDATA'))



############## Analyze Results ##############



# rm(list=ls())
# ls()
# base.dir <- c('C:/Users/Kiersten.Curti/Desktop/Herring/Run26_2017')
# run.dir <- file.path(base.dir,'Remove_One_Survey')
# load(file.path(run.dir,'Intermediate.Summary.RDATA'))


### Figure specs

# Color palette for figures
liz.palette = c( "black"  ,  "purple3" ,  "blue"  ,  "turquoise2"  ,
                "red2" ,   "orange" ,  "#994411",   "#770000"    ,
                "#335500"  ,  "springgreen3" , "green1" ,  "gold3",
                "#333388" ,      "orchid"   ,     "mediumpurple1"      , "gray60"  , 
                "deeppink4"  ,    "violetred2"  ,     "#9900AA"    , "#8888EE",
                "yellow1"   ,     "yellowgreen"  ,  "#778800" ,      "#FFBB11"  ,  
                "#CC5588"  ,"#11BB77"   , "#11AADD"   ,   "#335522"   ,  
                "#BB1133"   ,     "#4400AA",  "#FF7755"   ,  "#77AACC"   , 
                "#FF00BB" ,  "grey50"   ,  "#FF2233" , "#99BB77"  ,  
                "grey35"   ,    "#CCDD00" ,    "#AA77FF"   ,  "#88BB88"    )
liz.palette <- rep(liz.palette, 25)

# Line-type list
lty.list <- c(1,3,5,rep(1:6,25))

# Orig.run specs
orig.lty <- lty.list[1]
orig.col <- liz.palette[1]
orig.lwd <- 1
CI.lwd <- 2
CI.lty <- lty.list[2]

     
### Create template matrix for storing data to be plotted
survey.template <- data.frame( structure( matrix(NA,nrow=nyrs,ncol=length(IAA.obj.names)), dimnames=list(yrs,IAA.obj.names) ))
orig.template <- data.frame( structure( matrix(NA,nrow=nyrs,ncol=3), dimnames=list(yrs,c('Pred','LB.5th','UB.95th')) ) )


### MCMC outputs
mcmc.base.names <-c('Freport','Full.F','Jan1.B','ssb')
  names(mcmc.base.names) <- c('F.report','F.age','tot.jan1.B','SSB') 


### Function to extract data, save as permanent object, and plot basic results
plot.basic.results <- function(plot.var, plot.fname, y.label)
{
  # plot.var <- 'SSB'; plot.fname='SSB'; y.label='Spawning stock biomass';

  # Matrix of surveys to be plotted
  survey.data <- survey.template

  # Survey loop to pull data from rdat objects
  for (survey.name in IAA.obj.names)
  {
    print(survey.name)
    # survey.name <- IAA.obj.names[1]
    rdat.name <- paste(survey.name,'rdat',sep='.') 

    if(exists(rdat.name))
    {
      rdat <- get(rdat.name)
        var.data <- rdat[[plot.var]]
        if (plot.var=='N.age') { var.data <- rdat[[plot.var]][,'1']  }
        if (plot.var=='F.age') { var.data <- apply(rdat[[plot.var]],1, max) }
      survey.data[,survey.name] <- var.data
    }
  } # End of survey.name loop

  # Save survey.data to permanent object
  assign(paste(plot.var,'surveys',sep='.'), survey.data)
  
  # Pull data from original run
  orig.data <- orig.template
    orig.var <- orig.rdat[[plot.var]]
    if (plot.var=='N.age') { orig.var <- orig.rdat[[plot.var]][,'1']  }
    if (plot.var=='F.age') { orig.var <- apply(orig.rdat[[plot.var]],1, max) }
  orig.data$Pred <- orig.var  

  # Pull confidence intervals from MCMC files, if they exist
  if(plot.var %in% names(mcmc.base.names))
  {
    mcmc.file <- file.path(orig.dir, 'plots', paste( mcmc.base.names[plot.var], '90pi.csv', sep='.' ) )
    mcmc <- read.csv(mcmc.file, header=TRUE)
    orig.data$LB.5th <- mcmc$X5th
    orig.data$UB.95th <- mcmc$X95th
  }
  
  # Plot indices from each run
  x.data <- as.numeric(rownames(survey.data))
  y.range <- range( c(orig.data,survey.data), na.rm=TRUE )
    y.range[1] <- y.range[1]*0.97
    y.range[2] <- y.range[2]*1.03
  
  windows(width=6,height=4)
  par(mfrow=c(1,1))
  par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,5.5))
  
  # Survey runs
  for (survey.num in 1:length(IAA.obj.names))
  {
    # survey.num <- 1
    survey.name <- IAA.obj.names[survey.num]  
    if(survey.num==1) 
    {
      plot(x.data, survey.data[,survey.name], ylim=y.range, axes=FALSE, xlab='', ylab='', lty=lty.list[survey.num+1], col=liz.palette[survey.num+1], lwd=1, type='l')
    } else {
      lines(x.data, survey.data[,survey.name], lty=lty.list[survey.num+1], col=liz.palette[survey.num+1], lwd=1, type='l')
    }  
  }

  # Orig run
  lines(x.data, orig.data$Pred, lty=orig.lty, col=orig.col, lwd=orig.lwd, type='l')
  # Plot bounds if MCMC output exists for the variable of interest
  if(!is.na(orig.data$LB.5th[1])) 
    {
    lines(x.data, orig.data$LB.5th, lty=CI.lty, col=orig.col, lwd=CI.lwd, type='l')
    lines(x.data, orig.data$UB.95th, lty=CI.lty, col=orig.col, lwd=CI.lwd, type='l')
    }

  axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=0.8, padj = -0.5)
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
  box()
  mtext(side=1,'Year', line=0, outer=TRUE, cex=0.9)
  mtext(side=2,y.label, line=0.2, outer=TRUE, cex=0.9)

  # Plot empty figures for placement of legends
  par(oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend('right', IAA_used, xjust=0, title='Dropped survey index', bty="n", col=liz.palette[2:(length(IAA.obj.names)+1)], lty=lty.list[2:(length(IAA.obj.names)+1)], cex=0.7, lwd=1) 
  par(oma=c(0, 0, 0, 0), mar=c(0, 0, 2.5, 2), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    if(!is.na(orig.data$LB.5th[1])) 
    {
      legend('topright', c('Base run','90% CIs'), bty="n", col=rep(orig.col,2), lty=c(orig.lty,CI.lty), cex=0.7, lwd=c(orig.lwd,CI.lwd))
    } else {
      legend('topright', c('Base run'), bty="n", col=orig.col, lty=orig.lty, cex=0.7, lwd=orig.lwd)
    }

  if (save.plots==T)  savePlot( file.path(run.dir, paste(plot.fname,fig.type,sep='.')), type=fig.type)

} # End of plot basic results function


### Function calls
plot.basic.results(plot.var='F.report', plot.fname='F.report', y.label='F report')
plot.basic.results(plot.var='SSB', plot.fname='SSB', y.label='Spawning stock biomass')
plot.basic.results(plot.var='tot.jan1.B', plot.fname='Jan1.Biomass', y.label='January 1 Biomass')
plot.basic.results(plot.var='exploitable.B', plot.fname='exploitable.Biomass', y.label='Exploitable biomass')
plot.basic.results(plot.var='N.age', plot.fname='recruitment', y.label='Recruitment')
plot.basic.results(plot.var='F.age', plot.fname='F.Full', y.label='F full')
  

### Save final workspace    
save.image(file.path(run.dir,'Final.Summary.RDATA'))

  

