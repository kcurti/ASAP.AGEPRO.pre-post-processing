############################################
# The following program assumes that the base/original run has already been completed
############################################

library(stringr)


rm(list=ls())
ls()

# param.abbrev <- 'H'
# profile.name <- 'Hprofile'
# dat.param.name <- 'steepness_ini'
# param.label.name <- 'Steepness'
param.abbrev <- 'M'
profile.name <- 'Mprofile'
dat.param.name <- 'M'
param.label.name <- 'Natural mortality'

param.vec <- seq(0.05,0.75,0.01)
  names(param.vec)[param.vec<0.1]  <- paste0(param.abbrev,str_sub((param.vec[param.vec<0.1]),-2))
  names(param.vec)[param.vec>=0.1] <- paste0(param.abbrev,100*param.vec[param.vec>=0.1])
    

### Paths to directories with original ASAP dat files and original ASAP dat file names
orig.fname <- 'Run4'

assess.dir <- 'C:/users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling'
orig.dir <- file.path(assess.dir,orig.fname)


### Path to directory where all runs completed in this program call will be saved
profile.dir <- file.path(assess.dir, paste(orig.fname,profile.name,sep='.'))


### Path to directory with ASAP3 executable
asap.dir <- c('C:/NFT/ASAPV30')


### Figure type for summary plots (cannot alter plot type from Liz's code)
fig.type <- 'wmf'
save.plots <- TRUE


################################# End of specifications ##################################################



# Source R programs with the read and write functions
source(paste(assess.dir,'Read.ASAP3.dat.file.R', sep='/'))
source(paste(assess.dir,'Write.ASAP3.dat.file.R',  sep='/'))

  
############## Read in results from orig run ############## 

orig.rdatfile <- paste(orig.fname,'RDAT',sep='.')
orig.rdat <- dget(file.path(orig.dir,orig.rdatfile))


############## Create run directory ############## 

if(!dir.exists(profile.dir))  {
  dir.create(profile.dir)
} 


############## Create dat files and run ASAP ##############

  
### Read in and organize original ASAP dat file
orig.datfile <- paste(orig.fname,'DAT',sep='.')
asap <- read.asap3.dat.file(datf=file.path(orig.dir,orig.datfile))
orig.dat <- asap$dat
orig.dat.param <- orig.dat[[dat.param.name]]
comments <- asap$comments
CAA_mats  <- asap$fleet.names
  names(CAA_mats) <- as.character(1:length(CAA_mats))
IAA_mats  <- asap$survey.names
  names(IAA_mats) <- as.character(1:length(IAA_mats))

nages <- orig.dat$n_ages
fyr  <- orig.dat$year1
nyrs <- orig.dat$n_years
  yrs <- as.character(seq(from=fyr, length=nyrs, by=1))

convergence <- rep(NA,length(param.vec))    
  names(convergence) <- names(param.vec)

### Loop over parameter values to create new dat file, run ASAP and read in resulting rdat file
for (param.run in 1:length(param.vec))
{
  # param.run <- 11
  param.value <- param.vec[param.run]
  param.name <- names(param.vec)[param.run]
  
  # Create directory for run
  param.dir <- file.path(profile.dir, param.name)
  if(!dir.exists(param.dir))  {
    dir.create(param.dir)
  } 
  
  # Create copy of dat file for modification  
  adj.dat <- orig.dat  
  
  # Modified parameter matrix
  if(class(orig.dat.param)=='matrix')                   {adj.dat.param <- matrix(param.value,nrow=nrow(orig.dat.param), ncol=ncol(orig.dat.param)) }
  if(class(orig.dat.param)%in%c('integer','numeric'))   {adj.dat.param <- rep(param.value, length(orig.dat.param))}
  adj.dat[[dat.param.name]] <- adj.dat.param
  
  # Write new ASAP dat file
  dat.obj <- list( dat=adj.dat, comments=comments, fleet.names=CAA_mats, survey.names=IAA_mats )
  dat.fname <- paste0(param.name,'.dat')
  write.asap3.dat.file( fname = file.path(param.dir, dat.fname),  dat.object = dat.obj,  header.text = paste(orig.datfile, 'but with M =', param.value))
  
  # Copy and paste ASAP executable into run directory if not already there
  if(!file.exists( file.path(param.dir, 'asap3.exe') ))  {
    file.copy( from=file.path(asap.dir,'asap3.exe'), to=file.path(param.dir,'asap3.exe'), copy.date=TRUE)
  }   

  # Run ADMB (using a dat file that has a different name than the executable)
  setwd(param.dir)
  assign( 'cmd.output',
          shell(paste('ASAP3.exe -ind', dat.fname, ">", 'asap3.out'), intern = TRUE)
  )
  
  # If converged, read in rdat file
  if (is.null(attributes(get('cmd.output'))) ) 
  {
    assign( paste(param.name,'rdat',sep='.'), dget(file.path(param.dir,'asap3.rdat')) )
    convergence[param.name] <-TRUE
  }  else {
    convergence[param.name] <-FALSE
  }

  # Remove objects
  rm(param.value, param.name, param.dir, adj.dat, dat.obj, dat.fname, cmd.output)
} # End of parameter value loop


### Save workspace progress
save.image(file.path(profile.dir,'Intermediate.Summary.RDATA'))



############## Create likelihood profiles ##############


# rm(list=ls())
# ls()
# orig.fname <- 'Run26_2017'
# assess.dir <- 'C:/users/kiersten.curti/Desktop/Herring'
# profile.name <- 'Hprofile'
# profile.dir <- file.path(assess.dir, paste(orig.fname,profile.name,sep='.'))
# load(file.path(profile.dir,'Intermediate.Summary.RDATA'))
# save.plots <- 'TRUE'

# Vector to store likelihood values
orig.like <- rep(NA,length(param.vec))
  names(orig.like) <- names(param.vec)

  
### Loop over parameter values
for (param.run in 1:length(param.vec))
{
  # param.run <- 1
  param.value <- param.vec[param.run]
  param.name <- names(param.vec)[param.run]
  if(convergence[param.name])
  {
    rdat <- get(paste(param.name,'rdat',sep='.'))
    orig.like[param.name] <- rdat$like$lk.total
  }
}  

total.like <- orig.like[!is.na(orig.like)]  # look at only those likelihoods that converged
min.like <- min(total.like)
min.param.name <- names(total.like[total.like==min.like])
min.param <- param.vec[min.param.name]

orig.param <- unique(as.vector(orig.dat.param))
orig.like <- orig.rdat$like$lk.total
orig.param.name <- paste(param.abbrev,strsplit(as.character(orig.param),"\\.")[[1]][2],sep="")
  if(nchar(orig.param.name)==2) {orig.param.name <- paste(orig.param.name,'0',sep='')}

ymin <- round(min.like*.995,0)  
ymax <- round(max(total.like)*1.005,0)  

windows(width=5,height=4)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
plot( param.vec[names(total.like)], total.like, ylim=c(ymin,ymax), axes=FALSE) 
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
  axis(side=1, at=param.vec, labels=TRUE, cex.axis=0.8, padj = -0.5)
  box()
  mtext(side=1,param.label.name, line=0, outer=TRUE, cex=0.9)
  mtext(side=2,'Objective function value', line=0, outer=TRUE, cex=0.9)
# Add colors for the base run and minimum
lines(min.param, rep(min.like,length(min.param)), type='p',col='red', pch=16, cex=1.3)
lines(orig.param, orig.like, type='p',col='midnightblue', pch=16, cex=1.3)

if (save.plots==T)  savePlot( file.path(profile.dir, paste('Likelihood.profile',fig.type,sep='.')), type=fig.type)


diff.like <- total.like-min.like
ymax.diff <- round(max(diff.like)*1.005,0)  

windows(width=5,height=4)
par(mar=c(2, 2, 0.1, 1) +0.1);  par(oma=c(1.5,1.5,1.0,0))
plot( param.vec[names(diff.like)], diff.like, ylim=c(0,ymax.diff), axes=FALSE) 
axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=0.8, padj = 0.5)
axis(side=1, at=param.vec, labels=TRUE, cex.axis=0.8, padj = -0.5)
box()
mtext(side=1,param.label.name, line=0, outer=TRUE, cex=0.9)
mtext(side=2,'Likelihood difference', line=0, outer=TRUE, cex=0.9)
# Add colors for the base run and minimum
lines(min.param, diff.like[min.param.name], type='p',col='red', pch=16, cex=1.3)
lines(orig.param, diff.like[orig.param.name], type='p',col='midnightblue', pch=16, cex=1.3)

if (save.plots==T)  savePlot( file.path(profile.dir, paste('Likelihood.difference.profile',fig.type,sep='.')), type=fig.type)



### Save final workspace    
save.image(file.path(profile.dir,'Final.Summary.RDATA'))



#############################################



rm(list=ls())
ls()

orig.fname <- 'Run4'
assess.dir <- 'C:/users/kiersten.curti/Desktop/Work/Mackerel/2021.MT.Modeling'
profile.name <- 'Mprofile'

orig.dir <- file.path(assess.dir,orig.fname)
profile.dir <- file.path(assess.dir, paste(orig.fname,profile.name,sep='.'))


load(file.path(profile.dir,'Final.Summary.RDATA'))

  
param.vec <- param.vec[param.vec<=0.5]
