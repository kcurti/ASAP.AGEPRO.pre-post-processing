#######
### This program reads in the short-term projection outputs and extracts the median as well as 90th and 95th percentiles.
#######


rm(list=ls())
ls()


# Run details
run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')


# Projection details

rect.name <- 'Rect.2Stanza' #   'Rect.2009' # 
f.name <- 'F20' # 'FMSY' # 

proj.dir.name <- 'rebuilding'
proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), toupper(f.name), sep='.')


### Output details
# Quantile values that go into percentile calculations
quant.values <- c(0.025, 0.05, 0.40, 0.75, 0.95, 0.975)

# Corresponding percentiles for summary table
table.percentiles <- c('5%','40%','95%')
  names(table.percentiles) <- c('5th Percentile', '40th Percentile', '95th Percentile')
# table.percentiles <- c('5%','75%','95%')
#   names(table.percentiles) <- c('5th Percentile', '75th Percentile', '95th Percentile')
  
  

######## ######## ########



### set directories and the agepro file name (proj.fname) based on above inputs

run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))

proj.master.folder <- paste('projections',proj.dir.name, sep='.')
proj.master.dir <- file.path(run.dir, proj.master.folder)

proj.dir <- file.path(proj.master.dir, f.name)
# proj.dir <- file.path(proj.master.dir, 'AvgABC_4800mt')



### Read in rdat file

rdat <- dget(file.path(proj.dir, paste(proj.fname, 'RDAT', sep='.')))

proj.fyr <- rdat$genparms$startyear
proj.lyr <- rdat$genparms$endyear
proj.yrs <- as.character(proj.fyr:proj.lyr)



### Read in AGEPRO auxillary files

# SSB (xx3)
ssb <- read.table(file.path(proj.dir, paste(proj.fname,'xx3',sep='.')))
  colnames(ssb) <- proj.yrs
ssb.median <- apply(ssb,2,median)
ssb.CI <- apply(ssb,2,function(x){quantile(x, quant.values) })

# Combined catch (xx6)
catch <- read.table(file.path(proj.dir, paste(proj.fname,'xx6',sep='.')))
  colnames(catch) <- proj.yrs
catch.median <- apply(catch,2,median)
catch.CI <- apply(catch,2,function(x){quantile(x, quant.values) })

# Fmult (xx9)
fmult <- read.table(file.path(proj.dir, paste(proj.fname,'xx9',sep='.')))
  colnames(fmult) <- proj.yrs
fmult.median <- apply(fmult,2,median)
fmult.CI <- apply(fmult,2,function(x){quantile(x, quant.values) })

# # Stock Biomass (Jan-1) (xx4)
# biomass <- read.table(file.path(proj.dir, paste(proj.fname,'xx4',sep='.')))
#   colnames(biomass) <- proj.yrs
# biomass.median <- apply(biomass,2,median)
# biomass.CI <- apply(biomass,2,function(x){quantile(x, quant.values) })

# Recruitment (xx2)
rect <- read.table(file.path(proj.dir, paste(proj.fname,'xx2',sep='.')))
  colnames(rect) <- proj.yrs
rect.median <- apply(rect,2,median)
rect.CI <- apply(rect,2,function(x){quantile(x, quant.values) })



### Create summary tables for each variable

ests <- c('Median', names(table.percentiles))  
summary.template <- data.frame(matrix(NA,nrow=length(ests), ncol=(length(proj.yrs)+2)))
  colnames(summary.template) <- c(rep(NA,2),proj.yrs)
  rownames(summary.template) <- ests
summary.template[,2] <- ests

create.summary.table <- function(var.name, var.label, unit.scalar, round.digits)
{
  # var.name <- 'ssb';  var.label <- toupper(var.name);  unit.scalar <- 1;  round.digits <- 0
  summary.table <- summary.template
  summary.table[1,1] <- var.label
  summary.table['Median',proj.yrs]          <- round(get(paste(var.name,'median',sep='.'))/unit.scalar, round.digits)
  for (p in 1:length(table.percentiles))
  {  
    # p <- 1
    percent.value <- table.percentiles[p]
    percent.name  <- names(table.percentiles)[p]
    var.table <- get(paste(var.name,'CI',sep='.'))
    summary.table[percent.name, proj.yrs]  <-  round(var.table[percent.value,]/unit.scalar, round.digits)
  }
  summary.table
}

ssb.table   <- create.summary.table('ssb', 'SSB (mt)', 1, 0)
catch.table <- create.summary.table('catch', 'Catch (mt)', 1, 0)
f.table     <- create.summary.table('fmult','F', 1, 2)
# biomass.table <- create.summary.table('biomass','January 1 biomass (mt)', 1, 0)
rect.table  <- create.summary.table('rect','Recruitment (000s)', 1000, 0)

# combined.table <- rbind.data.frame(ssb.table, f.table, catch.table, rect.table, biomass.table)
combined.table <- rbind.data.frame(ssb.table, f.table, catch.table, rect.table)


### Save image and export CSV file

rm(ssb, catch, fmult, rect)
# save.image(file.path(proj.dir, 'Projection.summary.RDATA'))
# write.csv(combined.table,file.path(proj.dir, 'Short.term.projection.summary.csv'), na='')

save.image(file.path(proj.dir, paste(f.name, rect.name, 'Projection.summary.RDATA', sep='.')))
write.csv(combined.table, file.path(proj.dir, paste(f.name, rect.name, 'Short.term.projection.summary.csv', sep='.')), na='', row.names=FALSE)



####################################################################



# load(file.path(proj.dir, paste(f.name, rect.name, 'Projection.summary.RDATA', sep='.')))
