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

proj.dir.name <- 'short.term'
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


##### AgePro analyze NAA (xx1) file #####

rm(list=ls())
ls()


# Run details
run.no <- '8'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/2025.Management.Track')

# Projection details
rect.name <- 'Rect.2Stanza' #   'Rect.2009' # 
f.name <- 'F20' # 'FMSY' # 

proj.dir.name <- 'short.term'
proj.fname <- paste('PROJECTIONS.THROUGH2032', toupper(rect.name), toupper(f.name), sep='.')


### set directories and the agepro file name (proj.fname) based on above inputs
run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.master.folder <- paste('projections',proj.dir.name, sep='.')
proj.master.dir <- file.path(run.dir, proj.master.folder)
proj.dir <- file.path(proj.master.dir, f.name)
naa.dir <- file.path(proj.master.dir, f.name, "With_NAA")

load(file.path(proj.dir, paste(f.name, rect.name, 'Projection.summary.RDATA', sep='.')))

min.age <- rdat$genparms$minage
max.age <- rdat$genparms$maxage
ages <- as.character(min.age:max.age)


### NAA (xx2)

niter <- rdat$genparms$nsims * rdat$genparms$nboot
nyrs <- length(proj.yrs)
naa.wide <- read.table(file.path(naa.dir, paste(proj.fname,'xx1',sep='.')))
  colnames(naa.wide) <- ages
nrow(naa.wide)
niter*nyrs

naa.wide$Year <- rep(proj.yrs, niter)
naa.wide$Iteration <- rep(1:niter, each=nyrs)
  
library(tidyverse)

naa.wide.median <- 
  naa.wide %>%
  group_by(Year) %>%
  summarize(across(as.character(1:10),median))

year.class <- 
  bind_cols(
    Year=rep(as.integer(proj.yrs), times=length(ages)),
    Age=rep(as.integer(ages), each=length(proj.yrs))
    ) %>%
  mutate(Year.class = Year-Age)
year.class.wide <- pivot_wider(year.class, names_from=Age, values_from=Year.class)

naa <- pivot_longer(naa.wide, cols=ages, names_to="Age", values_to="NAA")
naa.median <- naa %>%
  group_by(Year, Age) %>%
  summarize(NAA.median = median(NAA)) %>%
  mutate(Age = as.integer(Age),
         Year = as.integer(Year))


### Import biological inputs and calculate SSB

inputs <- new.env()
load(file.path(run.dir, "projections.brps", 'ASAP.Data.For.Projections.RDATA'), envir=inputs)

maturity <- data.frame(maturity = inputs$maturity.summary$avg)
  maturity$Age <- as.integer(names(inputs$maturity.summary$avg))
waa <- data.frame(WAA = inputs$ssb.waa.summary$avg)
  waa$Age <- as.integer(names(inputs$ssb.waa.summary$avg))

ssb <- full_join(naa.median, maturity) %>%
  full_join(., waa) %>%
  mutate(SSB.kg = NAA.median * maturity * WAA,
         SSB.mt = SSB.kg/1000)
  

### Create labels for paper fish
  
paper.fish <- 
  year.class %>%
  mutate(category = 
           case_when(
             Year.class == 2024 ~ "terminal.yr",
             Year.class > 2024 ~ "projected",
             Year.class < 2024 ~ "estimated"
             )
  )

ssb.paper.fish <- full_join(ssb, paper.fish) %>%
  group_by(Year, category) 
  # summarize(SSB.mt = sum(SSB.mt))
  # 

