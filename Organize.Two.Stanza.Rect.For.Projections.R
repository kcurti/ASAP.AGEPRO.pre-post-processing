

library(tidyverse)


rm(list=ls())
ls()

run.no <- '9'
proj.run <- 'Rect.1975.Onward'
current.assess.dir <- c('C:/Users/kiersten.curti/Desktop/Work/Mackerel/2023.Management.Track')

estimate.type <- 'point.est'   # 'median' or 'point.est'
                               #  i.e the median MCMC estimates or the ASAP point estimates 


#########################


run.dir <- file.path(current.assess.dir, paste('Run',run.no,sep=''))
proj.dir <- file.path(run.dir,'projections.brps',proj.run)
output.dir <- file.path(run.dir,'outputs')


### Organize reference points

brp.env <- new.env()
load(file.path(proj.dir, 'Projection.summary.RDATA'), envir = brp.env)

ssb.brp <- brp.env$ssb.brp
ssb.thres <- 0.5*ssb.brp
ssb.brp
ssb.thres


### Organize model estimates
ests.env <- new.env()
load( file.path(output.dir, paste('Run',run.no,'.Summary.Tables.with.CIs.RDATA',sep='')) , envir = ests.env)


### Estimates to use for projection specs (specified above as either the median MCMC estimates or the ASAP point estimates)
if(estimate.type == 'median')    { model.ests <- ests.env$median.annual.ests %>% 
                                                 mutate(Year = as.integer(rownames(.))) %>%
                                                 filter(Year>=1975) }
if(estimate.type == 'point.est') { model.ests <- ests.env$annual.ests %>% 
                                                 mutate(Year = as.integer(rownames(.))) %>%
                                                 filter(Year>=1975) }
### Estimates below SSB threshold
low.stanza.ests <- model.ests %>%
  filter(SSB<ssb.thres)

### Estimates greater than or equal to SSB threshold
high.stanza.ests <- model.ests %>%
  filter(SSB>=ssb.thres)

nrow(low.stanza.ests)
nrow(high.stanza.ests)
nrow(low.stanza.ests) + nrow(high.stanza.ests)
nrow(model.ests)


low.stanza.ests %>% pull(Rect)
model.ests %>% pull(Rect)


ssb.thres


model.ests %>%
  filter(Year >= 2009) %>%
  pull(Rect)



