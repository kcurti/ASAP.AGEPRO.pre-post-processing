# ASAP.AGEPRO.pre-post-processing

This repository contains R code for post-processing ASAP output, getting the data and ASAP outputs ready for input into AGEPRO, and post-processing AGEPRO output.

The following programs require a specific file structure for the modeling directory
   current.assess.dir
   current.assess.dir/Run4
   current.assess.dir/Run4/mcmc.2000.it.1000.thin 
   current.assess.dir/Run4/retro

... and require that they are run in order


R Program list:  

**Run ASAP Model**

Complete.ASAP.Plots.R
   --> Runs ASAPplots package

Summarize.ASAP.model.estimates.R 
   --> Summary.Tables.RDATA
   Summarizes SSB, B, F, FAA, NAA, selectivity and terminal year estimates

Summarize.Terminal.yr.ests.with.CIs.R 
   --> Summary.Tables.with.CIs.RDATA
   Takes the MCMC results and summarizes the 90% CIs

Complete.retro.adjs.and.compare.Terminal.yr.ests.R 
   --> Retrospective.Analysis.RDATA
   Takes terminal year estimates and mohns rho estimates from the restrospective analysis to calculate rho adjusted values
   Compares the rho adjusted values to the terminal year estimates with CIs to determine if a retro adjustment is needed

Projection.Prep.ASAP.Output.R
   --> ASAP.Data.For.Projections.RDATA
   Gets ASAP outputs ready for projections

**Run Long-term projections in AGEPRO**

Analyze.Long.Term.Projection.Output.R
   --> Projection.summary.RDATA

Compare.Terminal.yr.ests.to.BRPs.R
   --> Comparison.with.BRPs.RDATA
   Creates phase plot with and without CIs

**Run Short-term projections in AGEPRO**

Analyze.Short.Term.Projection.Output.R
   --> Projection.summary.RDATA

Plot.Short.Term.Projections.R
   --> Summary.All.Projections.RDATA

Compile.Summary.Catch.Status.Table.R
   --> Summary.Catch.Status.Table.RDATA


-------------------------


Plot.Historical.Retrospective.R
   --> Historical.retrospective.comparison.RDATA
   Creates plots comparing outputs with those of previous assessments

Run.Likelihood.Profile.R
   --> Final.Summary.RDATA
   Completes likelihood profiling over a specified parameter


