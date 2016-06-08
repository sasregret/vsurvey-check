## Inputs ----------------------------------------------------------------------

macro        <- list()
macro$path   <- "C:/Projects- R/"
macro$legend <- "Legend (WBIE - LSW) 2016.xlsx"
macro$data   <- "mev16003.txt"
macro$proc   <- F # boolean for if your raw data's column names have already been processed
macro$vlists <- c(1:5) # vlist number you want to calculate vversions for. Can enter more than one
macro$ppull  <- F # boolean for previous pull or not
macro$max.vv <- -1 # max vversion for previous pull


## Source ----------------------------------------------------------------------

source(paste0(macro$path, "vsurvey check/source/01.Setup.R"))

## Update Dictionary before running
source(paste0(macro$path, "vsurvey check/source/02.Output.R"))
