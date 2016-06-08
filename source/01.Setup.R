## Packages --------------------------------------------------------------------

require(data.table)
require(xlsx)
require(Hmisc)


## Functions -------------------------------------------------------------------

sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        source(file.path(path, nm), ...)
    }
}

sourceDir(paste0(macro$path, "vsurvey check/functions"))


## Import Survey Data ----------------------------------------------------------

dat <-
    suppressWarnings(
        fread(paste0(macro$path, macro$data), 
              sep = "|", data.table = F))

## Process Column Names
if (!macro$proc) {
  
    names(dat) <- gsub("_", ".", tolower(names(dat)))
}
   
## Import Formats
formats <-
    read.xlsx2(paste0(macro$path, macro$legend),
          sheetName = "formats",
          stringsAsFactors = F)

vars <-
    unique(formats$raw[formats$raw != "" & 
                           !is.na(formats$raw)])

if (any(vars %nin% names(dat))) {
    stop(paste0("Variable(s) not in data: ",
                paste(vars[vars %nin% names(dat)], collapse = ", ")))
}

## Assign Max Vversion
if (!macro$ppull) {
    
    macro$max.vv <- -1
}


## Dictionary Creation ---------------------------------------------------------

dictionary <-
    dict.creation(dat = dat, 
                  vars = vars,
                  vlists = macro$vlists,
                  vversion = macro$max.vv)


## Export ----------------------------------------------------------------------
write.csv(dictionary, paste0(macro$path, "vsurvey check/output/dictionary.csv"), 
          row.names = F)


## Removal ---------------------------------------------------------------------
rm(dictionary, dat, formats, vars)
