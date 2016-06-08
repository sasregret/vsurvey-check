## Pre-Formatting --------------------------------------------------------------

## Import Dictionary
dictionary <-
    read.csv(paste0(macro$path, "vsurvey check/output/dictionary.csv"))

## Pre-Format
vversion <-
    preformat(dictionary)


## Previous Data Merge (if needed) ---------------------------------------------

if (macro$ppull) {
    
    # import previous data
    old <-
        read.csv(paste0(macro$path, "vsurvey check/output/vlist.vversion.csv"), 
                 stringsAsFactors = F)
    
    # check if variables were added
    if (any(vversion$raw %nin% old$raw)) {
            
        new.vars <-
            new.variables(dat = vversion,
                          old = old)
    }
    
    # check if variables were taken out
    if (any(old$raw %nin% vversion$raw)) {
            
        old <-
            old[old$raw %in% vversion$raw,]
    }
    
    # merge in data
    new <-
       merge.clean(old = old, 
                   new = vversion, 
                   by.x = "raw", 
                   by.y = "raw")
    
    # merge overlapping vlists
    combined <-
        merge.overlap(dat = new)
    
    # add on new vars if added
    if (any(vversion$raw %nin% old$raw)) {
        
        combined <-
            rbind(combined, 
                  new.vars)
    }
    
    vversion <-
        combined
}


## Final Formatting ------------------------------------------------------------

output <-
    shortener(dat = vversion)


## Export ----------------------------------------------------------------------

write.csv(vversion, 
          paste0(macro$path, "vsurvey check/output/vlist.vversion.csv"), 
          row.names = FALSE)

write.csv(output, paste0(macro$path, "vsurvey check/output/vsurvey.csv"), row.names = FALSE)


## Removal ---------------------------------------------------------------------

toRemove <-
    c("combined", "new", "old", "dictionary", "new.vars", ls(patt="vlist"),
      "toRemove", "vversion", "output")

rm.me(toRemove)
