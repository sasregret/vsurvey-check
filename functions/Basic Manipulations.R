rm.me <- function(toRemove) {
    
    rm(list = intersect(toRemove, ls(.GlobalEnv)), envir = .GlobalEnv)
    
    gc()
    
}