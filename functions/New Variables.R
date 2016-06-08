new.variables <- function(dat, old) {
    
    # find new variables
    new.vars <-
        dat[dat$raw %nin% old$raw,]
    
    # check old vlists
    if (any(ncol(old) %nin% ncol(new))) {
        
        old.vlists <-
            names(old)[names(old) %nin% names(dat)]
    }
    
    # fill in values for old vlists
    new.vars[, old.vlists] <-
        rep("0", nrow(new.vars))
    
    # sort columns of new variables data.frame
    vlists <-
        sort(grep("vlist", names(new.vars), value = T))
    
    new.vars <-
        new.vars[,c("raw", vlists)]
    
    return(new.vars)
}