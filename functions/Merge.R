merge.clean <- function(old, new, by.x, by.y) {
    
    temp.data <-
        merge(old,
              new,
              by.x = by.x,
              by.y = by.y,
              all.x = T)
    
    temp.data[] <-
        lapply(temp.data, as.character)
    
    x <-
        grep(".x", names(temp.data), value = T)

    subs <-
        gsub(".x", "", x)
    
    temp.data[, subs] <-
        rep(1, nrow(temp.data))
    
    return(temp.data)
}


merge.overlap <- function(dat) {
    
    # create list of numeric vectors
    key <-
        list.creation(dat = dat[, grep("x|y", names(new), value = T)], 
                      merger = T)
    
    # merge overlapping vlists
    for (j in key) { # j = "vlist.3"
        
        vlist.x <- 
            eval(parse(text = paste0(j, ".x")))
        
        vlist.x <- 
            lapply(vlist.x, function(x) replace(x, x == 4 | x == 0, -99))
        
        vlist.y <- 
            eval(parse(text = paste0(j, ".y")))
        
        vlist <-
            list()
        
        for (i in 1:length(dat[,j])) { # i = 1
            
            vlist[[i]] <-
                c(unlist(vlist.x[[i]]), unlist(vlist.y[[i]]))
            
            vlist[[i]] <-
                vlist[[i]][which(vlist[[i]] != -99)]
            
            dat[i, j] <-
                paste(vlist[[i]], collapse = ", ")
        }
    }
    
    dat <-
        dat[, names(dat)[names(dat) %nin% grep("x|y", names(dat), value = T)]]
    
    dat <-
        dat[, c("raw", sort(grep("vlist", names(dat), value = T)))]
    
    return(dat)
}