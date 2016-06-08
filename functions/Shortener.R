shortener <- function(dat) {
    
    key <-
        list.creation(dat[, grep("vlist", names(vversion), value = T)],
                      merger = F)
    
    output <- 
        data.frame(dat$raw)
    
    for (k in key) {
        
        shortened <- vector()
        final <- FALSE
        j <- 1
        
        vlist <- 
            eval(parse(text = k))
        
        while (j <= length(vlist)) {
            
            value <- vector()
            i <- 2
            
            start <- 
                vlist[[j]][i-1]
            
            if (is.na(vlist[[j]][i])) {
                
                while (is.na(vlist[[j]][i]) & !final) {
                    
                    if (j < length(vlist)) {
                        
                        j <- j + 1
                        
                        shortened <- 
                            c(shortened, paste("c(", start, ")", sep = ""))
                        
                        start <- 
                            vlist[[j]][i-1]
                        
                    } else {
                        
                        final <- T
                        
                        shortened <- 
                            c(shortened, paste("c(", start, ")", sep = ""))
                    }
                }
            }
            
            while (i < length(vlist[[j]])) {
                
                if (vlist[[j]][i-1] != (vlist[[j]][i] - 1)) {
                    
                    end <- vlist[[j]][i-1]
                    
                    if (end != start) {
                        
                        value <- 
                            c(value, paste(start, ":", end, sep = ""))
                        
                    } else {
                        
                        value <- 
                            c(value, as.character(end))
                    }
                    
                    start <- 
                        vlist[[j]][i]
                }
                
                i <- i + 1
            }
            
            if (!final) {
                
                if (vlist[[j]][i-1] == (vlist[[j]][i] - 1)) {
                    
                    hanging <- 
                        vlist[[j]][i]
                    
                    value <- 
                        c(value, paste(start, ":", hanging, sep = ""))
                    
                } else {
                    
                    end <- 
                        vlist[[j]][i-1]
                    
                    hanging <- 
                        vlist[[j]][i]
                    
                    if (start != end) {
                        
                        value <- 
                            c(value, paste(start, ":", end, sep = ""), 
                              as.character(hanging))
                        
                    } else {
                        
                        value <- 
                            c(value, start, as.character(hanging))
                    }
                    
                }
                
                shortened <- 
                    c(shortened, 
                      paste("c(", paste(value, collapse = ","), ")", sep = ""))
                
            }
            
            j <- j + 1
        }
        
        output[,k] <- shortened
    }
    
    return(output)
}