dict.creation <- function(dat, vars, vlists, vversion) {
    
    dictionary <- data.frame()
    
    for (y in vlists) { # y <- 1
        
        for (x in as.character(vars)) { # x = "ua1r10"
            
            temp.data <-
                dat[dat$vlist == y & dat$vversion >= vversion, c(x,"vversion")]
            
            if (nrow(temp.data) == 0) {
                stop(paste0("Adjust vlists to be only active vlists, the following vlist does not contain any active vversions: ",
                            print(y)))
            }
            
            names(temp.data)[names(temp.data) == x] <- "raw"
            
            temp.data	<-
                data.table(temp.data[,c("raw","vversion")])
            
            props <-
                temp.data[,.(zero = sum(!is.na(raw) & raw == 0)/
                                 (sum(!is.na(raw), is.na(raw))),
                             exist = sum(!is.na(raw))/
                                 (sum(!is.na(raw), is.na(raw))),
                             sample = sum(!is.na(raw), is.na(raw))),
                          by = .(vversion)]
            
            temp.data       <- as.data.frame(props)
            temp.data$raw   <- x
            temp.data$vlist <- rep(y, nrow(temp.data))
            
            temp.data$wm.exist <-
                ifelse(any(temp.data$exist > 0), 
                       weighted.mean(temp.data$exist[temp.data$exist > 0], 
                                     temp.data$sample[temp.data$exist > 0]), 0)
            
            temp.data$wm.zero <-
                ifelse(any(temp.data$zero > 0),
                       weighted.mean(temp.data$zero, 
                                     temp.data$sample), 0)
            
            
            temp.data$flag <-
                check(temp.data$sample, temp.data$wm.zero, temp.data$zero,
                      temp.data$wm.exist, temp.data$exist)
            
            dictionary <-
                rbind(dictionary, 
                      temp.data[, c("vlist", "raw", "vversion", 
                                    "wm.zero", "zero", "sample", "wm.exist", 
                                    "exist", "flag")])
            
        }
        
    }
    
    return(dictionary)
}