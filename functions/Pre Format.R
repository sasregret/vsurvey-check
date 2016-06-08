preformat <- function(dictionary) {
    
    temp.data <-
        dictionary[c("flag", "vlist", "raw", "vversion")]
    
    data.vlists <-
        sort(unique(temp.data$vlist))
    
    vlist.vversion <- data.frame()
    
    for (i in unique(temp.data$raw)) { # i <- "c5r2" 
        
        crosstable <-
            table(temp.data[temp.data$raw == i & temp.data$flag == "include", 
                            "vlist"],
                  temp.data[temp.data$raw == i & temp.data$flag == "include", 
                            "vversion"])
        
        crosstable <-
            array(crosstable, 
                  dim = c(length(unique(temp.data[temp.data$raw == i & temp.data$flag == "include", "vlist"])), 
                          length(unique(temp.data[temp.data$raw == i & temp.data$flag == "include", "vversion"]))),
                  dimnames = list(sort(as.numeric(unique(temp.data[temp.data$raw == i & temp.data$flag == "include", "vlist"]))),
                                  sort(as.numeric(unique(temp.data[temp.data$raw == i & temp.data$flag == "include", "vversion"])))))
        
        if (length(crosstable) == 0) {
            
            values <-
                rep("0", length(data.vlists))
            
            names(values) <-
                as.character(data.vlists)
            
        } else {
            
            values <-
                try(apply(crosstable, 1, function(z)
                    paste(names(z)[which(z > 0)], collapse = ", ")))
            
            if (!(class(values) %in% "Error"))
                values[as.character(setdiff(data.vlists, names(values)))] <- "0"
        }
        
        names(values) <-
            paste("vlist", names(values), sep = ".")
        
        raw <- c(i)
        
        values <-
            data.frame(raw, t(values), check.names = FALSE, stringsAsFactors = FALSE)
        
        vlist.vversion <-
            rbind(vlist.vversion, values)
    }
    
    return(vlist.vversion)
}