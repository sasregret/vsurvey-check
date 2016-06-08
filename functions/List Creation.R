list.creation <- function(dat, merger) {
    
    key <- vector()
    
    for (j in names(dat)) # j = "vlist.2.x"
    {
        listofvects <- list()
        
        for (i in 1:nrow(dat)) # i = 1
        {
            charvector <- 
                dat[i,j]
            
            numvector <- 
                list(as.numeric(as.vector(unlist(strsplit(charvector, ", ")))))
            listofvects <- 
                c(listofvects, numvector)
        }
        
        assign(j, listofvects, envir=globalenv())
        
        if (merger) {
            
            if (length(grep("x", j)) > 0) {

                key <-
                    c(key, gsub(".x", "", j))
            }
            
        } else {
            
            key <-
                c(key, j)
        }
    }
    
    return(key)
}