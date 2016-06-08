check <- function(sample, wm.zero, zero, wm.non.na, non.na) {
  
  
  non.na.cutoff <-
    ifelse(sample %in% 0:20, .32,
           ifelse(sample %in% 21:50, .20,
                  ifelse(sample %in% 51:100, .16,
                         ifelse(sample %in% 101:200, .12,
                                ifelse(sample %in% 201:400, .08, .06)))))
  
  zero.cutoff <-
    ifelse(sample %in% 0:20, .32,
           ifelse(sample %in% 21:50, .20,
                  ifelse(sample %in% 51:100, .16,
                         ifelse(sample %in% 101:200, .12,
                                ifelse(sample %in% 201:400, .08, .06)))))
  
  flag <-
    ifelse(abs(non.na - wm.non.na) > non.na.cutoff, "non.na",
           ifelse(wm.non.na == 0, "missing",
                  ifelse(non.na == 0, "non.na",
                         ifelse(abs(zero - wm.zero) > zero.cutoff, "zero", "include"))))
  
  return(flag)
  
}