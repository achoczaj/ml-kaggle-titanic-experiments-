convert_to_GBP <- function(l,s,d) {
  cat("Check:", is.na(l), is.na(s), is.na(d) )
  
  if (all(c(is.na(l), is.na(s), is.na(d))==TRUE)) {
    NA
  } else if (all(c(is.na(l), is.na(s), is.na(d))==FALSE)) {  
    l+(s/20)+(d/240)
  } else if (all(c(is.na(l), is.na(s), !is.na(d))==FALSE)) {  
    l+(s/20)
  } else if (all(c(is.na(l), !is.na(s), !is.na(d))==FALSE)) {  
    l
  } else if (all(c(!is.na(l), is.na(s), is.na(d))==FALSE)) {  
    (s/20)+(d/240)
  } else if (all(c(!is.na(l), !is.na(s), is.na(d))==FALSE)) {  
    (d/240)  
  } else {
    -1 #use negative value as error 
  }
}


a <- c(1,1,1,NA)
b <- c(5,5,5,5)
c <- c(25,25,25,25)

convert_to_GBP(a,b,c)  
check.options()
