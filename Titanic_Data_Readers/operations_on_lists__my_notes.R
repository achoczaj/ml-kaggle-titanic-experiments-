#----------------------------
# Create a symmetric list

my.list1    <- list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12))
length(my.list1)
lengths(my.list1)
str(my.list1)

# convert symmetric list to a matrix
matrix( unlist(my.list1), nrow=length(my.list1), byrow=TRUE )
matrix( unlist(my.list1), nrow=length(my.list1), byrow=FALSE )

# alternative method to convert symmetric list to a matrix
do.call(rbind, my.list1)
do.call(cbind, my.list1)


#----------------------------------
# Extract the second subelement of every element in a list while ignoring NA's in sapply in R


mylist <- list(c(6,7), NA, c(8,9), c(""), c(1), c(1:5))
length(mylist)
#> 6

# as a char vector
t <- sapply(mylist, function(x) x[2])
t
t <- sapply(mylist, `[`, 2)
t
t <- sapply(mylist, `[`, 2)
t

mylist[1] ## extract the first sublist in of mylist
mylist[[1]] ## extract all elements in the first sublist of mylist

mylist[[1]][2] ## extract the secound element in the first sublist of mylist
mylist[[c(1,2)]] ## extract the secound element in the first sublist of mylist


mylist[[]][2] ## this is wrong and does not work


is.vector(t)
t[3]
class(t)
typeof(t)

# as a list
t <- lapply(mylist, function(x) x[2])
t
t <- lapply(mylist, `[`, 2)
t

class(t)
typeof(t)


#------------------------------------
df <- data.frame()
df <- structure(list(lab = c("N00", "N01", "N02", "B00", "B01", "B02", 
                       "Z21", "BA01", "NA03")), .Names = "lab", row.names = c(NA, -9L
                       ), class = "data.frame")

df$pre<-strsplit(df$lab, "[0-9]+")
df
df$suf<-strsplit(df$lab, "[A-Z]+")
df
# extract the second element of each list item
df$suf 
sapply(df$suf, "[", 2)
sapply(df$suf, "[[", 2) ## works the same

df$pre <- sub("^([A-Z]+)[0-9]+", "\\1", df$lab)
df$suf <- sub("^[A-Z]+([0-9]+)", "\\1", df$lab)

df
