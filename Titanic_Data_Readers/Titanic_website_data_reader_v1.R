#---------------------------------------------

# Scrape the data about Titanic's crew and passangers to collect their ages, jobs
# from: https://www.encyclopedia-titanica.org/titanic-ages/0.html (age < 1 Y)
# ...
# to: https://www.encyclopedia-titanica.org/titanic-ages/81.html (age 81 Y) 
#
# used method to parse html: html_table()
#---------------------------------------------


# Install some goodness:
# install.packages("magrittr")
# install.packages("xml2")
# install.packages("rvest")


library(magrittr)
## provides “pipe”-like operator %>% with which you may pipe a value forward into an expression or function call
library(xml2)

library(rvest)
## Scrape (or harvest) data from html web pages, inspired by libraries like beautiful soup.
## Wrappers around the 'xml2' and 'httr' packages to make it easy to download, then manipulate, HTML and XML.



# test code on one html page - passengers' age less than 1Y
# https://www.encyclopedia-titanica.org/titanic-ages/0.html
html <- read_html("https://www.encyclopedia-titanica.org/titanic-ages/0.html")


# Parse an html table into data frames with html_table().
# assumptions:
# - Headers are in the first row
# - No cells span multiple rows

df_0 <- data.frame()
df_0 <- data.frame(html_table(html, header = TRUE, trim = FALSE, fill = FALSE))
# add info for webpage source name 
df_0$website_no_check <- 0 ## '0' for https://.../0.html

# some checks
tail(df_0)
ncol(df_0)

View(df_0)
## results for Age in months (m)
##test OK


# Run code for parsing data for ages: 0-75

df_all <- data.frame()
for(i in 1:75) {
  link <- paste( c("https://www.encyclopedia-titanica.org/titanic-ages/", as.character(i),".html"), collapse = "") 
  try({
    page <- read_html(link)
    
    # parse an html table into a data frame
    new_data <- data.frame(html_table(page, header = TRUE, trim = FALSE, fill = FALSE))
    # add new column with info for webpage source name 
    new_data$website_no_check <- i
    ## '0' for https://.../0.html
    
    df_all <- rbind(df_all, new_data)
  })  
  }

dim(df_all)
#> [1] 2214    9

tail(df_all)


df <- rbind(df_0, df_all)
dim(df)
View(df)
#> 2225    9

## Should be 2249 people according to the webpage info.


# Write df_all dataframe as .csv file
write.csv(df, file="Titanic_Crew_and_Passangers_List_v1.csv", row.names = TRUE)
