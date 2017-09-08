#---------------------------------------------

# Scrape the data about Titanic's passangers to collect their cabins
# from: https://www.encyclopedia-titanica.org/cabins.html  
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


library(rvest)
library(dplyr)
library(purrr)


url <- "https://www.encyclopedia-titanica.org/cabins.html"
pgsession <- html_session(url)               ## create session

# pull page once and store in case you want to parse multiple elements
page <- pgsession %>% jump_to(url) %>% read_html()

titanic_cabin_tabl <- page %>% 
  html_nodes("[itemprop='articleBody'] .Normal table") %>%
  html_table(fill=TRUE)

titanic_cabin_tabl %>%  View()

# Write df_all dataframe as .csv file
write.csv(titanic_cabin_tabl, file="Titanic_Cabin_List_v1.csv", row.names = TRUE)




#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# other options
# 
# df_all <- tibble()
# 
# # loop 
# # from:  tr:nth-child(1) td
# # to:  tr:nth-child(362) td
# output <- vector("character", length(4))
# for (i in 2:362) {
#   i <- 4 ## only for test
#   output <- page %>%
#     html_nodes(paste( c("tr:nth-child(", as.character(i), ") td"), collapse = "")) %>% 
#     # html_nodes(sprintf("tr:nth-child(%d) td", i)) %>% 
#     html_text()
# 
#   output
#   
#   df_all <- rbind(df_all, output)
# 
#   df_all
#   }
# 
# #sprintf(base_url, i)
# 
# 
# df_all %>% View()
# class(df_all)
# 
# # tr:nth-child(362) td
# # Year works okay because there is exactly one per house
# year_build <- houses %>%
#   html_node(".built-year") %>%
#   html_text() %>% 
#   tidyr::extract_numeric()