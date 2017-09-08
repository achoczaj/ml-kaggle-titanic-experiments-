#---------------------------------------------
# 
# Tests to scrape the data about Titanic's crew and passangers to collect their ages, jobs
# from: https://www.encyclopedia-titanica.org/titanic-ages/0.html (age < 1 Y)
# ...
# to: https://www.encyclopedia-titanica.org/titanic-ages/81.html (age 81 Y) 
#
#---------------------------------------------


# Install some goodness:
# install.packages("magrittr")
# install.packages("xml2")
# install.packages("rvest")

library(magrittr)
## provides “pipe”-like operator %>% with which you may pipe a value forward into an expression or function call

library(xml2)

library(rvest)
## Wrappers around the 'xml2' and 'httr' packages to make it easy to download, then manipulate, HTML and XML.
## Scrape (or harvest) data from html web pages, inspired by libraries like beautiful soup

library(tidyr)

library(tidyverse)

library(stringr)

# define test page addresses
# html_0 <- read_html("https://www.encyclopedia-titanica.org/titanic-ages/0.html")
# html_1 <- read_html("https://www.encyclopedia-titanica.org/titanic-ages/1.html")


# use R to extract the values from webpage 
## use e.g.  selectorgadget to figure out which css selector matches the data you want
##read vignette("selectorgadget")

titanic_all_results <- data.frame()
for(i in 0:81) {
  # i <- 30 # only for tests
  link <- paste( c("https://www.encyclopedia-titanica.org/titanic-ages/", as.character(i),".html"), collapse = "") 

  try({
    html <- read_html(link)
    
    
    # Get the full name of passenger
    FullName <- html %>% 
      html_nodes("#manifest span [itemprop='name'] ")  %>%
      html_text()
    
    if (is.character(FullName) && length(FullName) == 0L ) {
      next
    }
    
    # Get the family name of passenger
    FamilyName <- html %>% 
      html_nodes("#manifest span [itemprop='familyName'] ")  %>%
      html_text()
    
    
    # Get the first name/names of passenger
    GivenName <- html %>% 
      html_nodes("#manifest span [itemprop='givenName'] ")  %>%
      html_text()
    
    
    # Get the honorific Prefix of passenger
    HonorificPrefix <- html %>% 
      html_nodes("#manifest span [itemprop='honorificPrefix'] ")  %>%
      html_text()
    
    
    # Get the age of passenger
    Age <- html %>% 
      html_nodes("#manifest td:nth-child(2)")  %>%
      html_text() %>% 
      ## clean up with gsub(pattern, replacement, x)
      gsub("[\r\n]", "", .) %>%
      trimws()
    
    # Get the Class of passenger or Dep of crew member
    PasClass_CrewDep <- html %>% 
      html_nodes("#manifest td:nth-child(3)")  %>%
      html_text() #%>% 
      #trimws()
    
    
    # Get the Ticket No and Fare of passenger
    TicketNo_Fare_concatenated <- html %>% 
      html_nodes("#manifest td:nth-child(4)")  %>%
      html_text() %>% 
      trimws()
    
    #====================================
    ## dummy data tests: 
    # TicketNo_Fare_concatenated <- c("", "£13 11s", "336439£7 15s","87£3", "13509£26 11s", "£10 10s 5p" )
    
    # Separate Ticket No from Fare
    TicketNo_Fare_list <-
      TicketNo_Fare_concatenated %>% 
      str_split("£", simplify = FALSE)  ## use simplify = FALSE to return a list
    
    
    # Get the Ticket_Full_No of passenger
    Ticket_Full_No <- 
      sapply(TicketNo_Fare_list, `[`, 1)
    ## fill empty values with NA
    Ticket_Full_No[Ticket_Full_No == ""] <- NA
    
    
    
    # Get the TicketFare values for tickets
    TicketFare_vector <- 
      sapply(TicketNo_Fare_list, `[`, 2)
    
    ## fill empty values with NA
    TicketFare_vector[TicketFare_vector == ""] <- NA
    
    ## create TicketFare matrix
    TicketFare_matrix <- 
      TicketFare_vector %>% 
      str_split(boundary("word"), simplify = TRUE)  ## use simplify = TRUE to return a matrix
    
    
    ## check if matrix has 3 columns - it should
    ## if not add missing columns with NA values
    no_col <- ncol(TicketFare_matrix)
    while (3-no_col) {
      TicketFare_matrix <- cbind(TicketFare_matrix, NA) 
      no_col <- no_col+1
      }
  
    ## remove letters (char) from values
    if (ncol(TicketFare_matrix)<=3 || ncol(TicketFare_matrix)==0) {
    try({
      TicketFare_matrix[,1] <- str_replace(TicketFare_matrix[,1], "\u00A3", "") ## remove £ char, if any 
      TicketFare_matrix[,2] <- str_replace(TicketFare_matrix[,2], "s", "") ## remove s char, if any
      TicketFare_matrix[,3] <- str_replace(TicketFare_matrix[,3], "p", "") ## remove d char, if any
      
      ## fill empty values with NA
      TicketFare_matrix[TicketFare_matrix == ""] <- NA  
    })  
    } else {
      ## generate error values as mess
      TicketFare_matrix[,c(1,2,3)] <- -1
    }  
    
    ## convert char to integers
    Fare__Old_Pounds <- as.integer(TicketFare_matrix[,1])
    Fare__Old_Shillings <- as.integer(TicketFare_matrix[,2])
    Fare__Old_Pennies <- as.integer(TicketFare_matrix[,3])
    
    ## create int tibble 
    TicketFare_tbl_df <- tibble(Fare__Old_Pounds, Fare__Old_Shillings, Fare__Old_Pennies)

    ## convert Fare to GBP
    TicketFares <- TicketFare_tbl_df %>% 
      mutate(Fare_in_GBP = ifelse(!is.na(Fare__Old_Pounds), Fare__Old_Pounds, 0) 
             + ifelse(!is.na(Fare__Old_Shillings), Fare__Old_Shillings/20, 0)
             + ifelse(!is.na(Fare__Old_Pennies), Fare__Old_Pennies/250, 0) )
    
    ## change 0 to NA in Fare_in_GBP 
    TicketFares$Fare_in_GBP[is.na(Fare__Old_Pounds)==TRUE] <- NA

    
    # Get the Port of Embarkment of passenger
    PortEmba <- html %>% 
      html_nodes("#manifest td:nth-child(5)") %>% 
      html_text() %>% 
      trimws()
    PortEmba[PortEmba == ""] <- NA 
    
    # Get the job of passenger / crew
    Job <- html %>% 
      html_nodes("#manifest td:nth-child(6)") %>% 
      html_text() %>% 
      gsub("[\r\n]", "", .) %>%
      trimws()
    Job[Job == ""] <- NA
    
    
    # Get the Boat or [Body] of passenger / crew
    Boat_Body <- html %>% 
      html_nodes("#manifest td:nth-child(7)") %>% 
      html_text()
    Boat_Body[Boat_Body == ""] <- NA
    
    # Get the link to passenger webpage
    link_temp <- 
      html %>% 
      html_nodes("#manifest span [itemprop='url'] ")  %>%
      html_attr("href")  
    
    Passenger_link <-
      ifelse(is.na(link_temp), NA, paste('https://www.encyclopedia-titanica.org', link_temp, sep=''))
    
    # Get the passenger status: survivor/victim from his link
    Passenger_status <-
      if(str_detect(link_temp, "-survivor") ) {
        "Survivor"
      } else if (str_detect(link_temp, "-victim") ) {
          "Victim"
        } else { "Other"}
    
    
    # Get the link to Pict of passenger / crew member
    Pict_link_part2 <- html %>% 
      html_nodes("#manifest td:nth-child(8)")  %>%
      html_node("[class='profile']")  %>%
      html_attr("src")
    
    Pict_link <- 
      ifelse(is.na(Pict_link_part2), NA, paste('https://www.encyclopedia-titanica.org', Pict_link_part2, sep=''))
    
    
    # Get the ALT attribute of Pict of passenger / crew member
    Pict_alt <- html %>% 
      html_nodes("#manifest td:nth-child(8)")  %>%
      html_node("[class='profile']")  %>%
      html_attr("alt")
  
  ## end of try() ## 
})


  new_data <- data.frame(FamilyName, GivenName, HonorificPrefix, Age, PasClass_CrewDep, Ticket_Full_No, TicketFares, PortEmba, Job, Passenger_link, Pict_link, Pict_alt, Boat_Body, Passenger_status, FullName, stringsAsFactors = FALSE) 
  # new_data %>% View()

  # add new column with info for webpage source name 
  new_data$Website_AgeNo <- i
  ## '0' for https://.../0.html
  
  titanic_all_results <- rbind(titanic_all_results, new_data)

## end of for loop ##
## reading webpages ##  
}

dim(titanic_all_results)
#> 2225 19
## page with ages < 1 not included in process - webpage issue
## Should be 2249 people according to the webpage info.

titanic_all_results %>% View()


# Write df_all dataframe as .csv file
write.csv(titanic_all_results, file="Titanic_Crew_and_Passangers_List_v2.2.csv", row.names = TRUE)
