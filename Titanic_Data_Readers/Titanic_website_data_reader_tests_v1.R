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



# define test page address
html_0 <- read_html("https://www.encyclopedia-titanica.org/titanic-ages/0.html")
html_1 <- read_html("https://www.encyclopedia-titanica.org/titanic-ages/1.html")
# use R to extract the valuesfrom webpage
## use e.g.  selectorgadget to figure out which css selector matches the data you want
##read vignette("selectorgadget")


html_0 %>% 
  html_nodes("#manifest span") %>% 
  html_structure()

# Select nodes from an HTML document in table cells
table_cells <- html_0 %>% 
  html_nodes("#manifest span")

length(table_cells)
table_cells[1]

#-------------------------------------------------------------

# Extract components with html_tag() (the name of the tag), 
# html_text() (all text inside the tag), 
# html_attr() (contents of a single attribute) 
# and html_attrs() (all attributes).

#-------------------------------------------------------

# Get the link to passenger webpage
link_part2 <- html_0 %>% 
  html_nodes("#manifest span [itemprop='url'] ")  %>%
  html_attr("href")

length(link_part2)
link_part2

link_part2[1]
#> [1] "/titanic-survivor/frank-philip-aks.html"

link <-
  ifelse(is.na(Link_part2), NA, paste('https://www.encyclopedia-titanica.org', link_part2, sep=''))

link
link[1]
#> [1] "https://www.encyclopedia-titanica.org/titanic-survivor/frank-philip-aks.html"
link[5]
#> [1] "https://www.encyclopedia-titanica.org/titanic-victim/gilbert-sigvard-emanuel-danbom.html"

## TBD extract: 'survivor' or 'victim' from a passenger link



# Get the full name of passenger
fullName <- html_0 %>% 
  html_nodes("#manifest span [itemprop='name'] ")  %>%
  html_text()

length(fullName)
fullName
fullName[1]
#> [1] "AKS, Master Philip\n"
##OK


# Get the family name of passenger
familyName <- html_0 %>% 
  html_nodes("#manifest span [itemprop='familyName'] ")  %>%
  html_text()

length(familyName)
familyName
familyName[1]
#> [1] "AKS"
##OK


# Get the first name/names of passenger
givenName <- html_0 %>% 
  html_nodes("#manifest span [itemprop='givenName'] ")  %>%
  html_text()

length(givenName)
givenName
givenName[1]
#> [1] "Philip"
##OK


# Get the honorific Prefix of passenger
honorificPrefix <- html_0 %>% 
  html_nodes("#manifest span [itemprop='honorificPrefix'] ")  %>%
  html_text()

length(honorificPrefix)
honorificPrefix
honorificPrefix[1]
#> [1] "Master"
##OK

# Get the age of passenger for age < 1
age_for_0Y <- html_0 %>% 
  html_nodes("#manifest td:nth-child(2)")  %>%
  html_text()

length(age_for_0Y)
age_for_0Y
age_for_0Y[1]
#[1] "\n10 m\n"
##OK

# Get the age of passenger for age >= 1
age <- html_1 %>% 
  html_nodes("#manifest td:nth-child(2)")  %>%
  html_text()%>%
  as.numeric()

length(age)
age
age[1]
#> [1] 1
#OK



# Get the Class of passenger or Dep of crew member
PasClass_CrewDep <- html_0 %>% 
  html_nodes("#manifest td:nth-child(3)")  %>%
  html_text() %>% 
  trimws()

length(PasClass_CrewDep)
PasClass_CrewDep
PasClass_CrewDep[1]
#> [1] "3rd Class Passenger "
##OK

# Get the Ticket no and fare of passenger
TicketNo_Fare <- html_0 %>% 
  html_nodes("#manifest td:nth-child(4)")  %>%
  html_text() %>% 
  trimws()

length(TicketNo_Fare)
TicketNo_Fare
TicketNo_Fare[1]
#> [1] "392091£9 7s"
#OK

## TBD split TicketNo and Fare

temp <-
TicketNo_Fare %>% 
  strsplit("\u00A3") ## '£'


TicketNo <- 
  temp %>% 
  purrr::map_chr(1) %>% 
  trimws()

length(TicketNo)
TicketNo[1]
#> [1] "392091"
  
Fare_temp <- purrr::map_chr(temp, 2)
Fare <-  trimws(paste("\u00A3", Fare_temp, sep=''))

Fare
length(Fare)
Fare[1]
#> [1] "£9 7s"

# Get the Port of Embarkment of passenger
PortEmba <- html_0 %>% 
  html_nodes("#manifest td:nth-child(5)") %>% 
  html_text() %>% 
  trimws()

length(PortEmba)
PortEmba
PortEmba[1]
#> [1] "Southampton"
##OK

# Get the job of passenger / crew
Job <- html_0 %>% 
  html_nodes("#manifest td:nth-child(6)") %>% 
  html_text() %>% 
  trimws() %>% 
  ifelse(.=="\n", NA)

length(Job)
Job
Job[1]
#> [1] NA
##OK

## TBD: if [1] "\n" change to 'NA'


# Get the Boat or [Body] of passenger / crew
Boat_Body <- html_0 %>% 
  html_nodes("#manifest td:nth-child(7)") %>% 
  html_text()

length(Boat_Body)
Boat_Body
Boat_Body[1]
#> "11 "



# html_nodes("#manifest span [itemprop='url'] ")  %>%
#   html_attr("href")

# Get the link to Pict of passenger / crew member
Pict_link_part2 <- html_0 %>% 
  html_nodes("#manifest td:nth-child(8)")  %>%
  html_node("[class='profile']")  %>%
  html_attr("src")

length(Pict_link_part2)
Pict_link_part2
Pict_link_part2[1]
#> [1] "/images/aks_fp.jpg"
Pict_link_part2[3]
#> [1] NA

Pict_link <- 
  ifelse(is.na(Pict_link_part2), NA, paste('https://www.encyclopedia-titanica.org', Pict_link_part2, sep=''))

Pict_link
Pict_link[1]
#> [1] "https://www.encyclopedia-titanica.org/images/aks_fp.jpg"
Pict_link[3]
#> [1] NA


# Get the ALT attribute of Pict of passenger / crew member
Pict_alt <- html_0 %>% 
  html_nodes("#manifest td:nth-child(8)")  %>%
  html_node("[class='profile']")  %>%
  html_attr("alt")

length(Pict_alt)
Pict_alt
Pict_alt[1]
#> [1] "P. Aks"



data.frame(familyName, givenName, honorificPrefix, age_for_0Y, PasClass_CrewDep, TicketNo, Fare, PortEmba, Job, link, Pict_link, Pict_alt, Boat_Body, stringsAsFactors = FALSE) %>% View()



# Navigate around a website as if you’re in a browser with
#html_session(), jump_to(), follow_link(), back(), and forward().

# http://stackoverflow.com/questions/15853204
# 
# s <- html_session("http://hadley.nz")
# s %>% jump_to("hadley-wickham.jpg") %>% jump_to("/") %>% session_history()
# s %>% jump_to("hadley-wickham.jpg") %>% back() %>% session_history()
# 
# s %>% follow_link(css = "p a")
