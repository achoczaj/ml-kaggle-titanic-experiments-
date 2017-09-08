#---------------------------------------------

# Scrape the data about Titanic's crew and passangers to collect their home country 
# from: https://www.encyclopedia-titanica.org/titanic-passenger-crew-home-country/
#
#---------------------------------------------

# Titanic_website_data_reader: Country_of_residence

# Install some goodness:
# install.packages("magrittr")
# install.packages("xml2")
# install.packages("rvest")


library(magrittr)
library(xml2)
library(rvest)
library(plyr)

page_homeCountries <- read_html("https://www.encyclopedia-titanica.org/titanic-passenger-crew-home-country/")

homeCountries <- html_table(page_homeCountries, header = FALSE, trim = FALSE, fill = TRUE)
homeCountries
length(homeCountries)

# countries
# for(i in 1:7) {
# temp_countries <- unlist(homeCountries[[1]][i,], use.names=FALSE)
# countries <- c(countries, temp_countries)
# }
# typeof(countries)
# countries <- countries[!is.na(countries)]

# countries <- unlist(homeCountries, use.names=FALSE)
# countries <- countries[!is.na(countries)]
# countries
# 
# countries_links <- tolower(countries)
# countries_links[8] <- "channel-islands"
# countries_links[9] <- "china-hong-kong"
# countries_links[19] <- "hong-kong"
# countries_links[28] <- "northern-ireland"
# countries_links[35] <- "south-africa"
# countries_links[40] <- "united-states"
# 
# countries_links

links <- page_homeCountries %>%
  html_nodes(".table1 ") %>%
  html_attr("href")
# 
# links <- sort(links)
links

# i <- 1
new_df <- data.frame()
for(link in links) {
# for(country in countries_links) {
  # country <- countries_links[1] ##test loop
  
  # https://www.encyclopedia-titanica.org/titanic-passengers-crew-lived/country-1/argentina.html
  # https://www.encyclopedia-titanica.org/titanic-passengers-crew-lived/country-2/australia.html
  
  # full_link <- paste( c("https://www.encyclopedia-titanica.org/titanic-passengers-crew-lived/country-",i,"/",country, ".html"), collapse = "") 
  # link <- links[1] ##test loop
  full_link <- paste( c("https://www.encyclopedia-titanica.org/", link), collapse = "") 
  try({
    page <- read_html(full_link)
    
    new_data <- data.frame(html_table(page, header = TRUE, trim = FALSE, fill = FALSE))
    
    new_data$website_no_check <- link
    # 
    # new_data$website_no_check <- country
    # new_data$country <- countries[i]
    #     i <- i +1
    
    new_df <- rbind.fill(list(new_df, new_data))
  })  
}
dim(new_df)
tail(new_df)
View(new_df)
