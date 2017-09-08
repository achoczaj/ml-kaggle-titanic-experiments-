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


html_homeCountries <- read_html("https://www.encyclopedia-titanica.org/titanic-passenger-crew-home-country/")

homeCountries <- html_table(html_homeCountries, header = FALSE, trim = FALSE, fill = TRUE)
homeCountries
length(homeCountries)

# countries <- 
for(i in 1:7) {
temp_countries <- unlist(homeCountries[[1]][i,], use.names=FALSE)
countries <- c(countries, temp_countries)
}
typeof(countries)
countries <- countries[!is.na(countries)]

# countries <- unlist(homeCountries, use.names=FALSE)
# countries <- countries[!is.na(countries)]

links <- page_homeCountries %>% 
  html_nodes(".table1 ") %>%
  html_attr("href")

links <- sort(links)


i <- 1
new_df <- data.frame()
for(link in links) {
  # link="/titanic-passengers-crew-lived/country-1/argentina.html" ##test loop
  
  # https://www.encyclopedia-titanica.org/titanic-passengers-crew-lived/country-1/argentina.html
  # https://www.encyclopedia-titanica.org/titanic-passengers-crew-lived/country-2/australia.html
  
  full_link <- paste( c("https://www.encyclopedia-titanica.org", link), collapse = "") 
  try({
    page <- read_html(full_link)
    
    new_data <- data.frame(html_table(page, header = TRUE, trim = FALSE, fill = FALSE))
    
    new_data$website_no_check <- link
    new_data$country <- countries[i]
        i <- i +1
    
    new_df <- rbind(new_df, new_data)
  })  
}
dim(new_df)
tail(new_df)
View(new_df)
