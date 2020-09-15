# Web Scraping in R: rvest Tutorial
# source : https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
# site:Trustpilot:review businesses and services
#___________________________
install.packages("tidyverse")
library(tidyverse)  
# Parsing of HTML/XML files  
install.packages("rvest")
library(rvest)  
library(stringr)   
# Verbose regular expressions
library(rebus)
# Eases DateTime manipulation
install.packages("lubridate")
library(lubridate)
# URL
url <-'http://www.trustpilot.com/review/www.amazon.com'
# function to extract last page
get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.pagination-page ') %>% 
    # Extract the raw text as a list
    html_text()
    #pages_data
#      # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
   unname()  %>%                                     
    # Convert to number
    as.numeric()                                     
}
# to test the function
first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))
# to generate the urls of all pages
list_of_pages <- str_c(url, '?page=', 1:latest_page_number)
# validate pages
list_of_pages
#---------Extract the Information of One Page---------
get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.review-content__text') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.consumer-information__name') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}

get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    # The status information is this time a tag attribute
    html_attrs() %>%             
    # Extract the second element
    map(2) %>%                    
    unlist()
  dates <- html %>%
    html_nodes('time') %>%
    html_attrs() %>%
    map(1) %>%
    ymd_hms() %>%
    unlist()
  
}