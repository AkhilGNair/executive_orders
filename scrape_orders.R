library(stringr)
library(rvest)
library(XML)
library(dplyr)
library(tidyr)
library(data.table)

xpath_links = '//*[@id="content"]/p/strong/a'
css_text    = 'strong:first-child'
root_domain = 'http://www.archives.gov'

attach_domain = function(subdomain, domain = root_domain) {
  paste0(domain, subdomain)
}
  
drop_from_urls = function(urls, chr_drop = "subjects") {
  urls[!(urls %like% chr_drop)]
}

# get the list of presidents who have issued executive orders
url <- "http://www.archives.gov/federal-register/executive-orders/disposition.html"
read <- read_html(url)
link_nodes <- read %>% html_nodes(xpath = xpath_links)
text_nodes <- read %>% html_nodes(css = css_text)
text_content <- text_nodes %>% html_text()
text_content <- str_replace_all(text_content[1:13], "\\r\\n", " ")  # the presidents

# Set up initial url to president table, group by president
dt <- as.tbl(data.frame(President = text_content, url = link_nodes %>% html_attr("href")))
dt <- dt %>% mutate(url = attach_domain(url))
dt <- dt %>% mutate(pres_code = str_extract(url, "[a-z]+(?=\\.html)"))
dt <- dt %>% group_by(President)

# Read in the webpage - wrap in list to nest in tbl
dt <- dt %>% mutate(XML = list(read_html(url)))
# Scrape all links from nodesets for each webpage
dt <- dt %>%  mutate(year_urls = list(XML[[1]] %>% 
                        html_nodes(xpath = '//*[@id="content"]/ul/li/a') %>% 
                        html_attr("href")))
# Attach domain to url, rewrap in list. Drop bad links, extract year
dt <- dt %>% mutate(year_urls = list(attach_domain(year_urls[[1]])))
dt <- dt %>% mutate(year_urls = list(drop_from_urls(year_urls[[1]])))
dt <- dt %>% mutate(years = list(as.numeric(str_extract(year_urls[[1]], "[0-9]{4}"))))
# Expand table for all years each president was in term
dt <- dt %>% select(pres_code, years, year_urls) %>% unnest() 
dt <- dt %>% group_by(year_urls)
# read each page to find all orders given per year and extract them
dt <- dt %>% mutate(XML = list(read_html(year_urls)))
dt <- dt %>% mutate(signings = list(XML[[1]] %>% 
                            html_nodes(xpath = '//*[@id="content"]/ul/li[1]') %>%
                            html_text()))
# Extract the date from the orders given text
dt <- dt %>% select(President, pres_code, years, signings) %>% 
  unnest() %>% 
  mutate(date = as.POSIXct(signings, format = "Signed: %B %d, %Y"))

# Clean up
dt = dt %>% ungroup %>% select(President, years, date)
  
# little index table to join season
dt_seasons = data.table(chr_season = c("winter", "spring", "summer", "autumn"))
dt_seasons[, season := 1:.N]
# Extract season
dt = dt %>% mutate(season = lubridate::quarter(date))
dt = dt %>% left_join(dt_seasons, by = "season") %>% 
  select(President, years, date, season = chr_season)
