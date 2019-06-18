library(sf)
library(tidyverse)
library(geojsonio)
library(lubridate)

# Bevingen downloaden van BevingGevoeld.nl
url <- "http://www.opengis.eu/geoservice/bevingen.json"
bevingen <- st_read(url, stringsAsFactors = FALSE) %>% 
             as.data.frame() %>%
             select(datum, locatie, magnitude = mag, diepte = depth, latitude = lat, longitude = lon) %>%
             mutate(jaar = year(datum)) 
write_csv(bevingen, "./data/bevingen.csv")

# Alle jaren sinds 1986
jaren <- data.frame(jaar = 1986:format(now(), "%Y"))

# Aantal bevingen per jaar sinds 1986
aantal_per_jaar <- bevingen %>%
                     count(jaar, name = "aantal")

# Gaswinning downloaden van BevingGevoeld.nl
url <- "https://bevinggevoeld.nl/gasbevingen/content/gaswinningpermaand.json"
gaswinning <- st_read(url, stringsAsFactors = FALSE) %>% 
                as.data.frame() %>%
                mutate(gaswinning = round(as.numeric(winningtotaal), 2),
                       jaar = as.integer(jaar)) %>%
                select(jaar, gaswinning) %>%
                filter(jaar >= 1986) 

# Eén dataset met gegevens per jaar
left_join(jaren, aantal_per_jaar, by = "jaar") %>%
  left_join(gaswinning, by = "jaar") %>%
  replace_na(list(aantal = 0, gaswinning = 0)) %>%
  write_csv("./data/gegevens-geaggregeerd-per-jaar.csv")

# Gasvelden downloaden van de site van de NAM
url <- "https://www.nlog.nl/arcgis/rest/services/nlog/gdw_ng_field_utm/MapServer/0/query?where=1=1&f=geojson"
st_read(url) %>% 
  filter(FIELD_NAME == 'Groningen') %>%
  topojson_write(file = "./data/groningen-veld.json", overwrite = TRUE)

# Objecten opruimen
rm(bevingen, jaren, aantal_per_jaar, url, gaswinning)
