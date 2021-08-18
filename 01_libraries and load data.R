library(PL94171)
library(tidyverse)
library(readr)
library(xlsx)
library(janitor)
library(scales)
library(tidycensus)

Parish <- c("Jefferson",              
                  "Orleans",                 
                  "Plaquemines",            
                  "St. Bernard",             
                  "St. Charles",
                  "St. James",            
                  "St. John the Baptist",    
                  "St. Tammany")
GEOID <-c("22051", "22071", "22075", "22087", "22089", "22093", "22095", "22103")  
parish_xwalk <- data.frame(Parish, GEOID)

NOLAcrosswalk2020 <- read_csv("inputs/NOLAcrosswalk2020.csv")

pl_raw <- pl_read("https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/Louisiana/la2020.pl.zip")

pl_parish <- pl_subset(pl_raw, sumlev="050")

pl_tract <- pl_subset(pl_raw, sumlev="140")

pl_std_parish <- pl_select_standard(pl_parish)
pl_std_tract <- pl_select_standard(pl_tract)

stJpop <- stJ2020 <- pl_parish %>%
  filter(GEOID == "22093") %>%
  select(GEOID, P0010001)

library(tigris)
pop2010 <- tigris::tracts(year = 2010, state = "LA", county = "071", cb = TRUE, class = "sf", progress_bar = FALSE) %>%
  rename(area = CENSUSAREA) %>%
  mutate(GEOID = substr(GEO_ID, 10, 21)) %>%
  select(GEOID, area) %>%
  mutate(TRACT = str_sub(GEOID,-6))

nbhdChildren_pop2020 <- pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select(Neighborhood = geo, TRACT, P0010001, AREALAND) %>%
  # group_by(Neighborhood) %>%
  mutate(areaTot = AREALAND/ 2589988) %>%
  full_join(pop2010, by = "TRACT")
summarise(totPop = sum(P0010001), totArea = sum(areaTot), dens = totPop/totArea)

nbhdChildren_pop2010 <- pl_tract10 %>%
  filter(COUNTY == "071") %>%
  # right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select( TRACT, P0010001, AREALAND) %>%
  # group_by(Neighborhood) %>%
  mutate(areaTot = AREALAND/ 2589988) %>%
  mutate(popTot = sum(`Population, 2020`),
         `Share of                    total,                2020` = percent(`Population, 2020`/popTot, accuracy = .01)) %>%
  adorn_totals("row")%>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` =
           round(`Population, 2020`/areaTot, digits = 0))
