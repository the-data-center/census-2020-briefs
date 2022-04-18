library(PL94171)
library(tidyverse)
library(readr)
library(xlsx)
library(janitor)
library(scales)
library(tidycensus)

npData <- read_csv("inputs/NP Max! data.csv")%>%  ### this file can be found here: https://github.com/the-data-center/np-max-plus/blob/main/Neighborhood%20Profiles/NP%20Max!%20data.csv
  rename(Neighborhood = geo) %>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood), ## you'll see this chunck of code mutating the nbhd names around -- they need to match in order to join year-to-year
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))

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

bg.la <- sf::st_read("inputs/tl_2020_22_bg/tl_2020_22_bg.shp") %>%
  filter(COUNTYFP == "071")

NOLAcrosswalk2020 <- read_csv("inputs/NOLAcrosswalk2020.csv")

pl_raw <- pl_read("https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/Louisiana/la2020.pl.zip")

pl_parish <- pl_subset(pl_raw, sumlev="050")

pl_tract <- pl_subset(pl_raw, sumlev="140")

pl_bg <- pl_subset(pl_raw, sumlev="150")

pl_std_parish <- pl_select_standard(pl_parish)
pl_std_tract <- pl_select_standard(pl_tract)

stJpop <- stJ2020 <- pl_parish %>%
  filter(GEOID == "22093") %>%
  select(GEOID, P0010001)

#### 2010 data
NOLAcrosswalk2010 <- read_csv("inputs/neighborhoodCrosswalk2010.csv") %>%
  mutate(tract = str_sub(GEOID, 6,))%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))


## can find the 2010 pl files here: https://www2.census.gov/programs-surveys/decennial/2010/data/01-Redistricting_File--PL_94-171/Louisiana/
##  you can try to download it using the r pkg form the url like with the 2020 file above, but for me that only downloaded 2020 data somehow...
pl_raw_2010 <- pl_read("inputs/la2010.pl")

pl_tract_2010 <- pl_subset(pl_raw_2010, sumlev="140")

pl_std_tract_2010 <- pl_select_standard(pl_tract_2010)

pl_parish_2010 <- pl_subset(pl_raw_2010, sumlev="050")

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
  full_join(pop2010, by = "TRACT") %>%
summarise(totPop = sum(P0010001), totArea = sum(areaTot), dens = totPop/totArea)

nbhdChildren_pop2010 <- pl_tract_2010 %>%
  filter(COUNTY == "071") %>%
  # right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select( TRACT, P0010001, AREALAND) %>%
  # group_by(Neighborhood) %>%
  mutate(areaTot = as.numeric(AREALAND)/ 2589988) %>%
  mutate(popTot = sum(`Population, 2020`),
         `Share of                    total,                2020` = percent(`Population, 2020`/popTot, accuracy = .01)) %>%
  adorn_totals("row")%>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` =
           round(`Population, 2020`/areaTot, digits = 0))



#### Mapping pieces
library(sf)
library(directlabels)
library(stringr)
library(grid)
library(scales)
library(tigris)
library(RColorBrewer)
tracts.la <- sf::st_read("inputs/tl_2010_22_tract10/tl_2010_22_tract10.shp")
nbhds2020 <- sf::st_read("inputs/Let_me_know_if_these_shape_files_work/NewOrleansNH2020a.shp") %>%
  mutate(Name2 = ifelse(grepl("Cath", Name2), "Lake Catherine/Village de L'est", Name2))

nbhds2010 <- sf::st_read("inputs/NO neighborhood boundaries shp/Neighborhood_2015_newer.shp")%>%
  mutate(NBHD_NAME = ifelse(NBHD_NAME == "Desire", "Desire Dev & Neighborhood", NBHD_NAME))

parishes_sf <- tigris::counties(state = "22", class = "sf")

Orleans.water_sf <- tigris::area_water("22", "Orleans Parish", class = "sf")
Jefferson.water_sf <- tigris::area_water("22", "Jefferson Parish", class = "sf")
# StCharles.water_sf <- tigris::area_water("22", "St. Charles Parish", class = "sf")
# otherwater.simple_sf <- sf::st_read(here("inputs/water/Otherwater_clipped_SimplifyP.shp"))
# wetlands.simple_sf <- sf::st_read(here("inputs/water/Wetlands2_Clip_SimplifyPolyg.shp"))

