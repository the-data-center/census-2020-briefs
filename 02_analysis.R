#### BRIEF 1 ####
### TOTAL POP & GROUP QUARTERS ###

## old data
hhGq_parish_old <- read_csv("inputs/census2020briefs_data_HHsize_GQ - parish.csv")

### tab1_parish

hhGq_parish2020 <- pl_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, P0010001, H0010002, contains("P005")) %>%
  rename(Population = P0010001,
         `Occupied \nunits` = H0010002,
         `Total group quarters population` =P0050001,
         `Total institutional`= P0050002,
         `Adult correctional\nfacilties`=P0050003,
         `Juvenile \nfacilities`= P0050004,
         `Nursing \nfacilities`= P0050005,
         `Other institutional`= P0050006,
         `Total noninstitutional` = P0050007,
         `College/\nuniversity student housing` = P0050008,
         `Military\nquarters`=P0050009,
         `Other  noninstitutional` = P0050010,) %>%
  # mutate_all(as.character) %>%
  mutate(Year = 2020,
         `Average household size` = round(((Population - `Total group quarters population`) / `Occupied \nunits`), 
                                          digits = 2))

hhGq_metro2020 <- hhGq_parish2020 %>%
  summarise(Parish = "New Orleans Metro Total",
            Population= sum(Population),
            `Occupied \nunits` = sum(`Occupied \nunits`),
            `Total group quarters population` = sum(`Total group quarters population`),
            `Total institutional` = sum(`Total institutional`),
            `Adult correctional\nfacilties` = sum(`Adult correctional\nfacilties`),
            `Juvenile \nfacilities` = sum(`Juvenile \nfacilities`),
            `Nursing \nfacilities` = sum(`Nursing \nfacilities`),
              `Other institutional` = sum(`Other institutional`),
              `Total noninstitutional` = sum(`Total noninstitutional`),
            `College/\nuniversity student housing` = sum(`College/\nuniversity student housing`),
            `Military\nquarters` = sum(`Military\nquarters`),
            `Other  noninstitutional` = sum(`Other  noninstitutional`))%>%
  mutate(Year = 2020,
         `Average household size` = round(((Population - `Total group quarters population`) / `Occupied \nunits`), 
                                          digits = 2))

hhGq_parish_updated <- bind_rows(mutate_all(hhGq_parish_old, as.character), mutate_all(hhGq_parish2020, as.character), mutate_all(hhGq_metro2020, as.character)) %>%
  select(-GEOID)

# write_csv(hhGq_parish_updated,"outputs/hhGq_parish_updated.csv")

### tab2_nbhd
hhGq_nbhd2020 <- pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract")) %>%
  select(geo, P0010001, H0010002, contains("P005")) %>%
  rename(Neighborhood = geo,
         Population = P0010001,
         `Occupied \nunits` = H0010002,
         `Total group quarters population` =P0050001,
         `Total institutional`= P0050002,
         `Adult correctional\nfacilties`=P0050003,
         `Juvenile \nfacilities`= P0050004,
         `Nursing \nfacilities`= P0050005,
         `Other institutional`= P0050006,
         `Total noninstitutional` = P0050007,
         `College/\nuniversity student housing` = P0050008,
         `Military\nquarters`=P0050009,
         `Other  noninstitutional` = P0050010) %>%
    group_by(Neighborhood) %>%
    summarise(Population= sum(Population),
              `Occupied \nunits` = sum(`Occupied \nunits`),
              `Total group quarters population` = sum(`Total group quarters population`),
              `Total institutional` = sum(`Total institutional`),
              `Adult correctional\nfacilties` = sum(`Adult correctional\nfacilties`),
              `Juvenile \nfacilities` = sum(`Juvenile \nfacilities`),
              `Nursing \nfacilities` = sum(`Nursing \nfacilities`),
              `Other institutional` = sum(`Other institutional`),
              `Total noninstitutional` = sum(`Total noninstitutional`),
              `College/\nuniversity student housing` = sum(`College/\nuniversity student housing`),
              `Military\nquarters` = sum(`Military\nquarters`),
              `Other  noninstitutional` = sum(`Other  noninstitutional`)) %>%
  mutate(`Average household size` = round(((Population - `Total group quarters population`) / `Occupied \nunits`), 
                                          digits = 2)) %>%
  adorn_totals("row")


# write_csv(hhGq_nbhd2020,"outputs/hhGq_nbhd_updated.csv")

#### BRIEF 2 ###
## children in neighborhoods ##

### old data
nbhdChildren_pop_old <- read_csv("inputs/census2020briefs_nbhdChildren - population.csv") %>%
  mutate(Neighborhood = gsub(pattern =  "[*]", replacement = "", x = Neighborhood))%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))
nbhdChildren_chil_old <- read_csv("inputs/census2020briefs_nbhdChildren - children.csv") %>%
  mutate(Neighborhood = gsub(pattern =  "[*]", replacement = "", x = Neighborhood))%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))
nbhdChildren_adu_old <- read_csv("inputs/census2020briefs_nbhdChildren - adults.csv") %>%
  mutate(Neighborhood = gsub(pattern =  "[*]", replacement = "", x = Neighborhood))%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))


### tab 1 - population
nbhdChildren_pop2020 <- pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select(Neighborhood = geo, P0010001, AREALAND) %>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood)) %>%
  group_by(Neighborhood) %>%
  summarise(`Population, 2020` = sum(P0010001), areaTot = sum(AREALAND)/ 2589988) %>%
  mutate(popTot = sum(`Population, 2020`),
         `Share of                    total,                2020` = percent(`Population, 2020`/popTot, accuracy = .01)) %>%
  adorn_totals("row")%>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` =
           round(`Population, 2020`/areaTot, digits = 0)) %>%
  select(-areaTot, -popTot)

nbhdChildren_pop_updated <- inner_join(nbhdChildren_pop_old, nbhdChildren_pop2020) %>%
  mutate(`Total change, 2010-20` = `Population, 2020` - `Population,              2010`,
         `Percent change, 2010-20` = (`Population, 2020` - `Population,              2010`)/`Population,              2010`)

pop2020 <- nbhdChildren_pop_updated %>% select(`Population, 2020`)

# write_csv(nbhdChildren_pop_updated,"outputs/nbhdChildren_pop_updated.csv")
### tab 3 - adults
nbhdChildren_adu2020 <- pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select(Neighborhood = geo, P0030001, AREALAND)%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood)) %>%
  group_by(Neighborhood) %>%
  summarise(`Adults, 2020` = sum(P0030001), areaTot = sum(AREALAND)/ 2589988) %>%
  mutate(popTot = sum(`Adults, 2020`),
         `Share of                    total,                2020` = percent(`Adults, 2020`/popTot, accuracy = .01)) %>%
  adorn_totals("row")%>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` =
           round(`Adults, 2020`/areaTot, digits = 0)) %>%
  select(-areaTot, -popTot)

nbhdChildren_adu_updated <- full_join(nbhdChildren_adu_old, nbhdChildren_adu2020) %>%
  mutate(`Total change, 2010-20` = `Adults, 2020` - `Adults,              2010`,
         `Percent change, 2010-20` = percent((`Adults, 2020` - `Adults,              2010`)/`Adults,              2010`),
         `Percent Adults, 2020` = percent(`Adults, 2020`/ pop2020$`Population, 2020`))

# write_csv(nbhdChildren_adu_updated,"outputs/nbhdChildren_adu_updated.csv")
### tab 2 - children
nbhdChildren_adu2020.inclAreaTot <-pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract"))  %>%
  select(Neighborhood = geo, P0030001, AREALAND)%>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood)) %>%
  group_by(Neighborhood) %>%
  summarise(`Adults, 2020` = sum(P0030001), areaTot = sum(AREALAND)/ 2589988) %>%
  mutate(popTot = sum(`Adults, 2020`),
         `Share of                    total,                2020` = percent(`Adults, 2020`/popTot, accuracy = .01)) %>%
  adorn_totals("row")%>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` =
           round(`Adults, 2020`/areaTot, digits = 0)) 


nbhdChildren_chil2020 <- left_join(nbhdChildren_pop2020, nbhdChildren_adu2020.inclAreaTot, by = "Neighborhood") %>%
  transmute(Neighborhood = Neighborhood,
            `Children, 2020` = `Population, 2020` - `Adults, 2020`,
            areaTot = areaTot) %>%
  mutate(`Density per                       sq mi of                       developed land,                            2020` = round(`Children, 2020`/areaTot, digits = 0),
         popTot = sum(`Children, 2020`)) %>%
  mutate(`Share of                    total,                2020` = percent(`Children, 2020`/popTot, accuracy = .01)) %>%
  select(-areaTot, -popTot)

nbhdChildren_chil_updated <- left_join(nbhdChildren_chil_old, nbhdChildren_chil2020) %>%
  mutate(`Total change, 2010-20` = `Children, 2020` - `Children,              2010`,
         `Percent change, 2010-20` = percent((`Children, 2020` - `Children,              2010`)/`Children,              2010`),
         `Percent Children, 2020` = percent(`Children, 2020`/ pop2020$`Population, 2020`))

# write_csv(nbhdChildren_chil_updated,"outputs/nbhdChildren_chil_updated.csv")


### BRIEF 3 ###
## population & race
metro_pop_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - pop.csv")
metro_black_nh_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - black_nh.csv")%>%
  rename(Parish = Parishes)
metro_white_nh_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - white_nh.csv")%>%
  rename(Parish = Parishes)
metro_other_nh_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - other_nh.csv")%>%
  rename(Parish = Parishes)
metro_api_nh_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - api_nh.csv")%>%
  rename(Parish = Parishes)
metro_hisp_old <- read_csv("inputs/census2020briefs_popHousRaceMetro - hisp.csv")%>%
  rename(Parish = Parishes)

metro_pop <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, pop)%>%
  adorn_totals()%>%
  mutate(Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

###pop
stJ_racePop <- get_decennial(geography = "county",
                             state = "LA",
                             county = "093",
                             variables = "P001001",
                             year = 2010)
metro_pop_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, Population = pop) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_pop_2020 %>%
  filter(Parish == "New Orleans Metro Total") 

metro_pop_updated <- bind_rows(mutate_all(metro_pop_old, as.character), 
                                    mutate_all(metro_pop_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_Population` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_Population`)) %>%
  mutate(tot = totRace$Population) %>%
  mutate(`2020_Share of total` = as.numeric(`2020_Population`)/tot,
         `2020_Total change from previous decade` = as.numeric(`2020_Population`) - as.numeric(`2010_Population`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Population`)) %>%
  select(-tot) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(metro_pop_updated, "outputs/metro_pop_updated.csv")

###black
stJ_racePop <- get_decennial(geography = "county",
                              state = "LA",
                              county = "093",
                              variables = "P005004",
                             year = 2010)
metro_black_nh_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, `Black or African American Alone, Not Hispanic` = pop_black) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_black_nh_2020 %>%
  filter(Parish == "New Orleans Metro Total") 
         
metro_black_nh_updated <- bind_rows(mutate_all(metro_black_nh_old, as.character), 
                                    mutate_all(metro_black_nh_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_Black or African American Alone, Not Hispanic` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_Black or African American Alone, Not Hispanic`)) %>%
  mutate(tot = totRace$`Black or African American Alone, Not Hispanic`) %>%
  left_join(metro_pop, by = "Parish") %>%
  mutate(`2020_Share of total` = as.numeric(`2020_Black or African American Alone, Not Hispanic`)/tot,
         `2020_Percent  Black or African American Alone, Not Hispanic` = as.numeric(`2020_Black or African American Alone, Not Hispanic`)/pop,
         `2020_Total change from previous decade` = as.numeric(`2020_Black or African American Alone, Not Hispanic`) - as.numeric(`2010_Black or African American Alone, Not Hispanic`),
        `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Black or African American Alone, Not Hispanic`)) %>%
  select(-tot, -pop, -GEOID) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6, )) %>%
  pivot_wider(names_from = vars, values_from = vals)
  
# write_csv(metro_black_nh_updated, "outputs/metro_black_nh_updated.csv")

###white
stJ_racePop <- get_decennial(geography = "county",
                             state = "LA",
                             county = "093",
                             variables = "P005003")
metro_white_nh_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, `White Alone, Not Hispanic` = pop_white) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_white_nh_2020 %>%
  filter(Parish == "New Orleans Metro Total") 

metro_white_nh_updated <- bind_rows(mutate_all(metro_white_nh_old, as.character), 
                                    mutate_all(metro_white_nh_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_White Alone, Not Hispanic` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_White Alone, Not Hispanic`)) %>%
  mutate(tot = totRace$`White Alone, Not Hispanic`) %>%
  left_join(metro_pop, by = "Parish") %>%
  mutate(`2020_Share of total` = as.numeric(`2020_White Alone, Not Hispanic`)/tot,
         `2020_Percent         White Alone, Not Hispanic` = as.numeric(`2020_White Alone, Not Hispanic`)/pop,
         `2020_Total change from previous decade` = as.numeric(`2020_White Alone, Not Hispanic`) - as.numeric(`2010_White Alone, Not Hispanic`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_White Alone, Not Hispanic`)) %>%
  select(-tot, -pop, -GEOID) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(metro_white_nh_updated, "outputs/metro_white_nh_updated.csv")

###api
stJ_racePop <- get_decennial(geography = "county",
                             state = "LA",
                             county = "093",
                             variables = c("P005006", "P005007")) %>%
  summarise(value = sum(value))
metro_api_nh_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, pop_nhpi, pop_asian) %>%
  mutate(`Asian or Pacific Islander, Not Hispanic` = pop_nhpi + pop_asian) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_api_nh_2020 %>%
  filter(Parish == "New Orleans Metro Total") 

metro_api_nh_updated <- bind_rows(mutate_all(metro_api_nh_old, as.character), 
                                    mutate_all(metro_api_nh_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_Asian or Pacific Islander, Not Hispanic` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_Asian or Pacific Islander, Not Hispanic`)) %>%
  mutate(tot = totRace$`Asian or Pacific Islander, Not Hispanic`) %>%
  left_join(metro_pop, by = "Parish") %>%
  mutate(`2020_Share of total` = as.numeric(`2020_Asian or Pacific Islander, Not Hispanic`)/tot,
         `2020_Percent Asian or Pacific Islander, Not Hispanic` = as.numeric(`2020_Asian or Pacific Islander, Not Hispanic`)/pop,
         `2020_Total change from previous decade` = as.numeric(`2020_Asian or Pacific Islander, Not Hispanic`) - as.numeric(`2010_Asian or Pacific Islander, Not Hispanic`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Asian or Pacific Islander, Not Hispanic`)) %>%
  select(-tot, -pop, -GEOID) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(metro_api_nh_updated, "outputs/metro_api_nh_updated.csv")


###other
stJ_racePop <- get_decennial(geography = "county",
                             state = "LA",
                             county = "093",
                             variables = c("P005005", "P005008", "P005009")) %>%
  summarise(value = sum(value))
metro_other_nh_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, pop_other, pop_aian, pop_two) %>%
  mutate(`Other, Not Hispanic` = pop_other + pop_aian + pop_two) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_other_nh_2020 %>%
  filter(Parish == "New Orleans Metro Total") 

metro_other_nh_updated <- bind_rows(mutate_all(metro_other_nh_old, as.character), 
                                  mutate_all(metro_other_nh_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_Other, Not Hispanic` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_Other, Not Hispanic`)) %>%
  mutate(tot = totRace$`Other, Not Hispanic`) %>%
  left_join(metro_pop, by = "Parish") %>%
  mutate(`2020_Share of total` = as.numeric(`2020_Other, Not Hispanic`)/tot,
         `2020_Percent         Other, Not Hispanic` = as.numeric(`2020_Other, Not Hispanic`)/pop,
         `2020_Total change from previous decade` = as.numeric(`2020_Other, Not Hispanic`) - as.numeric(`2010_Other, Not Hispanic`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Other, Not Hispanic`)) %>%
  select(-tot, -pop, -GEOID) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(metro_other_nh_updated, "outputs/metro_other_nh_updated.csv")

###hisp
stJ_racePop <- get_decennial(geography = "county",
                             state = "LA",
                             county = "093",
                             variables = "P004003")
metro_hisp_2020 <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(GEOID, Parish, `Hispanic (Any Race)` = pop_hisp) %>%
  adorn_totals() %>%
  select(-GEOID) %>%
  mutate(Year = "2020",
         Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish))

totRace <- metro_hisp_2020 %>%
  filter(Parish == "New Orleans Metro Total") 

metro_hisp_updated <- bind_rows(mutate_all(metro_hisp_old, as.character), 
                                    mutate_all(metro_hisp_2020, as.character)) %>%
  pivot_longer(!c(Parish, Year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(Year, vars), values_from = vals) %>%
  mutate(`2010_Hispanic (Any Race)` = ifelse(Parish == "St. James", stJ_racePop$value, `2010_Hispanic (Any Race)`)) %>%
  mutate(tot = totRace$`Hispanic (Any Race)`) %>%
  left_join(metro_pop, by = "Parish") %>%
  mutate(`2020_Share of total` = as.numeric(`2020_Hispanic (Any Race)`)/tot,
         `2020_Percent Hispanic (Any Race)` = as.numeric(`2020_Hispanic (Any Race)`)/pop,
         `2020_Total change from previous decade` = as.numeric(`2020_Hispanic (Any Race)`) - as.numeric(`2010_Hispanic (Any Race)`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Hispanic (Any Race)`)) %>%
  select(-tot, -pop, -GEOID) %>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(metro_hisp_updated, "outputs/metro_hisp_updated.csv")


###two or more

pop2010 <- metro_pop_updated %>% 
  filter(Year == 2010) %>% 
  select(Parish, Population)

two_nh_2010_raw <- get_decennial(geography = "county",
                             state = "LA",
                             county = c("051", "071", "075", "087", "089", "093", "095", "103") ,
                             variables = "P005009") %>%
  transmute(Parish = str_sub(NAME, 1, -19),
            `2010_Two or More Races, Not Hispanic` = value) %>%
  adorn_totals() %>% 
  mutate(Parish = ifelse(Parish =="Total", "New Orleans Metro Total", Parish)) 

totRace <- two_nh_2010_raw %>%
  filter(Parish == "New Orleans Metro Total") 

two_nh_2010 <- two_nh_2010_raw %>%
  left_join(pop2010, by = "Parish") %>%
  mutate(tot = totRace$`2010_Two or More Races, Not Hispanic`) %>%
  mutate(Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish),
         `2010_Share of total` = as.numeric(`2010_Two or More Races, Not Hispanic`)/tot,
         `2010_Percent  Two or More Races, Not Hispanic` = as.numeric(`2010_Two or More Races, Not Hispanic`)/as.numeric(Population)) %>%
  select(-tot)

two_nh_2020_raw <- pl_std_parish %>%
  right_join(parish_xwalk, by = "GEOID") %>%
  select(Parish, `2020_Two or More Races, Not Hispanic` = pop_two) %>%
  adorn_totals()  %>% 
  mutate(Parish = ifelse(Parish =="Total", "New Orleans Metro Total", Parish)) 

totRace <- two_nh_2020_raw %>%
  filter(Parish == "New Orleans Metro Total")

two_nh_2020 <- two_nh_2020_raw %>%
  mutate(tot = totRace$`2020_Two or More Races, Not Hispanic`) %>%
  mutate(Parish = ifelse(Parish =="-", "New Orleans Metro Total", Parish),
         `2020_Share of total` = as.numeric(`2020_Two or More Races, Not Hispanic`)/tot,
         `2020_Percent  Two or More Races, Not Hispanic` = as.numeric(`2020_Two or More Races, Not Hispanic`)/metro_pop$pop) %>%
  select(-tot)

two_nh_updated <- left_join(two_nh_2010, two_nh_2020, by = "Parish") %>%
  select(-Population) %>%
  mutate(`2020_Total change from previous decade` = as.numeric(`2020_Two or More Races, Not Hispanic`) - as.numeric(`2010_Two or More Races, Not Hispanic`),
         `2020_Percent change from previous decade` = `2020_Total change from previous decade`/as.numeric(`2010_Two or More Races, Not Hispanic`))%>%
  mutate_all(as.character) %>%
  pivot_longer(!c(Parish), names_to = "vars", values_to = "vals") %>%
  mutate(Year = str_sub(vars,1,4), vars = str_sub(vars, 6,)) %>%
  pivot_wider(names_from = vars, values_from = vals)

# write_csv(two_nh_updated, "outputs/metro_two_nh_2020.csv")
