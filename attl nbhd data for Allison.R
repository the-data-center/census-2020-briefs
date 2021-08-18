housing_nbhd2020 <- pl_tract %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2020, by  = c("TRACT" = "tract")) %>%
  select(Neighborhood = geo, H0010001, H0010002, H0010003) %>%
  group_by(Neighborhood) %>%
  summarise(total_units = sum(H0010001),
            occupied_units = sum(H0010002),
            vacant_units = sum(H0010003))

race_nbhd2020 <- pl_std_tract %>%
  filter(county == "071") %>%
  mutate(tract = str_sub(GEOID, 6,)) %>%
  right_join(NOLAcrosswalk2020, by  = c("tract")) %>%
  rename(Neighborhood = geo) %>%
  group_by(Neighborhood) %>%
  summarise(white_not_hisp = sum(pop_white),
            black_not_hisp = sum(pop_black),
            hispanic = sum(pop_hisp),
            asian_pacific_isl_not_hisp = sum(pop_asian) + sum(pop_nhpi),
            two_or_more_not_hisp = sum(pop_two),
            other_not_hisp = sum(pop_aian) + sum(pop_other))

addtl_nbhd2020 <- full_join(housing_nbhd2020, race_nbhd2020, by = "Neighborhood") %>%
  mutate(year = 2020)



#### 2010 data
NOLAcrosswalk2010 <- read_csv("inputs/neighborhoodCrosswalk2010.csv") %>%
  mutate(tract = str_sub(GEOID, 6,))
  

pl_raw_2010 <- pl_read("inputs/la2010.pl")

pl_tract_2010 <- pl_subset(pl_raw_2010, sumlev="140")

pl_std_tract_2010 <- pl_select_standard(pl_tract_2010)

housing_nbhd2010 <- pl_tract_2010 %>%
  filter(COUNTY == "071") %>%
  right_join(NOLAcrosswalk2010, by  = c("TRACT" = "tract")) %>%
  select(Neighborhood, H0010001, H0010002, H0010003) %>%
  group_by(Neighborhood) %>%
  summarise(total_units = sum(H0010001),
            occupied_units = sum(H0010002),
            vacant_units = sum(H0010003))

race_nbhd2010 <- pl_std_tract_2010 %>%
  filter(county == "071") %>%
  mutate(tract = str_sub(GEOID, 6,)) %>%
  right_join(NOLAcrosswalk2010, by  = c("tract")) %>%
  group_by(Neighborhood) %>%
  summarise(white_not_hisp = sum(pop_white),
            black_not_hisp = sum(pop_black),
            hispanic = sum(pop_hisp),
            asian_pacific_isl_not_hisp = sum(pop_asian) + sum(pop_nhpi),
            two_or_more_not_hisp = sum(pop_two),
            other_not_hisp = sum(pop_aian) + sum(pop_other))

addtl_nbhd2010 <- full_join(housing_nbhd2010, race_nbhd2010, by = "Neighborhood") %>%
  mutate(year = 2010)

addtl_nbhd <- bind_rows(addtl_nbhd2010, addtl_nbhd2020)
write_csv(addtl_nbhd, file = "outputs/Additional neighborhood data, 2020 census.csv")
