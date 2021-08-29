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
  mutate(year = 2020) %>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))





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
  mutate(year = 2010) %>%
  mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood),
         Neighborhood = ifelse(grepl("Lakesh", Neighborhood), "Lakeshore/Lake Vista", Neighborhood),
         Neighborhood = ifelse(grepl("Marl", Neighborhood), "Marlyville/Fontainebleau", Neighborhood),
         Neighborhood = ifelse(grepl("New A", Neighborhood), "New Aurora/English Turn", Neighborhood),
         Neighborhood = ifelse(grepl("Tall", Neighborhood), "Tall Timbers/Brechtel", Neighborhood),
         Neighborhood = ifelse(grepl("Viav", Neighborhood), "Viavant/Venetian Isles", Neighborhood))

addtl_nbhd <- bind_rows(addtl_nbhd2010, addtl_nbhd2020)
# write_csv(addtl_nbhd, file = "outputs/Additional neighborhood data, 2020 census.csv")

addtl_nbhd_wide <- addtl_nbhd %>%
  pivot_longer(!c(Neighborhood, year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(year, vars), values_from = vals)
