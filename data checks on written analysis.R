### datachecking 

###  neighborhood change brief
censusComp <- nbhdChildren_pop_updated %>%
  mutate(check2000 = ifelse(as.numeric(`Population, 2020`)>= as.numeric(`Population,              2000`), "YES", "*")) %>%
  select(Neighborhood, check2000)

addtl_nbhd_wide <- addtl_nbhd %>%
  pivot_longer(!c(Neighborhood, year), names_to = "vars", values_to = "vals") %>%
  pivot_wider(names_from = c(year, vars), values_from = vals) %>%
  mutate(checkUnits = `2020_total_units` - `2010_total_units`)

unit_pop <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Total change, 2010-20`) %>%
  left_join(addtl_nbhd_wide) %>%
  filter(`Total change, 2010-20`<0) 

hisp_pop <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pctHisp10 = `2010_hispanic`/`Population,              2010`,
         pctHisp2020 = `2020_hispanic`/`Population, 2020`) %>%
  select(Neighborhood, pctHisp10, pctHisp2020)

black_pop <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pctBlk10 = `2010_black_not_hisp`/`Population,              2010`,
         pctBlk20 = `2020_black_not_hisp`/`Population, 2020`) %>%
  select(Neighborhood, pctBlk10, pctBlk20, `2010_black_not_hisp`, `2020_black_not_hisp`, `2010_white_not_hisp`, `2020_white_not_hisp`) %>%
  filter(pctBlk20 >= .6 & pctBlk20 <= .8)

black_pop_bel60 <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pctBlk10 = `2010_black_not_hisp`/`Population,              2010`,
         pctBlk20 = `2020_black_not_hisp`/`Population, 2020`) %>%
  select(Neighborhood, pctBlk10, pctBlk20, `2010_black_not_hisp`, `2020_black_not_hisp`, `2010_white_not_hisp`, `2020_white_not_hisp`, `2010_hispanic`, `2020_hispanic`) %>%
  filter(pctBlk20 <= .6)


white_pop_over60 <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pctWht10 = `2010_white_not_hisp`/`Population,              2010`,
         pctWht20 = `2020_white_not_hisp`/`Population, 2020`) %>%
  select(Neighborhood, pctWht10, pctWht20, `2010_black_not_hisp`, `2020_black_not_hisp`, `2010_white_not_hisp`, `2020_white_not_hisp`, `2010_hispanic`, `2020_hispanic`) %>%
  filter(pctWht20 <= .6 & pctWht10 >= .6)

two_pop <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pcttwo10 = `2010_two_or_more_not_hisp`/`Population,              2010`,
         pcttwo20 = `2020_two_or_more_not_hisp`/`Population, 2020`) %>%
  select(Neighborhood, pcttwo10, pcttwo20, `2010_two_or_more_not_hisp`, `2020_two_or_more_not_hisp`, 
         `2010_black_not_hisp`, `2020_black_not_hisp`, `2010_white_not_hisp`, `2020_white_not_hisp`, `2010_hispanic`, `2020_hispanic`) %>%
  filter(pcttwo20 >=.04)

asian_pop <- nbhdChildren_pop_updated %>%
  select(Neighborhood, `Population,              2010`, `Population, 2020`) %>%
  left_join(addtl_nbhd_wide) %>%
  mutate(pctasian10 = `2010_asian_pacific_isl_not_hisp`/`Population,              2010`,
         pctasian20 = `2020_asian_pacific_isl_not_hisp`/`Population, 2020`) %>%
  select(Neighborhood, pctasian10, pctasian20, `2010_asian_pacific_isl_not_hisp`, `2020_asian_pacific_isl_not_hisp`, 
         `2010_black_not_hisp`, `2020_black_not_hisp`, `2010_white_not_hisp`, `2020_white_not_hisp`, `2010_hispanic`, `2020_hispanic`) %>%
  filter(pctasian20 >=.04)
