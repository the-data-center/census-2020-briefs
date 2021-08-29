
## creating data points for the mapping
nbhdPop2000 <- npData %>%
  filter(year == "2000",
         meas == "pop") %>%
  select(Neighborhood, val) %>%
  mutate(pop2000 = as.numeric(gsub(",","",val)))

nbhdChildren_chil_updated_names <- nbhdChildren_chil_updated  %>%
  select(Neighborhood, `Total change, 2010-20`) 

## data points for maps
addtl_nbhd_maps <- addtl_nbhd_wide %>%
  left_join(nbhdPop2000, by = "Neighborhood") %>%
  left_join(nbhdChildren_pop_updated %>% select(Neighborhood, `Population, 2020`, `Population,              2010`), by = "Neighborhood") %>%
  left_join((nbhdChildren_chil_updated  %>%
               select(Neighborhood, chil10_20 = `Total change, 2010-20`)), by = "Neighborhood") %>%
  mutate(popChange00_20 = `Population, 2020` - pop2000,
         pctHisp10 = `2010_hispanic`/`Population,              2010`,
         pctHisp2020 = `2020_hispanic`/`Population, 2020`,
         pctBlk10 = `2010_black_not_hisp`/`Population,              2010`,
         pctBlk20 = `2020_black_not_hisp`/`Population, 2020`,
         pctWht10 = `2010_white_not_hisp`/`Population,              2010`,
         pctWht20 = `2020_white_not_hisp`/`Population, 2020`,
         pcttwo20 = `2020_two_or_more_not_hisp`/`Population, 2020`,
         pctasian10 = `2010_asian_pacific_isl_not_hisp`/`Population,              2010`,
         pctasian20 = `2020_asian_pacific_isl_not_hisp`/`Population, 2020`)

## join shapefile and data
tomap <- nbhds2020 %>%
  left_join(addtl_nbhd_maps, by = c("Name2" = "Neighborhood"))


# make map
fig <- tomap %>%
  ggplot() +
  geom_sf(aes(fill = popChange00_20), color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(Jefferson.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = "gray90", color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "PiYG") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0)) + 
  labs(title = "Is it a new neighborhood?",
    fill = "")+
  ggsave(filename = "outputs/newNbhds test.pdf", device = cairo_pdf, width = 8, height = 5)

fig
