nbhds2020%>%
  filter(Name2 != "Z-no land mass-all Lake") %>%
  mutate(lat = as.numeric(INTPTLAT), 
         lon = as.numeric(INTPTLON)*-1) %>%
  ggplot() +
  geom_sf(aes(fill = COUNTYFP), color = DCcolor.p1grayblue80, fill = DCcolor.p2teal50, size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  # geom_text(data = (nbhds2020 %>%
  #                     mutate(lat = as.numeric(INTPTLAT), 
  #                            lon = as.numeric(INTPTLON)*-1)),
  #           aes(label = Name2, x = lon, y = lat)) +
  # geom_sf_text_repel(aes(label = stringr::str_wrap(Name2, 9)), size = 1.2) +
  
  ggrepel::geom_text_repel(aes(label=stringr::str_wrap(Name2,12), geometry = geometry), 
                           stat = "sf_coordinates", size =1.7,  
                           point.padding = NA, box.padding = 0, label.padding = 0,
                           arrow = arrow(length = unit(0.01, "npc")),
                           max.overlaps = Inf,
                           force = 0.01,
                           force_pull = 0)+
  coord_sf(xlim = c(-90.14, -89.92), ylim = c(29.89, 30.06), expand = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "New Orleans neighborhoods 2020",
       fill = "",
       caption = "Source: The Data Center, U.S. Census Bureau.")+
  ggsave(filename = "outputs/nbhd_reference", device = "eps")


## creating data points for the mapping
nbhdPop2000 <- npData %>%
  filter(year == "2000",
         meas == "pop") %>%
  select(Neighborhood, val) %>%
  mutate(pop2000 = as.numeric(gsub(",","",val)))

nbhdChildren_chil_updated_names <- nbhdChildren_chil_updated  %>%
  select(Neighborhood, `Total change, 2010-20`, `Percent Children, 2020`) 

## data points for maps
addtl_nbhd_maps <- addtl_nbhd_wide %>%
  left_join(nbhdPop2000, by = "Neighborhood") %>%
  left_join(nbhdChildren_pop_updated %>% select(Neighborhood, `Population, 2020`, `Population,              2010`), by = "Neighborhood") %>%
  left_join((nbhdChildren_chil_updated  %>%
               select(Neighborhood, 
                      chil10_20 = `Total change, 2010-20`, 
                      chil20 = `Children, 2020`)), 
            by = "Neighborhood") %>%
  mutate(pctChange00_20 = `Population, 2020`/pop2000,
         popChange10_20 = `Population, 2020` - `Population,              2010`,
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
  left_join(addtl_nbhd_maps, by = c("Name2" = "Neighborhood")) %>%
  filter(Name2 != "Z-no land mass-all Lake")

tomap.summary <- summary(tomap)

tomap2010 <- nbhds2010 %>%
  left_join((addtl_nbhd_maps %>%
              mutate(Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine", Neighborhood))), 
            by = c("NBHD_NAME" = "Neighborhood"))%>%
  filter(NBHD_NAME != "Z-no land mass-all Lake")


### PCT CHANGE 00 - 20 ###

tomap$pctChange00_20.brks <- cut(tomap$pctChange00_20, 
                                 include.lowest = TRUE,
                                 breaks=c(0, .5, .7, 1,2.5), 
                                 labels=c("< 50% of 2000 population", "50-70% of 2000 population", "70-100% of 2000 population", 
                                          "> 100% of 2000 population"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pctChange00_20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.7,.19),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Change in population, 2000 - 2020",
       subtitle = "2020 population as a percent of 2000 population",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2000 Census, 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctChange00_20.pdf", device = cairo_pdf, width = 8, height = 5)


### POP CHANGE 10 - 20 ###
tomap$popChange10_20.brks <- cut(tomap$popChange10_20, 
                                 include.lowest = TRUE,
                   breaks=c(-1400, 0, 1400, 2000,4800), 
                   labels=c("< 0", "0 - 1,400", "1,400 - 2,000", 
                            "2,000 - 4,800"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = popChange10_20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Change in population, 2010 - 2020",
    fill = "",
    caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census, 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/popChange10_20.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT HISP 10  ###
tomap2010$pctHisp10.brks <- cut(tomap2010$pctHisp10, 
                                 include.lowest = TRUE,
                                 breaks=c(0, .02, .05, .1,1), 
                                 labels=c("0-2%", "2-5%", "5-10%", ">10%"))
# make map
tomap2010 %>%
  ggplot() +
  geom_sf(aes(fill = pctHisp10.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Hispanic, 2010",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctHisp10.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT HISP 20  ###
tomap$pctHisp2020.brks <- cut(tomap$pctHisp2020, 
                            include.lowest = TRUE,
                            breaks=c(0, .02, .05, .1, .18, 1), 
                            labels=c("0-2%", "2-5%", "5-10%", "10-18%", ">18%"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pctHisp2020.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Hispanic, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctHisp2020.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT BLACK 10  ###
tomap2010$pctBlk10.brks <- cut(tomap2010$pctBlk10, 
                            include.lowest = TRUE,
                            breaks=c(0, .2, .6, .8,1), 
                            labels=c("0-20%", "20-60%", "60-80%", ">80%"))
# make map
tomap2010 %>%
  ggplot() +
  geom_sf(aes(fill = pctBlk10.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Black, 2010",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctBlk10.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT BLACK 20  ###
tomap$pctBlk20.brks <- cut(tomap$pctBlk20, 
                           include.lowest = TRUE,
                           breaks=c(0, .2, .6, .8,1), 
                           labels=c("0-20%", "20-60%", "60-80%", ">80%"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pctBlk20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Black, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctBlk20.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT WHITE  10  ###
tomap2010$pctWht10.brks <- cut(tomap2010$pctWht10, 
                           include.lowest = TRUE,
                           breaks=c(0, .2, .4, .6,1), 
                           labels=c("0-20%", "20-40%", "40-60%", ">60%"))
# make map
tomap2010 %>%
  ggplot() +
  geom_sf(aes(fill = pctWht10.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is White, 2010",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctWht10.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT WHITE 20  ###
tomap$pctWht20.brks <- cut(tomap$pctWht20, 
                           include.lowest = TRUE,
                           breaks=c(0, .2, .4, .6,1), 
                           labels=c("0-20%", "20-40%", "40-60%", ">60%"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pctWht20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is White, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctWht20.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT ASIAN 10  ###
tomap2010$pctasian10.brks <- cut(tomap2010$pctasian10, 
                           include.lowest = TRUE,
                           breaks=c(0, .03, .1, .9,1), 
                           labels=c("0-3%", "3-10%", "10-90%", ">90%"))
# make map
tomap2010 %>%
  ggplot() +
  geom_sf(aes(fill = pctasian10.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Asian, 2010",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctasian10.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT ASIAN 20  ###
tomap$pctasian20.brks <- cut(tomap$pctasian20, 
                           include.lowest = TRUE,
                           breaks=c(0, .03, .1, .9,1), 
                           labels=c("0-3%", "3-10%", "10-90%", ">90%"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pctasian20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is Asian, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pctasian20.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT TWO+ 20  ###
tomap$pcttwo20.brks <- cut(tomap$pcttwo20, 
                             include.lowest = TRUE,
                             breaks=c(0, .04, .05, 1), 
                             labels=c("0-4%", "4-5%", ">5%"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = pcttwo20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Percent of population that is two or more races, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pcttwo20.pdf", device = cairo_pdf, width = 8, height = 5)


### CHILDREN POP CHANGE 10 - 20 ###
tomap$chil10_20.brks <- cut(tomap$chil10_20, 
                                 include.lowest = TRUE,
                                 breaks=c(-725, -150, 0, 300,1000, 2000), 
                                 labels=c("-725 - -150", "-150 - 0", "0 - 300", 
                                          "300 - 1000", ">1000"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = chil10_20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Change in population of children, 2010 - 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census, 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/chil10_20.pdf", device = cairo_pdf, width = 8, height = 5)


### PCT CHILDREN 20  ###
tomap$chil20.brks <- cut(tomap$chil20, 
                           include.lowest = TRUE,
                           breaks=c(0, 250,1000,2000,3500,10000), 
                           labels=c("0-250", "250-1,000", "1,000-2,000", "2,000-3,500", ">3,500"))
# make map
tomap %>%
  ggplot() +
  geom_sf(aes(fill = chil20.brks), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  # geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  # scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "GnBu") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.14, -89.71), ylim = c(29.88, 30.13), expand = FALSE) +
  theme(legend.position = c(.75,.2),
        legend.direction = "vertical",
        legend.key.size = unit(.5,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Population of children, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/chil20.pdf", device = cairo_pdf, width = 8, height = 5)


### PIE CHARTS ###
# Create Data
metro_pies_10 <- metro_black_nh_old %>%
  filter(Parish == "Orleans", Year == "2010") %>%
  select(3) %>%
  bind_cols(metro_white_nh_old %>%
              filter(Parish == "Orleans", Year == "2010") %>%
              select(3))%>%
  bind_cols(metro_hisp_old %>%
              filter(Parish == "Orleans", Year == "2010") %>%
              select(3))%>%
  bind_cols(metro_api_nh_old %>%
              filter(Parish == "Orleans", Year == "2010") %>%
              select(3))%>%
  bind_cols(metro_other_nh_old %>%
              filter(Parish == "Orleans", Year == "2010") %>%
              select(3)) %>%
  bind_cols(two_nh_2010 %>%
              filter(Parish == "Orleans") %>%
              select(2)) %>%
  mutate(`Other, Not Hispanic` = `Other, Not Hispanic` - `2010_Two or More Races, Not Hispanic`,
         `Two or More Races, Not Hispanic` = `2010_Two or More Races, Not Hispanic`) %>%
  select(-`2010_Two or More Races, Not Hispanic`) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  mutate(var.fac = factor(.$var, levels = c("Black or African American Alone, Not Hispanic",
                                            "White Alone, Not Hispanic",
                                            "Hispanic (Any Race)",
                                            "Two or More Races, Not Hispanic",
                                            "Asian or Pacific Islander, Not Hispanic",
                                            "Other, Not Hispanic")))

# Compute the position of labels
metro_pies_10 <- metro_pies_10 %>% 
  arrange(-desc(var)) %>%
  mutate(prop = val / sum(metro_pies$val) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(metro_pies_10, aes(x="", y=val, fill=var.fac)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggrepel::geom_text_repel(aes(label = comma(val), x = 1.3), size = 3.3, min.segment.length = 3.1,
            nudge_x = 0.45,) +
  scale_fill_manual(values = c(DCcolor.p2blue, DCcolor.p2green, DCcolor.p3yellowochre, DCcolor.p2yellow, DCcolor.p2teal, DCcolor.p2magenta)) +
  theme_void()  + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5)) +
  labs(title = "Racial and ethnic makeup of Orleans Parish population, 2010",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2010 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pie 10.pdf", device = cairo_pdf, width = 8, height = 5)


metro_pies <- metro_black_nh_2020 %>%
  filter(Parish == "Orleans") %>%
  bind_cols(metro_white_nh_2020 %>%
              filter(Parish == "Orleans") %>%
              select(-Parish, -Year))%>%
  bind_cols(metro_hisp_2020 %>%
              filter(Parish == "Orleans") %>%
              select(-Parish, -Year))%>%
  bind_cols(metro_api_nh_2020 %>%
              filter(Parish == "Orleans") %>%
              select(-Parish, -Year))%>%
  bind_cols(metro_other_nh_2020 %>%
              filter(Parish == "Orleans") %>%
              select(-Parish, -Year)) %>%
  mutate(`Other, Not Hispanic` = pop_other + pop_aian,
         `Two or More Races, Not Hispanic` = pop_two) %>%
  select(-contains("pop_"), -Year, -Parish) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  mutate(var.fac = factor(.$var, levels = c("Black or African American Alone, Not Hispanic",
                                            "White Alone, Not Hispanic",
                                            "Hispanic (Any Race)",
                                            "Two or More Races, Not Hispanic",
                                            "Asian or Pacific Islander, Not Hispanic",
                                            "Other, Not Hispanic")))

# Compute the position of labels
metro_pies <- metro_pies %>% 
  arrange(-desc(var)) %>%
  mutate(prop = val / sum(metro_pies$val) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(metro_pies, aes(x="", y=val, fill=var.fac)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggrepel::geom_text_repel(aes(label = comma(val), x = 1.3), size = 3.3, min.segment.length = 2.6,
                           nudge_x = 0.35) +
  scale_fill_manual(values = c(DCcolor.p2blue, DCcolor.p2green, DCcolor.p3yellowochre, DCcolor.p2yellow, DCcolor.p2teal, DCcolor.p2magenta)) +
  theme_void()  + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5)) +
  labs(title = "Racial and ethnic makeup of Orleans Parish population, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/nbhd change maps/pie 20.pdf", device = cairo_pdf, width = 8, height = 5)
