hhGq_parish2020_p <- hhGq_parish2020 %>% 
  select(-GEOID, -Population, -`Occupied \nunits`, -`Total group quarters population`,-`Total noninstitutional`, -`Total institutional`, -Year, -`Average household size`) %>%
  pivot_longer(-Parish, names_to = "var", values_to = "val")

chart.hhGq_parish2020 <- hhGq_parish2020_p %>%
  ggplot(aes(Parish, as.numeric(val), fill=var)) +
  geom_bar(stat="identity", 
           position="stack", 
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2orange,
                               DCcolor.p2green,
                               DCcolor.p2teal,
                               DCcolor.p2purple,
                               DCcolor.p2violet,
                               DCcolor.p2limegreen,
                               DCcolor.p1darkblue90)) +  
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) +
  themeDC_horizontal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        # legend.spacing.y = unit(10, "lines"),
        plot.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_text(size = 10, vjust=.8, angle = 15),
        axis.text.y = element_text(size = 12)) +
  labs(title = "Group quarters population by type, 2020", 
       subtitle = "Metro parishes",
       x="",
       y="",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/gqType_parish.pdf", device = cairo_pdf, width = 8, height = 5)


hhGq_parish_updated_p <- hhGq_parish_updated %>%
  filter(Year %in% c("2010", "2020"),
         Parish != "New Orleans Metro Total") %>%
  select(Parish, Year, `Adult correctional\nfacilties`) %>% 
  mutate(val = as.numeric(gsub(",","",`Adult correctional\nfacilties`))) %>%
  bind_rows(data.frame(Parish = "St. James", Year = "2010", val = 92))

chart.hhGq_parish_updated_acf <- hhGq_parish_updated_p %>%
  ggplot(aes(Parish, val, fill=Year)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(val, accuracy = 1)), position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0, 4100)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue,
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 15, vjust = .8)) +
  labs(title = "Adult correctional facilities population, 2020",
       x="",
       y="",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/adult correctional pop.pdf", device = cairo_pdf, width = 8, height = 5)

hhGq_parish_updated_p <- hhGq_parish_updated %>%
  filter(Year %in% c("2010", "2020"),
         Parish != "New Orleans Metro Total") %>%
  select(Parish, Year, `Nursing \nfacilities`) %>% 
  mutate(val = as.numeric(gsub(",","",`Nursing \nfacilities`)))%>%
  bind_rows(data.frame(Parish = "St. James", Year = "2010", val = 72))

chart.hhGq_parish_updated_nursing <- hhGq_parish_updated_p %>%
  ggplot(aes(Parish, val, fill=Year)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(val, accuracy = 1)), position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0, 2100)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue,
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 15, vjust = .8)) +
  labs(title = "Nursing facilities population, 2020",
       x="",
       y="",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/nursing pop.pdf", device = cairo_pdf, width = 8, height = 5)



hhGq_parish_updated_hh <- hhGq_parish_updated %>%
  filter(Year %in% c("2020"),
         Parish != "New Orleans Metro Total") %>%
  select(Parish, Year, `Average household size`) %>%
  mutate(val = as.numeric(`Average household size`))

chart.hhsize <- hhGq_parish_updated_hh %>%
  ggplot(aes(Parish, val)) +
  geom_bar(stat="identity",
           width = .7,
           color="gray50", fill = DCcolor.p1mediumblue) +
  geom_text(aes(label = comma(val, accuracy = .01)), vjust = -.7, size=3, family="Asap") +
  # scale_y_continuous(labels = comma_format(), expand = c(0,0)) +
  # scale_fill_manual(values = c(DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 15, vjust = 1)) +
  labs(title = "Average household size, 2020",
       x="",
       y="",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/hhSize_parish.pdf", device = cairo_pdf, width = 8, height = 5)



hhGq_nbhd_tomap <- nbhds2020 %>%
  left_join((hhGq_nbhd2020%>%
  select(Neighborhood, `Average household size`) %>%
  mutate(val = as.numeric(`Average household size`),
         Neighborhood = ifelse(grepl("Cath", Neighborhood), "Lake Catherine/Village de L'est", Neighborhood))), by = c("Name2" = "Neighborhood")) %>%
  filter(Name2 != "Z-no land mass-all Lake")

hhGq_nbhd_tomap$brks <- cut(hhGq_nbhd_tomap$val, 
                                 include.lowest = TRUE,
                                 breaks=c(0, 1.5, 1.749, 2.749,3,10), 
                                 labels=c("<1.5 people", "1.5 - 1.75 people", "1.75 - 2.75 people", 
                                          "2.75 - 3 people", ">3 people"))
# make map
hhGq_nbhd_tomap %>%
  ggplot() +
  geom_sf(aes(fill = brks), color = "gray70", size = .1) +
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
  labs(title = "Average household size, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/hhSize_nbhd map.pdf", device = cairo_pdf, width = 8, height = 5)



hhGq_ni_bg_tomap <- hhGq_bg %>%
  select(GEOID, `College/\nuniversity student housing`, `Military quarters` = `Military\nquarters`, `Other  noninstitutional`) %>%
  pivot_longer(-GEOID, names_to = "var", values_to = "val") %>%
  left_join(bg.la, by = "GEOID") %>%
  mutate(lat = as.numeric(INTPTLAT), 
         lon = as.numeric(INTPTLON)) 

library(plyr)
hhGq_ni_bg_tomap %>%
  arrange(desc(val)) %>%
  filter(val != 0) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), lwd = 0, color = "gray90") +
  geom_sf(data = nbhds2020, fill = "transparent", color = "gray50", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_point(aes(color = var, size = val, geometry = geometry), stat = "sf_coordinates") +
  geom_point(data = (hhGq_ni_bg_tomap %>% filter(var == "Military quarters", val >0)), size = 1, aes(geometry = geometry), color = DCcolor.p2yellow, stat = "sf_coordinates") +
  themeDC_map() +
  scale_color_manual(values = c(DCcolor.p2orange, DCcolor.p2yellow, DCcolor.p2purple))+
  scale_size(breaks = c(0,20, 100, 1300, 3000), labels = c("xx", "20 people", "100 people",  "1,300 people", "3,000+ people"))+
  coord_sf(xlim = c(-90.14, -89.81), ylim = c(29.88, 30.09), expand = FALSE) +
  theme(legend.position = c(.7,.2),
        legend.direction = "vertical",
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.2, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) +
  labs(title = "Noninstitutional group quarters distribution, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/gq noninst map.pdf", device = cairo_pdf, width = 8, height = 5)


hhGq_i_bg_tomap <- hhGq_bg %>%
  select(GEOID, `Adult correctional\nfacilties`, `Juvenile facilities` = `Juvenile \nfacilities`, `Nursing facilities` = `Nursing \nfacilities`, `Other institutional`) %>%
  pivot_longer(-GEOID, names_to = "var", values_to = "val") %>%
  left_join(bg.la, by = "GEOID") %>%
  mutate(lat = as.numeric(INTPTLAT), 
         lon = as.numeric(INTPTLON))

hhGq_i_bg_tomap %>%
  filter(val != 0) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), lwd = 0, color = "gray90") +
  geom_sf(data = nbhds2020, fill = "transparent", color = "gray50", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = DCcolor.p1skyblue60, color = "gray70", size = .1) +
  geom_point(aes(color = var, size = val, geometry = geometry), stat = "sf_coordinates") +
  themeDC_map() +
  scale_color_manual(values = c(DCcolor.p2green, DCcolor.p1darkblue, DCcolor.p2magenta, DCcolor.p2yellow))+
  scale_size(breaks = c(0,20, 100, 200, 1500), labels = c("xx", "20 people", "100 people", "200 people", "1,500+ people"))+
  coord_sf(xlim = c(-90.14, -89.81), ylim = c(29.88, 30.09), expand = FALSE) +
  theme(legend.position = c(.7,.2),
        legend.direction = "vertical",
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.2, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        strip.text = element_text(hjust = 0),
        legend.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90", colour = "grey90"), 
        panel.grid.major = element_blank()) + 
  labs(title = "Institutional group quarters distribution, 2020",
       fill = "",
       caption = "Source: The Data Center analysis of data from the U.S. Census Bureau: 2020 Census.")+
  ggsave(filename = "outputs/group quarters graphics/gq inst map.pdf", device = cairo_pdf, width = 8, height = 5)

