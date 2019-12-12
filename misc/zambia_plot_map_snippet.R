
# Map of plot locations in Zambia
map_zambia <- borders(database = "world", regions = iso.expand("ZMB"), fill = "grey90", colour = "black")

plot_zambia <- ggplot() + 
  map_zambia + 
  geom_point(data = filter(ssaw8$plotInfoFull, country == "Zambia"), 
    aes(x = longitude_of_centre, y = latitude_of_centre), alpha = 0.6) + 
  geom_point(data = filter(plot_data, country == "Zambia"),
    aes(x = longitude_of_centre, y = latitude_of_centre), 
    colour = "black", fill = "#048FD4", shape = 21, size = 2, alpha = 1) + 
  theme_classic() +
  coord_map() + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()

pdf(file = "img/plot_loc_zambia.pdf", width = 8, height = 5)
plot_zambia
dev.off()
