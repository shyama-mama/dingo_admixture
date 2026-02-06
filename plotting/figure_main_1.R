source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")

### Get simpler dfs
microsat_simpler <- microsat_data %>% 
  dplyr::filter(str_detect(analyses,'Interpolation') & x3q_result != "less than 50% dingo") %>%
  group_by(long, lat) %>% 
  summarise(dingo_ancestry = mean(q_proportion_dingo_ancestry, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::rename(longitude = long,
                latitude = lat)

q_value <- cairns_q_value %>%
  dplyr::filter(latitude != "" & population %in% c("West", "South", "East", "Mallee") & sample_name %in% unrelated_dingoes$sample_name) %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(q_value, na.rm = TRUE)) %>%
  ungroup()

qpadm_values <- qpadm_results_wgs_snp_meta %>%
  dplyr::filter(latitude != "" & population %in% c("West", "South", "East", "Mallee") & target %in% unrelated_dingoes$sample_name) %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(dingo_ancestry, na.rm = TRUE)) %>%
  ungroup()


###### Interpolate
sf_use_s2(TRUE)
microsat_results <- plot_heat_map(microsat_simpler, grid, australia)
range(microsat_results$var1.pred)
q_value_results <- plot_heat_map(q_value, grid, australia)
range(q_value_results$var1.pred)
qpadm_results <- plot_heat_map(qpadm_values, grid, australia)
range(qpadm_results$var1.pred)

microsat_results_kgari <- plot_heat_map(microsat_simpler, grid_kgari_circle, australia)
q_value_results_kgari <- plot_heat_map(q_value, grid_kgari_circle, australia)
qpadm_results_kgari <- plot_heat_map(qpadm_values, grid_kgari_circle, australia)


########## Plot map
sf_use_s2(FALSE)

# To play with later:
#legend.key.height= unit(2, 'cm'),
#legend.key.width= unit(4, 'cm')
# https://www.statology.org/ggplot2-legend-size/

plot1 <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_sf_inset(data = microsat_results, aes(colour=var1.pred), size=0.8, map_base = "normal", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "normal", map_inset="none") +
  geom_sf_inset(data = microsat_results_kgari, aes(colour=var1.pred), size=0.5, map_base = "none", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "none", map_inset="normal") +
  scale_colour_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list, values=scales::rescale(c(0.65, 0.7, 0.8, 0.9, 1), limits=c(0.65,1)),
                         breaks = c(0.65, 0.8, 1), limits = c(0.65, 1)) +
  geom_sf(data=dingo_fence, linewidth=0.3) +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5)) + 
  theme_void() +
  theme(
    legend.position = "bottom"
  ) + 
  geom_inset_frame(colour="black", target.aes = list(linewidth=0.7)) +
  coord_sf_inset(configure_inset(
    shape_circle(
      centre = st_centroid(kgari_multipolygon),
      radius = 70
    ),
    scale = 10,  translation = c(530, 1200), units = "km"),
    ylim=c(-45,-5), xlim=c(110,170))

plot1

plot2 <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_sf_inset(data = q_value_results, aes(colour=var1.pred), size=0.8, map_base = "normal", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "normal", map_inset="none") +
  geom_sf_inset(data = q_value_results_kgari, aes(colour=var1.pred), size=0.5, map_base = "none", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "none", map_inset="normal") +
  scale_colour_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list, values=scales::rescale(c(0.65, 0.7, 0.8, 0.9, 1)), limits=c(0.65,1)) +
  geom_sf(data=dingo_fence, linewidth=0.3) +
  theme_void() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  ) + 
  geom_inset_frame(colour="black", target.aes = list(linewidth=0.7)) +
  coord_sf_inset(configure_inset(
    shape_circle(
      centre = st_centroid(kgari_multipolygon),
      radius = 70
    ),
    scale = 10,  translation = c(530, 1200), units = "km"),
    ylim=c(-45,-5), xlim=c(110,170))
plot2

wgs_samples_w_meta <- qpadm_results_wgs_snp_meta %>% 
  dplyr::filter(data_type == "WGS" & snp_used == "1.9M" & !is.na(latitude))

plot3 <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") + # Draw the australian map
  geom_sf_inset(data = qpadm_results, aes(colour=var1.pred), size=0.8, map_base = "normal", map_inset="auto") + # plot the qpAdm points and colour 
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "normal", map_inset="none") + # Drawing an inverse map of Australia - you can ignore this.
  geom_sf_inset(data = qpadm_results_kgari, aes(colour=var1.pred), size=0.5, map_base = "none", map_inset="auto")+ # Drawing just the K'gari points
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "none", map_inset="normal") + # Drawing an inverse map of Australia - you can ignore this.
  geom_sf_inset(data = st_as_sf(wgs_samples_w_meta, coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=dingo_ancestry), colour="black", alpha=1, size=3, shape=24, map_base = "normal", map_inset="auto") +
  scale_colour_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list_qpadm, values=scales::rescale(c(0.7, 0.8, 0.9, 1)), limits=c(0.7,1)) + # Setting the colour gradient
  scale_fill_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list_qpadm, values=scales::rescale(c(0.7, 0.8, 0.9, 1)), limits=c(0.7,1)) + # Setting fill gradient
  geom_sf(data=dingo_fence, linewidth=0.3) +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5, direction = "horizontal")) + 
  theme_void() +
  theme(
    #plot.margin = unit(c(0, 0, 0, 0)),
    legend.position = c(0.3,0.1) 
  ) + 
  geom_inset_frame(colour="black", target.aes = list(linewidth=0.7)) + # Drawing the inset 
  coord_sf_inset(configure_inset(
    shape_circle(
      centre = st_centroid(kgari_multipolygon),
      radius = 70
    ),
    scale = 10,  translation = c(530, 1200), units = "km"),
    ylim=c(-45,-5), xlim=c(110,170))
plot3


figure_a <- ggarrange(plot1, plot2, plot3, common.legend = TRUE, ncol=1, legend = "bottom") 
figure_a

##################
#### Figure B Rain cloud
###############
cairns_dingoes_sf <- cairns_q_value %>%
  dplyr::filter(latitude != "" & population %in% c("West", "Alpine", "East", "Mallee") & longitude < 155 & latitude < -5) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)


sf_polygons <- cairns_dingoes_sf %>%
  group_by(population) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%  # Merge points into a single geometry
  rowwise() %>%
  mutate(geometry = st_sfc(st_polygon(list(concaveman::concaveman(as.matrix(st_coordinates(geometry)))))), crs = 4326) %>%
  st_set_crs(4326) %>%  # Explicitly set CRS
  st_as_sf() %>%
  st_cast("MULTIPOLYGON")

microsat_sf <- st_as_sf(microsat_simpler, coords = c("longitude", "latitude"), crs = 4326)

# Check which points are inside a polygon
microsat_sf <- microsat_sf %>%
  mutate(population = sapply(st_within(geometry, sf_polygons), function(x) if (length(x) == 1) sf_polygons$population[x] else NA))

multiple_populations <- sapply(st_intersects(microsat_sf, sf_polygons), length) > 1
microsat_sf <- microsat_sf[!multiple_populations, ]  # Remove those points

no_population <- is.na(microsat_sf$population)

# ggplot() +
#   geom_sf(data=australia) +
#   geom_sf(data=microsat_sf, aes(colour=population))

if (any(no_population)) {
  closest_indices <- st_nearest_feature(microsat_sf[no_population, ], sf_polygons)
  microsat_sf$population[no_population] <- sf_polygons$population[closest_indices]
}

captive_microsat <- microsat_data %>%
  dplyr::filter(id %in% c("di1444", "di1441", "di1442", "di1443")) %>%
  dplyr::mutate(population = "Captive") %>%
  dplyr::rename(dingo_ancestry = q_proportion_dingo_ancestry) %>%
  dplyr::select(dingo_ancestry, population, long ,lat) %>%
  st_as_sf(., coords = c("long", "lat"), crs=st_crs(australia))

microsat_rain <- rbind(microsat_sf,captive_microsat)  %>% 
  dplyr::mutate(
    type = "Stephens et al., 2015",
    type2 = "STR"
    ) %>%
  dplyr::select(population, type, type2, dingo_ancestry) %>%
  dplyr::filter(!is.na(population)) %>%
  st_drop_geometry()

cairns_rain <- cairns_q_value %>%
  dplyr::filter(population %in% c("West", "Alpine", "East", "Mallee", "Captive")) %>% # & sample_name %in% unrelated_dingoes$sample_name ) %>%
  dplyr::mutate(type = "Cairns et al., 2023",
                type2 = "SNP+FS") %>%
  dplyr::rename(dingo_ancestry = q_value) %>%
  dplyr::select(population, type, type2, dingo_ancestry)

qpadm_rain <- qpadm_results_wgs_snp_meta %>%
  dplyr::filter(data_type != "WGS", population %in% c("West", "Alpine", "East", "Mallee", "Captive")) %>% # & target %in% unrelated_dingoes$sample_name ) %>%
  dplyr::mutate(type = "This Paper",
                type2 = "SNP+qpAdm") %>%
  dplyr::select(population, type, type2, dingo_ancestry)

merged_rain <- rbind(microsat_rain, cairns_rain, qpadm_rain) %>%
  dplyr::mutate(population=factor(population, levels=c("Mallee", "Alpine", "East", "West", "Captive")),
                type=factor(type, levels=c("Stephens et al., 2015", "Cairns et al., 2023", "This Paper")),
                type2=factor(type2, levels=c("STR", "SNP+FS", "SNP+qpAdm"))) 

merged_rain_sample_numbers <- merged_rain %>% 
  group_by(population, type2) %>%
  summarise(n=n()) %>%
  mutate(type3 = paste0(type2, "\n(n=", n, ")")) %>%
  ungroup() %>%
  mutate(type3 = factor(type3, levels=c("STR\n(n=3)", "STR\n(n=445)", "STR\n(n=36)", "STR\n(n=1684)",
                                        "STR\n(n=4)", "SNP+FS\n(n=22)", "SNP+FS\n(n=55)", "SNP+FS\n(n=106)",
                                        "SNP+FS\n(n=133)", "SNP+FS\n(n=61)","SNP+qpAdm\n(n=22)", 
                                        "SNP+qpAdm\n(n=55)", "SNP+qpAdm\n(n=104)","SNP+qpAdm\n(n=132)", "SNP+qpAdm\n(n=60)")))

merged_rain_w_sample_counts <- merge(merged_rain, merged_rain_sample_numbers, by=c("population", "type2")) %>%
  mutate(population := case_when(
    population == "South" ~ "Alpine",
    .default = population
  ))

# figure_b_stephens <- ggplot(merged_rain %>% dplyr::filter(type == "Stephens et al., 2015"), aes(x = population, y = 1-dingo_ancestry, fill=population)) +
#   geom_rain( 
#     point.args = list(alpha=0.5, shape=21, size=1, colour="white"), 
#     boxplot.args = list(outlier.shape = NA, lwd = 0.5),
#     violin.args = list(colour=NA)) +
#   scale_fill_manual(values=snp_population_colour) +
#   labs(y="") + 
#   theme_bw() +
#   coord_cartesian(ylim=c(0,0.5)) + 
#   facet_wrap(~factor(type, levels=c('Stephens et al., 2015', 'Cairns et al., 2023', 'This Paper')), ncol=1) + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         plot.margin = unit(c(0, 0, 0, 0), "pt")) 
# figure_b_stephens
# 
# figure_b_this_paper <- ggplot(merged_rain %>% dplyr::filter(!str_detect(population,'Captive') & type == "This Paper"), aes(x = population, y = 1-dingo_ancestry, fill=population)) +
#   geom_rain( 
#     point.args = list(alpha=0.5, shape=21, size=1, colour="white"), 
#     boxplot.args = list(outlier.shape = NA, lwd = 0.5),
#     violin.args = list(colour=NA)) +
#   scale_fill_manual(values=snp_population_colour) +
#   labs(y="") + 
#   theme_bw() +
#   #scale_y_continuous(position="right") +
#   coord_cartesian(ylim=c(0,0.5)) + 
#   facet_wrap(~factor(type, levels=c('Stephens et al., 2015', 'Cairns et al., 2023', 'This Paper')), ncol=1) + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         plot.margin = unit(c(0, 0, 0, 0), "pt")) 
# figure_b_this_paper
# 
# figure_b_cairns_rain <- ggplot(merged_rain %>% dplyr::filter(!str_detect(population,'Captive') & type == "Cairns et al., 2023"), aes(x = population, y = 1-dingo_ancestry, fill=population)) +
#   geom_rain( 
#     point.args = list(alpha=0.5, shape=21, size=1, colour="white"), 
#     boxplot.args = list(outlier.shape = NA, lwd = 0.5),
#     violin.args = list(colour=NA)) +
#   scale_fill_manual(values=snp_population_colour) +
#   scale_x_discrete(breaks=c("Mallee", "South", "East", "West")) +
#   labs(y="Dog Admixture Estimate", ) + 
#   theme_bw() +
#   #scale_y_continuous(position="right") +
#   facet_wrap(~factor(type, levels=c('Stephens et al., 2015', 'Cairns et al., 2023', 'This Paper')), ncol=1) + 
#   coord_cartesian(ylim=c(0,0.5)) + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "none",
#         plot.margin = unit(c(0, 0, 0, 0), "pt")) 
# figure_b_cairns_rain

#### Plot pos on map
cairns_pops_df <- coords %>%
  dplyr::filter(longitude < 155 & latitude < -5 & population != "Captive" & data_type == "SNP Array")

cairns_pops_captive <- coords %>%
  dplyr::filter(population == "Captive") %>%
  dplyr::select(-c(sample_id, longitude, latitude, age, data_type, age_type)) %>%
  left_join(., dd_7_w_fake_coords, by="sample_name") %>%
  dplyr::select(sample_name, sample_id, longitude, latitude, age, data_type, population, age_type)

cairns_pops_df <- rbind(cairns_pops_wild, )

cairns_pops_map <- ggplot() +
  geom_point(data=cairns_pops_df, aes(x=longitude, y=latitude, colour=population), size=3) + 
  geom_sf(data=st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black") +
  #geom_sf(data=australia, fill="NA", colour="black") +
  geom_sf(data=dingo_fence, linewidth=0.5) +
  facet_grid(~"Population Stratification") +
  scale_fill_manual(values=snp_population_colour) +
  scale_colour_manual(values=snp_population_colour) + 
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_x_continuous(sec.axis = dup_axis()) +
  coord_sf(xlim=c(155,110), ylim=c(-45, -10)) + 
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line.y.right = element_line(colour="black"),
    axis.line.y.left = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 20, 0), "pt"),
    panel.border = element_rect(colour="black", fill="NA", linewidth=1))
cairns_pops_map


figure_b_cairns_rain_map <- figure_b_cairns_rain + annotation_custom(ggplotGrob(cairns_pops_map), xmin=0.5, xmax=2, ymin = 0.2, ymax = 0.5)



figure_b <- figure_b_stephens / figure_b_cairns_rain_map / figure_b_this_paper
figure_b


########################### Population density
admixture_summary_with_meta <- admixture_proportions %>% 
  subset(population != "Captive" & sample_name %in% unrelated_dingoes$sample_name) %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(dingo_ancestry, na.rm = TRUE),
            population = paste(unique(population), collapse = ","),
            south_of_fence = paste(unique(south_of_fence), collapse = ","),
            samples_name = paste(unique(sample_name), collapse = ",")) %>%
  ungroup()

write.csv(admixture_summary_with_meta %>% dplyr::select(longitude, latitude, dingo_ancestry, south_of_fence),
          "admixture_summary_fence_status.csv", quote = F, row.names = FALSE)

# Calculate dingo home radius
buffer_radius_km <- sqrt(400 / pi)  # ~11.28 km
buffer_radius_m <- buffer_radius_km * 1000  # Convert to meters
# 
sf_use_s2(TRUE)
# Create buffer zones around each point
qpadm_values_sf <- st_as_sf(admixture_summary_with_meta, coords=c("longitude", "latitude"), crs=st_crs(australia))
sf_points_buffer <- st_buffer(qpadm_values_sf, dist = buffer_radius_m)

# Get the mean population density within the radius for each coordinate
extracted_values <- exactextractr::exact_extract(dessity_data_crs, sf_points_buffer, fun = "mean")

# A lot of samples have a mean of 0
# Randomly adding a very small number to these
# So the log10 will be b/w -4 and -5. 
# And it is scattered so easier to see. 
# Dodgy hack ik but i like the look it gives
pop_density_qpadm <- cbind(admixture_summary_with_meta, extracted_values) %>%
  rowwise() %>%
  mutate(
    extracted_values_plotting := 
      case_when(
        extracted_values == 0 ~ sample(c(0.00001, 0.00002, 0.00003, 0.00004, 0.00005, 0.00006, 0.00007, 0.00008, 0.00009), 1),
        .default = extracted_values
      )
  )

pop_density_qpadm$log_human_density <- log10(pop_density_qpadm$extracted_values+1)  


# Plot 
pop_density_plot <- ggplot() +
  geom_rect(aes(ymin=0.5, ymax=1.05, xmin=-3.7, xmax=-5.3), fill="grey50", colour="NA", alpha=0.2) +
  geom_smooth(data=pop_density_qpadm , aes(x=log10(extracted_values_plotting), y=dingo_ancestry, colour=south_of_fence), method="lm", linetype="dashed", linewidth=1) + 
  geom_point(data=pop_density_qpadm, aes(x=log10(extracted_values_plotting), y=dingo_ancestry, fill=south_of_fence, colour=south_of_fence), shape=21, size=3, alpha=0.5) +
  # scale_fill_manual(values=colours2) +
  scale_colour_manual(values=c("TRUE"="red", "FALSE"="black")) + 
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="black")) + 
  scale_x_continuous(breaks = c(-4.5, -2, 0, 1, 2, 3), labels = c(0, 0.01, 1, 10, 100, 1000)) +
  labs(y="Dingo Ancestry", x=bquote(Mean~Human~Population~Density~per~km^2)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_text(aes(x=2.3, y=1.04, label="p = < 0.001"))
pop_density_plot

p <- admixture_proportions %>% 
  subset(population != "Captive" & sample_name %in% unrelated_dingoes$sample_name) %>%
  mutate(south_of_fence = as.factor(south_of_fence)) %>% 
  ggplot(aes(x = south_of_fence, y = dingo_ancestry, 
             colour = south_of_fence, fill = south_of_fence)) +
  geom_rain(alpha = 0.5, point.args = list(alpha=0.1)) +
  labs(
    x = "", 
    y = "Dingo Ancestry") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_x_discrete(breaks=c("FALSE", "TRUE"), labels=c("North of\nDingo Fence", "South of\nDingo Fence"), expand=c(0,0))+
  scale_colour_manual("Within Fence", values = c("FALSE" = "black", "TRUE" = "red")) +
  # Add the comparison bar
  geom_signif(comparisons = list(c("FALSE", "TRUE")), 
              test = "wilcox.test",
              map_signif_level = function(p) sprintf("p = %.2g", p), 
              y_position = 1.01) + # Adjust y_position as needed
  theme_classic() +
  guides(fill="none", colour="none") +
  theme(legend.position = "right",
        axis.line.x=element_blank(),
        axis.ticks.x = element_blank())
p



################### Box plot idea two


mallee <- ggplot(merged_rain_w_sample_counts %>% dplyr::filter(population == "Mallee"), aes(x=type3, y=1-dingo_ancestry, fill=population)) +
  geom_rain(point.args = list(alpha=0.7, shape=21, size=1, colour="white"), 
            boxplot.args = list(outlier.shape = NA, lwd = 0.5),
            violin.args = list(colour=NA, scale="width")) +
  coord_cartesian(y=c(0,0.6)) + 
  scale_fill_manual(values=snp_population_colour) +
  #scale_x_discrete(sec.axis = dup_axis()) + 
  labs(x="Mallee", y="Dog Admixture Estimate") + 
  theme_classic() +
  facet_grid(~population) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 20, 0), "pt")) 
mallee

south <- ggplot(merged_rain_w_sample_counts %>% dplyr::filter(population == "Alpine"), aes(x=type3, y=1-dingo_ancestry, fill=population)) +
  geom_rain(point.args = list(alpha=0.7, shape=21, size=1, colour="white"), 
            boxplot.args = list(outlier.shape = NA, lwd = 0.5),
            violin.args = list(colour=NA, scale="width")) +
  coord_cartesian(y=c(0,0.6)) + 
  scale_fill_manual(values=snp_population_colour) +
  labs(x="South", y="Dog Admixture Estimate") + 
  facet_grid(~population) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 20, 0), "pt")) 
south

east <- ggplot(merged_rain_w_sample_counts %>% dplyr::filter(population == "East"), aes(x=type3, y=1-dingo_ancestry, fill=population)) +
  geom_rain(point.args = list(alpha=0.7, shape=21, size=1, colour="white"), 
            boxplot.args = list(outlier.shape = NA, lwd = 0.5),
            violin.args = list(colour=NA, scale="width")) +
  coord_cartesian(y=c(0,0.6)) + 
  scale_fill_manual(values=snp_population_colour) +
  facet_grid(~population) +
  labs(x="East", y="Dog Admixture Estimate") + 
  #scale_y_continuous(sec.axis = dup_axis()) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 20, 0), "pt")) 
east
west <- ggplot(merged_rain_w_sample_counts %>% dplyr::filter(population == "West"), aes(x=type3, y=1-dingo_ancestry, fill=population)) +
  geom_rain(point.args = list(alpha=0.7, shape=21, size=1, colour="white"), 
            boxplot.args = list(outlier.shape = NA, lwd = 0.5),
            violin.args = list(colour=NA, scale="width")) +
  coord_cartesian(y=c(0,0.6)) + 
  scale_fill_manual(values=snp_population_colour) +
  labs(x="West", y="Dog Admixture Estimate") + 
  theme_classic() +
  facet_grid(~population) +
  #scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        #axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.title.y.right = element_blank(),
        #axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 20, 0), "pt")) 
west

captive <-  ggplot(merged_rain_w_sample_counts %>% dplyr::filter(population == "Captive"), aes(x=type3, y=1-dingo_ancestry, fill=population)) +
  geom_rain(point.args = list(alpha=0.7, shape=21, size=1, colour="white"), 
            boxplot.args = list(outlier.shape = NA, lwd = 0.5),
            violin.args = list(colour=NA, scale="width")) +
  coord_cartesian(y=c(0,0.6)) + 
  scale_fill_manual(values=snp_population_colour) +
  facet_grid(~population) +
  labs(x="East", y="Dog Admixture Estimate") + 
  #scale_y_continuous(sec.axis = dup_axis()) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 20, 0), "pt")) 
captive 
  
  
#comparions <- (mallee + labs(tag = "A") )+ annotation_custom(ggplotGrob(cairns_pops_map), xmin=0.5, xmax=2, ymin = 0.3, ymax = 0.5) | south | east | west

fig_1a <- west | mallee | south | east | captive | cairns_pops_map
fig_1a
ggsave("../publication_plots/figure_1a.png", fig_1a, dpi=700, height=4, width=18)

cairns_pops_map

#comparions
#plot3
#pop_density_plot
#p

figure_1_option_a <- (comparions) / ((plot3 + labs(tag = "B"))| ((pop_density_plot + labs(tag = "C"))| (p + labs(tag = "D"))))
figure_1_option_a + theme_update(plot.tag = element_text(face = "bold"))
ggsave("../plots/figure_dingo_admixture_map_option2.png", figure_1_option_a, dpi=700, height=9, width=13)
ggsave("../plots/figure_dingo_admixture_map_option2.jpg", figure_1_option_a, dpi=700, height=9, width=13)
ggsave("../plots/figure_dingo_admixture_map_option2.pdf", figure_1_option_a, dpi=700, height=9, width=13)


figure_1_bcd <- wrap_plots(plot_spacer(), plot3 , plot_spacer(), pop_density_plot , plot_spacer(), p, plot_spacer(), gen_time_plot, ncol=8, widths = c(-0.15,1.5,-0.1, 0.8,0.1, 0.8, 0, 0.8)) 
figure_1_bcd
ggsave("../publication_plots/figure_1bcde.png", figure_1_bcd, dpi=700, height=4, width=18)



design <- "
A
B"

figure_1 <- (fig_1a / figure_1_bcd) + plot_layout(design = design)

ggsave("../publication_plots/figure_1_revisions_2.png", figure_1, dpi=600, width=16, height=8)



##### P-values
ggbetweenstats(merged_rain %>% subset(population == "Mallee") %>% mutate(dog_admixture = 1-dingo_ancestry), type, dog_admixture, 
               xlab = "Source", 
               ylab = "Estimated Dog Admixture", 
               type="r", 
               pairwise.display = "s",
               results.subtitle=FALSE,
               violin.args = c(colour="NA", fill="NA")) +
  scale_color_manual(values = c("#1982c4", "#1982c4", "#1982c4")) +
  labs(title="West") +
  coord_cartesian(ylim=c(0,0.65)) +
  theme(axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank())
