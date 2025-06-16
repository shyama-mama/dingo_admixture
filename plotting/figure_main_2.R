source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")


##### Affinity plot
f4_affinity_simpler <- f4_affinity %>%   
  subset(latitude != "" & population != "Dingo x Dog Hybrid" & data_type == "SNP Array") %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(affinity, na.rm = TRUE)) %>%
  ungroup()

f4_affinity_wgs <- f4_affinity %>%
  filter(data_type == "WGS") %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs=st_crs(australia))

sf_use_s2(TRUE)
f4_results <- plot_heat_map(f4_affinity_simpler, grid, australia)  
f4_results_kgari <- plot_heat_map(f4_affinity_simpler, grid_kgari_circle, australia)

# switch off for plotting
sf_use_s2(FALSE)

#colour pallete
#"#0d3b66","#faf0ca","#f4d35e"
affinity_plot <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_sf_inset(data = f4_results, aes(colour=var1.pred), size=0.8, map_base = "normal", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "normal", map_inset="none") +
  geom_sf_inset(data = f4_results_kgari, aes(colour=var1.pred), size=0.5, map_base = "none", map_inset="auto")+ # normal, none, clip & auto, normal, none
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "none", map_inset="normal") +
  geom_sf(data = dingo_fence) + 
  geom_sf(data = f4_affinity_wgs, aes(fill=affinity), shape=24, size=4, colour="black") +
  scale_colour_gradientn(name="Ancestry Affinity", colours=c("#6f1d1b", "white", "#3c096c"), values=scales::rescale(c(-0.031,0, 0.02)),
                         breaks = c(-0.030, 0.019),
                         labels = c("Nullarbor 1k", "Curracurrang 2k"),
                         limits=c(-0.031, 0.02))+
  scale_fill_gradientn(name="Ancestry Affinity", colours=c("#6f1d1b", "white", "#3c096c"), values=scales::rescale(c(-0.031,0, 0.02)),
                           breaks = c(-0.030, 0.019),
                           labels = c("Nullarbor 1k", "Curracurrang 2k"),
                           limits=c(-0.031, 0.02))+
  guides(fill="none", colour = guide_colorbar(title.position = "top", title.hjust = 0.5, direction = "horizontal")) + 
  new_scale_fill() + 
  geom_sf(data = st_as_sf(coords %>% subset(age == "Ancient"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=sample_name), shape=25, size=4, colour="white") +
  scale_fill_manual(values=c("Dingo_Ancient_Curracurrang" = "#3c096c",
                             "Dingo_Ancient_Nullarbor" = "#6f1d1b"), labels=c("Dingo_Ancient_Curracurrang" = "Curracurrang 2k",
                                                                              "Dingo_Ancient_Nullarbor" = "Nullarbor 1k")) +
  guides(fill="none") + 
  theme_void() +
  theme(legend.position = c(0.3,0.1) ) +
  coord_sf(ylim=c(-45,-5), xlim=c(110,154))

affinity_plot

### FEEMS
feems <- ggplot() +
  geom_sf(data = australia, fill = "#F7F7F7", color = "black", size = 0.7) +
  #geom_raster(data = topo_df, aes(x = x, y = y, fill = elevation_bin), alpha=1) +
  geom_sf(data=dingo_fence) + 
  #scale_fill_gradient(low = "grey100", high = "grey20") +  
  geom_segment(data = edges_weights,
               aes(x = nodes$Longitude[Node1_ID+1], 
                   y = nodes$Latitude[Node1_ID+1], 
                   xend = nodes$Longitude[Node2_ID+1], 
                   yend = nodes$Latitude[Node2_ID+1], colour=Weight), size = 0.6) +
  geom_point(data = obs_nodes_sizes, 
             aes(x = nodes$Longitude[NodeID+1], y = nodes$Latitude[NodeID+1], size = Size), shape=21, fill="#D9D9D9", alpha = 1) +
  scale_size_continuous(range = c(1, 5)) +
  scale_colour_gradient2(low="red", mid="white", high="blue", limits=c(-3,2), name = expression(log[10](w))) +
  guides(size="none", 
         fill="none",
         alpha="none",
         colour = guide_colorbar(title.position = "top", title.hjust = 0.5)) + 
  theme_void() +
  coord_sf(xlim = c(110, 160), ylim = c(-45, -5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.95, 0.6),
        #legend.justification = c("right", "bottom")
  )
feems

#### T-SNE
tsne_plot <- ggplot() +
  geom_mark_hull(data=dd_7_names , aes(x=t_sne1, y=t_sne2, colour=as.factor(cluster7), fill=as.factor(cluster7)), concavity = 1000, expand = unit(2, "mm"), radius = unit(1,"mm"), label.fontsize = 6) +
  #geom_mark_hull(data=dd_7_names %>% subset(cluster7 == "7"), aes(x=t_sne1, y=t_sne2, colour=as.factor(cluster7), fill=as.factor(cluster7)), 
  #               label.buffer = unit(20, "mm"), label.fontsize = 6,
  #               concavity = 1000, expand = unit(1, "mm"), radius = unit(0.5,"mm")) +
  geom_point(data=dd_7_names %>% subset(!population %in% c("Din", "aDin")), aes(x=t_sne1, y=t_sne2, fill=as.factor(cluster7)), colour="NA", size=3, alpha=0.5, shape=21) +
  geom_point(data=dd_7_names %>% subset(population == "Din"), aes(x=t_sne1, y=t_sne2, fill=as.factor(cluster7)), size=5, shape=24) +
  geom_point(data=dd_7_names %>% subset(population == "aDin"), aes(x=t_sne1, y=t_sne2, fill=as.factor(cluster7)), size=5, shape=25, stroke=1) +
  geom_text(aes(x=28, y=-15, label="Mallee"), colour=cluster_pallete["4"]) +
  geom_text(aes(x=20, y=22, label="Alpine"), colour=cluster_pallete["1"]) +
  geom_text(aes(x=-12, y=-31, label="Captive"), colour=cluster_pallete["3"]) +
  geom_text(aes(x=-19.5, y=13, label="East"), angle=62, colour=cluster_pallete["2"]) +
  geom_text(aes(x=5, y=-19, label="West"), colour=cluster_pallete["6"], angle=10) +
  geom_text(aes(x=2, y=22), label="Biripi - Dainggatti\nCluster", colour=cluster_pallete["7"]) +
  geom_text(aes(x=20, y=-6),label="Bundjalung\nCluster", colour=cluster_pallete["5"]) +
  scale_colour_manual(values=cluster_pallete) + 
  scale_fill_manual(values=cluster_pallete) + 
  xlab("t-SNE1") +
  ylab("t-SNE2") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  + 
  coord_cartesian(ylim=c(-31,30))

tsne_plot


 ########
left <- affinity_plot / tsne_plot

cluster1_points <- dd_7_w_fake_coords %>% 
  dplyr::filter(cluster7 == "3") %>% 
  summarise(longitude = mean(longitude),
            latitude = mean(latitude))

cluster2_points <- dd_7_w_fake_coords %>% 
  dplyr::filter(cluster7 == "2" & sample_id != "cd623") %>% 
  summarise(longitude = mean(longitude),
            latitude = mean(latitude))

##### Map

base_map <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_tile(data = topo_df, aes(x = x, y = y, fill = elevation_bin), alpha=0.7) +
  scale_fill_gradientn(colours=c("grey100", "black"), values=scales::rescale(c(1,9)), limits=c(1,9)) +
  new_scale_fill() +
  geom_sf(data=dingo_fence) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% subset(!population %in% c("Din", "aDin")), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), colour="NA", size=3.5,shape=21, alpha=0.8) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(!population %in% c("Din", "aDin")) %>% subset(cluster7 %in% c(5,7)), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), colour="NA", size=3.5,shape=21, alpha=0.8) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% subset(population == "Din"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), size=5,shape=24, alpha=0.9) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(population == "aDin"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), size=5,shape=25, alpha=0.9, stroke=1) +
  scale_fill_manual(values=cluster_pallete) + 
  #geom_rect(aes(xmin=152, xmax=153.2, ymin=-31.6, ymax=-30.5), colour=cluster_pallete["7"], fill="NA") +
  #geom_rect(aes(xmin=152.8, xmax=153.4, ymin=-25.8, ymax=-24.7), colour=cluster_pallete["7"], fill="NA") +
  #geom_rect(aes(xmin=149.5, xmax=150.8, ymin=-33.6, ymax=-32.5), colour=cluster_pallete["2"], fill="NA") +
  # geom_label_repel(data = coords %>% subset(sample_id == "D10"),
  #                 aes(x = longitude, y = latitude), label="Curracurrang 2k", fill=cluster_pallete["7"],
  #                 nudge_y=-2,
  #                 nudge_x=6, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Northern_Territory_D06"),
  #                  aes(x = longitude, y = latitude), label="Northern Territory",
  #                  nudge_y=1, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Simpson_Desert_D03"),
  #                  aes(x = longitude, y = latitude), label="Simpson Desert",
  #                  nudge_y=-1, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Alpine_Cooinda"),
  #                 aes(x = longitude, y = latitude), label="Cooinda", fill=cluster_pallete["6"],
  #                 nudge_y=1, nudge_x=4, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Desert_Sandy"),
  #                  aes(x = longitude, y = latitude), label="Sandy",
  #                  nudge_y=1, nudge_x=-3, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Gibson_Desert_D08"),
  #                  aes(x = longitude, y = latitude), label="Gibson Desert",
  #                  nudge_y=-1, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Kimberley_D09"),
  #                  aes(x = longitude, y = latitude), label="Kimberly",
  #                  nudge_y=-1, size=2) +
  # geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Northern_Queensland_D10"),
  #                  aes(x = longitude, y = latitude), label="Northern Queensland",
  #                  nudge_y=1.5, size=2) +
  #geom_inset_frame(colour="black") +
  #coord_sf_inset(configure_inset(
  #  shape_circle(
  #    centre = st_centroid(kgari_multipolygon),
  #    radius = 70
  #  ),
  #  scale = 10,  translation = c(530, 1200), units = "km"),
  #  ylim=c(-50,-5)) +
  #geom_label(aes(x=156, y=-11),label="K'gari", size = 2) +
  #geom_label(aes(x=145, y=-36),label="Alpine", size = 2, fill=cluster_pallete["5"]) +
  #geom_label(aes(x=129.5, y=-32.7),label="Nullarbor 1k", size = 2) +
  geom_label(aes(x=127, y=-40.5),label="Captive", size = 4) +
  coord_sf(ylim=c(-44,-9), xlim=c(112.5,154), expand=F) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "pt")
    )

#base_map

#base_map 

figure_2 <- wrap_plots(
  left,
  plot_spacer(), 
  base_map, 
  ncol=3, widths = c(1,-0.2,2)
) 

ggsave("../plots/figure_2.png", figure_2,  dpi=600, height=9, width=16)

ggsave("topo_map.png", base_map, dpi=700, height=5, width=5)
topo_df_kgari <- topo_df %>%
  dplyr::filter(x >= 152.8 & x <= 153.4 & y >= -25.8 & y <= -24.7)

dd_7_w_fake_coords_kgari <- dd_7_w_fake_coords %>%
  dplyr::filter(longitude >= 152.8 & longitude <= 153.4 & latitude >= -25.8 & latitude <= -24.7) %>%
  rowwise() %>%
  dplyr::mutate(
    longitude := case_when(
      population.x == "Din" ~ longitude+sample(c(-0.05, -0.04, -0.03, -0.01, 0, 0.04, 0.03, 0.02, 0.01, 0.05), 1),
      .default = longitude,
    ),
    latitude := case_when(
      population.x == "Din" ~ latitude+sample(c(-0.05, -0.04, -0.03, -0.01, 0, 0.04, 0.03, 0.02, 0.01, 0.05), 1),
      .default = latitude,
    )
  )

# K'gari
kgari_plot <- ggplot() +
  geom_tile(data = topo_df_kgari, aes(x = x, y = y, fill = elevation_bin), alpha=1) +
  geom_sf(data = australia, fill = NA, color = "black", linewidth=0.5) +
  scale_fill_gradientn(colours=c("grey100", "grey20"), values=scales::rescale(c(1,9)), limits=c(1,9)) +  
  #scale_fill_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list, values=scales::rescale(c(0.65, 0.7, 0.8, 0.9, 1)), limits=c(0.65,1)) + # Setting fill gradient
  new_scale_fill() +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords_kgari %>% subset(!population.x %in% c("Din", "aDin")), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), colour="NA", size=4,shape=21, alpha=1) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords_kgari %>% subset(population.x == "Din"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), size=5,shape=24, alpha=1) +
  scale_fill_manual(values=cluster_pallete) + 
  coord_sf(xlim=c(152.8,153.4), y=c(-25.8,-24.7)) +
  scale_y_continuous(breaks=c(-25.5, -25)) +
  scale_x_continuous(breaks=c(152.9, 153.3)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size=8),
    axis.ticks = element_line(colour=cluster_pallete["7"], linewidth = 2),
    legend.position = "none",
    panel.border = element_rect(colour=cluster_pallete["7"], linewidth = 2),
    #axis.line = element_line(colour="red")
    plot.margin = unit(c(0, 0, 0, 0), "pt")
  )
kgari_plot

# Bripi

topo_df_bripi <- topo_df %>%
  dplyr::filter(x >= 151 & x <= 154 & y >= -33 & y <= -29) %>%
  mutate(elevation_bin_2 = ceiling(elevation/10))




bripi_subset <- ggplot() +
  geom_tile(data = topo_df_bripi, aes(x = x, y = y, fill = elevation_bin_2), alpha=1) +
  geom_sf(data = australia, fill = NA, color = "black", linewidth=0.5) +
  scale_fill_gradientn(colours=c("white", "black"), values=scales::rescale(c(1,max(topo_df_bripi$elevation_bin_2))), limits=c(1,max(topo_df_bripi$elevation_bin_2))) +  
  new_scale_fill() +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(cluster7 != 7), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), colour="NA", size=4,shape=21, alpha=1) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(cluster7 == 7), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), size=5,colour="NA",shape=21, alpha=1) +
  scale_fill_manual(values=cluster_pallete) + 
  coord_sf(xlim=c(152,153.2), y=c(-31.6,-30.5)) + 
  scale_y_continuous(breaks=c(-31.4, -30.8)) +
  scale_x_continuous(breaks=c(152.2, 153)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #axis.text = element_text(size=8),
    #axis.ticks = element_line(colour=cluster_pallete["1"], linewidth = 2),
    legend.position = "none",
    panel.border = element_rect(colour=cluster_pallete["7"], linewidth = 2),
  )
bripi_subset

### Coolamatta 

topo_df_coolamatta <- topo_df %>%
  dplyr::filter(x >= 149 & x <= 151 & y >= -35 & y <= -30.5) %>%
  mutate(elevation_bin_2 = ceiling(elevation/50))

# 
# dd_7_w_fake_coords_coolamatta <- dd_7_w_fake_coords %>%
#   dplyr::filter(longitude >= 149 & longitude <= 151 & latitude >= -35 & latitude <= -30.5) %>%
#   rowwise() %>%
#   dplyr::mutate(
#     longitude = longitude+sample(c(-0.05, -0.04, -0.03, -0.01, 0, 0.04, 0.03, 0.02, 0.01, 0.05), 1),
#     latitude = latitude+sample(c(-0.05, -0.04, -0.03, -0.01, 0, 0.04, 0.03, 0.02, 0.01, 0.05), 1),
#   )



coolamatta_subset <- ggplot() +
  geom_tile(data = topo_df_coolamatta, aes(x = x, y = y, fill = elevation_bin_2), alpha=1) +
  geom_sf(data = australia, fill = NA, color = "black", linewidth=0.5) +
  scale_fill_gradientn(colours=c("grey100", "grey20"), values=scales::rescale(c(1,max(topo_df_coolamatta$elevation_bin_2))), limits=c(1,max(topo_df_coolamatta$elevation_bin_2))) +  
  new_scale_fill() +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords_coolamatta %>% subset(!population.x %in% c("Din", "aDin")), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), colour="NA", size=4,shape=21, alpha=1) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords_coolamatta %>% subset(cluster7 == 2), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster7)), size=5,shape=21, colour="NA", alpha=1) +
  scale_fill_manual(values=cluster_pallete) + 
  coord_sf(xlim=c(149.5,150.8), y=c(-33.6,-32.5)) + 
  scale_y_continuous(breaks=c(-33.4, -32.8)) +
  scale_x_continuous(breaks=c(149.8, 150.6)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size=8),
    axis.ticks = element_line(colour=cluster_pallete["2"], linewidth = 2),
    legend.position = "none",
    panel.border = element_rect(colour=cluster_pallete["2"], linewidth = 2),
    #axis.line = element_line(colour="red")
  )

coolamatta_subset

  (bripi_subset | coolamatta_subset)







ggplotly(ggplot() +
  geom_sf(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_sf(data=dingo_fence) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(!population.x %in% c("Din", "aDin")), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster20)), colour="NA", size=2.5,shape=21, alpha=0.8) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% subset(population.x == "Din"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster20)), size=4,shape=24, alpha=0.9) +
  geom_sf(data=st_as_sf(dd_7_w_fake_coords %>% subset(population.x == "aDin"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(cluster20)), size=4,shape=25, alpha=0.9, stroke=1) +
  #geom_rect(aes(xmin=152, xmax=153.2, ymin=-31.6, ymax=-30.5), colour=cluster_pallete["1"], fill="NA") +
  theme_void())


