source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")


##### Affinity plot
f4_affinity_simpler <- f4_affinity %>%   
  filter(latitude != "", !population %in% c("Dingo x Dog Hybrid", "Captive"), data_type == "SNP Array") %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(affinity, na.rm = TRUE)) %>%
  ungroup()

ggplot(f4_affinity, aes(x=population, y=affinity)) +
  geom_boxplot()

f4_affinity_wgs <- f4_affinity %>%
  filter(data_type == "WGS", latitude != "") %>%
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
  scale_colour_gradientn(name="Ancestry Affinity", colours=c("#6f1d1b", "white", "#3c096c"), values=scales::rescale(c(-0.031,0, 0.024)),
                         breaks = c(-0.030, 0.02),
                         labels = c("Nullarbor", "Curracurrang"),
                         limits=c(-0.032, 0.02))+
  scale_fill_gradientn(name="Ancestry Affinity", colours=c("#6f1d1b", "white", "#3c096c"), values=scales::rescale(c(-0.031,0, 0.024)),
                           breaks = c(-0.032, 0.02),
                           labels = c("Nullarbor", "Curracurrang"),
                           limits=c(-0.032, 0.02))+
  guides(fill="none", colour = guide_colorbar(title.position = "top", title.hjust = 0.5, direction = "horizontal")) + 
  new_scale_fill() + 
  geom_sf(data = st_as_sf(coords %>% subset(age == "Ancient"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=population), shape=25, size=4, colour="white") +
  scale_fill_manual(values=c("Curracurrang 2k" = "#3c096c",
                             "Nullarbor 1k" = "#6f1d1b")) +
  guides(fill="none") + 
  theme_void() +
  theme(legend.position = c(0.3,0.1) ) +
  coord_sf(ylim=c(-45,-5), xlim=c(110,154))

affinity_plot
ggsave("../figure_affinity_map.png", affinity_plot, dpi=500)



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
  #geom_mark_hull(data=dd_7_names , aes(x=t_sne1, y=t_sne2, colour=as.factor(cluster8), fill=as.factor(cluster8)), concavity = 1000, expand = unit(2, "mm"), radius = unit(1,"mm"), label.fontsize = 6) +
  #geom_mark_hull(data=dd_7_names %>% subset(cluster7 == "7"), aes(x=t_sne1, y=t_sne2, colour=as.factor(cluster7), fill=as.factor(cluster7)), 
  #               label.buffer = unit(20, "mm"), label.fontsize = 6,
  #               concavity = 1000, expand = unit(1, "mm"), radius = unit(0.5,"mm")) +
  geom_point(data=dd_7_names %>% filter(data_type == "SNP Array"), aes(x=umap1, y=umap2, fill=as.factor(pop_name_manual), label=pop_id), colour="NA", size=3, alpha=0.5, shape=21) +
  geom_point(data=dd_7_names %>% filter(age_type == "WGS Modern"), aes(x=umap1, y=umap2, fill=as.factor(pop_name_manual), label=pop_id), size=4, shape=24) +
  geom_point(data=dd_7_names %>% filter(age == "Ancient"), aes(x=umap1, y=umap2, fill=as.factor(pop_name_manual), label=pop_id), size=4, shape=25) +
  #geom_text(aes(x=28, y=-15, label="Mallee"), colour=cluster_pallete["4"]) +
  #geom_text(aes(x=20, y=22, label="Alpine"), colour=cluster_pallete["1"]) +
  #geom_text(aes(x=-12, y=-31, label="Captive"), colour=cluster_pallete["3"]) +
  #geom_text(aes(x=-19.5, y=13, label="East"), angle=62, colour=cluster_pallete["2"]) +
  #geom_text(aes(x=5, y=-19, label="West"), colour=cluster_pallete["6"], angle=10) +
  #geom_text(aes(x=2, y=22), label="Biripi - Dainggatti\nCluster", colour=cluster_pallete["7"]) +
  #geom_text(aes(x=20, y=-6),label="Bundjalung\nCluster", colour=cluster_pallete["5"]) +
  scale_colour_manual(values=cluster_pallete) + 
  scale_fill_manual(values=cluster_pallete) + 
  xlab("UMAP1") +
  ylab("UMAP2") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

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
sf_use_s2(FALSE)

base_map <- ggplot() +
  geom_sf_inset(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_tile(data = topo_df, aes(x = x, y = y, fill = elevation_bin), alpha=0.7) +
  scale_fill_gradientn(colours=c("grey100", "black"), values=scales::rescale(c(1,9)), limits=c(1,9)) +
  new_scale_fill() + 
  geom_sf(data=dingo_fence) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% filter(data_type=="SNP Array"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(pop_name_manual)), colour="NA", size=3.5,shape=21, alpha=0.8) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% filter(age_type == "WGS Modern"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(pop_name_manual)), size=5,shape=24, alpha=0.9) +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% filter(data_type=="SNP Array", cluster8 == 4), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(pop_name_manual)), colour="NA", size=3.5,shape=21, alpha=0.8, map_base = "normal", map_inset="none") +
  geom_sf_inset(data=st_as_sf(dd_7_w_fake_coords %>% filter(age == "Ancient"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=as.factor(pop_name_manual)), size=5,shape=25, alpha=0.9, stroke=1, map_base = "normal", map_inset="none") +
  scale_fill_manual(values=cluster_pallete) + 
  geom_inset_frame(colour="black", target.aes = list(linewidth=0.7)) + # Drawing the inset 
  coord_sf_inset(configure_inset(
    shape_circle(
      centre = st_centroid(kgari_multipolygon),
      radius = 70
    ),
    scale = 10,  translation = c(530, 1200), units = "km"),
    ylim=c(-45,-5), xlim=c(110,165)) +
  #geom_text(aes(x=157, y=-6),label="K'gari", size = 3) +
  #geom_text(aes(x=127, y=-40.5),label="Captive", size = 4) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "pt")
  )


figure_2 <- wrap_plots(
  left,
  plot_spacer(), 
  base_map, 
  ncol=3, widths = c(1,-0.2,2)
) 

ggsave("../figure_2_manual.png", figure_2,  dpi=600, height=9, width=16)

