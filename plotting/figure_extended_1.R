source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")

figure1 <- ggplot() +
  geom_sf_inset(data = australia, fill = "white", colour="black") +
  geom_sf_inset(
    data = st_as_sf(coords %>% subset(longitude < 155 & latitude < -5), coords=c("longitude", "latitude"), crs=st_crs(australia)),
    aes(shape=age_type, fill=age, size=data_type, alpha=data_type, colour=data_type)) +
  scale_shape_manual("Data Type", values=wgs_snp_anc_modern_shape) +
  scale_alpha_manual("Data Type", values=wgs_snp_alpha) +
  scale_colour_manual("Data Type", values=wgs_snp_colours) +
  scale_fill_manual("Fill", values = ancient_modern_fills) +
  scale_size_manual("Data Type", values = wgs_snp_sizes) +
  geom_sf(data=dingo_fence) +
  geom_label_repel(data = coords %>% subset(sample_id == "D10"),
                   aes(x = longitude, y = latitude), label="Curracurrang 2k",
                   nudge_y=-1,
                   nudge_x=4) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Northern_Territory_D06"),
                   aes(x = longitude, y = latitude), label="Northern Territory",
                   nudge_y=-1) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Simpson_Desert_D03"),
                   aes(x = longitude, y = latitude), label="Simpson Desert",
                   nudge_y=-1) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Alpine_Cooinda"),
                   aes(x = longitude, y = latitude), label="Cooinda",
                   nudge_y=2, nudge_x=-3.5) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Desert_Sandy"),
                   aes(x = longitude, y = latitude), label="Sandy",
                   nudge_y=1, nudge_x=-3) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Gibson_Desert_D08"),
                   aes(x = longitude, y = latitude), label="Gibson Desert",
                   nudge_y=-1) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Kimberley_D09"),
                   aes(x = longitude, y = latitude), label="Kimberley",
                   nudge_y=-1) +
  geom_label_repel(data = coords %>% subset(sample_id == "Dingo_Northern_Queensland_D10"),
                   aes(x = longitude, y = latitude), label="Northern Queensland",
                   nudge_y=1.5) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#92c5de"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, linetype="solid"),
        legend.position = c(0.1,0.85) ) +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(alpha="none", size="none", colour="none", fill = guide_legend("Sample Age", override.aes = list(shape = 21, size=3))) + 
  #geom_inset_frame(colour="black") +
  #coord_sf_inset(configure_inset(
  #  shape_circle(
  #    centre = st_centroid(kgari_multipolygon),
  #    radius = 70
  #  ),
  #  scale = 10,  translation = c(530, 1200), units = "km"),
  #  ylim=c(-45,-5)) +
  coord_sf(ylim=c(-45,-10)) + 
  #geom_label(aes(x=156, y=-11),label="K'gari", size = 4) +
  geom_label(aes(x=145, y=-36),label="Alpine", size = 4) +
  geom_label(aes(x=129.5, y=-32.7),label="Nullarbor 1k", size = 4)+
  theme(
    axis.title = element_text(size=15)
  )

figure1

ggsave("figure_sample_map.png", figure1, dpi=600, width=9.5, height=9.5)
