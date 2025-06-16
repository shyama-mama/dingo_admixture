pacman::p_load(
  ggplot2,
  dplyr,
  tidyr,
  ggpubr,
  patchwork,
  stringr,
  rnaturalearth,
  rnaturalearthdata,
  ggrain,
  Ternary,
  ggtern,
  broom,
  purrr,
  factoextra,
  cluster,
  ggforce,
  ggnewscale,
  tidyterra,
  ggstatsplot,
  scales,
  plotly,
  ggrepel,
  sf,
  stringr,
  gstat,
  raster,
  terra,
  data.table, 
  ggmapinset,
  mclust
)

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/data/")


####### Functions
plot_heat_map <- function(data, grid, australia, grid_density=100, radius=200, neighbours=2) {
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  
  grid_sf <- sf::st_as_sf(x=grid, coords = c("longitude", "latitude"), crs= 4326 )
  grid_within_australia <- grid_sf[st_within(grid_sf, australia, sparse = FALSE), ]
  grid_sf <- st_as_sf(grid_within_australia, coords = c("longitude", "latitude"), crs = 4326)
  
  # Compute pairwise distances between grid points and sample points
  dist_matrix <- st_distance(grid_sf, data_sf)
  
  # Define a search radius (e.g., 10 km)
  radius_km <- radius * 1000  # Convert km to meters
  
  # Count how many sample points are within the radius for each grid point
  neighbor_count <- apply(dist_matrix, 1, function(x) sum(x <= radius_km))
  
  # Filter grid points that have at least 2 sample neighbors
  filtered_grid_sf <- grid_sf[neighbor_count >= neighbours, ]
  
  # Convert back to a dataframe if needed
  filtered_grid <- as.data.frame(st_coordinates(filtered_grid_sf)) %>%
    rename(Long = X, Lat = Y)
  
  data_sp <- as(data_sf, "Spatial")  # Convert to Spatial object for gstat
  vgm_model <- variogram(dingo_ancestry ~ 1, data_sp)  # Compute experimental variogram
  fit_vgm <- fit.variogram(vgm_model, vgm("Sph"))  # Fit spherical model
  plot(vgm_model, fit_vgm)  # Plot the variogram
  
  kriging_result_filt <- krige(dingo_ancestry ~ 1, data_sp, filtered_grid_sf, model = fit_vgm, nmax = 50, nmin = 2)
  kriging_result_filt$var1.pred <- pmin(kriging_result_filt$var1.pred, 1)
  return(kriging_result_filt)
}


#########
###### Map things 
#########
dingo_fence <- read_sf("merged_output.shp")
australia <- ne_countries(scale = "large", country = "Australia", returnclass = "sf")
topo_df <- read.csv("australian_topography_simpler.csv")

sf_use_s2(FALSE)
### Get inverse australia
bounding_box <- st_as_sfc(st_bbox(c(xmin = 100, xmax = 180, ymin = -60, ymax = 0), crs = st_crs(australia)))
# Subtract Australia from the bounding box to get the inverse polygon
inverse_polygon <- st_difference(bounding_box, st_union(australia))

#### Get K'gari
kgari_df <- fread("kgari_coords.tsv") %>%
  janitor::clean_names()
kgari_sf <- st_as_sf(kgari_df, coords = c("longitude","latitude"))
k_coords <- st_coordinates(kgari_sf)
k_coords <- rbind(k_coords, k_coords[1, ])
polygon_geom <- st_polygon(list(k_coords))
kgari_polygon <- st_sf(geometry = st_sfc(polygon_geom))
kgari_multipolygon <- st_cast(kgari_polygon, "MULTIPOLYGON")
kgari_multipolygon <- st_set_crs(kgari_multipolygon, 4326)
kgari_centroid <- st_centroid(kgari_multipolygon)
kgari_circle <- st_buffer(kgari_centroid, dist = 1)

### Grid for interpolation
set.seed(42)
bbox <- st_bbox(australia)
grid_density<- 200
grid <- expand.grid(
  longitude = seq(bbox["xmin"], bbox["xmax"], length.out = grid_density),
  latitude = seq(bbox["ymin"], bbox["ymax"], length.out = grid_density)
)

bbox_kgari_circle <- st_bbox(kgari_circle)
grid_density_kgari <- 100  
grid_kgari_circle <- expand.grid(
  longitude = seq(bbox_kgari_circle["xmin"], bbox_kgari_circle["xmax"], length.out = grid_density_kgari),
  latitude = seq(bbox_kgari_circle["ymin"], bbox_kgari_circle["ymax"], length.out = grid_density_kgari)
)

############
###### Metadata for Cairns and WGS (ancient and modern) samples
############
coords <- fread('sample_coordinates.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    age_type = paste(data_type, age)
  ) 

unrelated_dingoes <- fread("dingo_unrelates_samples_max.list", header = F, col.names = c("sample_name"))

#########
###### Human Population Density  
#########
density_data <- rast("apg23r_1_0_0.tif")
dessity_data_crs <- project(density_data, "EPSG:4326")

admixture_proportions <- fread('snp_array_dingo_fence_status.csv') %>% 
  janitor::clean_names() %>%
  dplyr::select(sample_name, dingo_ancestry, q_value, south_of_fence) %>%
  dplyr::left_join(., coords, by="sample_name")

admixture_proportions_simple <- admixture_proportions %>%
  dplyr::select(sample_name, south_of_fence)

admixture_summary_with_meta <- admixture_proportions %>% 
  dplyr::filter(population != "Captive") %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(dingo_ancestry, na.rm = TRUE),
            population = paste(unique(population), collapse = ","),
            south_of_fence = paste(unique(south_of_fence), collapse = ","),
            samples_name = paste(unique(sample_name), collapse = ",")) %>%
  ungroup()

#########
###### qValue for Cairns samples
#########
cairns_q_value <- fread("cairns_q_value.tsv") %>%
  janitor::clean_names() %>%
  left_join(., coords, by="sample_name")

###### Stephens et al., 2015 data and metadata
microsat_data <- fread("microsat_data.csv") %>%
  janitor::clean_names()

##########
###### ADMIXTURE for Cairns and WGS samples
##########
# k2 <- fread("admixture_unsupervised_02b_k2.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo2 = dingo_proportion)
# 
# k3 <- fread("admixture_unsupervised_02b_k3.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo3 = dingo_proportion)
# 
# k4 <- fread("admixture_unsupervised_02b_k4.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo4 = dingo_proportion)
# 
# k5 <- fread("admixture_unsupervised_02b_k5.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo5 = dingo_proportion)
# 
# k6 <- fread("admixture_unsupervised_02b_k6.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo6 = dingo_proportion)
# 
# k7 <- fread("admixture_unsupervised_02b_k7.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo7 = dingo_proportion)
# 
# k8 <- fread("admixture_unsupervised_02b_k8.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo8 = dingo_proportion)
# 
# k9 <- fread("admixture_unsupervised_02b_k9.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo9 = dingo_proportion)
# 
# k10 <- fread("admixture_unsupervised_02b_k10.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, average_dingo_proportion) %>%
#   rename(dingo10 = average_dingo_proportion)
# 
# k11 <- fread("admixture_unsupervised_02b_k11.csv") %>% 
#   janitor::clean_names() %>%
#   dplyr::select(v1, v2, dingo_proportion) %>%
#   rename(dingo11 = dingo_proportion)
# 
# l <- list(k2, k3, k4, k5, k6, k7, k8, k9, k10, k11)
# admixture_k2_k11 <- purrr::reduce(.x = l, merge, by = c('v1', 'v2'))
# admixture_k2_k11$pop<- substr(admixture_k2_k11$v1, 1, 3)
# admixture_k2_k11_dingoes <- admixture_k2_k11 %>% 
#   dplyr::filter(pop %in% c("BIG", "CAP", "EAS", "SOU", "WES", "Din", "HYB") & !str_detect(v1, "Ancient")) %>%
#   dplyr::mutate(
#     type := case_when(
#       pop == "Din" ~ "WGS",
#       .default = "SNP Array"
#     ),
#     population := case_when(
#       pop == "BIG" ~ "Mallee",
#       pop == "CAP" ~ "Captive",
#       pop == "EAS" ~ "East",
#       pop == "SOU" ~ "South",
#       pop == "WES" ~ "West",
#       pop == "HYB" ~ "Dingo x Dog Hybrid",
#       v1 == "Dingo_Alpine_Cooinda" ~ "Cooinda",
#       str_detect(v1, "Alpine_D") ~ "Apline",
#       str_detect(v1 , "Sandy") ~ "Sandy",
#       str_detect(v1, "FraserIsland") ~ "K'gari",
#       str_detect(v1, "Gibson_Desert") ~ "Gibson Desert",
#       str_detect(v1, "Kimberley") ~ "Kimberley",
#       str_detect(v1, "Northern_Queensland") ~ "Northern Queensland",
#       str_detect(v1, "Northern_Territory") ~ "Northern Territory",
#       str_detect(v1, "Simpson_Desert") ~ "Simpson Desert"
#     )
#   )
# 
# 
# admixture_k2_k11_dingoes_long <- admixture_k2_k11_dingoes %>%
#   dplyr::filter(pop %in% c("BIG", "CAP", "EAS", "SOU", "WES")) %>%
#   pivot_longer(cols = starts_with("dingo"), names_to = "k", values_to = "value") %>%
#   dplyr::mutate(k = gsub("dingo", "", k)) 

admixture_k2_k11_dingoes <- fread("admixture_k2_k12_20250611.csv") %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    type := case_when(
      str_detect(sample_name, "Din") ~ "WGS",
      .default = "SNP Array"
    ),
    population := case_when(
      str_detect(sample_name, "BIGDESERT") ~ "Mallee",
      str_detect(sample_name, "CAPTIVE") ~ "Captive",
      str_detect(sample_name, "EAST") ~ "East",
      str_detect(sample_name, "SOUTH") ~ "South",
      str_detect(sample_name, "WEST") ~ "West",
      str_detect(sample_name, "HYBRID") ~ "Dingo x Dog Hybrid",
      str_detect(sample_name, "Dingo_Alpine_Cooinda") ~ "Cooinda",
      str_detect(sample_name, "Alpine_D") ~ "Apline",
      str_detect(sample_name , "Sandy") ~ "Sandy",
      str_detect(sample_name, "FraserIsland") ~ "K'gari",
      str_detect(sample_name, "Gibson_Desert") ~ "Gibson Desert",
      str_detect(sample_name, "Kimberley") ~ "Kimberley",
      str_detect(sample_name, "Northern_Queensland") ~ "Northern Queensland",
      str_detect(sample_name, "Northern_Territory") ~ "Northern Territory",
      str_detect(sample_name, "Simpson_Desert") ~ "Simpson Desert",
      str_detect(sample_name, "Curracurrang") ~ "Curracurrang",
      str_detect(sample_name, "Nullarbor") ~ "Nullarbor",
      .default = "Dog"
    )
  ) %>%
  dplyr::filter(population != "Dog") 

admixture_k2_k11_dingoes_long <- admixture_k2_k11_dingoes %>%
  pivot_longer(cols = starts_with("dingo"), names_to = "k", values_to = "value") %>%
  dplyr::mutate(k = as.numeric(gsub("dingo", "", k)))

##########
###### qpAdm for Cairns and WGS samples
##########
qpadm_wgs <- fread("wgs_qpadm_best_model.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-model) %>% ### Has extra column 
  dplyr::mutate(snp_used = "1.9M") 

qpadm_snp_array <- fread("snp_array_qpadm_best_model.csv") %>%
  janitor::clean_names() %>%
  dplyr::mutate(snp_used = "46K")


qpadm_results_wgs_snp <- rbind(qpadm_wgs, qpadm_snp_array) %>%
  dplyr::mutate(
    dingo_ancient_curracurrang := case_when(
      is.na(dingo_ancient_curracurrang) ~ 0,
      .default = dingo_ancient_curracurrang
    ),
    dingo_ancient_nullarbor := case_when(
      is.na(dingo_ancient_nullarbor) ~ 0,
      .default = dingo_ancient_nullarbor
    ),
    se_dingo_ancient_curracurrang := case_when(
      is.na(se_dingo_ancient_curracurrang) ~ 0,
      .default = se_dingo_ancient_curracurrang
    ),
    se_dingo_ancient_nullarbor := case_when(
      is.na(se_dingo_ancient_nullarbor) ~ 0,
      .default = se_dingo_ancient_nullarbor
    ),
    dingo_ancestry = dingo_ancient_curracurrang + dingo_ancient_nullarbor,
    dingo_ancestry_se = se_dingo_ancient_curracurrang + se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, source, right, snp_used)

qpadm_results_wgs_snp_meta <- left_join(qpadm_results_wgs_snp, coords, by=join_by("target"=="sample_name"))

qpadm_results_wgs_snp_meta_simple <- qpadm_results_wgs_snp_meta %>% 
  dplyr::filter(snp_used=="46K") %>%
  dplyr::select(target, dingo_ancestry)

#########
###### Outgroup f4 for Cairns and WGS samples
#########
f4_affinity <- fread("f4_affinity_20250609.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename(sample_name = pop1) %>%
  #dplyr::mutate(
  #affinity = (ancient_dingo_curracurrang-ancient_dingo_nullarbor)/(ancient_dingo_curracurrang+ancient_dingo_nullarbor),
  #  sample_name = paste0(pop, "_", pop1)
  #) %>%
  left_join(., coords, by="sample_name")

f4_affinity_simple <- f4_affinity %>%
  dplyr::select(sample_name, est_dingo_ancient_nullarbor, est_dingo_ancient_curracurrang, affinity)

#########
### FEEMS things
#########
nodes <- read.csv('node_pos.csv')
edges <- read.csv('edges_pos.csv')
weights <- read.csv('weights.csv')
corrected <- read.csv('corrected_weights.csv')
obs_nodes <- read.csv('observed_nodes.csv')
sizes <- read.csv('size_observed_nodes.csv')

obs_nodes_sizes <- merge(obs_nodes, sizes)
edges_weights <- merge(edges, corrected)


#########
### tSNE things
#########
dd_7_names <- fread("kmeans_1989_250611.csv") %>%
  janitor::clean_names()

coords_ancient_fixed <- coords %>% 
  mutate(sample_name:= case_when(
    sample_name %in% c("Dingo_Ancient_Curracurrang", "Dingo_Ancient_Nullarbor") ~ paste0(sample_name, "_", sample_id),
    .default = sample_name),
    sample_name := case_when(
      sample_name == "Dingo_Ancient_Nullarbor_A19058_libmerged" ~ "Dingo_Ancient_Nullarbor_A19058",
      .default = sample_name
    )
  )

#dd_7_names_coords <- left_join(dd_7_names, coords_ancient_fixed, by="sample_name")

# Split coords and no coords samples
dd_7_w_coords <- dd_7_names %>%
  dplyr::filter(!is.na(latitude))

dd_7_no_coords <- dd_7_names %>%
  dplyr::filter(is.na(latitude)) %>%
  dplyr::arrange(desc(cluster7))

# Fake latitudes
latitudes <- c(rep(-37, 15), rep(-38, 15), rep(-39, 13)) 
# Fake longitudes
longitudes <- rep(seq(120, 134, length.out = 15), length.out = 43)

dd_7_no_coords$latitude <- latitudes
dd_7_no_coords$longitude <- longitudes

dd_7_w_fake_coords <- rbind(dd_7_w_coords, dd_7_no_coords) %>%
  dplyr::filter(longitude < 155) %>%
  dplyr::mutate(cluster7 = factor(cluster7, levels=c(7,6,5,4,3,2,1)))

dd_7_simple <- dd_7_names %>% 
  dplyr::select(sample_name,dingo_ancestry,cluster7)

#########
###### Admixture f4 for Cairns and WGS samples 
#########
f4_dog_admixture <- fread("../../arranged_folder/analyses/f4/f4_resutls.csv")
f4_dog_admixture_transversions <- fread("../../arranged_folder/analyses/f4/f4_resutls_transversions.csv")

#### Supp table pop structure summary
#dd_7_simple f4_affinity_simple admixture_proportions_simple
table_s3 <- left_join(dd_7_simple, f4_affinity_simple, by="sample_name") %>%
  left_join(., admixture_proportions_simple, by="sample_name")

write.csv(table_s3, "table_s3.csv", quote = F, row.names = F)

#########
#### Figure Asthetics
#########
ancient_modern_fills <- c("Modern" = "#f46036", "Ancient" = "#2e294e")

wgs_snp_colours <- c("WGS" = "black", "SNP Array" = "white")
wgs_snp_sizes <- c("WGS" = 5, "SNP Array" = 3)
wgs_snp_alpha <- c("WGS"= 1, "SNP Array"=0.2)
wgs_snp_shapes <- c("WGS" = 24, "SNP Array" = 21)
wgs_snp_anc_modern_shape <- c("WGS Ancient"=25, "WGS Modern"=24, "SNP Array Modern"=21)

snp_population_shape <- c(
  "Mallee"=15,
  "Captive"=16,
  "East"=17,
  "South"=18,
  "West"=19,
  "Dingo x Dog Hybrid"=20
)

snp_population_colour <- c(
  "Mallee"="#ff595e",
  "Captive"="#ff924c",
  "East"="#ffca3a",
  "South"="#8ac926",
  "West"="#1982c4",
  "Dingo x Dog Hybrid"="#6a4c93"
)

wgs_population_colours <- c(
  "Alpine" = "#000000",
  "Cooinda" = "darkorange",
  "Gibson Desert"="#254d49",
  "K'gari"="#841c26",
  "Kimberley"="#8aaec4",
  "Northern Queensland"="#bb9262",
  "Northern Territory"="#ffffff",
  "Sandy"="#4061ba",
  "Simpson Desert"="#e2c9a8"
)

cluster_pallete <- c(
  "1"="#8ac926",
  "2"="#ffca3a",
  "3"="#ff924c",
  "4"="#ff595e",
  "5"="#6a4c93",
  "6"="#1982c4",
  "7"="darkgreen"
)

#"#000000"     "#E69F00"     "#56B4E9"     "#009E73"     "#F0E442"     "#0072B2"     "#D55E00"     "#CC79A7"     "#999999" 

### Classic tableau colours



# dingo_ancestry_colour_list <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261","#e76f51")
# dingo_ancestry_colour_list <- c("#e63946","#f1faee","#a8dadc","#457b9d","#1d3557")
# dingo_ancestry_colour_list <- c("black", "#2a9d8f","#fcbf49","#f77f00", "#d62828")
dingo_ancestry_colour_list <- c("black", "#264653", "#2a9d8f", "#e9c46a", "#e76f51")
dingo_ancestry_colour_list_qpadm <- c("#264653", "#2a9d8f", "#e9c46a", "#e76f51")
