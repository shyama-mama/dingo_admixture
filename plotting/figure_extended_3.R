setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/data")

## Admixture CVs
cvs <- fread("admixture_k_cvs.tsv") %>%
  janitor::clean_names() %>%
  dplyr::filter(k < 18)

cv_plot <- ggplot(cvs, aes(x=k, y=cv)) +
  geom_line() +
  geom_point(aes(colour=as.factor(min)), shape=21, size=3, fill="white") +
  scale_colour_manual(values=c("1" = "red",
                               "0" = "black")) +
  guides(colour="none") +
  labs(y="Cross-validation error", x="K") + 
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16)) +
  theme_classic()
cv_plot

##### K trend
admixture_k2_k11_dingoes_long_for_plot <- admixture_k2_k11_dingoes_long %>%
  dplyr::filter(population %in% c("Captive", "Mallee", "East", "West", "South"))
admixture_k_trend <- ggplot(admixture_k2_k11_dingoes_long_for_plot, aes(x=as.numeric(k), y=value)) +
  geom_smooth(aes(group=population, colour=population, fill=population)) +
  labs(y="ADMIXTURE\nDingo Ancestry", x="K") +
  scale_fill_manual(values=snp_population_colour) + 
  scale_colour_manual(values=snp_population_colour) + 
  scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
admixture_k_trend

cv_plot | admixture_k_trend

##### Admixture for k=11
admixture_k_11 <- fread("admixture_k_11_20250611.tsv") %>%
  janitor::clean_names() %>%
  mutate(sample_name = sample) %>%
  separate(sample, into=c("population", "sample_id"), sep="_") %>%
  mutate(
    population := case_when(
      str_detect(sample_name, "Dingo_Alpine_Cooinda") ~ "Cooinda",
      str_detect(sample_name, "Alpine_D") ~ "Alpine",
      str_detect(sample_name , "Sandy") ~ "Sandy",
      str_detect(sample_name, "FraserIsland") ~ "K'gari",
      str_detect(sample_name, "Gibson_Desert") ~ "Gibson Desert",
      str_detect(sample_name, "Kimberley") ~ "Kimberley",
      str_detect(sample_name, "Northern_Queensland") ~ "Northern Queensland",
      str_detect(sample_name, "Northern_Territory") ~ "Northern Territory",
      str_detect(sample_name, "Simpson_Desert") ~ "Simpson Desert",
      str_detect(sample_name, "Curracurrang") ~ "Curracurrang 2k",
      str_detect(sample_name, "Nullarbor") ~ "Nullarbor 1k",
      .default = population
    )) %>%
  arrange(population, dingo1, dingo2, dingo3, dingo4, dingo5, dingo6, dingo7, dog8, dog9, dog10, dog11)

sample_order <- unique(admixture_k_11$sample_name)

admixture_k_11_as_factor <- admixture_k_11 %>%
  mutate(sample_order = factor(sample_name, levels=sample_order))

dog_pops <- c("BELS", "TURV", "BORD", "GSD", "PGOD", "KELPIE", "MIXED", "GOLD", "ACD", 
              "ASTCD", "LAB", "VIET")

snp_array_dingo <- c("WEST", "SOUTH", "EAST", "CAPTIVE", "HYBRID", "BIGDESERT")

wgs_dingo <- c("Cooinda", "Alpine", "Sandy", "K'gari", "Gibson Desert",
               "Kimberley", "Northern Queensland", "Northern Territory", "Simpson Desert",
               "Curracurrang 2k", "Nullarbor 1k")


#### using function
order_labels <- fread("admixture_labels_order.tsv", header=F, col.names = c("population", "order"))

pop<-read.table("02j_Cairns_with_ancient_modern_unrelated.fam") %>%
  mutate(sample_name = V1) %>%
  separate(V1, into=c("population", "sample_id"), sep="_") %>%
  mutate(
    population := case_when(
      str_detect(sample_name, "Dingo_Alpine_Cooinda") ~ "Cooinda",
      str_detect(sample_name, "Alpine_D") ~ "Alpine",
      str_detect(sample_name , "Sandy") ~ "Sandy",
      str_detect(sample_name, "FraserIsland") ~ "K'gari",
      str_detect(sample_name, "Gibson_Desert") ~ "Gibson Desert",
      str_detect(sample_name, "Kimberley") ~ "Kimberley",
      str_detect(sample_name, "Northern_Queensland") ~ "Northern Queensland",
      str_detect(sample_name, "Northern_Territory") ~ "Northern Territory",
      str_detect(sample_name, "Simpson_Desert") ~ "Simpson Desert",
      str_detect(sample_name, "Curracurrang") ~ "Curracurrang 2k",
      str_detect(sample_name, "Nullarbor") ~ "Nullarbor 1k",
      .default = population
    )) %>% 
  dplyr::select(population, sample_id) %>%
  left_join(., order_labels, by="population")
  
q<-read.table("02j_Cairns_with_ancient_modern_unrelated.11.Q",stringsAsFactors=T)



pallete <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928","#1B9E77","#999999")

# order according to population and plot the ADMIXTURE results
ord<-orderInds(pop = as.vector(pop$population), q = q)

q_w_meta <- cbind(pop, q)


## For Dogs
q_w_meta_dogs <- q_w_meta %>%
  filter(population %in% dog_pops)

q_dogs <- q_w_meta_dogs %>%
  dplyr::select(starts_with("V"))

pop_dogs <- q_w_meta_dogs %>%
  dplyr::select(population, order)

ord_dog <- orderInds(pop = as.vector(pop_dogs$order), q = q_dogs, popord=unique(sort(pop_dogs$order)))
png("../plots/dog_admixture.png", width=960, height=240)
plotAdmix(q_dogs,ord=ord_dog,pop=pop_dogs$population, colorpal=pallete, main=paste0("SNP Array Dogs (n=", nrow(pop_dogs), ")"), rotatelab=45)
dev.off()
## For Dingo SNP Array
q_w_meta_snp_dingo <- q_w_meta %>%
  filter(population %in% snp_array_dingo)

q_snp_dingo <- q_w_meta_snp_dingo %>%
  dplyr::select(starts_with("V"))

pop_snp_dingo <- q_w_meta_snp_dingo %>%
  dplyr::select(population, order)

ord_snp_dingo <- orderInds(pop = as.vector(pop_snp_dingo$order), q = q_snp_dingo, popord=unique(sort(pop_snp_dingo$order)))
png("../plots/snp_dingo_admixture.png", width=960, height=240)
plotAdmix(q_snp_dingo,ord=ord_snp_dingo,pop=pop_snp_dingo$population, colorpal=pallete, main=paste0("SNP Array Dingoes (n=", nrow(pop_snp_dingo), ")"), rotatelab=45)
dev.off()
## For WGS Dingo
q_w_meta_wgs_dingo <- q_w_meta %>%
  filter(population %in% wgs_dingo)

q_wgs_dingo <- q_w_meta_wgs_dingo %>%
  dplyr::select(starts_with("V"))

pop_wgs_dingo <- q_w_meta_wgs_dingo %>%
  dplyr::select(population, order)

ord_wgs_dingo <- orderInds(pop = as.vector(pop_wgs_dingo$order), q = q_wgs_dingo, popord=unique(sort(pop_wgs_dingo$order)))
png("../plots/wgs_dingo_admixture.png", width=960, height=240)
plotAdmix(q_wgs_dingo,ord=ord_wgs_dingo,pop=pop_wgs_dingo$population, colorpal=pallete, main=paste0("WGS Dingoes (n=", nrow(pop_wgs_dingo), ")"), rotatelab=45)
dev.off()

#plotAdmix_gg(q,ord=ord,pop=pop$population, colorpal=pallete, main="WGS Dingoes")

dog_admixture_p / dingo_admixture_p / wgs_dingo_admixture_p
