setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/data")

## Admixture CVs
cvs <- fread("cv_w_dog.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(k < 18) %>%
  mutate(
    min := case_when(
      k == 12 ~ 1,
      .default = 0
    )
  )
  

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
  dplyr::filter(population %in% c("Captive", "Mallee", "East", "West", "Alpine"))
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

ggsave("../w_dog_admixture.png", cv_plot | admixture_k_trend, dpi=600, height=4, width=11)

##### Admixture for k=11
admixture_k_11 <- fread("admixture_k_12_20260120.tsv") %>%
  janitor::clean_names() %>%
  left_join(., scarsbrook_metadata, by=join_by("sample_name" == "ena_id")) %>%
  mutate(
    sample_name := case_when(
      source == "Scarsbrook" ~ sample_id,
      .default = sample_name
    )
  ) %>%
  dplyr::select(-sample_id, -source) %>%
  mutate(sample = sample_name) %>%
  separate(sample, into=c("population", "sample_id"), sep="_") %>%
  left_join(., coords, by="sample_name") %>%
  mutate(
    population := case_when(
      species == "Dingo" ~ population.y,
      species == "Hybrid" ~ "Hybrid",
      .default = population.x
    ),
    population := case_when(
      str_detect(sample_name, "Nullarbor") ~ "Nullarbor 1k",
      str_detect(sample_name, "Curracurrang") ~ "Curracurrang 2k",
      .default = population
    ),
    data_type := case_when(
      str_detect(sample_name, "Nullarbor") ~ "WGS",
      str_detect(sample_name, "Curracurrang") ~ "WGS",
      .default = data_type
    )
  ) 

sample_order <- unique(admixture_k_11$sample_name)

admixture_k_11_as_factor <- admixture_k_11 %>%
  mutate(sample_order = factor(sample_name, levels=sample_order))

dog_pops <- c("BELS", "TURV", "BORD", "GSD", "PGOD", "KELPIE", "MIXED", "GOLD", "ACD", 
              "ASTCD", "LAB", "VIET")

snp_array_dingo <- c("West", "Alpine", "East", "Captive", "Mallee", "Dingo x Dog Hybrid")

wgs_dingo <- c("West", "Alpine", "East", "Captive", "Mallee", "Curracurrang 2k", "Nullarbor 1k")
pallete <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928","#1B9E77","#999999")

#### SNP Array
snp_array_dingoes <- admixture_k_11_as_factor %>% 
  filter(data_type == "SNP Array", species %in% c("Dingo", "Hybrid"))

q_dingos_snp_array <- setDT(
  snp_array_dingoes %>%
    dplyr::select(a1:a12))
write.table(q_dingos_snp_array, "q_dingos_snp_array.tsv", sep="\t", quote = F, row.names = F, col.names = F)
q_dingos_snp_array <- as.matrix(read.table("q_dingos_snp_array.tsv", header=F))


nrow(snp_array_dingoes)

ord_dingo_snp_array <- orderInds(pop = as.vector(snp_array_dingoes$population), q = q_dingos_snp_array, popord=snp_array_dingo)

png("../plots/snp_dingo_admixture.png", width=960, height=240)
plotAdmix(q_dingos_snp_array,ord=ord_dingo_snp_array,pop=snp_array_dingoes$population, colorpal=pallete, main=paste0("SNP-Array Dingoes"), rotatelab=45)
dev.off()


#### Dogs
snp_array_dogs <- admixture_k_11_as_factor %>% 
  filter(species == "Dog")

q_dogs_snp_array <- setDT(
  snp_array_dogs %>%
    dplyr::select(a1:a12))
write.table(q_dogs_snp_array, "q_dogs_snp_array.tsv", sep="\t", quote = F, row.names = F, col.names = F)
q_dogs_snp_array <- as.matrix(read.table("q_dogs_snp_array.tsv", header=F))


nrow(snp_array_dogs)

ord_dog_snp_array <- orderInds(pop = as.vector(snp_array_dogs$population), q = q_dogs_snp_array, popord=dog_pops)

png("../plots/snp_dog_admixture.png", width=960, height=240)
plotAdmix(q_dogs_snp_array,ord=ord_dog_snp_array,pop=snp_array_dogs$population, colorpal=pallete, main=paste0("SNP-Array Dogs"), rotatelab=45)
dev.off()

#### WGS Dingoes 
wgs_dingoes <- admixture_k_11_as_factor %>% 
  filter(data_type == "WGS")

q_dingo_wgs <- setDT(
  wgs_dingoes %>%
    dplyr::select(a1:a12))
write.table(q_dingo_wgs, "q_dingo_wgs.tsv", sep="\t", quote = F, row.names = F, col.names = F)
q_dingo_wgs <- as.matrix(read.table("q_dingo_wgs.tsv", header=F))


nrow(wgs_dingoes)

ord_dingo_wgs<- orderInds(pop = as.vector(wgs_dingoes$population), q = q_dingo_wgs, popord=wgs_dingo)

png("../plots/wgs_dingo_admixture.png", width=960, height=240)
plotAdmix(q_dingo_wgs,ord=ord_dingo_wgs,pop=wgs_dingoes$population, colorpal=pallete, main=paste0("WGS Dingoes"), rotatelab=45)
dev.off()




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




############################## Only Dingo ADMIXTURE
cvs <- fread("admixture_k_cvs_dingo_only.tsv", col.names = c("k", "cv")) %>%
  janitor::clean_names() %>%
  group_by(k) %>%
  summarise(
    cv_mean = mean(cv),
    cv_se = sd(cv) / sqrt(n())
  ) %>%
  mutate(
    min := case_when(
      k == 7 ~ 0,
      .default = 1
    )
  ) %>%
  filter(k < 10)

cv_plot <- ggplot(cvs, aes(x=k, y=cv_mean)) +
  geom_line() +
  geom_errorbar(aes(ymin=cv_mean-cv_se, ymax=cv_mean+cv_se), alpha=0.6, width=0.1) +
  geom_point(aes(colour=as.factor(min)), shape=21, size=3, fill="white") +
  guides(colour="none") +
  scale_colour_manual(values=c("0"="red", "1"="black")) + 
  labs(y="Cross-validation error", x="K") + 
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9)) +
  theme_classic()
cv_plot

ggsave("../figure_dingo_only_admixture.png", cv_plot, dpi=300)

k_translation <- data.frame(cluster_id = seq(1, 7),
                            pop_name = c("Mallee", "Alpine", "Central", "East", 
                                         "K'gari", "West", "Captive"))

manual_translation <- data.frame(manual_clusters = as.character(seq(1,8)),
                                pop_name_manual = c("Alpine", "Mallee", "Captive", "K'gari",
                                             "East", "North", "Central", "West"))

admixture_k_7_dingo_only <- fread("kmeans_260115_w_manual_cluster.csv") %>%
  mutate(
    manual_clusters := case_when(
    UMAP1 > 2 & UMAP1 < 3.4 & 
      UMAP2 > -10.83 & UMAP2 < -8.69 ~ "1",
    UMAP1 > 17.7 & UMAP1 < 18.3 & 
      UMAP2 > -3 & UMAP2 < -2.4 ~ "2",
    UMAP1 > 4.2 & UMAP1 < 5.3 & 
      UMAP2 > -1 & UMAP2 < 0.23 ~ "3",
    UMAP1 > -13.26 & UMAP1 < -13.18 & 
      UMAP2 > 3.7 & UMAP2 < 3.78 ~ "4",
    UMAP1 > -7 & UMAP1 < -1.3 & 
      UMAP2 > -3.9 & UMAP2 < 3.1 ~ "5",
    UMAP1 > 7.3 & UMAP1 < 7.8 & 
      UMAP2 > 4.5 & UMAP2 < 5.1 ~ "6",
    UMAP1 > -4.21 & UMAP1 < -3.2 & 
      UMAP2 > 5.1 & UMAP2 < 5.5 ~ "7",
    UMAP1 > -1.6 & UMAP1 < 0.23 & 
      UMAP2 > 6.2 & UMAP2 < 9.2 ~ "8"
  )) %>%
  left_join(., k_translation, by=join_by("cluster7"=="cluster_id")) %>%
  left_join(., manual_translation, by="manual_clusters") %>%
  mutate(
    pop_name := case_when(
      str_detect(pop_id, "Curracurrang") ~ "Curracurrang 2k",
      str_detect(pop_id, "Nullarbor") ~ "Nullarbor 1k",
      .default = pop_name
    ),
    pop_name_manual := case_when(
      str_detect(pop_id, "Curracurrang") ~ "Curracurrang 2k",
      str_detect(pop_id, "Nullarbor") ~ "Nullarbor 1k",
      .default = pop_name_manual
    )
  ) %>%
  arrange(desc(a1), desc(a2), desc(a3), desc(a4), desc(a5), desc(a6), desc(a7))

write.csv(admixture_k_7_dingo_only, "kmeans_260115_w_manual_cluster_actually.csv", quote=F,
          row.names = F)

# admixture_k_8_dingo_only <- fread("admixture_k8_dingo_only.tsv") %>%
#   janitor::clean_names() %>%
#   left_join(., scarsbrook_conversion, by=join_by("sample_id"=="ena_id")) %>%
#   dplyr::mutate(sample_id := case_when(
#     str_detect(sample_id, "SAMN") ~ sample_id.y,
#     .default = sample_id
#   )) %>%
#   dplyr::select(-sample_id.y) %>%
#   left_join(., dd_7_names, by=join_by("sample_id"=="pop_id")) %>%
#   left_join(., k_translation, by=join_by("cluster8"=="cluster_id")) %>%
#   mutate(
#     pop_name := case_when(
#       age == "Ancient" ~ population,
#       .default = pop_name
#     )
#          )%>%
#   arrange(desc(c1), desc(c2), desc(c3), desc(c4), desc(c5), desc(c6), desc(c7), desc(c8))

sample_order <- admixture_k_7_dingo_only$pop_id

admixture_k_7_dingo_only_as_factor <- admixture_k_7_dingo_only %>%
  mutate(sample_order = factor(pop_id, levels=sample_order))


pop_order_cluster_7 = c("Captive", "K'gari", "East", "Alpine", "Mallee", "West", "Central",
              "Curracurrang 2k", "Nullarbor 1k")
pop_order_manual= c("Captive", "K'gari", "East", "Alpine", "Mallee", "West", "Central", "North",
                        "Curracurrang 2k", "Nullarbor 1k")

q_dingos <- setDT(admixture_k_7_dingo_only %>% dplyr::select(a1:a7))
write.table(q_dingos, "dingo_admixture_correct_order.tsv", sep="\t", quote = F, row.names = F, col.names = F)
q_dingos <- read.table("dingo_admixture_correct_order.tsv", header=F)

ord_dingo <- orderInds(pop = as.vector(admixture_k_7_dingo_only$pop_name_manual), q = q_dingos, popord=pop_order_manual)
#pallete <- c("#841c26","#ff924c","darkorange","#8ac926","#8aaec4","#1982c4","#ffca3a","#ff595e")
pallete <- c("#ff924c", "#ff595e", "#1982c4", "#8ac926", "#8aaec4", "#841c26", "#ffca3a")
#pallete <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
png("../dingo_only_admixture_manual.png", width=1080, height=400)
plotAdmix(q_dingos,ord=ord_dingo,pop=admixture_k_7_dingo_only$pop_name_manual, colorpal=pallete, main=paste0("Dingo ADMIXTURE"))
dev.off()


evaladmix <- as.matrix(read.table("Canis_190K_masked_extra_filtered_qc.7_evaladmix"))
original_order <- fread("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/arranged_folder/dingo/SNP_array/emu_pca/Canis_190K_masked_extra_filtered.fam",
                        header=F, col.names=c("pop_id", "sample_id", "mom", "dad", "sex", "case")) %>%
  dplyr::select(-c(mom, dad, sex,case)) %>%
  left_join(., scarsbrook_metadata, by=join_by("pop_id"=="ena_id")) %>%
  dplyr::mutate(
    pop_id := case_when(
      source == "Scarsbrook" ~ sample_id.y,
      str_detect(pop_id, "Curracurrang") ~ paste0(pop_id, "_", sample_id.x),
      str_detect(pop_id, "Nullarbor") ~ paste0(pop_id, "_", sample_id.x),
      .default = pop_id
    ),
    order = seq(1,length(pop_id),
    )
  ) %>%
  left_join(., admixture_k_7_dingo_only %>% dplyr::select(pop_id, pop_name, pop_name_manual, a1:a7), by="pop_id")



q_dingos_original <- setDT(original_order %>% dplyr::select(a1:a7))
write.table(q_dingos_original, "dingo_admixture_original_order.tsv", sep="\t", quote = F, row.names = F, col.names = F)
q_dingos_original <- as.matrix(read.table("dingo_admixture_original_order.tsv", header=F))


ord_dingo <- orderInds(pop = as.vector(original_order$pop_name_manual), q = q_dingos_original, popord=pop_order_manual)
plotCorRes(cor_mat = evaladmix, pop = as.vector(original_order$pop_name_manual), ord=ord_dingo, title="Evaluation of dingo ancestral components proportions with K=7")


