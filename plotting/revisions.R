source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")

####### compare qpAdm results b/w GSD and ECS
# qpadm_results_wgs_snp_meta 
qpadm_snp_gsd <- qpadm_results_wgs_snp_meta %>% 
  dplyr::filter(snp_used == "46K", !is.na(population)) %>%
  dplyr::mutate(dog_source = "German Shepherd") %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used, sample_id,
                longitude, latitude, age, data_type, population, age_type)

qpadm_wgs_gsd <- qpadm_results_wgs_snp_meta %>% 
  dplyr::filter(snp_used == "1.9M") %>%
  dplyr::mutate(dog_source = "German Shepherd") %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used, sample_id,
                longitude, latitude, age, data_type, population, age_type)

# files <- "qpadm_snp_array_cockerspaniel.csv" "qpadm_wgs_cockerspaniel.csv"
qpadm_snp_ecs <- fread("qpadm_snp_array_cockerspaniel.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K")

qpadm_snp_ecs_zhang <- fread("qpadm_snp_array_zhang_w_ec.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K")

qpadm_snp_ecs <- rbind(qpadm_snp_ecs, qpadm_snp_ecs_zhang)

qpadm_wgs_ecs <- fread("qpadm_wgs_cockerspaniel.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M")

qpadm_wgs_snp_ecs <- rbind(qpadm_snp_ecs, qpadm_wgs_ecs) %>%
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
  dog_source = "English Cocker Spaniel",
  dingo_ancestry = dingo_ancient_curracurrang + dingo_ancient_nullarbor,
  dingo_ancestry_se = se_dingo_ancient_curracurrang + se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used)

qpadm_wgs_ecs_simple <- qpadm_wgs_snp_ecs %>%
  dplyr::filter(snp_used == "1.9M")

qpadm_snp_ecs_simple <- qpadm_wgs_snp_ecs %>%
  dplyr::filter(snp_used == "46K")

qpadm_wgs_gsd_ecs_merge <- merge(qpadm_wgs_gsd, qpadm_wgs_ecs_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

qpadm_snp_gsd_ecs_merge <- merge(qpadm_snp_gsd, qpadm_snp_ecs_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

  
snp_qpadm_mean_avg_gsd_ecs <- ggplot(qpadm_snp_gsd_ecs_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_gsd_ecs_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_ecs_merge$diff) + 1.96 * sd(qpadm_snp_gsd_ecs_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_ecs_merge$diff) - 1.96 * sd(qpadm_snp_gsd_ecs_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_gsd_ecs_merge %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_ecs_merge %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & ECS)", y = "Difference between\nGSD & ECS") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("Populations", values=c(snp_population_colour)) + 
  guides(fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_gsd_ecs

qpadm_snp_gsd_ecs <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_gsd_ecs_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_gsd_ecs_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_gsd_ecs_merge %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_ecs_merge %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nECS") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_snp_gsd_ecs <- qpadm_snp_gsd_ecs / snp_qpadm_mean_avg_gsd_ecs
comparison_snp_gsd_ecs
### WGS GSD + ECS

wgs_qpadm_mean_avg_gsd_ecs <- ggplot(qpadm_wgs_gsd_ecs_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_ecs_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_ecs_merge$diff) + 1.96 * sd(qpadm_wgs_gsd_ecs_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_ecs_merge$diff) - 1.96 * sd(qpadm_wgs_gsd_ecs_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_gsd_ecs_merge, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & ECS)", y = "Difference between\nGSD & ECS") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_gsd_ecs

qpadm_wgs_gsd_ecs <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_gsd_ecs_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_gsd_ecs_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_gsd_ecs_merge, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nECS") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_wgs_gsd_ecs <- qpadm_wgs_gsd_ecs / wgs_qpadm_mean_avg_gsd_ecs

figure_extended_gsd_ecs <- ggarrange(comparison_snp_gsd_ecs, comparison_wgs_gsd_ecs, ncol = 2, common.legend = T, legend = "bottom", labels = c("A", "B"))

#comparison_snp_gsd_ecs | comparison_wgs_gsd_ecs
################ Labrodor
qpadm_snp_lab <- fread("qpadm_snp_lab.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K")


qpadm_wgs_lab <- fread("qpadm_wgs_lab.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M")

qpadm_wgs_snp_lab <- rbind(qpadm_snp_lab, qpadm_wgs_lab) %>%
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
    dog_source = "Labrador",
    dingo_ancestry = dingo_ancient_curracurrang + dingo_ancient_nullarbor,
    dingo_ancestry_se = se_dingo_ancient_curracurrang + se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used)

qpadm_wgs_lab_simple <- qpadm_wgs_snp_lab %>%
  dplyr::filter(snp_used == "1.9M")

qpadm_snp_lab_simple <- qpadm_wgs_snp_lab %>%
  dplyr::filter(snp_used == "46K")

qpadm_wgs_gsd_lab_merge <- merge(qpadm_wgs_gsd, qpadm_wgs_lab_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

qpadm_snp_gsd_lab_merge <- merge(qpadm_snp_gsd, qpadm_snp_lab_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)


snp_qpadm_mean_avg_gsd_lab <- ggplot(qpadm_snp_gsd_lab_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_gsd_lab_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_lab_merge$diff) + 1.96 * sd(qpadm_snp_gsd_lab_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_lab_merge$diff) - 1.96 * sd(qpadm_snp_gsd_lab_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_gsd_lab_merge %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_lab_merge %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & LAB)", y = "Difference between\nGSD & LAB") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("Populations", values=c(snp_population_colour)) + 
  guides(fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_gsd_lab

qpadm_snp_gsd_lab <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_gsd_lab_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_gsd_lab_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_gsd_lab_merge %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_lab_merge %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nLAB") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_snp_gsd_lab <- qpadm_snp_gsd_lab / snp_qpadm_mean_avg_gsd_lab
comparison_snp_gsd_lab
### WGS GSD + LAB

wgs_qpadm_mean_avg_gsd_lab <- ggplot(qpadm_wgs_gsd_lab_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_lab_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_lab_merge$diff) + 1.96 * sd(qpadm_wgs_gsd_lab_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_lab_merge$diff) - 1.96 * sd(qpadm_wgs_gsd_lab_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_gsd_lab_merge, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & LAB)", y = "Difference between\nGSD & LAB") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_gsd_lab

qpadm_wgs_gsd_lab <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_gsd_lab_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_gsd_lab_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_gsd_lab_merge, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nLAB") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_wgs_gsd_lab <- qpadm_wgs_gsd_lab / wgs_qpadm_mean_avg_gsd_lab

figure_extended_gsd_lab <- ggarrange(comparison_snp_gsd_lab, comparison_wgs_gsd_lab, ncol = 2, common.legend = T, legend = "bottom", labels = c("A", "B"))

################ Collie
qpadm_snp_collie <- fread("qpadm_snp_bordercollie.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K")


qpadm_wgs_collie <- fread("qpadm_wgs_collie.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M")

qpadm_wgs_snp_collie <- rbind(qpadm_snp_collie, qpadm_wgs_collie) %>%
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
    dog_source = "Collie",
    dingo_ancestry = dingo_ancient_curracurrang + dingo_ancient_nullarbor,
    dingo_ancestry_se = se_dingo_ancient_curracurrang + se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used)

qpadm_wgs_collie_simple <- qpadm_wgs_snp_collie %>%
  dplyr::filter(snp_used == "1.9M")

qpadm_snp_collie_simple <- qpadm_wgs_snp_collie %>%
  dplyr::filter(snp_used == "46K")

qpadm_wgs_gsd_collie_merge <- merge(qpadm_wgs_gsd, qpadm_wgs_collie_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

qpadm_snp_gsd_collie_merge <- merge(qpadm_snp_gsd, qpadm_snp_collie_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)


snp_qpadm_mean_avg_gsd_collie <- ggplot(qpadm_snp_gsd_collie_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_gsd_collie_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_collie_merge$diff) + 1.96 * sd(qpadm_snp_gsd_collie_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_collie_merge$diff) - 1.96 * sd(qpadm_snp_gsd_collie_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_gsd_collie_merge %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_collie_merge %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & COLLIE)", y = "Difference between\nGSD & COLLIE") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("Populations", values=c(snp_population_colour)) + 
  guides(fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_gsd_collie

qpadm_snp_gsd_collie <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_gsd_collie_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_gsd_collie_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_gsd_collie_merge %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_collie_merge %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nCOLLIE") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_snp_gsd_collie <- qpadm_snp_gsd_collie / snp_qpadm_mean_avg_gsd_collie
comparison_snp_gsd_collie
### WGS GSD + COLLIE

wgs_qpadm_mean_avg_gsd_collie <- ggplot(qpadm_wgs_gsd_collie_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_collie_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_collie_merge$diff) + 1.96 * sd(qpadm_wgs_gsd_collie_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_collie_merge$diff) - 1.96 * sd(qpadm_wgs_gsd_collie_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_gsd_collie_merge, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & COLLIE)", y = "Difference between\nGSD & COLLIE") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_gsd_collie

qpadm_wgs_gsd_collie <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_gsd_collie_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_gsd_collie_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_gsd_collie_merge, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nCOLLIE") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_wgs_gsd_collie <- qpadm_wgs_gsd_collie / wgs_qpadm_mean_avg_gsd_collie

figure_extended_gsd_collie <- ggarrange(comparison_snp_gsd_collie, comparison_wgs_gsd_collie, ncol = 2, common.legend = T, legend = "bottom", labels = c("A", "B"))

################ Terrier
qpadm_snp_terrier <- fread("qpadm_snp_scottterrier.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K")


qpadm_wgs_terrier <- fread("qpadm_wgs_terrier.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M")

qpadm_wgs_snp_terrier <- rbind(qpadm_snp_terrier, qpadm_wgs_terrier) %>%
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
    dog_source = "Terrier",
    dingo_ancestry = dingo_ancient_curracurrang + dingo_ancient_nullarbor,
    dingo_ancestry_se = se_dingo_ancient_curracurrang + se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used)

qpadm_wgs_terrier_simple <- qpadm_wgs_snp_terrier %>%
  dplyr::filter(snp_used == "1.9M")

qpadm_snp_terrier_simple <- qpadm_wgs_snp_terrier %>%
  dplyr::filter(snp_used == "46K")

qpadm_wgs_gsd_terrier_merge <- merge(qpadm_wgs_gsd, qpadm_wgs_terrier_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

qpadm_snp_gsd_terrier_merge <- merge(qpadm_snp_gsd, qpadm_snp_terrier_simple, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)


snp_qpadm_mean_avg_gsd_terrier <- ggplot(qpadm_snp_gsd_terrier_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_gsd_terrier_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_terrier_merge$diff) + 1.96 * sd(qpadm_snp_gsd_terrier_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_gsd_terrier_merge$diff) - 1.96 * sd(qpadm_snp_gsd_terrier_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_gsd_terrier_merge %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_terrier_merge %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & TERRIER)", y = "Difference between\nGSD & TERRIER") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("Populations", values=c(snp_population_colour)) + 
  guides(fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_gsd_terrier

qpadm_snp_gsd_terrier <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_gsd_terrier_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_gsd_terrier_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_gsd_terrier_merge %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_gsd_terrier_merge %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nTERRIER") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_snp_gsd_terrier <- qpadm_snp_gsd_terrier / snp_qpadm_mean_avg_gsd_terrier
comparison_snp_gsd_terrier
### WGS GSD + TERRIER

wgs_qpadm_mean_avg_gsd_terrier <- ggplot(qpadm_wgs_gsd_terrier_merge, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_terrier_merge$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_terrier_merge$diff) + 1.96 * sd(qpadm_wgs_gsd_terrier_merge$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_gsd_terrier_merge$diff) - 1.96 * sd(qpadm_wgs_gsd_terrier_merge$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_gsd_terrier_merge, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(GSD & TERRIER)", y = "Difference between\nGSD & TERRIER") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_gsd_terrier

qpadm_wgs_gsd_terrier <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_gsd_terrier_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_gsd_terrier_merge %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_gsd_terrier_merge, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nGSD") +
  ylab("Dingo Ancestry\nTERRIER") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")


comparison_wgs_gsd_terrier <- qpadm_wgs_gsd_terrier / wgs_qpadm_mean_avg_gsd_terrier

figure_extended_gsd_terrier <- ggarrange(comparison_snp_gsd_terrier, comparison_wgs_gsd_terrier, ncol = 2, common.legend = T, legend = "bottom", labels = c("A", "B"))


####### All comparisons
all_dog_sources_qpadm <- rbind(qpadm_wgs_snp_terrier, qpadm_wgs_snp_collie, qpadm_wgs_snp_lab, qpadm_wgs_snp_ecs, rbind(qpadm_wgs_gsd,qpadm_snp_gsd) %>% dplyr::select(target, dingo_ancestry, dingo_ancestry_se, dog_source, snp_used)) %>%
  left_join(., rbind(qpadm_wgs_gsd,qpadm_snp_gsd) %>% dplyr::select(target, longitude, latitude, age, data_type, population, age_type), by="target") 

dog_source_plot <- ggplot(all_dog_sources_qpadm %>% filter(!is.na(population))) +
  geom_boxplot(aes(x=dog_source, y=1-dingo_ancestry, fill=population), outlier.alpha=.5) +
  facet_grid(~population) +
  scale_fill_manual(values=snp_population_colour) +
  scale_x_discrete(labels=c("BCOL", "ECS", "GSD", "LAB", "STER")) +
  labs(x="European dog source", y="Dog Ancestry") +
  guides(fill="none") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=90)
  )

all_dog_sources_qpadm %>%
  grouped_ggbetweenstats(., dog_source, dingo_ancestry, grouping.var = population,
                         pairwise.display = "ns")

ggsave("../figure_extended_dog_sources.png", dog_source_plot, dpi=300, width=10)

################ only Nulla
### snp, Zhang and scarsbrook
qpadm_snp_zhang_only_nulla <- fread("qpadm_snp_array_zhang_only_nulla.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K",
                dingo_ancestry = weight_dingo_ancient_nullarbor,
                dingo_ancestry_se = se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)
  
qpadm_snp_only_nulla <- fread("qpadm_snp_array_only_nulla.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K",
                dingo_ancestry = weight_dingo_ancient_nullarbor,
                dingo_ancestry_se = se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)

qpadm_snp_only_nulla_merged <- merge(qpadm_snp_gsd, rbind(qpadm_snp_zhang_only_nulla, qpadm_snp_only_nulla), by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)
  

snp_qpadm_mean_avg_only_nulla <- ggplot(qpadm_snp_only_nulla_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_only_nulla_merged$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_only_nulla_merged$diff) + 1.96 * sd(qpadm_snp_only_nulla_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_only_nulla_merged$diff) - 1.96 * sd(qpadm_snp_only_nulla_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_only_nulla_merged %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_only_nulla_merged %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(Both & Only Nullarbor)", y = "Difference between\nBoth & Only Nullarbor") +
  scale_colour_manual("SNP-Array Populations", values=snp_population_colour) + 
  scale_fill_manual("WGS Populations", values=c(wgs_population_colours, snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.15, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_only_nulla

qpadm_snp_only_nulla_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_only_nulla_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_only_nulla_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_only_nulla_merged %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_only_nulla_merged %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nBoth") +
  ylab("Dingo Ancestry\nOnly Nullarbor") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_snp_only_nulla_plot

snp_only_nulla_comparison <- qpadm_snp_only_nulla_plot / snp_qpadm_mean_avg_only_nulla
snp_only_nulla_comparison

### wgs and scarsbrook
qpadm_wgs_only_nulla <- fread("qpadm_wgs_only_nulla.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M",
                dingo_ancestry = weight_dingo_ancient_nullarbor,
                dingo_ancestry_se = se_dingo_ancient_nullarbor) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)

qpadm_wgs_gsd

qpadm_wgs_only_nulla_merged <- merge(qpadm_wgs_gsd, qpadm_wgs_only_nulla, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)
  


wgs_qpadm_mean_avg_only_nulla <- ggplot(qpadm_wgs_only_nulla_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_only_nulla_merged$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_only_nulla_merged$diff) + 1.96 * sd(qpadm_wgs_only_nulla_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_only_nulla_merged$diff) - 1.96 * sd(qpadm_wgs_only_nulla_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_only_nulla_merged, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(Both & Only Nullarbor)", y = "Difference between\nBoth & Only Nullarbor") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.15, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_only_nulla

qpadm_wgs_only_nulla_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_only_nulla_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_only_nulla_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_only_nulla_merged, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nBoth Dingo Ancestries") +
  ylab("Dingo Ancestry\nOnly Nullarbor") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_wgs_only_nulla_plot

wgs_only_nulla_comparison <- qpadm_wgs_only_nulla_plot / wgs_qpadm_mean_avg_only_nulla

snp_only_nulla_comparison | wgs_only_nulla_comparison

###### Only Curra
qpadm_snp_zhang_only_curra <- fread("qpadm_snp_array_zhang_only_curra.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K",
                dingo_ancestry = weight_dingo_ancient_curracurrang,
                dingo_ancestry_se = se_dingo_ancient_curracurrang) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)

qpadm_snp_only_curra <- fread("qpadm_snp_array_only_curra.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "46K",
                dingo_ancestry = weight_dingo_ancient_curracurrang,
                dingo_ancestry_se = se_dingo_ancient_curracurrang) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)

qpadm_snp_only_curra_merged <- merge(qpadm_snp_gsd, rbind(qpadm_snp_zhang_only_curra, qpadm_snp_only_curra), by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)


snp_qpadm_mean_avg_only_curra <- ggplot(qpadm_snp_only_curra_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_snp_only_curra_merged$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_snp_only_curra_merged$diff) + 1.96 * sd(qpadm_snp_only_curra_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_snp_only_curra_merged$diff) - 1.96 * sd(qpadm_snp_only_curra_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_snp_only_curra_merged %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_only_curra_merged %>% filter(data_type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(Both & Only Curracurrang)", y = "Difference between\nBoth & Only Curracurrang") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(fill = "none",) + 
  scale_y_continuous(limits=c(-0.15, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mean_avg_only_curra

qpadm_snp_only_curra_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_snp_only_curra_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_snp_only_curra_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_snp_only_curra_merged %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  #geom_point(data=qpadm_snp_only_curra_merged %>% dplyr::filter(data_type=="WGS" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nBoth") +
  ylab("Dingo Ancestry\nOnly Curracurrang") + 
  scale_colour_manual("Population", values=snp_population_colour) + 
  scale_fill_manual("Population", values=c( snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_snp_only_curra_plot

snp_only_curra_comparison <- qpadm_snp_only_curra_plot / snp_qpadm_mean_avg_only_curra
snp_only_curra_comparison

### wgs and scarsbrook
qpadm_wgs_only_curra <- fread("qpadm_wgs_only_curra.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-sum_above_3) %>%
  dplyr::mutate(snp_used = "1.9M",
                dingo_ancestry = weight_dingo_ancient_curracurrang,
                dingo_ancestry_se = se_dingo_ancient_curracurrang) %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, snp_used)

qpadm_wgs_gsd

qpadm_wgs_only_curra_merged <- merge(qpadm_wgs_gsd, qpadm_wgs_only_curra, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)



wgs_qpadm_mean_avg_only_curra <- ggplot(qpadm_wgs_only_curra_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(qpadm_wgs_only_curra_merged$diff), color = "red") +
  geom_hline(yintercept = mean(qpadm_wgs_only_curra_merged$diff) + 1.96 * sd(qpadm_wgs_only_curra_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(qpadm_wgs_only_curra_merged$diff) - 1.96 * sd(qpadm_wgs_only_curra_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=qpadm_wgs_only_curra_merged, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(Both & Only Curracurrang)", y = "Difference between\nBoth & Only Curracurrang") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.15, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mean_avg_only_curra

qpadm_wgs_only_curra_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbar(data=qpadm_wgs_only_curra_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=qpadm_wgs_only_curra_merged %>% filter(!is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6) + 
  geom_point(data=qpadm_wgs_only_curra_merged, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nBoth Dingo Ancestries") +
  ylab("Dingo Ancestry\nOnly Curracurrang") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  guides(fill="none") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_wgs_only_curra_plot

wgs_only_curra_comparison <- qpadm_wgs_only_curra_plot / wgs_qpadm_mean_avg_only_curra

snp_only_curra_comparison | wgs_only_curra_comparison

figure_extended_nulla_curra <- ggarrange(snp_only_nulla_comparison, wgs_only_nulla_comparison, snp_only_curra_comparison, wgs_only_curra_comparison, 
          ncol = 4, common.legend = T, legend = "bottom", labels = c("A", "B", "C", "D"))


######### MOSAIC Results
# WGS Samples
mosaic_wgs_samples <- fread("mosaic_wgs_dingoes.csv")
mosaic_wgs_samples_w_curra <- fread("mosaic_wgs_dingoes_w_curra.csv")

mosaic_wgs_samples_merged <- merge(qpadm_wgs_gsd,mosaic_wgs_samples, by.y="sample", by.x="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

wgs_qpadm_mosaic_mean_avg <- ggplot(mosaic_wgs_samples_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(mosaic_wgs_samples_merged$diff), color = "red") +
  geom_hline(yintercept = mean(mosaic_wgs_samples_merged$diff) + 1.96 * sd(mosaic_wgs_samples_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(mosaic_wgs_samples_merged$diff) - 1.96 * sd(mosaic_wgs_samples_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=mosaic_wgs_samples_merged, aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(qpAdm & MOSAIC)", y = "Difference between\nqpAdm & MOSAIC") +
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(colour = "none", fill = "none") + 
  scale_y_continuous(limits=c(-0.2, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
wgs_qpadm_mosaic_mean_avg

qpadm_mosaic_wgs_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbarh(data=mosaic_wgs_samples_merged, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se, xmax=dingo_ancestry.x+dingo_ancestry_se), colour="grey", alpha=0.6) + 
  geom_point(data=mosaic_wgs_samples_merged, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nqpAdm") +
  ylab("Dingo Ancestry\nMOSAIC") + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_mosaic_wgs_plot

wgs_qpadm_mosaic_comparison <- qpadm_mosaic_wgs_plot / wgs_qpadm_mosaic_mean_avg
wgs_qpadm_mosaic_comparison

# SNP Array
mosaic_snp_samples <- fread("mosaic_snp_dingoes.csv")
mosaic_snp_samples_w_curra <- fread("mosaic_snp_dingoes_w_curra.csv")

mosaic_snp_samples_merged <- merge(qpadm_snp_gsd,mosaic_snp_samples, by.y="sample", by.x="target") %>%
  dplyr::mutate(avg = (dingo_ancestry.x + dingo_ancestry.y)/2,
                diff = dingo_ancestry.x - dingo_ancestry.y)

snp_qpadm_mosaic_mean_avg <- ggplot(mosaic_snp_samples_merged, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(mosaic_snp_samples_merged$diff), color = "red") +
  geom_hline(yintercept = mean(mosaic_snp_samples_merged$diff) + 1.96 * sd(mosaic_snp_samples_merged$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(mosaic_snp_samples_merged$diff) - 1.96 * sd(mosaic_snp_samples_merged$diff), linetype = "dashed", color = "blue") +
  geom_point(data=mosaic_snp_samples_merged %>% filter(data_type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  labs(x = "Mean dingo ancestry\n(qpAdm & MOSAIC)", y = "Difference between\nqpAdm & MOSAIC") +
  scale_colour_manual("Populations", values=snp_population_colour) + 
  scale_fill_manual("WGS Populations", values=c(snp_population_colour)) + 
  guides(fill = "none") + 
  scale_y_continuous(limits=c(-0.2, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
snp_qpadm_mosaic_mean_avg

qpadm_mosaic_snp_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  geom_errorbarh(data=mosaic_snp_samples_merged, aes(y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se, xmax=dingo_ancestry.x+dingo_ancestry_se), colour="grey", alpha=0.6) + 
  geom_point(data=mosaic_snp_samples_merged %>% dplyr::filter(data_type=="SNP Array" & !is.na(population)), aes(x=dingo_ancestry.x, y=dingo_ancestry.y, colour=population), alpha=0.7, size=2) +
  xlab("Dingo Ancestry\nqpAdm") +
  ylab("Dingo Ancestry\nMOSAIC") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")
qpadm_mosaic_snp_plot

snp_qpadm_mosaic_comparison <- qpadm_mosaic_snp_plot / snp_qpadm_mosaic_mean_avg
snp_qpadm_mosaic_comparison
figure_extended_qpadm_mosaic <- ggarrange(snp_qpadm_mosaic_comparison, wgs_qpadm_mosaic_comparison, ncol = 2,
                                          common.legend = T, legend = "bottom", labels = c("A", "B"))

########## Save plots
qpadm_comparing_dogs_plot <- comparison_snp_gsd_ecs | comparison_wgs_gsd_ecs 

qpadm_comparing_single_and_dual_source <- snp_only_nulla_comparison | wgs_only_nulla_comparison | snp_only_curra_comparison | wgs_only_curra_comparison 

qpadm_to_mosaic <- snp_qpadm_mosaic_comparison | wgs_qpadm_mosaic_comparison 

ggsave("../figure_extended_dog_source.png", figure_extended_gsd_ecs, dpi=300, height=8, width=8)
ggsave("../figure_extended_dingo_source.png", figure_extended_nulla_curra, dpi=300, height=8, width=16)
ggsave("../figure_extended_qpadm_mosaic.png", figure_extended_qpadm_mosaic, dpi=300, height=8, width=8)


# We can potentially consider going -0.2 to 0.28 for all plots

#############
qpadm_simulations <- fread("qpadm_simulated.csv") %>%
  janitor::clean_names() %>%
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
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se)

  
  
simulations_metadata <- fread("simulation_metadata.tsv", header = F, 
                                    col.names = c("target", "sample", "num_snps", "replicate"))

qpadm_simulations_metadata <- merge(qpadm_simulations, simulations_metadata, by="target")

qpadm_simulations_metadata_merged <- merge(qpadm_simulations_metadata, qpadm_wgs_gsd, by.x="sample", by.y="target")

merged_ancestries <- qpadm_simulations_metadata_merged %>%
  filter(dingo_ancestry_se.x != 0) %>%
  group_by(sample, num_snps) %>%
  summarise(
    dingo_ancestry_mean = sum(dingo_ancestry.x / dingo_ancestry_se.x^2) / sum(1/dingo_ancestry_se.x^2),
    dingo_ancestry_mean_se = sqrt(1 / sum(1 / dingo_ancestry_se.x^2))
  )

merged_ancestries_w_original_qpadm <- merge(merged_ancestries, qpadm_wgs_gsd,  by.x="sample", by.y="target") 

qpadm_sim_plot <- merged_ancestries_w_original_qpadm %>%
  ggplot() +
  geom_errorbar(aes(x=dingo_ancestry, y=dingo_ancestry_mean, ymin=dingo_ancestry_mean-dingo_ancestry_mean_se, ymax=dingo_ancestry_mean+dingo_ancestry_mean_se), colour="grey", alpha=0.6) + 
  geom_errorbarh(aes(y=dingo_ancestry_mean, xmin=dingo_ancestry-dingo_ancestry_se, xmax=dingo_ancestry+dingo_ancestry_se), colour="grey", alpha=0.6, height=0) + 
  geom_abline(intercept = 0, linetype="dashed", alpha=0.7) +
  geom_point(aes(x=dingo_ancestry, y=dingo_ancestry_mean, fill=population), shape=24, alpha=0.9, size=4) +
  coord_cartesian(xlim=c(0.75,1), ylim=c(0.75,1)) +
  scale_x_continuous(breaks=c(0.8, 0.9, 1)) + 
  ylab("Dingo Ancestry\nqpAdm Reduced") +
  xlab("Dingo Ancestry\nqpAdm 1.2M SNPs") + 
  guides(fill="none") + 
  facet_grid(~num_snps, labeller = as_labeller(c('1000' = '1K SNPs',
                                                 '5000' = '5K SNPs',
                                                 '10000' = '10K SNPs',
                                                 '50000' = '50K SNPs',
                                                 '100000' = '100K SNPs'))) + 
  scale_y_continuous(breaks=c(0.8, 0.9, 1)) + 
  scale_fill_manual(values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),)

############ 
qpadm_simulations_wo_single <- fread("qpadm_simulated_wo_single_source.csv") %>%
  janitor::clean_names() %>%
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
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se)

qpadm_simulations_wo_single_metadata <- merge(qpadm_simulations_wo_single, simulations_metadata, by="target")

qpadm_simulations_wo_single_metadata_merged <- merge(qpadm_simulations_wo_single_metadata, qpadm_wgs_gsd, by.x="sample", by.y="target")

merged_ancestries_wo_single <- qpadm_simulations_wo_single_metadata_merged %>%
  filter(dingo_ancestry_se.x != 0) %>%
  group_by(sample, num_snps) %>%
  summarise(
    dingo_ancestry_mean = sum(dingo_ancestry.x / dingo_ancestry_se.x^2) / sum(1/dingo_ancestry_se.x^2),
    dingo_ancestry_mean_se = sqrt(1 / sum(1 / dingo_ancestry_se.x^2))
  )

merged_ancestries_w_original_qpadm_wo_single <- merge(merged_ancestries_wo_single, qpadm_wgs_gsd,  by.x="sample", by.y="target") 

qpadm_plot_wo_single <- merged_ancestries_w_original_qpadm_wo_single %>%
  ggplot() +
  geom_errorbar(aes(x=dingo_ancestry, y=dingo_ancestry_mean, ymin=dingo_ancestry_mean-dingo_ancestry_mean_se, ymax=dingo_ancestry_mean+dingo_ancestry_mean_se), colour="grey", alpha=0.6) + 
  geom_errorbarh(aes(y=dingo_ancestry_mean, xmin=dingo_ancestry-dingo_ancestry_se, xmax=dingo_ancestry+dingo_ancestry_se), colour="grey", alpha=0.6, height=0) + 
  geom_abline(intercept = 0, linetype="dashed", alpha=0.7) +
  geom_point(aes(x=dingo_ancestry, y=dingo_ancestry_mean, fill=population), shape=24, alpha=0.9, size=4) +
  coord_cartesian(xlim=c(0.75,1), ylim=c(0.75,1)) +
  scale_x_continuous(breaks=c(0.8, 0.9, 1)) + 
  ylab("Dingo Ancestry\nqpAdm Reduced") +
  xlab("Dingo Ancestry\nqpAdm 1.2M SNPs") + 
  facet_grid(~num_snps, labeller = as_labeller(c('1000' = '1K SNPs',
                                                 '5000' = '5K SNPs',
                                                 '10000' = '10K SNPs',
                                                 '50000' = '50K SNPs',
                                                 '100000' = '100K SNPs'))) + 
  scale_y_continuous(breaks=c(0.8, 0.9, 1)) + 
  scale_fill_manual("Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))

#simulations_summary_plot <- qpadm_sim_plot / qpadm_plot_wo_single

simulations_summary_plot <- ggarrange(qpadm_sim_plot, qpadm_plot_wo_single, nrow = 2, common.legend = T, legend = "bottom", labels = c("A", "B"))

ggsave("../figure_extended_simulations.png", simulations_summary_plot, dpi=300, height=8, width=16)


##### Intropolation for gen since admixture
mosaic_snp_gen_time <- fread("mosaic_snp_dingoes_gen_times.csv")
mosaic_wgs_gen_time <- fread("mosaic_wgs_dingoes_gen_times.csv")
mosaic_gen_time <- rbind(mosaic_snp_gen_time, mosaic_wgs_gen_time)
mosaic_ancestry <- rbind(mosaic_snp_samples %>% dplyr::select(-dog_ancestry), mosaic_wgs_samples)

gen_time_values <- merge(mosaic_gen_time, coords,by.x="sample", by.y="sample_name") #%>%
  dplyr::filter(latitude != "" & population %in% c("West", "Alpine", "East", "Mallee"))

gen_time_qpadm <- left_join(qpadm_snp_gsd, mosaic_gen_time, by=join_by("target"=="sample")) %>%
  left_join(., mosaic_ancestry, by=join_by("target"=="sample"))


gen_time_distribution_by_pop <- gen_time_qpadm %>% 
  filter(dingo_ancestry.x < 0.95, population %in% c("West", "Alpine", "East", "Mallee", "Captive")) %>%
  mutate(population = factor(population, levels = c("West", "Mallee", "Alpine", "East", "Captive"))) %>%
  ggplot(aes(x=population, y=gen_time*3, fill=population)) +
  geom_hline(yintercept=60, linetype = "dashed", alpha=0.7) + 
  geom_rain(point.args = list(aes(colour=population, shape=data_type, size=data_type, alpha=data_type)),
            violin.args = list(colour=NA)) +
  scale_fill_manual("Population", values=snp_population_colour) +
  scale_colour_manual("Population", values=snp_population_colour) +
  scale_shape_manual("Data Type", values=c("WGS"=24, "SNP Array"=16)) + 
  scale_size_manual("Data Type", values=c("WGS"=1, "SNP Array"=0.5)) + 
  scale_alpha_manual("Data Type", values=c("WGS"=1, "SNP Array"=0.7)) + 
  guides(colour="none", fill="none", shape=guide_legend(override.aes = list(shape=c(16,24), size=c(3,5)))) +
  ylab("Timing of admixture (years ago)") +
  xlab("Population") + 
  scale_y_continuous(breaks=c(0,15,30,60,90,120,150)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    legend.background = element_rect(fill="white",
                                     linewidth=0.3, colour = "black", linetype="solid")
  )

gen_time_distribution_by_pop_all <- ggplot(gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, population %in% c("West", "Alpine", "East", "Mallee", "Captive")), aes(x=after_stat(density), y=gen_time*3)) +
  geom_density() +
  geom_hline(yintercept=60, linetype = "dashed", alpha=0.7) +
  ylab("Timing of admixture (years ago)") +
  xlab("density") + 
  scale_y_continuous(breaks=c(0,15,30,60,90,120,150)) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    legend.background = element_rect(fill="white",
                                     linewidth=0.3, colour = "black", linetype="solid"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

gen_time_plot <- (gen_time_distribution_by_pop | gen_time_distribution_by_pop_all) + plot_layout(widths = c(1,0.1))
ggbetweenstats(gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, population %in% c("West", "Alpine", "East", "Mallee", "Captive")), population, gen_time,
               pairwise.display = "all")

#### Admixture b/w 21 - 25 generations ago (1950s) (none are significant...)
  

time_long <- ggplot() +
  #geom_hline(yintercept=25, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=50, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=75, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=100, linetype="dashed", alpha=0.5) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, latitude != "", data_type=="SNP Array", population %in% c("West", "Alpine", "East", "Mallee")), aes(x=longitude, y=gen_time, colour=population, size=data_type), alpha=0.8) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, latitude != "", data_type=="WGS", population %in% c("West", "Alpine", "East", "Mallee")), aes(x=longitude, y=gen_time, fill=population, size=data_type), shape=24) +
  scale_colour_manual(values=snp_population_colour) +
  scale_fill_manual(values=snp_population_colour) +
  scale_size_manual(values=wgs_snp_sizes) +
  guides(size="none", fill="none", colour="none") +
  xlab("Longitude") +
  ylab("Generations after admixture") + 
  scale_y_continuous(breaks=c(0,5,10,20,30,40,50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),)
time_long

time_lat <- ggplot() +
  #geom_hline(yintercept=25, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=50, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=75, linetype="dashed", alpha=0.5) +
  #geom_hline(yintercept=100, linetype="dashed", alpha=0.5) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, latitude != "", data_type=="SNP Array", population %in% c("West", "Alpine", "East", "Mallee")), aes(x=latitude, y=gen_time, colour=population, size=data_type), alpha=0.8) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, latitude != "", data_type=="WGS", population %in% c("West", "Alpine", "East", "Mallee")), aes(x=latitude, y=gen_time, fill=population, size=data_type), shape=24) +
  scale_colour_manual(values=snp_population_colour) +
  scale_fill_manual(values=snp_population_colour) +
  scale_size_manual(values=wgs_snp_sizes) +
  guides(size="none", fill="none", colour="none") +
  xlab("Latitude") +
  ylab("Generations after admixture") + 
  scale_y_continuous(breaks=c(0,5,10,20,30,40,50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),)
time_lat

time_qpadm <- ggplot() +
  #geom_vline(xintercept=25, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=50, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=75, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=100, linetype="dashed", alpha=0.5) +
  geom_smooth(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, population != "Dingo x Dog Hybrid"), aes(x=dingo_ancestry.x, y=gen_time), method="loess", linetype="dashed", se=T, colour="black", size=0.5) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, data_type=="SNP Array", population != "Dingo x Dog Hybrid"), aes(x=dingo_ancestry.x, y=gen_time, colour=population, size=data_type), alpha=0.8) +
  geom_point(data=gen_time_qpadm %>% filter(dingo_ancestry.x < 0.95, data_type=="WGS", population != "Dingo x Dog Hybrid"), aes(x=dingo_ancestry.x, y=gen_time, fill=population, size=data_type), shape=24) +
  scale_colour_manual(values=snp_population_colour) +
  scale_fill_manual(values=snp_population_colour) +
  scale_size_manual(values=wgs_snp_sizes) +
  guides(size="none", fill="none", colour="none") +
  xlab("Dingo Ancestry") +
  ylab("Generations after admixture") + 
  scale_y_continuous(breaks=c(0,5,10,20,30,40,50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),)
time_qpadm

# gen_time_plots <- (gen_time_distribution_by_pop |time_qpadm)/(time_long | time_lat)

gen_time_plots <- ggarrange(gen_time_distribution_by_pop, time_qpadm, time_long, time_lat, ncol=2,nrow=2, common.legend = T, legend="bottom", labels=c("A", "B", "C", "D"))
ggsave("../figure_extended_gen_times.png", gen_time_plots, dpi=300, height=8, width=10)


###### Diff b/w qpadm and admixture with gen times
admixture_diff <- admixture_vs_qpadm_df %>%
  dplyr::select(sample_name, diff)

head(gen_time_qpadm)

merged_admixture_diff <- merge(gen_time_qpadm, admixture_diff, by.x="target", by.y="sample_name") %>%
  filter(dingo_ancestry.x < 0.95, population != "Dingo x Dog Hybrid")
  

cutt_off <- 0.95

lm_fit <- lm(diff ~ gen_time, data = merged_admixture_diff %>% filter(dingo_ancestry.x < cutt_off))
pval <- summary(lm_fit)$coefficients["gen_time", "Pr(>|t|)"]
r2   <- summary(lm_fit)$r.squared
label <- paste0(
  "R² = ", round(r2, 3), "\n",
  "p = ", format.pval(pval, digits = 3)
)

difference_time_after_admixture <-ggplot() +
  geom_smooth(data=merged_admixture_diff %>% filter(dingo_ancestry.x < cutt_off), aes(y=gen_time, x=diff), method="lm", linetype="dashed", se=T, colour="black", size=0.5) +
  #geom_vline(xintercept=25, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=50, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=75, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=100, linetype="dashed", alpha=0.5) +
  geom_point(data=merged_admixture_diff %>% filter(data_type=="SNP Array", dingo_ancestry.x < cutt_off), aes(y=gen_time, x=diff, colour=population, size=data_type), alpha=0.8) +
  geom_point(data=merged_admixture_diff %>% filter(data_type=="WGS", dingo_ancestry.x < cutt_off), aes(y=gen_time, x=diff, fill=population, size=data_type), shape=24) +
  xlab(expression(Delta~"qpAdm & ADMIXTURE")) +
  ylab("Generations after admixture") + 
  scale_colour_manual(values=snp_population_colour) + 
  scale_fill_manual(values=snp_population_colour) + 
  scale_shape_manual(values=c("SNP Array"=16, "WGS"=24)) + 
  scale_size_manual(values=wgs_snp_sizes) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),) + 
  guides(colour="none", shape="none", size="none", fill="none") + 
  scale_y_continuous(breaks=c(0,5,10,20,30,40,50)) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = label,
    hjust = 1.1, vjust = 1.2,
    size = 4
  )

difference_time_after_admixture

difference_vs_qpadm <-ggplot() +
  geom_smooth(data=merged_admixture_diff %>% filter(dingo_ancestry.x < cutt_off), aes(y=dingo_ancestry.x, x=diff), method="lm", linetype="dashed", se=T, colour="black", size=0.5) +
  #geom_vline(xintercept=25, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=50, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=75, linetype="dashed", alpha=0.5) +
  #geom_vline(xintercept=100, linetype="dashed", alpha=0.5) +
  geom_point(data=merged_admixture_diff %>% filter(data_type=="SNP Array", dingo_ancestry.x < cutt_off), aes(y=dingo_ancestry.x, x=diff, colour=population, size=data_type), alpha=0.8) +
  geom_point(data=merged_admixture_diff %>% filter(data_type=="WGS", dingo_ancestry.x < cutt_off), aes(y=dingo_ancestry.x, x=diff, fill=population, size=data_type), shape=24) +
  xlab(expression(Delta~"qpAdm & ADMIXTURE")) +
  ylab("Generations after admixture") + 
  scale_colour_manual(values=snp_population_colour) + 
  scale_fill_manual(values=snp_population_colour) + 
  scale_shape_manual(values=c("SNP Array"=16, "WGS"=24)) + 
  scale_size_manual(values=wgs_snp_sizes) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),) + 
  guides(colour="none", shape="none", size="none", fill="none") + 
  annotate(
    "text",
    x = Inf, y = Inf,
    label = label,
    hjust = 1.1, vjust = 1.2,
    size = 4
  )
difference_vs_qpadm

supp_figure_gen_time <- ggarrange(time_long, time_lat, time_qpadm, difference_time_after_admixture, ncol = 2, nrow=2, labels = c("A", "B", "C", "D"))
ggsave("../figure_extended_gen_times_2.png", supp_figure_gen_time, dpi=300, height=8, width=10)

lm_fit_2 <- lm(dingo_ancestry.x ~ gen_time, data = merged_admixture_diff %>%
                 filter(dingo_ancestry.x < cutt_off))
pval_2 <- summary(lm_fit_2)$coefficients["gen_time", "Pr(>|t|)"]
r2_2   <- summary(lm_fit_2)$r.squared
pval_2
r2_2
#####################
### WGS samples qpAdm and SNP Array capture. 
head(qpadm_wgs_gsd)
head(qpadm_snp_gsd)

ind_both_seq <- merge(qpadm_wgs_gsd , qpadm_snp_gsd %>% filter(snp_used == "46K", data_type=="SNP Array"), by="sample_id") 

ind_both_seq_plot <- ggplot() +
  geom_abline(intercept = 0) + 
  geom_errorbar(data=ind_both_seq,aes(x=dingo_ancestry.x, y=dingo_ancestry.y, ymin=dingo_ancestry.y-dingo_ancestry_se.y, ymax=dingo_ancestry.y+dingo_ancestry_se.y), colour="grey", alpha=0.6) + 
  geom_errorbarh(data=ind_both_seq,aes(y=dingo_ancestry.y, xmin=dingo_ancestry.x-dingo_ancestry_se.x, xmax=dingo_ancestry.x+dingo_ancestry_se.x), colour="grey", alpha=0.6, height=0.008) + 
  geom_point(data=ind_both_seq, aes(x=dingo_ancestry.x, y=dingo_ancestry.y, fill=population.y), size=5, shape=21) +
  #geom_point(data=mosaic_same_inds, aes(x=mosaic_wgs, y=mosaic_snp, fill=population), size=5, shape=24) +
  coord_cartesian(xlim=c(0.75,1), ylim=c(0.75,1)) +
  scale_x_continuous(breaks=c(0.8, 0.9, 1)) + 
  scale_y_continuous(breaks=c(0.8, 0.9, 1)) + 
  xlab("WGS Data") +
  ylab("SNP-Array Data") + 
  #guides(fill="none") + 
  #coord_cartesian(xlim=c(0.6,1), ylim=c(0.6,1)) + 
  scale_fill_manual("Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),)
ind_both_seq_plot

mosaic_same_inds <- merge(mosaic_snp_samples_merged %>%
        dplyr::rename(mosaic_snp=dingo_ancestry.y) %>%
        dplyr::select(target, sample_id, population, mosaic_snp),
      mosaic_wgs_samples_merged %>%
        dplyr::rename(mosaic_wgs=dingo_ancestry.y) %>%
        dplyr::select(sample_id, mosaic_wgs), by="sample_id") 
  
ind_both_seq_plot_mosaic <- ggplot() +
  geom_abline(intercept = 0) + 
  geom_point(data=mosaic_same_inds, aes(x=mosaic_wgs, y=mosaic_snp, fill=population), size=5, shape=21) +
  coord_cartesian(xlim=c(0.75,1), ylim=c(0.75,1)) +
  scale_x_continuous(breaks=c(0.8, 0.9, 1)) + 
  scale_y_continuous(breaks=c(0.8, 0.9, 1)) + 
  xlab("WGS Data") +
  ylab("SNP-Array Data") + 
  #guides(fill="none") + 
  #coord_cartesian(xlim=c(0.6,1), ylim=c(0.6,1)) + 
  scale_fill_manual("Population", values=c(snp_population_colour)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),)
ind_both_seq_plot_mosaic

combined_plot <- ggarrange(ind_both_seq_plot,ind_both_seq_plot_mosaic, common.legend = T, legend = "bottom", labels=c("A", "B"))

ggsave("../figure_extended_same_ind.png", combined_plot, dpi=300, height=4, width=8)

#### MOSAIC both SNP and WGS inds




####### Affinity and Pops
merge(f4_affinity %>% dplyr::select(sample_name, affinity), dd_7_names, by.x="sample_name", by.y="pop_id") %>%
  filter(pop_name != "Captive") %>%
  ggplot(aes(x=pop_name, y=affinity, fill=pop_name)) +
  geom_rain(point.args = list(aes(colour=pop_name, shape=data_type))) +
  scale_colour_manual(values=cluster_pallete) +
  scale_fill_manual(values=cluster_pallete) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )



######################### Het plots
##### het
het_dingo_missingness <- fread("Canis_190K_masked_extra_filtered_miss.imiss") %>%
  janitor::clean_names() %>%
  left_join(., scarsbrook_conversion, by=join_by("fid"=="ena_id")) %>%
  dplyr::mutate(fid := case_when(
    str_detect(fid, "SAMN") ~ sample_id,
    .default = fid
  )) %>%
  dplyr::select(-sample_id, -iid,-miss_pheno) %>%
  left_join(., coords, by=join_by("fid"=="sample_name")) %>%
  left_join(., mosaic_ancestry, by=join_by("fid"=="sample")) #%>%
  #left_join(., rbind(qpadm_wgs_gsd, qpadm_snp_gsd), by=join_by("fid"=="target")) 
  

missing_ancestry_plot <- ggplot(het_dingo_missingness, aes(x=dingo_ancestry, y=1-f_miss)) + 
  geom_abline(intercept = 0, linetype="dashed") +
  geom_point(data=het_dingo_missingness %>% filter(data_type == "SNP Array"), aes(colour=population), size=3, alpha=0.7) +
  geom_point(data=het_dingo_missingness %>% filter(data_type == "WGS"), aes(fill=population), size=4, shape=24) +
  scale_colour_manual(values=snp_population_colour) +
  scale_fill_manual(values=snp_population_colour) +
  scale_shape_manual(values=wgs_snp_shapes)+
  labs(x="MOSAIC Dingo Ancestry", y="fraction SNPs retained") + 
  guides(fill="none", shape="none") + 
  coord_cartesian(xlim=c(0.5,1), ylim=c(0.5,1)) + 
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

ggsave("../figure_extended_missing_ancestry.png", missing_ancestry_plot, dpi=300)

het_dingo_only <- fread("Canis_190K_masked_extra_filtered_het.het") %>%
  janitor::clean_names() %>%
  left_join(., scarsbrook_conversion, by=join_by("fid"=="ena_id")) %>%
  dplyr::mutate(fid := case_when(
    str_detect(fid, "SAMN") ~ sample_id,
    .default = fid
  )) %>%
  dplyr::select(-sample_id, -iid) %>%
  left_join(., dd_pop_names_merged, by=join_by("fid"=="pop_id")) %>%
  filter(age != "Ancient") %>% 
  left_join(., qpadm_results_wgs_snp_meta %>% filter(!(data_type == "WGS" & snp_used == "46K")) %>% dplyr::select(target, dingo_ancestry, dingo_ancestry_se), by=join_by("fid"=="target")) %>%
  left_join(., het_dingo_missingness %>% dplyr::select(fid, n_miss, n_geno, f_miss), by="fid") %>%
  mutate(f_het = (n_nm - o_hom)/n_nm)

het_dingo_dog <- fread("Canis_190K_subset_to_pca_not_masked_het.het") %>%
  janitor::clean_names() %>%
  left_join(., scarsbrook_conversion, by=join_by("fid"=="ena_id")) %>%
  dplyr::mutate(fid := case_when(
    str_detect(fid, "SAMN") ~ sample_id,
    .default = fid
  )) %>%
  mutate(f_het = (n_nm - o_hom)/n_nm) %>%
  dplyr::select(fid, f_het) %>%
  dplyr::rename(f_het_w_dog = f_het) %>%
  left_join(het_dingo_only, ., by=join_by("fid"=="fid")) 

het_dingo_only_plot_artefact <- het_dingo_only %>%
  ggplot(aes(x=pop_name, y=f_miss, fill=pop_name)) +
  geom_rain(point.args = list(aes(shape=data_type, colour="black"))) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_colour_manual(values=cluster_pallete) + 
  scale_size_manual(values=wgs_snp_sizes) + 
  ylab("n_miss") +
  xlab("Population Name") + 
  scale_shape_manual(values=wgs_snp_shapes) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

het_dingo_only_plot_ancestry <- het_dingo_only %>%
  mutate(pop_name_manual = factor(pop_name_manual, levels=c("Mallee", "K'gari", "East", "Alpine", "Captive", "West", "North", "Central"))) %>%
  ggplot(aes(x=pop_name_manual, y=dingo_ancestry, fill=pop_name_manual)) +
  geom_rain(point.args = list(aes(shape=data_type, colour="black"))) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_colour_manual(values=cluster_pallete) + 
  scale_size_manual(values=wgs_snp_sizes) + 
  ylab("Dingo Ancestry") +
  xlab("Population Name") + 
  scale_shape_manual(values=wgs_snp_shapes) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

ggplotly(het_dingo_only %>%
  ggplot(aes(x=dingo_ancestry, y=1-f_miss)) +
    geom_abline(intercept = 0) +
  geom_point(aes(colour=as.factor(pop_name), label=fid))+
  scale_colour_manual(values=cluster_pallete))



het_dingo_only_plot <- het_dingo_only %>%
  mutate(pop_name_manual = factor(pop_name_manual, levels=c("Mallee", "K'gari", "East", "Alpine", "Captive", "West", "North", "Central"))) %>%
  ggplot(aes(x=pop_name_manual, y=f_het, fill=as.factor(pop_name_manual))) +
  geom_rain(point.args = list(aes(shape=data_type, colour=data_type, size=data_type, alpha=data_type)),
            violin.args = list(colour=NA, scale="width")) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_colour_manual(values=c("WGS"="black", "SNP Array"=NA)) + 
  scale_size_manual(values=c("WGS"=3, "SNP Array"=2)) + 
  scale_alpha_manual(values=c("WGS"=1, "SNP Array"=0.7)) + 
  ylab("Proportion of\nObserved Heterozygosity") +
  xlab("Population Name") + 
  scale_shape_manual(values=wgs_snp_shapes) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


diversity_fence <- left_join(het_dingo_dog, admixture_summary_with_meta %>% dplyr::select(samples_name, south_of_fence), by=join_by("fid"=="samples_name")) %>%
  filter(south_of_fence != "", !pop_name_manual %in% c("Captive", "K'gari")) %>%
  ggplot(aes(x=south_of_fence, y=f_het, colour = south_of_fence, fill = south_of_fence)) +
  geom_rain(alpha = 0.5, point.args = list(alpha=0.1)) +
  labs(
    title = "After masking European dog admixture",
    x = "", 
    y = "Proportion of\nObserved Heterozygosity") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_x_discrete(breaks=c("FALSE", "TRUE"), labels=c("North of\nDingo Fence", "South of\nDingo Fence"), expand=c(0,0))+
  scale_colour_manual("Within Fence", values = c("FALSE" = "black", "TRUE" = "red")) +
  # Add the comparison bar
  geom_signif(comparisons = list(c("FALSE", "TRUE")), 
              test = "wilcox.test",
              map_signif_level = function(p) sprintf("p = %.2g", p), 
              y_position = 0.25) + # Adjust y_position as needed
  theme_bw() +
  guides(fill="none", colour="none") +
  theme(legend.position = "right",
        axis.line.x=element_blank(),
        #axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12))

diversity_fence_w_dog <- left_join(het_dingo_dog, admixture_summary_with_meta %>% dplyr::select(samples_name, south_of_fence), by=join_by("fid"=="samples_name")) %>%
  filter(south_of_fence != "", !pop_name_manual %in% c("Captive", "K'gari")) %>%
  ggplot(aes(x=south_of_fence, y=f_het_w_dog, colour = south_of_fence, fill = south_of_fence)) +
  geom_rain(alpha = 0.5, point.args = list(alpha=0.1)) +
  labs(
    title = "With European dog admixture",
    x = "", 
    y = "Proportion of\nObserved Heterozygosity") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_x_discrete(breaks=c("FALSE", "TRUE"), labels=c("North of\nDingo Fence", "South of\nDingo Fence"), expand=c(0,0))+
  scale_colour_manual("Within Fence", values = c("FALSE" = "black", "TRUE" = "red")) +
  # Add the comparison bar
  geom_signif(comparisons = list(c("FALSE", "TRUE")), 
              test = "wilcox.test",
              map_signif_level = function(p) sprintf("p = %.2g", p), 
              y_position = 0.25) + # Adjust y_position as needed
  theme_bw() +
  guides(fill="none", colour="none") +
  theme(legend.position = "right",
        axis.line.x=element_blank(),
        #axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12))

sof <- left_join(het_dingo_only, admixture_summary_with_meta %>% dplyr::select(samples_name, south_of_fence), by=join_by("fid"=="samples_name")) %>% 
  filter(south_of_fence=="TRUE")

nsof <- left_join(het_dingo_only, admixture_summary_with_meta %>% dplyr::select(samples_name, south_of_fence), by=join_by("fid"=="samples_name")) %>% 
  filter(south_of_fence=="FALSE")

t.test(sof$f,nsof$f)

het_dingo_only %>%
  filter(latitude != "", pop_name_manual=="East") %>%
  ggplot(aes(y=f, x=latitude)) +
  geom_point(aes(colour=geo_clusters)) +
  geom_smooth(method="lm")


ggbetweenstats(het_dingo_only, geo_clusters, f)


het_dingo_dog_plot <- het_dingo_dog %>%
  ggplot(aes(x=pop_name_manual, y=f_het_w_dog, fill=as.factor(pop_name_manual))) +
  geom_rain(point.args = list(aes(shape=data_type, colour="black"))) +
  #scale_fill_manual(values=cluster_pallete) + 
  #scale_colour_manual(values=cluster_pallete) + 
  scale_size_manual(values=wgs_snp_sizes) + 
  ylab("Fhom") +
  xlab("Population Name") + 
  scale_shape_manual(values=wgs_snp_shapes) + 
  theme_bw() +
  #coord_cartesian(ylim=c(0, 0.25)) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


##### Het and admixture
hey_all_segs <- lm(f_het_w_dog ~ dingo_ancestry, data=het_dingo_dog)
summary(hey_all_segs)
pval_all_segs <- summary(hey_all_segs)$coefficients["dingo_ancestry", "Pr(>|t|)"]
r2_all_segs   <- summary(hey_all_segs)$r.squared
label_all_segs <- paste0(
  "R² = ", round(r2_all_segs, 3), "\n",
  "p = ", format.pval(pval_all_segs, digits = 3)
)


het_admixture_plot_all <- ggplot() +
  geom_smooth(data=het_dingo_dog, aes(x=dingo_ancestry, y=f_het_w_dog), method="lm", colour="black", linetype="dashed", size=0.5) +
  geom_point(data=het_dingo_dog %>% filter(data_type == "SNP Array"), aes(x=dingo_ancestry, y=f_het_w_dog, size=data_type, colour=as.factor(pop_name_manual))) + 
  geom_point(data=het_dingo_dog %>% filter(data_type == "WGS"), aes(x=dingo_ancestry, y=f_het_w_dog, shape=data_type, size=data_type, fill=as.factor(pop_name_manual))) +
  scale_shape_manual(values=wgs_snp_shapes) +
  scale_size_manual(values=wgs_snp_sizes) +
  #coord_cartesian(ylim=c(-0.4, 0.9)) + 
  #scale_colour_manual(values=cluster_pallete) +
  #scale_fill_manual(values=cluster_pallete) +
  theme_bw() +
  ylab("Fhet") +
  xlab("Dingo Ancestry") + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = label_all_segs,
    hjust = 1.1, vjust =-.5,
    size = 4
  )

hey_dingo_segs <- lm(f_het ~ dingo_ancestry, data=het_dingo_dog)
summary(hey_dingo_segs)
pval_dingo_segs <- summary(hey_dingo_segs)$coefficients["dingo_ancestry", "Pr(>|t|)"]
r2_dingo_segs   <- summary(hey_dingo_segs)$r.squared
label_dingo_segs <- paste0(
  "R² = ", round(r2_dingo_segs, 3), "\n",
  "p = ", format.pval(pval_dingo_segs, digits = 3)
)

het_admixture_plot_dingo_only <- ggplot() +
  geom_smooth(data=het_dingo_only, aes(x=dingo_ancestry, y=f_het), method="lm", colour="black", linetype="dashed", size=0.5) +
  geom_point(data=het_dingo_only %>% filter(data_type == "SNP Array"), aes(x=dingo_ancestry, y=f_het, size=data_type, colour=as.factor(pop_name_manual))) + 
  geom_point(data=het_dingo_only %>% filter(data_type == "WGS"), aes(x=dingo_ancestry, y=f_het, shape=data_type, size=data_type, fill=as.factor(pop_name_manual))) +
  scale_shape_manual(values=wgs_snp_shapes) +
  scale_size_manual(values=wgs_snp_sizes) +
  #coord_cartesian(ylim=c(-0.4, 0.9)) + 
  #scale_colour_manual(values=cluster_pallete) +
  #scale_fill_manual(values=cluster_pallete) +
  theme_bw() +
  ylab("Fhet") +
  xlab("Dingo Ancestry") + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = label_dingo_segs,
    hjust = 1.1, vjust = -.5,
    size = 4
  )

heterozygosity_summary <- ( het_dingo_dog_plot | het_dingo_only_plot ) / (het_admixture_plot_all | het_admixture_plot_dingo_only)
ggsave("../figure_extended_heterozygosity.png",heterozygosity_summary, dpi=300, height=7, width=12)


##### 
het_dingo_only_simpler <- het_dingo_only %>%   
  filter(latitude != "", !pop_name %in% c("Captive", "K'gari"), data_type == "SNP Array") %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(f_het, na.rm = TRUE)) %>%
  ungroup()

het_dingo_only_kgari <- het_dingo_only %>%
  filter(latitude != "", pop_name == "K'gari", data_type == "SNP Array") %>%
  group_by(longitude, latitude) %>%
  summarise(dingo_ancestry = mean(f_het, na.rm = TRUE)) %>%
  ungroup()

grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326)
grid_kgari_circle_sf <- st_as_sf(grid_kgari_circle, coords = c("longitude", "latitude"), crs = 4326)


grid_wo_kgari <- grid %>%
  filter(!(longitude > 152.95 & latitude > -25.8 & latitude < -24))


sf_use_s2(TRUE)
het_dingo_only_simpler_results <- plot_heat_map(het_dingo_only_simpler, st_as_sf(grid_wo_kgari, coords = c("longitude", "latitude"), crs = 4326), australia)  
het_dingo_only_simpler_results_kgari <- plot_heat_map(het_dingo_only_kgari, grid_kgari_circle %>% filter(longitude > 152.95 & longitude < 153.5 & latitude > -25.8 & latitude < -24.7), australia)

grid_kgari_circle[sample(nrow(grid_kgari_circle), 1000), ]

ggplotly(ggplot() +
  geom_point(data= grid_kgari_circle %>% filter(longitude > 152.95 & longitude < 153.5 & latitude > -25.8 & latitude < -24.7), aes(x=longitude, y=latitude), alpha=0.1) +
  geom_sf(data = australia, fill = NA, color = "white", size = 0.5))


# switch off for plotting
sf_use_s2(FALSE)

hom_map <- ggplot() +
  geom_sf(data = australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") + # Draw the australian map
  geom_sf_inset(data = het_dingo_only_simpler_results, aes(colour=var1.pred), size=0.8, map_base = "normal", map_inset="auto") + # plot the qpAdm points and colour 
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "normal", map_inset="none") + # Drawing an inverse map of Australia - you can ignore this.
  geom_sf_inset(data = het_dingo_only_simpler_results_kgari, aes(colour=var1.pred), size=0.8, map_base = "none", map_inset="auto") + # plot the qpAdm points and colour 
  geom_sf_inset(data = st_cast(inverse_polygon, "MULTIPOLYGON"), fill="white", colour="black", size=0.5, map_base = "none", map_inset="normal") + # Drawing an inverse map of Australia - you can ignore this.
  geom_sf_inset(data = st_as_sf(het_dingo_only %>% filter(data_type == "WGS", latitude != "") %>% mutate(f := case_when(f < 0 ~ 0, .default = f)), coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=f_het, label=fid), colour="black", alpha=1, size=3, shape=24, map_base = "normal", map_inset="auto") +
  geom_point(data=het_dingo_only %>% filter(data_type == "WGS", latitude != "", fid=="Dingo28") %>% mutate(f := case_when(f < 0 ~ 0, .default = f)), aes(x=longitude, y=latitude, fill=f_het), colour="black", alpha=1, size=3, shape=24)+
  #scale_colour_gradientn(name="Dingo Ancestry", colours=dingo_ancestry_colour_list_qpadm, values=scales::rescale(c(0.7, 0.8, 0.9, 1)), limits=c(0.7,1)) + # Setting the colour gradient
  scale_colour_gradientn(name=expression("Heterozygosity"), colours=rev(c("#ef476f","#f78c6b","#ffd166","#06d6a0","#118ab2","#073b4c", "black")), limits=c(0.0,0.18)) + # Setting the colour gradient
  scale_fill_gradientn(name=expression("Heterozygosity"), colours=rev(c("#ef476f","#f78c6b","#ffd166","#06d6a0","#118ab2","#073b4c", "black")), limits=c(0,0.18)) + # Setting the colour gradient
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
  #coord_sf(ylim=c(-45,-5), xlim=c(110,155))

hom_map
ggsave("../figure_extended_hom_map.png", hom_map, dpi=300, height=5, width=5)


diversity_figure <- (diversity_fence_w_dog | diversity_fence) / (hom_map | het_dingo_only_plot)
diversity_figure <- ggarrange(diversity_fence_w_dog, hom_map, diversity_fence, het_dingo_only_plot, nrow = 2, ncol = 2, labels=c("A", "C", "B","D"))
diversity_figure
ggsave("../figure_3_main.png", diversity_figure, dpi=600, height=10, width=12)

het_dingo_only_w_affinity <- left_join(het_dingo_only, f4_affinity %>% dplyr::select(sample_name, affinity), by=join_by("fid"=="sample_name")) %>%
  mutate(pop_name_manual = factor(pop_name_manual, levels=c("West", "North", "Central", "Mallee", "Captive", "K'gari", "Alpine", "East"))) %>% 
  filter(fid != "WEST_3-100")

population_affinity_plot <- ggplot(het_dingo_only_w_affinity, aes(x=pop_name_manual, y=affinity, fill=as.factor(pop_name_manual))) +
  geom_rain(point.args = list(aes(shape=data_type, colour=data_type, size=data_type, alpha=data_type)),
            violin.args = list(colour=NA, scale="width")) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_colour_manual(values=c("WGS"="black", "SNP Array"=NA)) + 
  scale_size_manual(values=c("WGS"=3, "SNP Array"=2)) + 
  scale_alpha_manual(values=c("WGS"=1, "SNP Array"=0.7)) + 
  ylab("Ancestral Affinity") +
  xlab("Population Name") + 
  scale_shape_manual(values=wgs_snp_shapes) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

longitude_affinity_plot <- ggplot() +
  geom_smooth(data=het_dingo_only_w_affinity, aes(x=longitude, y=affinity), method="lm", colour="black", linetype="dashed") + 
  geom_point(data=het_dingo_only_w_affinity %>% filter(data_type=="SNP Array"), aes(x=longitude, y=affinity, colour=pop_name_manual), size=3) +
  geom_point(data=het_dingo_only_w_affinity %>% filter(data_type=="WGS"), aes(x=longitude, y=affinity, fill=pop_name_manual), size=5, shape=24) +
  ylab("Ancestral Affinity") +
  xlab("Longitude") + 
  scale_colour_manual(values=cluster_pallete) + 
  scale_fill_manual(values=cluster_pallete) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

affinity_to_pops <- ggarrange(population_affinity_plot, longitude_affinity_plot, ncol = 2, labels=c("A", "B"))


ggsave("../figure_affinity_to_pops.png", affinity_to_pops, dpi=300, width=10)


ggplot() +
  geom_point(data=het_dingo_only_w_affinity %>% filter(data_type=="SNP Array"), aes(x=latitude, y=affinity, colour=pop_name_manual), size=3) +
  geom_point(data=het_dingo_only_w_affinity %>% filter(data_type=="WGS"), aes(x=latitude, y=affinity, fill=pop_name_manual), size=5, shape=24) +
  scale_colour_manual(values=cluster_pallete) + 
  scale_fill_manual(values=cluster_pallete) + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
  

ggplot() +
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


het_dingo_only_no_coords <- het_dingo_only_w_affinity %>% filter(is.na(latitude)) %>%
  arrange(pop_name_manual)

# Fake latitudes
latitudes <- c(rep(-37, 12), rep(-38, 12), rep(-39, 10)) 
# Fake longitudes
longitudes <- rep(seq(120, 134, length.out = 12), length.out = 34)

het_dingo_only_no_coords$latitude <- latitudes
het_dingo_only_no_coords$longitude <- longitudes

pop_locations <- ggplot() +
  geom_sf_inset(data=australia, fill = NA, color = "black", size = 0.5, map_base = "normal", map_inset="auto") +
  geom_tile(data = topo_df, aes(x = x, y = y, fill = elevation_bin), alpha=0.7) +
  scale_fill_gradientn(colours=c("grey100", "black"), values=scales::rescale(c(1,9)), limits=c(1,9)) +
  new_scale_fill() + 
  geom_sf_inset(data=st_as_sf(het_dingo_only_w_affinity %>% filter(data_type=="SNP Array", !is.na(latitude)), coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(colour=pop_name_manual), size=2.5, alpha=0.7) +
  geom_sf_inset(data=st_as_sf(het_dingo_only_w_affinity %>% filter(data_type=="WGS", !is.na(latitude)), coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=pop_name_manual), size=4, shape=24) +
  geom_sf(data=st_as_sf(het_dingo_only_no_coords %>% filter(data_type=="SNP Array", !is.na(latitude)), coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(colour=pop_name_manual), size=2.5) +
  geom_sf(data=st_as_sf(het_dingo_only_no_coords %>% filter(data_type=="WGS", !is.na(latitude)), coords =c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=pop_name_manual), size=4, shape=24) +
  geom_sf(data = st_as_sf(coords %>% subset(population == "Nullarbor 1k"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=population), shape=25, size=4, colour="black") +
  geom_sf(data = st_as_sf(coords %>% subset(population == "Curracurrang 2k"), coords=c("longitude", "latitude"), crs=st_crs(australia)), aes(fill=population), shape=25, size=4, colour="black") +
  geom_sf(data=dingo_fence) + 
  scale_colour_manual(values=cluster_pallete) + 
  scale_fill_manual(values=cluster_pallete) + 
  theme_void() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  geom_inset_frame(colour="black", target.aes = list(linewidth=0.7)) + # Drawing the inset 
  coord_sf_inset(configure_inset(
    shape_circle(
      centre = st_centroid(kgari_multipolygon),
      radius = 70
    ),
    scale = 10,  translation = c(530, 1200), units = "km"),
    ylim=c(-45,-5), xlim=c(110,170))

ggsave("../figure_pop_locations.png", pop_locations, dpi=500)
  

latitude_affinity <- het_dingo_only_w_affinity %>% filter(pop_name_manual == "East") %>%
  ggplot(aes(x=latitude, y=affinity)) +
  geom_vline(xintercept = -27, colour="red") + 
  geom_smooth(method="lm", linetype="dashed", colour="black") +
  geom_point(aes(shape=data_type, size=data_type, fill=pop_name_manual, colour=data_type, alpha=data_type)) + 
  labs(x="Latitude", y="Ancestral Affinity") + 
  scale_size_manual(values=c("WGS"=4, "SNP Array"=3)) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_shape_manual(values=c("WGS"=24, "SNP Array"=21)) + 
  scale_colour_manual(values=c("WGS"="black", "SNP Array"="#ffca3a")) +
  scale_alpha_manual(values=c("WGS"=1, "SNP Array"=0.7)) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggplotly(ggplot(australia) + geom_sf() + geom_sf(data=dingo_fence) + geom_point(data=coords, aes(x=longitude, y=latitude)))

latitude_a7 <- het_dingo_only_w_affinity %>% filter(pop_name_manual == "East") %>%
  ggplot(aes(x=latitude, y=value)) +
  geom_vline(xintercept = -27, colour="red") + 
  geom_smooth(method="lm", linetype="dashed", colour="black") +
  geom_point(aes(shape=data_type, size=data_type, fill=pop_name_manual, colour=data_type, alpha=data_type)) + 
  labs(x="Latitude", y="ADMIXTURE component") + 
  scale_size_manual(values=c("WGS"=4, "SNP Array"=3)) +
  scale_fill_manual(values=cluster_pallete) + 
  scale_shape_manual(values=c("WGS"=24, "SNP Array"=21)) + 
  scale_colour_manual(values=c("WGS"="black", "SNP Array"="#ffca3a")) +
  scale_alpha_manual(values=c("WGS"=1, "SNP Array"=0.7)) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )
  

figure_latitude_changes <- ggarrange(latitude_affinity, latitude_a7, nrow=1, labels=c("A", "B"))

ggsave("../figure_extended_latitude_east.png", figure_latitude_changes, dpi=300, width=8, height=4)

  ####### Land use 
# land_use_data <- rast("NLUM_v7_250_ALUMV8_2020_21_alb.tif")
# land_use_data_crs <- project(land_use_data, "EPSG:4326")
# land_use_metadata <- fread("NLUM_v7_250_ALUMV8_2020_21_alb.csv") %>%
#   janitor::clean_names()
# 
# pts <- vect(
#   het_dingo_only %>% dplyr::select(longitude, latitude, f),
#   geom = c("longitude", "latitude"),
#   crs = "EPSG:4326"
# )
# 
# pts_m <- project(pts, "EPSG:3577")
# land_use_m <- project(land_use_data, "EPSG:3577", method = "near")
# 
# radius_m <- sqrt(400e6 / pi)  # ≈ 11284 m
# buffers <- buffer(pts_m, width = radius_m)
# 
# lu_match <- extract(
#   land_use_m,
#   buffers,
#   count=T
# )
# 
# counts <- lu_match %>% 
#   group_by(ID, NLUM_v7_250_ALUMV8_2020_21_alb) %>%
#   summarise(count=n()) %>%
#   ungroup() %>%
#   tidyr::complete(ID, NLUM_v7_250_ALUMV8_2020_21_alb, fill = list(count = 0)) %>%
#   dplyr::rename(land_use_id = NLUM_v7_250_ALUMV8_2020_21_alb) %>%
#   left_join(., land_use_metadata, by=join_by("land_use_id"=="value"))
# 
# het_dingo_only$id <- seq(1,nrow(het_dingo_only))
# 
# het_dingo_only_counts <- left_join(counts, het_dingo_only, by=join_by("ID"=="id"))
# 
# het_dingo_only_counts %>%
#   filter(simp == "Dryland cropping", !str_detect(pop_name, "Captive"), !pop_name %in% c("K'gari", "West")) %>%
#   group_by(pop_name, f, simp) %>%
#   summarise(simp_count = sum(count.x)) %>%
#   ggplot(aes(x=log(1+simp_count), y=f)) +
#   geom_smooth(method="lm", linetype="dashed") +
#   geom_point(aes(colour=pop_name)) +
#   theme_bw()
# 
# het_dingo_only_counts_plus_others <- het_dingo_only_counts %>%
#   filter(!pop_name %in% c("K'gari", "Captive1", "Captive2")) %>%
#   group_by(fid, pop_name, f,secv8n) %>%
#   summarise(counts = sum(count.x)) %>%
#   mutate(
#     total = sum(counts),
#     pct = counts/total,
#     log = log(1+counts, base=10)) 
# 
# het_lu_wide <- het_dingo_only_counts_plus_others %>%
#   pivot_wider(
#     id_cols = c(fid, pop_name, f),
#     names_from = secv8n,
#     values_from = pct
#   )
# 
# 
# # select only the land-use count columns
# land_use_cols <- setdiff(names(het_lu_wide), c("fid", "pop_name", "f"))
# 
# cor_results <- sapply(het_lu_wide[land_use_cols], function(x) cor(x, het_lu_wide$f))
# 
# # sort descending by absolute correlation
# sort(cor_results, decreasing = TRUE)
# 
# het_dingo_only_counts_plus_others %>%
#   filter(secv8n == 11) %>%
#   ggplot(aes(x=pct, y=f)) +
#   geom_smooth(method="lm") + 
#   geom_point(aes(colour=pop_name))


mosaic_wgs_samples_merged 
ggbetweenstats(mosaic_wgs_samples_merged, x=)

table(mosaic_wgs_samples_merged$population)

mosaic_wgs_samples_merged %>%
  dplyr:select()
  pivot_longer(
    
  )
  
t.test(mosaic_wgs_samples_merged$dingo_ancestry.x, mosaic_wgs_samples_merged$dingo_ancestry.y)
t.test(mosaic_snp_samples_merged$dingo_ancestry.x, mosaic_snp_samples_merged$dingo_ancestry.y)


merge(gen_time_qpadm %>% dplyr::select(target, gen_time), admixture_proportions, by.x="target", by.y="sample_name") %>%
  subset(population != "Captive" & target %in% unrelated_dingoes$sample_name) %>%
  mutate(south_of_fence = as.factor(south_of_fence)) %>% 
  ggplot(aes(x = south_of_fence, y = gen_time, 
             colour = south_of_fence, fill = south_of_fence)) +
  geom_rain(alpha = 0.5, point.args = list(alpha=0.1)) +
  labs(
    x = "", 
    y = "Generations since admixture") +
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


ggplotly(ggplot() +
  geom_sf(data=australia)+
  geom_point(data=coords, aes(x=longitude, y=latitude, label=sample_id)) + 
  geom_point(data=het_dingo_only_w_affinity %>% filter(pop_name_manual == "East"), aes(x=longitude, y=latitude), colour="yellow") +
  geom_sf(data=dingo_fence))


het_dingo_dog %>%
  ggplot(aes(x=f_het, y=f_het_w_dog, colour=pop_name_manual)) +
  geom_point() +
  geom_abline(intercept = 0)


t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="West"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="West"]$f_het_w_dog) # 4e-8
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="East"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="East"]$f_het_w_dog) # 2.8e-6
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="Captive"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="Captive"]$f_het_w_dog) # 0.56
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="Central"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="Central"]$f_het_w_dog) # 0.033
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="Alpine"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="Alpine"]$f_het_w_dog) # 3.1e-8
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="North"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="North"]$f_het_w_dog) # 0.0011
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="K'gari"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="K'gari"]$f_het_w_dog) # 0.1089
t.test(het_dingo_dog[het_dingo_dog$pop_name_manual=="Mallee"]$f_het, 
       het_dingo_dog[het_dingo_dog$pop_name_manual=="Mallee"]$f_het_w_dog) # 0.003055

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "West"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "West"), aes(x="w Dog", y=f_het_w_dog)) +
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "East"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "East"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Captive"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Captive"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))


ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Central"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Central"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Alpine"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Alpine"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "North"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "North"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "K'gari"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "K'gari"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))

ggplot() +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Mallee"), aes(x="Dingo only", y=f_het)) +
  geom_boxplot(data=het_dingo_dog %>% filter(pop_name_manual == "Mallee"), aes(x="w Dog", y=f_het_w_dog))+
  coord_cartesian(ylim=c(0,0.25))


write.csv( , "het_dingo_dog.csv", quote = F, row.names = F)

write.csv(left_join(het_dingo_dog, f4_affinity %>% dplyr::select(sample_name, curracurrang_est, curracurrang_se, nullarbor_est, nullarbor_se, affinity), 
          by=join_by("fid"=="sample_name")) %>% 
  dplyr::select(fid, sample_id, pop_name_manual, dingo_ancestry, dingo_ancestry_se, f_het, f_het_w_dog, curracurrang_est, curracurrang_se, nullarbor_est, nullarbor_se,  affinity),
  "supp_table3.csv" , quote = F, row.names = F)

f4_affinity


