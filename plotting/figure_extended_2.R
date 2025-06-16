source("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/publication_plots/utilities.R")

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/publication_plots/plots/")

# cairns_q_value
# qpadm_results_wgs_snp_meta


cairns_q_value_qpadm_meta <- left_join(cairns_q_value, qpadm_results_wgs_snp_meta, by=join_by("sample_name"=="target")) %>%
  dplyr::select(sample_name, q_value, dingo_ancestry, dingo_ancestry_se, population.x) %>%
  dplyr::rename(population = population.x) %>%
  dplyr::filter(!is.na(dingo_ancestry) & !is.na(population)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    plotting_q :=
      case_when(
        q_value == 1 ~ q_value+sample(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1), 1),
        .default = q_value
      ),
    plotting_qpadm :=
      case_when(
        dingo_ancestry ==1 ~ dingo_ancestry+sample(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1), 1),
        .default = dingo_ancestry
      )
  )
  
cairns_q_value_qpadm_meta$avg <- (cairns_q_value_qpadm_meta$q_value + cairns_q_value_qpadm_meta$dingo_ancestry) / 2
cairns_q_value_qpadm_meta$diff <- cairns_q_value_qpadm_meta$q_value - cairns_q_value_qpadm_meta$dingo_ancestry



q_vs_qpadm_mean_avg <- ggplot(cairns_q_value_qpadm_meta, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(cairns_q_value_qpadm_meta$diff), color = "red") +
  geom_hline(yintercept = mean(cairns_q_value_qpadm_meta$diff) + 1.96 * sd(cairns_q_value_qpadm_meta$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(cairns_q_value_qpadm_meta$diff) - 1.96 * sd(cairns_q_value_qpadm_meta$diff), linetype = "dashed", color = "blue") +
  geom_point(aes(colour=population), alpha=0.7, size=2) +
  labs(x = "Mean dingo ancestry\n(SNP+FS & SNP+qpAdm)", y = "Difference between\nSNP+FS & SNP+qpAdm") +
  scale_colour_manual("SNP-Array Populations", values=snp_population_colour) + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

q_vs_qpadm <- ggplot() + 
  geom_rect(aes(ymin=1, ymax=1.11, xmin=0.4, xmax=1.11), fill="grey", alpha=0.2) + 
  geom_rect(aes(xmin=1, xmax=1.11, ymin=0.4, ymax=1.11), fill="grey", alpha=0.2) + 
  geom_errorbar(data=cairns_q_value_qpadm_meta, aes(x=plotting_q, y=plotting_qpadm, ymin=plotting_qpadm-dingo_ancestry_se, ymax=plotting_qpadm+dingo_ancestry_se), colour="grey", alpha=0.6) + 
  geom_point(data=cairns_q_value_qpadm_meta, aes(x=plotting_q, y=plotting_qpadm, colour=population), alpha=0.7, size=2) +
  xlab("Dingo Ancestry\nSNP+FS") +
  ylab("Dingo Ancestry\nSNP+qpAdm") + 
  scale_colour_manual(values=snp_population_colour) + 
  labs(colour="SNP-Array Populations") +
  scale_y_continuous(breaks = c(0.6, 0.8, 1.055), labels = c(0.6, 0.8, 1)) + 
  scale_x_continuous(breaks = c(0.6, 0.8, 1.055), labels = c(0.6, 0.8, 1)) + 
  coord_cartesian(xlim=c(0.35,1.11), ylim=c(0.35,1.11)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")

comparison1 <- q_vs_qpadm / q_vs_qpadm_mean_avg
comparison1

############ WGS SNP subset and 1M 
wgs_trans_qpadm <- qpadm_results_wgs_snp_meta %>%
  dplyr::filter(snp_used == "1.9M" & data_type == "WGS") %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se, population) %>%
  dplyr::rename(dingo_ancestry_trans = dingo_ancestry,
                dingo_ancestry_se_trans = dingo_ancestry_se)

wgs_subset_qpadm <- qpadm_results_wgs_snp_meta %>%
  dplyr::filter(snp_used == "46K" & data_type == "WGS") %>%
  dplyr::select(target, dingo_ancestry, dingo_ancestry_se) %>%
  dplyr::rename(dingo_ancestry_subset = dingo_ancestry,
                dingo_ancestry_se_subset = dingo_ancestry_se)
nrow(wgs_subset_qpadm)
nrow(wgs_trans_qpadm)

wgs_qpadm_for_plotting <- left_join(wgs_trans_qpadm, wgs_subset_qpadm, by="target") %>%
  dplyr::mutate(avg = (dingo_ancestry_subset+dingo_ancestry_trans)/2,
                diff = dingo_ancestry_subset - dingo_ancestry_trans)


wgs_qpadm_mean_avg <- ggplot(wgs_qpadm_for_plotting, aes(x = avg, y = diff)) +
  geom_hline(yintercept = mean(wgs_qpadm_for_plotting$diff), color = "red") +
  geom_hline(yintercept = mean(wgs_qpadm_for_plotting$diff) + 1.96 * sd(wgs_qpadm_for_plotting$diff), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(wgs_qpadm_for_plotting$diff) - 1.96 * sd(wgs_qpadm_for_plotting$diff), linetype = "dashed", color = "blue") +
  geom_point(aes(fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(WGS_46K & WGS_1.9M)", y = "Difference between\nWGS_46K & WGS_1.9M") +
  scale_fill_manual("WGS Populations", values=wgs_population_colours) + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
wgs_qpadm_mean_avg

wgs_qpadm_plot <- ggplot() + 
  geom_errorbar(data=wgs_qpadm_for_plotting, 
                aes(x=dingo_ancestry_subset, y=dingo_ancestry_trans, 
                    ymin=dingo_ancestry_trans-dingo_ancestry_se_trans, ymax=dingo_ancestry_trans+dingo_ancestry_se_trans,
                    xmin=dingo_ancestry_subset-dingo_ancestry_se_subset, xmax=dingo_ancestry_subset+dingo_ancestry_se_subset), colour="grey", alpha=0.6) + 
  geom_point(data=wgs_qpadm_for_plotting, aes(x=dingo_ancestry_subset, y=dingo_ancestry_trans, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nWGS_46K") +
  ylab("Dingo Ancestry\nWGS_1.9M") + 
  scale_fill_manual(values=wgs_population_colours) + 
  labs(fill="WGS Populations") +
  scale_y_continuous(breaks = c(0.6, 0.8, 1), labels = c(0.6, 0.8, 1)) + 
  scale_x_continuous(breaks = c(0.6, 0.8, 1), labels = c(0.6, 0.8, 1)) + 
  coord_cartesian(xlim=c(0.5,1), ylim=c(0.5,1)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")

comparison2 <- wgs_qpadm_plot / wgs_qpadm_mean_avg
comparison2


###### ADMIXTURE vs qpAdm
admixture_vs_qpadm_df <- left_join(admixture_k2_k11_dingoes, qpadm_results_wgs_snp_meta %>% dplyr::filter(snp_used == "46K"), by=join_by("sample_name"=="target")) %>%
  dplyr::select(sample_name, dingo11, dingo_ancestry, dingo_ancestry_se, population.y, type) %>%
  dplyr::rename(population = population.y) %>%
  dplyr::mutate(avg = (dingo_ancestry+dingo11)/2,
                diff = dingo11 - dingo_ancestry) %>%
  rowwise() %>%
  dplyr::mutate(
    plotting_admix :=
      case_when(
        dingo11 > 0.99 ~ dingo11+sample(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1), 1),
        .default = dingo11
      ),
    plotting_qpadm :=
      case_when(
        dingo_ancestry == 1 ~ dingo_ancestry+sample(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1), 1),
        .default = dingo_ancestry
      )
  )


admixture_vs_qpadm_mean_avg <- ggplot(admixture_vs_qpadm_df) +
  geom_hline(yintercept = mean(admixture_vs_qpadm_df$diff, na.rm=TRUE), color = "red") +
  geom_hline(yintercept = mean(admixture_vs_qpadm_df$diff, na.rm=TRUE) + 1.96 * sd(admixture_vs_qpadm_df$diff, na.rm=TRUE), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(admixture_vs_qpadm_df$diff, na.rm=TRUE) - 1.96 * sd(admixture_vs_qpadm_df$diff, na.rm=TRUE), linetype = "dashed", color = "blue") +
  geom_point(data=admixture_vs_qpadm_df %>% filter(type=="SNP Array" & !is.na(population)), aes(x = avg, y = diff, colour=population), alpha=0.7, size=2) +
  geom_point(data=admixture_vs_qpadm_df %>% filter(type=="WGS" & !is.na(population)), aes(x = avg, y = diff, fill=population), alpha=0.9, size=4, shape=24) +
  labs(x = "Mean dingo ancestry\n(SNP+ADMIX & SNP+qpAdm)", y = "Difference between\nSNP+ADMIX & SNP+qpAdm") +
  scale_colour_manual("SNP-Array Populations", values=snp_population_colour) + 
  scale_fill_manual("WGS Populations", values=wgs_population_colours) + 
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, direction = "horizontal"),
         fill = guide_legend(title.position = "top", title.hjust = 0.5, direction = "horizontal")) + 
  scale_y_continuous(limits=c(-0.08, 0.28)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",
                                         linewidth=0.3, colour = "black", linetype="solid"))
admixture_vs_qpadm_mean_avg

admixture_vs_qpadm <- ggplot() + 
  geom_rect(aes(ymin=1, ymax=1.11, xmin=0.4, xmax=1.11), fill="grey", alpha=0.2) + 
  geom_rect(aes(xmin=1, xmax=1.11, ymin=0.4, ymax=1.11), fill="grey", alpha=0.2) + 
  geom_errorbar(data=admixture_vs_qpadm_df %>% filter(!is.na(population)), aes(x=plotting_admix, y=plotting_qpadm, ymin=plotting_qpadm-dingo_ancestry_se, ymax=plotting_qpadm+dingo_ancestry_se), colour="grey", alpha=0.6) + 
  geom_point(data=admixture_vs_qpadm_df %>% dplyr::filter(type=="SNP Array" & !is.na(population)), aes(x=plotting_admix, y=plotting_qpadm, colour=population), alpha=0.7, size=2) +
  geom_point(data=admixture_vs_qpadm_df %>% dplyr::filter(type=="WGS" & !is.na(population)), aes(x=plotting_admix, y=plotting_qpadm, fill=population), alpha=0.9, size=4, shape=24) +
  xlab("Dingo Ancestry\nSNP+ADMIX") +
  ylab("Dingo Ancestry\nSNP+qpAdm") + 
  scale_colour_manual("SNP Array Population", values=snp_population_colour) + 
  scale_fill_manual("WGS Population", values=wgs_population_colours) + 
  scale_y_continuous(breaks = c(0.6, 0.8, 1.055), labels = c(0.6, 0.8, 1)) + 
  scale_x_continuous(breaks = c(0.6, 0.8, 1.055), labels = c(0.6, 0.8, 1)) + 
  coord_cartesian(xlim=c(0.35,1.11), ylim=c(0.35,1.11)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha=0.8) +  # Add x = y line
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")

comparison3 <- (admixture_vs_qpadm / admixture_vs_qpadm_mean_avg) 

comparison3
bald_man_plot<-ggarrange(comparison1, comparison2, comparison3, common.legend = T, legend.grob = get_legend(comparison3), legend = "bottom", ncol=3, labels = c("A", "B", "C"))
ggsave("../Figure_Extended_2.png", bald_man_plot, dpi=600, height=7, width=12)




##### P-values
ggbetweenstats(merged_rain %>% subset(population == "Captive") %>% mutate(dog_admixture = 1-dingo_ancestry), type, dog_admixture, 
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
