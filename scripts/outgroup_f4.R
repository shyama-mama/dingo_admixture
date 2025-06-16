library(stringr)
library(admixtools)
library(tidyr)
library(dplyr)
library(data.table)

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/arranged_folder/analyses/SNP_array/f4_affinity/")

pop2 <- fread("dingo_names.txt", header = F, col.names = c("id"))

f4 <- qpdstat(
  "02b_Cairns_with_ancient_modern_dingo_andeanfox",
  pop1 = pop2$id,
  pop2 = "CoyoteCalifornia",
  pop3 = c("Dingo_Ancient_Nullarbor", "Dingo_Ancient_Curracurrang"),
  pop4 = "AndeanFox",
  allsnps = TRUE,
  auto_only = FALSE,
  f4mode = TRUE)

f4_df <- f4 %>%
  pivot_wider(
    names_from = pop3,
    values_from = c(est, se, z, p, n),
  )
f4_df$affinity <- (f4_df$est_Dingo_Ancient_Curracurrang - f4_df$est_Dingo_Ancient_Nullarbor)/(f4_df$est_Dingo_Ancient_Curracurrang + f4_df$est_Dingo_Ancient_Nullarbor)

write.table(f4_df, file="f4_affinity.csv", sep = ",", row.names = FALSE, quote = FALSE)