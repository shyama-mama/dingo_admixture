pacman::p_load(admixtools,
               dplyr,
               ggplot2)


# f4(CoyoteCalifornia,German Shepherd;Ancient Dingo,Modern Dingo X)
# Coyote, GermanSheperd, Ancient Dingo and Modern Dingo X (Cairns and WGS)
# All 190k SNPs

setwd("~/Library/CloudStorage/Box-Box/projects/dingo/heritage_paper/arranged_folder/analyses/f4")

data <- "01_cairns_souilmi_bergstrom_plassias_zhang_plus_more_gatk4"
pop3 <- c("Dingo_Ancient_Curracurrang", "Dingo_Ancient_Nullarbor")
pop4 <- read.csv("pops.list", col.names = c("id"), header=FALSE)
f4_matrix_transversions <- qpdstat(data, pop1="CoyoteCalifornia", pop2="GermanShepherdDog", pop3=pop3, pop4=pop4$id,
                     verbose = TRUE, 
                     sure=TRUE,
                     allsnps=TRUE,
                     auto_only=FALSE,
                     boot=FALSE,
                     f4mode=TRUE)

write.csv(f4_matrix_transversions, "f4_resutls_transversions.csv", quote=F, row.names = F)
