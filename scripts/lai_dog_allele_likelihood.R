pacman::p_load(
  MOSAIC,
  dplyr,
  data.table,
  ggplot2
)

samples_list <- fread("~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/wgs_dingo_sample.list", 
                      header = F, col.names = c("id"))$id
sample_ids <- seq(1,length(samples_list))

fpath <- "~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/snp_array_snps/"

load("~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/MOSAIC_RESULTS_WGS/localanc_Dingo_2way_1-45_1-38_222_60_0.99_100.RData")
load("~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/MOSAIC_RESULTS_WGS/Dingo_2way_1-45_1-38_222_60_0.99_100.RData")

local_pos=grid_to_pos(localanc, fpath, g.loc, chrnos)



for (chr in 1:38) {
  snps_ids <- fread(paste0(fpath, "/snpfile.", chr), header = F, col.names = c("id", "chr", "zero", "pos", "a1", "a2"))
  dog_alleles_per_samples <- data.frame()
  print(chr)
  for (id in sample_ids) {
    sample <- samples_list[id]
    curr_sample_dal1 <- local_pos[[chr]][2,2*id-1,]
    curr_sample_dal2 <- local_pos[[chr]][2,2*id,]
    curr_sample_dal <- (curr_sample_dal1 + curr_sample_dal2)/2
    curr_dal_df <- data.frame(sample = rep(sample, length(curr_sample_dal)),
                              dal = curr_sample_dal,
                              snp_id = snps_ids$id,
                              pos = snps_ids$pos) %>%
      dplyr::filter(dal > 0.2) %>%
      dplyr::select(snp_id, sample)
    dog_alleles_per_samples <- rbind(dog_alleles_per_samples, curr_dal_df)
    
  }
  write.csv(dog_alleles_per_samples, paste0("~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/masked_ancestry/wgs_dogs_on_snp_array_snps_zero_cluster_chr", chr ,".clust"), 
            quote=F, col.names =F, row.names = F, sep="\t")
}

grid_to_pos


