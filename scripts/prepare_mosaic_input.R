pathin="~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/data_wgs_batch260114/"
haps_stem="merged_phased_annotated.chr"
haps_end="_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin_haps.haps"
inds.data="samples.list" 
pathout="~/Library/CloudStorage/Box-Box/projects/dingo/MOSAIC/inputData_wgs_batch260114/"

for (chrno in 1:38) {

  print(chrno)
  hapsfile=paste0(haps_stem,chrno,haps_end)
  shapeithaps=read.table(file.path(pathin,hapsfile))
  S=nrow(shapeithaps)
  NN=ncol(shapeithaps)
  locs=shapeithaps[,3]
  rsid=paste0(shapeithaps[,1], "_", shapeithaps[,3])
  rsids=shapeithaps[,2]

  # now read in population information
  allpops=read.table(file.path(pathin,inds.data),header=FALSE)
  # now reduce to parts we need
  pops=as.character(unique(allpops[,1]))
  keep=rep(TRUE,nrow(allpops)); # remove none to start
  keep=which(keep)
  allpops=allpops[keep,]
  write.table(allpops[,c(1,2,3)],file=file.path(pathout,"sample.names"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  hap.pops=rep(allpops[,1],each=2)
  
  # create list to store populations
  for (i in 1:length(pops)) {
    y=shapeithaps[,5+which(hap.pops==pops[i])]
    write.table(y,file=file.path(pathout,paste0(pops[i],"genofile.",chrno)),sep="",col.names=FALSE,row.names=FALSE)
  }
  # create matrix of snps for which we have haplotypes
  snps = matrix(NA, S, 6)
  
  snps[,1] = paste0(chrno, "_", locs)
  snps[,2] = chrno
  snps[,3] = 0
  snps[,4] = locs
  snps[,5] = shapeithaps[,4]  # REF allele
  snps[,6] = shapeithaps[,5]  # ALT allele
  
  write.table(snps, file=file.path(pathout,paste0("snpfile.", chrno)),
              quote=FALSE, col.names=FALSE, row.names=FALSE)
}
