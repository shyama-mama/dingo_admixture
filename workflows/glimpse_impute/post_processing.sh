#!/bin/bash

# for seq in $(seq 1 38); do  sbatch -J process_chr${seq} -o $PWD/results/logs/process_chr${seq}.out --mem=8000M -N 1 -c 1 -p icelake --time=08:00:00 -D $PWD post_processing.sh $seq ; done

module purge

module load BCFtools
module load VCFtools
module load Singularity

set -e 
set -x 


seq=$1

RESULTS_DIR="results/04_merged"
chr="chr${seq}"
RESULTS_DIR_CHR=${RESULTS_DIR}/${chr}
REF_PANEL_DIR_CHR=/gpfs/users/a1880987/projects/dingo/glimpse_imputation/${RESULTS_DIR}

# Subset refpanel to SNPs in imputed samples
bcftools view -R ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8.vcf.gz -Oz -o ${RESULTS_DIR_CHR}/ref-panel_${chr}_sample-snp_filltags_filter_subset.phased.vcf.gz /hpcfs/users/a1880987/projects/dingo/public_data/bougiouri_et_al/dog_imputation_pipeline/data/reference_panel_vcf/ref-panel_${chr}_sample-snp_filltags_filter.phased.vcf.gz
bcftools index ${RESULTS_DIR_CHR}/ref-panel_${chr}_sample-snp_filltags_filter_subset.phased.vcf.gz

## Merge ref-panel with imputed samples
bcftools merge -Oz -o ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_merged.vcf.gz \
    ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8.vcf.gz ${RESULTS_DIR_CHR}/ref-panel_${chr}_sample-snp_filltags_filter_subset.phased.vcf.gz
bcftools index ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_merged.vcf.gz

## Subset to samples to run MOSAIC
bcftools view -S ${RESULTS_DIR}/samples_to_subset.list -Oz \
    -o ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset.vcf.gz \
    ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_merged.vcf.gz
bcftools index ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset.vcf.gz

## Think vcf
vcftools --thin 1000 --gzvcf ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset.vcf.gz \
    --out ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin --recode 
bgzip ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin.recode.vcf  
bcftools index -t ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin.recode.vcf.gz

## Create HAPs
singularity exec -B /gpfs/ /hpcfs/groups/acad_users/containers/plink2_2.0.0a.6.9--h9948957_0.sif \
    plink2 --vcf ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin.recode.vcf.gz --export haps --out ${RESULTS_DIR_CHR}/merged_phased_annotated.${chr}_MAF_0.01_recalibrated_INFO_0.8_refpanel_mosaic_subset_thin_haps --dog --threads 1


