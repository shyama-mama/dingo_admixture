#!/usr/bin/env nextflow

nextflow.enable.dsl = 2

// Compute GLs of bams (column with path to bamfile must be names "Bam")
process compute_GLs_imputed_samples {
    publishDir "results/00_GL/${sample}/", mode: 'copy'

    input:
    tuple val(sample), val(chrom), file(imputation_bams), file(imputation_bams_index), file(ref_panel_sites_vcf), file(ref_panel_sites_tsv), file(ref_fasta_chr)

    output:
    tuple val(sample), val(chrom), file("${sample}_${chrom}.vcf.gz"), file("${sample}_${chrom}.vcf.gz.csi")

    script:
    def GL_imputed_bam = "${sample}_${chrom}.vcf.gz"
    if(params.test) {
    """
    touch ${GL_imputed_bam}
    touch ${GL_imputed_bam}.csi
    """
    } else {
    """
    bcftools mpileup -f ${ref_fasta_chr} -I -E -a 'FORMAT/DP' -T ${ref_panel_sites_vcf} -r ${chrom} ${imputation_bams} -Ou | \
    bcftools call -Aim -C alleles -T ${ref_panel_sites_tsv} -Oz -o ${GL_imputed_bam} --threads ${task.cpus} 
        
    bcftools index -f ${GL_imputed_bam}
    """
    }
}

// Impute all samples individually
process impute {
    input:
    tuple val(sample), val(chrom), file(GL_imputed_bam), file(GL_imputed_bam_index), file(ref_panel_phased), file(ref_panel_phased_index), file(chunks), file(gen_map)

    output:
    tuple val(sample), val(chrom), file("${sample}_imputed.${chrom}*")

    script:
    def prefix = "${sample}_imputed.${chrom}"
    if(params.test) {
    """
    while IFS="" read -r LINE || [ -n "\$LINE" ]; do
        printf -v ID "%02d" \$(echo \$LINE | cut -d" " -f1)
        IRG=\$(echo \$LINE | cut -d" " -f3)
        ORG=\$(echo \$LINE | cut -d" " -f4)
        OUT=${prefix}.\${ID}.bcf
        touch \${OUT}
        touch \${OUT}.csi
    done < ${chunks}
    """
    } else {
    """
    while IFS="" read -r LINE || [ -n "\$LINE" ]; do
            printf -v ID "%02d" \$(echo \$LINE | cut -d" " -f1)
            IRG=\$(echo \$LINE | cut -d" " -f3)
            ORG=\$(echo \$LINE | cut -d" " -f4)
            OUT=${prefix}.\${ID}.bcf
            GLIMPSE_phase \
            --input ${GL_imputed_bam} \
            --reference ${ref_panel_phased} \
            --map ${gen_map} \
            --input-region \${IRG} \
            --output-region \${ORG} --output \${OUT} \
            --thread ${task.cpus}
            bcftools index -f \${OUT}
    done < ${chunks}
    """
    }
}

// Merge all imputed chunks
process ligate{
    publishDir "results/01_impute/${sample}/", mode: 'copy'

    input:
    tuple val(sample), val(chrom), file(bcfs)

    output:
    tuple val(sample), val(chrom), file("${sample}_ligated.${chrom}.bcf"), file("${sample}_ligated.${chrom}.bcf.csi")

    script:
    def ligated_bcf = "${sample}_ligated.${chrom}.bcf"
    if(params.test) {
    """
    ls -1 *bcf | sort  > ${ligated_bcf}
    touch ${ligated_bcf}.csi
    """
    } else {
    """
    ls -1 *bcf | sort > ligated.list

    GLIMPSE_ligate \
        --input ligated.list \
        --output ${ligated_bcf} \
        --thread ${task.cpus}

    bcftools index -f ${ligated_bcf}
    """
    }

}

// Phase!!!
process sample_haplotype{
    publishDir "results/02_phase/${sample}/", mode: 'copy'

    input:
    tuple val(sample), val(chrom), file(ligated_bcf), file(ligated_bcf_index)

    output:
    tuple val(sample), val(chrom), file("${sample}_phased.${chrom}.bcf"), file("${sample}_phased.${chrom}.bcf.csi")

    script:
    def phased_bcf = "${sample}_phased.${chrom}.bcf"
    if(params.test) {
    """
    touch ${phased_bcf}
    touch ${phased_bcf}.csi
    """
    } else {
    """
    GLIMPSE_sample \
        --input ${ligated_bcf} \
        --solve \
        --output ${phased_bcf} \
        --thread ${task.cpus} 
    
    bcftools index -f ${phased_bcf}
    """
    }
}

// Add annotations to the phased files
process annotate_fields {
    publishDir "results/03_annotate/${sample}/", mode: 'copy'

    input:
    tuple val(sample), val(chrom), file(ligated_bcf), file(ligated_bcf_index), file(phased_bcf), file(phased_bcf_index)

    output:
    tuple val(sample), val(chrom), file("${sample}_phased_annotated.${chrom}.vcf.gz"), file("${sample}_phased_annotated.${chrom}.vcf.gz.tbi")

    script:
    def phased_vcf_annotate = "${sample}_phased_annotated.${chrom}.vcf.gz"
    if(params.test) {
    """
    touch ${phased_vcf_annotate}
    touch ${phased_vcf_annotate}.tbi
    """
    } else {
    """
    bcftools annotate \
        --annotations ${ligated_bcf} \
        --columns FORMAT/DS,FORMAT/GP,FORMAT/HS \
        --output-type z \
        --output ${phased_vcf_annotate} ${phased_bcf} 2> {log}
        
        bcftools index --tbi ${phased_vcf_annotate}
    """
    }
}


// Merge all phased samples 
process merge_phased_bcfs{
    publishDir "results/04_merged/${chrom}/", mode: 'copy'

    input:
    tuple val(chrom), file(phased_vcfs_annotated), file(phased_vcf_index_annotated)

    output:
    tuple val(chrom), file("merged_phased_annotated.${chrom}.vcf.gz"), file("merged_phased_annotated.${chrom}.vcf.gz.csi")

    script:
    def merged_phased_vcf = "merged_phased_annotated.${chrom}.vcf.gz"
    if(params.test) {
    """
    echo "${phased_vcfs_annotated.join(' ')}" > ${merged_phased_vcf}
    touch ${merged_phased_vcf}.tbi
    """
    } else {
    """
    bcftools merge \
        ${phased_vcfs_annotated.join(' ')} \
        -Oz -o ${merged_phased_vcf} \
        --threads ${task.cpus}

    bcftools index -f ${merged_phased_vcf}
    """
    }
}

// Extract MAF sites from merged phased VCF
process maf_sites_phased_vcf {
    publishDir "results/04_merged/${chrom}/", mode: 'copy'
    
    input:
    tuple val(chrom), file(merged_phased_vcf), file(merged_phased_vcf_index), file(ref_maf_tsv), file(ref_maf_tsv_index)

    output:
    tuple val(chrom), file("merged_phased_annotated.${chrom}_MAF_0.01.vcf.gz"), file("merged_phased_annotated.${chrom}_MAF_0.01.vcf.gz.csi")

    script:
    def merged_phased_vcf_maf = "merged_phased_annotated.${chrom}_MAF_0.01.vcf.gz"
    if(params.test) {
    """
    touch ${merged_phased_vcf_maf}
    touch ${merged_phased_vcf_maf}.csi
    """
    } else {
    """
    bcftools view ${merged_phased_vcf} \
    --regions-file ${ref_maf_tsv} \
    --threads ${task.cpus} \
    -Oz -o ${merged_phased_vcf_maf} 

    bcftools index -f ${merged_phased_vcf_maf}
    """
    }
}

// Re-calibrate INFO scores based on all samples present in the merged VCF
process recalibrate_info_phased_vcf {
    publishDir "results/04_merged/${chrom}/", mode: 'copy'

    input:
    tuple val(chrom), file(merged_phased_vcf_maf), file(merged_phased_vcf_maf_index)

    output:
    tuple val(chrom), file("merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO.vcf.gz"), file("merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO.vcf.gz.csi")

    script:
    def merged_phased_vcf_maf_recalibrated = "merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO.vcf.gz"
    if(params.test) {
    """
    touch ${merged_phased_vcf_maf_recalibrated}
    touch ${merged_phased_vcf_maf_recalibrated}.csi
    """
    } else {
    """
    bcftools plugin impute-info \
    ${merged_phased_vcf_maf} \
    -Ob -o ${merged_phased_vcf_maf_recalibrated} \
    --threads ${task.cpus} 

    bcftools index -f ${merged_phased_vcf_maf_recalibrated}
    """
    }
}

// Filter for INFO after re-calibrating INFO score for all samples together
process filter_recalibrated_INFO_phased_vcf{
    publishDir "results/04_merged/${chrom}/", mode: 'copy'

    input:
    tuple val(chrom), file(merged_phased_vcf_maf_recalibrated), file(merged_phased_vcf_maf_recalibrated_index)

    output:
    tuple file("merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO_0.8.vcf.gz"), file("merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO_0.8.vcf.gz.csi")

    script:
    def merged_phased_vcf_maf_recalibrated_info = "merged_phased_annotated.${chrom}_MAF_0.01_recalibrated_INFO_0.8.vcf.gz"
    if(params.test) {
    """
    touch ${merged_phased_vcf_maf_recalibrated_info}
    touch ${merged_phased_vcf_maf_recalibrated_info}.csi
    """
    } else {
    """
    bcftools view \
    ${merged_phased_vcf_maf_recalibrated} \
    --include 'INFO/INFO >= 0.8' \
    --threads ${task.cpus} \
    -Oz -o ${merged_phased_vcf_maf_recalibrated_info} 

    bcftools index -f ${merged_phased_vcf_maf_recalibrated_info}
    """
    }
}
    

workflow {

    // Read bams from file with sample name. 
    channel_sample_bams = Channel
        .fromPath(params.bam_list, checkIfExists: true)
        .splitCsv(header: ['sample', 'bam'], sep: '\t', skip:1)
        .map { row ->
            tuple(
                row.sample,
                file(row.bam),
                file(row.bam + '.bai')
            )
        }

    // Read reference panel files
    channel_chrom_ref_panel_sites = Channel
        .fromPath(params.reference_panel_files, checkIfExists: true)
        .splitCsv(header: ['chrom', 'ref_fasta_chr', 'ref_panel_sites_vcf', 'ref_panel_sites_tsv', 'chunks', 'ref_maf_vcf', 'ref_maf_tsv', 'gen_map', 'ref_panel_phased'], sep: '\t')
        .map { row ->
            tuple(
                row.chrom,
                file(row.ref_fasta_chr),
                file(row.ref_fasta_chr + '.fai'),
                file(row.ref_panel_sites_vcf),
                file(row.ref_panel_sites_vcf + '.csi'),
                file(row.ref_panel_sites_tsv),
                file(row.ref_panel_sites_tsv + '.tbi'),
                file(row.chunks),
                file(row.ref_maf_vcf),
                file(row.ref_maf_vcf + '.csi'),
                file(row.ref_maf_tsv),
                file(row.ref_maf_tsv + '.tbi'),
                file(row.gen_map),
                file(row.ref_panel_phased),
                file(row.ref_panel_phased + '.tbi')
            )
        }
    
    mega_channel = channel_sample_bams
        .combine(channel_chrom_ref_panel_sites)    
        .map { sample, bam, bam_index,
            chrom, ref_fasta_chr, ref_fasta_chr_index, ref_panel_sites_vcf, ref_panel_sites_vcf_index, 
            ref_panel_sites_tsv, ref_panel_sites_tsv_index, chunks, ref_maf_vcf, ref_maf_vcf_index,
            ref_maf_tsv, ref_maf_tsv_index, gen_map, ref_panel_phased, ref_panel_phased_index ->

            tuple(
                sample,
                chrom,
                bam,
                bam_index,
                ref_fasta_chr,
                ref_panel_phased_index,
                ref_panel_sites_vcf,
                ref_panel_sites_vcf_index,
                ref_panel_sites_tsv,
                ref_panel_sites_tsv_index,
                chunks,
                ref_maf_vcf,
                ref_maf_vcf_index,
                ref_maf_tsv,
                ref_maf_tsv_index, 
                gen_map,
                ref_panel_phased,
                ref_panel_phased_index
            )
        }

    channel_for_compute_gls = mega_channel
        .map { sample, chrom, bam, bam_index, ref_fasta_chr, ref_fasta_chr_index, ref_panel_sites_vcf,
            ref_panel_sites_vcf_index, ref_panel_sites_tsv, ref_panel_sites_tsv_index, chunks, 
            ref_maf_vcf, ref_maf_vcf_index, ref_maf_tsv, ref_maf_tsv_index, gen_map, ref_panel_phased, ref_panel_phased_index -> 
            tuple(
                sample,
                chrom,
                bam,
                bam_index,
                ref_panel_sites_vcf,
                ref_panel_sites_tsv,
                ref_fasta_chr
            )}

    compute_GLs_imputed_samples(channel_for_compute_gls)

    mega_channel = mega_channel
        .combine(compute_GLs_imputed_samples.out, by: [0,1])

    channel_impute = mega_channel
        .map {
            sample, chrom, bam, bam_index, ref_fasta_chr, ref_fasta_chr_index, ref_panel_sites_vcf, ref_panel_sites_vcf_index, 
            ref_panel_sites_tsv, ref_panel_sites_tsv_index, chunks, ref_maf_vcf, ref_maf_vcf_index,
            ref_maf_tsv, ref_maf_tsv_index, gen_map, ref_panel_phased, ref_panel_phased_index, GL_imputed_bam, GL_imputed_bam_index ->
            tuple(
                sample,
                chrom,
                GL_imputed_bam, GL_imputed_bam_index,
                ref_panel_phased, ref_panel_phased_index,
                chunks, gen_map
            )
        }

    impute(channel_impute)
    ligate(impute.out)
    
    sample_haplotype(ligate.out)

    mega_channel = mega_channel
        .combine(ligate.out, by: [0,1])
        .combine(sample_haplotype.out, by: [0,1])

    channel_annotate = mega_channel
        .map {
            sample, chrom, bam, bam_index, ref_fasta_chr, ref_fasta_chr_index, ref_panel_sites_vcf, 
            ref_panel_sites_vcf_index, ref_panel_sites_tsv, ref_panel_sites_tsv_index, chunks, 
            ref_maf_vcf, ref_maf_vcf_index, ref_maf_tsv, ref_maf_tsv_index, gen_map, ref_panel_phased, 
            ref_panel_phased_index, GL_imputed_bam, GL_imputed_bam_index, ligated_bcf, ligated_bcf_index, 
            phased_bcf, phased_bcf_index -> 
            tuple(
                sample,
                chrom,
                ligated_bcf, ligated_bcf_index,
                phased_bcf, phased_bcf_index
            )
        }
    annotate_fields(channel_annotate)

    channel_merge = annotate_fields.out
        .map { sample, chrom, phased_vcf_annotate, phased_vcf_annotate_index ->
            tuple(
                chrom, phased_vcf_annotate, phased_vcf_annotate_index
            )}
        .groupTuple()

    merge_phased_bcfs(channel_merge)
    
    channel_maf_sites = merge_phased_bcfs.out
        .combine(channel_chrom_ref_panel_sites, by: 0)
        .map {
            chrom, merged_phased_vcf, merged_phased_vcf_index, ref_fasta_chr, ref_fasta_chr_index, ref_panel_sites_vcf, 
            ref_panel_sites_vcf_index, ref_panel_sites_tsv, ref_panel_sites_tsv_index, chunks, 
            ref_maf_vcf, ref_maf_vcf_index, ref_maf_tsv, ref_maf_tsv_index, gen_map, ref_panel_phased, 
            ref_panel_phased_index ->
            tuple(
                chrom, merged_phased_vcf, merged_phased_vcf_index, ref_maf_tsv, ref_maf_tsv_index
            )
        }

    maf_sites_phased_vcf(channel_maf_sites)
    recalibrate_info_phased_vcf(maf_sites_phased_vcf.out)
    filter_recalibrated_INFO_phased_vcf(recalibrate_info_phased_vcf.out)
}


