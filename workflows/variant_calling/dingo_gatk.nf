#!/usr/bin/env nextflow

nextflow.enable.dsl = 2

// Gatk Haplotype caller
process GATK {
    input:
    tuple val(sample), val(chr), file(bam), file(bai), file(alleles), file(alleles_index), file(fasta), file(fai), file(dict)

    output:
    tuple val(sample), file("${sample}_${chr}.haplotypecaller.vcf.gz"), file("${sample}_${chr}.haplotypecaller.vcf.gz.tbi")

    script:
    if(params.test) {
    """
    touch ${sample}_${chr}.haplotypecaller.vcf.gz
    touch ${sample}_${chr}.haplotypecaller.vcf.gz.tbi
    """
    } else {
    """
    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/gatk4_4.6.1.0--py310hdfd78af_0.sif \
        gatk HaplotypeCaller --java-options "-Xmx10G" \
        -R ${fasta} -I ${bam} -O ${sample}_${chr}.haplotypecaller.vcf \
        -stand-call-conf 30 --sample-ploidy 2 --alleles ${alleles} \
        --output-mode "EMIT_VARIANTS_ONLY" -L ${chr}
    
    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bgzip ${sample}_${chr}.haplotypecaller.vcf

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools index -t ${sample}_${chr}.haplotypecaller.vcf.gz
    """
    }
}

// Merge Chrs
process CONCAT_SHARDS {
    input:
    tuple val(sample), file(vcfs), file(tbis)

    output:
    tuple val(sample), file("${sample}.concat.vcf.gz"), file("${sample}.concat.vcf.gz.tbi")

    script:
    if(params.test) {
    """
    touch ${sample}.concat.vcf.gz
    touch ${sample}.concat.vcf.gz.tbi
    """
    } else {
    """    
    for seq in \$(seq 1 38); do
        echo "${sample}_chr\${seq}.haplotypecaller.vcf.gz"
    done > file.list

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools concat -f file.list -Oz -o ${sample}.concat.vcf.gz

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools index -t ${sample}.concat.vcf.gz
    """
    }
}

// Filter by cov
process FILTER_VCF {
    input:
    tuple val(sample), val(min_cov), val(max_cov), file(concat_vcf), file(concat_tbi)

    output:
    tuple file("${sample}.filtered.vcf.gz"), file("${sample}.filtered.vcf.gz.tbi")

    script:
    if(params.test) {
    """
    echo "${min_cov} ${max_cov}" > ${sample}.filtered.vcf.gz
    touch ${sample}.filtered.vcf.gz.tbi
    """
    } else {
    """
    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools view -i 'MIN(FMT/DP)>${min_cov} & MAX(FMT/DP)<${max_cov}' -M2 -v snps -Oz -o ${sample}.filtered.vcf.gz ${concat_vcf}

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools index -t ${sample}.filtered.vcf.gz
    """
    }
}

// merge samples
process MERGE_SAMPLES {
    publishDir "plink/", mode: 'copy'

    input:
    file vcfs
    val prefix

    output:
    file "${prefix}.*"

    script:
    if(params.test) {
    """
    touch ${prefix}.bim
    touch ${prefix}.bam
    touch ${prefix}.fam
    """
    } else {
    """
    ls -1 *vcf.gz > file.list

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools merge --file-list file.list -Oz -o merged_filtered.vcf.gz

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/nf-core-eager_2.4.5-sharding.sif \
    bcftools index -t merged_filtered.vcf.gz

    singularity exec -B /hpcfs/,/gpfs/,/scratchdata1/,/scratchdata2/ /hpcfs/groups/acad_users/containers/plink_1.90b6.21--h031d066_5.sif \
    plink --vcf merged_filtered.vcf.gz --dog --double-id --make-bed --out ${prefix} 
    """
    }
}

workflow {
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
    
    channel_sample_cutoffs = Channel
        .fromPath(params.sample_metadata, checkIfExists: true)
        .splitCsv(header: ['sample', 'min', 'max'], sep: '\t', skip:1)
        .map { row -> 
            tuple(
                row.sample,
                row.min,
                row.max
            )
        }

    fasta = file(params.fasta)
    fasta_index = file(params.fasta + '.fai')
    dict = file(params.dict)
    alleles = file(params.alleles)
    alleles_index = file(params.alleles + '.idx')
    prefix = params.prefix

    allele_channel = Channel.from(alleles).combine(Channel.from(alleles_index))
    fasta_channel = Channel.from(fasta).combine(Channel.from(fasta_index)).combine(Channel.from(dict))

    if(params.test) {
    chromosomes = Channel
        .from(1..2)
        .map {
            it -> "chr" + it
        }
    } else {
    chromosomes = Channel
        .from(1..38)
        .map {
            it -> "chr" + it
        }
    }

    gatk_input = channel_sample_bams
        .combine(chromosomes)
        .map {
            sample, bam, bai, chr -> 
                tuple(
                    sample, chr, bam, bai
                )
        }
        .combine(allele_channel)
        .combine(fasta_channel)

    GATK(gatk_input)

    merge_input = GATK.out
        .groupTuple(by:0)
    
    CONCAT_SHARDS(merge_input)

    filter_input = CONCAT_SHARDS.out
        .combine(channel_sample_cutoffs, by:0)
        .map {
            sample, vcf, index, min_cov, max_cov ->
            tuple(
                sample, min_cov, max_cov, vcf, index
            )
        }

    FILTER_VCF(filter_input)

    MERGE_SAMPLES(FILTER_VCF.out.collect(), params.prefix)

}
