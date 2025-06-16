#!/bin/bash


in1=$1
in2=poplist.txt

singularity exec -B /gpfs/ docker://quay.io/biocontainers/eigensoft:8.0.0--h6a739c9_3.sif smartpca -p <(echo "genotypename:	${in1}.geno
snpname:	${in1}.snp
indivname:	${in1}.ind
evecoutname:	${in1}.pca.evec.txt
evaloutname:	${in1}.pca.eval.txt
poplistname:	${in2}
lsqproject:	YES
numthreads:	4
shrinkmode:	YES
numoutevec:	10
numchrom: 38")
