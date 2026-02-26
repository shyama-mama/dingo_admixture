#!/bin/bash

BFILE=$1
OUT_PREFIX=$2

singularity exec -B /gpfs/ -B /scratchdata2/ /hpcfs/groups/acad_users/containers/emu_1.0.sif emu \
    --bfile ${BFILE} \
    -t 64 \
    --maf 0.01 \
    --n_eig 10 \
    -o ${OUT_PREFIX} \
    --iter 1000
