#!/bin/bash

#SBATCH --job-name=salts_exp_bv_SVO-OSV_bayes_nested
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=week
#SBATCH --constraint=cascadelake
#SBATCH --time=02-00:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.4.1-foss-2022b

echo Running script: scripts/bayesian_analysis/bayesian_analysis_nested.sh

cd analysis/

Rscript Bayesian\ scripts/models/model_nested.r
