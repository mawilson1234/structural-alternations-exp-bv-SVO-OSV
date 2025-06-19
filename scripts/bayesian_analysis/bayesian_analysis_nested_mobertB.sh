#!/bin/bash

#SBATCH --job-name=salts_exp_bv_SVO-OSV_bayes_nested_mobertB
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=day
#SBATCH --constraint=cascadelake
#SBATCH --time=12:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.4.1-foss-2022b

echo Running script: scripts/bayesian_analysis/bayesian_analysis_nested_mobertB.sh

cd analysis/

Rscript Bayesian\ scripts/models/model_nested_mobertB.r
