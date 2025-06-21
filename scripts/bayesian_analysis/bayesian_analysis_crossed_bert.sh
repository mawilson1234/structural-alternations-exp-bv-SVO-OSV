#!/bin/bash

#SBATCH --job-name=salts_exp_bv_SVO-OSV_bayes_crossed_bert
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16G
#SBATCH --constraint=cascadelake
#SBATCH --partition=day
#SBATCH --time=12:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module unload miniconda
module load R/4.4.1-foss-2022b

echo Running script: scripts/bayesian_analysis/bayesian_analysis_crossed_bert.sh

cd analysis/

Rscript Bayesian\ scripts/models/model_crossed_bert.r
