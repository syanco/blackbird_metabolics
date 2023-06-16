#!/bin/bash

#SBATCH -t 1-
#SBATCH --mail-type ALL
#SBATCH --mail-user scott.yanco@yale.edu
#SBATCH --partition day,week
#SBATCH -c 48
#SBATCH --mem-per-cpu 2G
#SBATCH -J Blackbird-Met-20230615

# Load conda env
module load miniconda
conda activate nichemapr

# Declare WD
wd=/home/sy522/project/blackbird_POL

# Move to WD
cd $wd

# Execute calc size script/
Rscript $wd/src/workflow/met_model_full_par.r

