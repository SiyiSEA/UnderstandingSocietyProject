#!/bin/sh
#SBATCH --export=ALL # export all environment variables to the batch job.
#SBATCH -p mrcq # submit to the serial queue
#SBATCH --time=24:00:00 # Maximum wall time for the job.
#SBATCH -A Research_Project-MRC190311 # research project to submit under. 
#SBATCH --nodes=1 # specify number of nodes.
#SBATCH --ntasks-per-node=16 # specify number of processors per node
#SBATCH --mail-type=END # send email at job completion 
#SBATCH --output=/lustre/home/sww208/USProject/USproject%j.o
#SBATCH --error=/lustre/home/sww208/USProject/USproject%j.e
#SBATCH --job-name=AgeEstimation

set -e
module load R/4.2.1-foss-2022a
homeDir="/lustre/home/sww208/USProject"
clocksDir="${homeDir}/2_PCClocks/PC-Clocks/"
SD=5


## Step 1 - Format the data: the script will generate four pairs of dataset
# Inputs
# 1. 1_RawData/OMIC-49-SUGDEN.gds: beta matrix of 2017 where selected from wave3
# 2. 1_RawData/OMIC-49-SUGDEN_epi_sendout.txt: corresponding phenotype of 2017
# 3. 1_RawData/OMIC-49-SUGDEN_2020.gds: beta matrix of 2020 where contain wave3+2
# 4. 1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt: corrsponding phenotype of 2020
# Outputs
# 1. BetasPhen2017.RData: betas + sorted pheno Data (1174)
# 2. BetasPhen2020.RData: betas + sorted pheno Data (2480)
# 3. BetasPhenCombined.RData:  betas + sorted pheno Data (3654)
# 4. BetasPhenWave2.RData from batch2020: betas + sorted pheno Data (2234)
Rscript ${homeDir}/5_Resources/OrganizedData.R


## Step 2 - Calculate the age clock
# Inputs
# 1. first argument: *.RData - beta matrix and phenotype data 
# 2. second argument: .txt - PCClockAge for output
# 3. thrid argument: PCClocks package comes from
# Outputs
# 1. PCClockAge2017.txt: predicted age based on diff clocks (1174)
# 2. PCClockAge2020.txt: predicted age based on diff clocks (2480)
# 3. PCClockAgeWave2.txt: predicted age based on diff clocks (2234)
# Noted
# no BetasPhenCombiend.RData as input, because it is too big to be loaed
for batch in 2017 2020 Wave2
do
    Rscript ${homeDir}/5_Resources/AgeEstimation.R \
        "${homeDir}/1_RawData/BetasPhen${batch}.RData" \
        "${homeDir}/4_Results/Age/PCClockAge${batch}.txt" \
        "${homeDir}/2_PCClocks/PC-Clocks/"
done


# Step 3 - Plot age clock
# Merge PCClockAge2017.txt and PCClockAge2020.txt into PCClockAgeCombined
# Plot the scatter plot, hist and QQ for the PCClockAgeCombined on each clock
# Output 
# 1. PCClockAgeCombined.txt: txt file with Predicted age based on different clock (3654)
# 2. AgePlot.pdf: viualization on each clocks
Rscript ${homeDir}/5_Resources/AgePlot.R \
   "${homeDir}/4_Results/Age/PCClockAge2017.txt" \
   "${homeDir}/4_Results/Age/PCClockAge2020.txt" \
   "${homeDir}/4_Results/Age/PCClockAgeCombined.txt" \
   "${homeDir}/4_Results/Age/AgePlot.pdf"

# Step 4 - Estimation on smoking
# Outputs
# 1. SmokeEstimated.txt: Combination of estimated smoking from 2017 and 2020 (3654)
# 2. SmokeEstimated.plink: plink format of the estimated smoking from 2017 and 2020 (3654)
# 3. SmokeEstimated.pdf: visluzation of the smoking score
# 4. SmokeEstimatedWave2.txt: estimated smoking only for wave2 (2234)
# 5. SmokeEstimatedWave2.plink: estimated smoking n plink format only for wave2 (2234)
# 6. SmokeEstimatedWave2.pdf: visluzation of the smoking score only for wave2 (2234)
Rscript ${homeDir}/5_Resources/SmokeEstimation.R \
   "${homeDir}/1_RawData/BetasPhen2017.RData" \
   "${homeDir}/1_RawData/BetasPhen2020.RData" \
   "${homeDir}/1_RawData/BetasPhenWave2.RData" \
   "${homeDir}/4_Results/Smoke/SmokeEstimated" \
   5

# Step 5 - Module Prediction
# Inputs:
# 1. first arg - covariates - age, sex, cell type, health measure
# 2. second arg - PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
# 3. thrid arg - smokPred - estimated smoking score txt file with header
# 4. forth arg - residOutput - path for output file 
# 5. fifth arg - wave2 - identify if the input files are all belongs to wave2 or not
# 6. sixth arg - whichClock - identify the models are built for specify clcok
# Outputs:
# 1. ModelXXX.pdf - plot(model), 4 plots for each model
# 2. ModelXXXCoeffSum.txt - Estimate  Std. Error  t value Pr(>|t|) with intercept
# 3. ModelXXXCoeffPvalueSum.txt - Estimate  Std. Error  t value Pr(>|t|) without intercept
# 4. ModelXXXResi.txt - sf1M1Resi sf1M2Resi sf1M3Resi sf1M4Resi scsf1M1Resi scsf1M2Resi scsf1M3Resi
# 5. ModelXXXlog.txt -log file for model
for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/ModelPrediction.R \
        "${homeDir}/1_RawData/BetasPhenCombined.RData" \
        "${homeDir}/4_Results/Age/PCClockAgeCombined.txt" \
        "${homeDir}/4_Results/Smoke/SmokeEstimated.txt" \
        "${homeDir}/4_Results/Module/ModelCombined" \
        "F"
        $clock
done


for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction for wave2 on $clock"
    Rscript ${homeDir}/5_Resources/ModelPrediction.R \
          "${homeDir}/1_RawData/pDataSortedWave2.RData" \
          "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
          "${homeDir}/4_Results/Smoke/SmokeEstimatedWave2.txt" \
          "${homeDir}/4_Results/Module/ModelWave2" \
          "F"
          $clock
done


# Step 6 - Boxplot for residual
# Inputs:
# 1. first arg - covariates - age, sex, cell type, health measure
# 2. second arg - PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
# 3. thrid arg - resiPath - fetch folder path to residual
# 4. forth arg - wave2 - TRUE or FALSE
# 5. fifth arg - whichClock - identify the models are built for specify clcok
# Outputs:
# 1. ModelWave2BoxPlotPC*AgeRegress.pdf : age residual on each health measure for wave 2 (contains smoking health statue)
# 2. ModelWave2BoxPlotPC*Age.pdf: predicted age on each health measure for wave 2 (contains smoking health statue)
# 3. ModelCombinedBoxPlotPC*AgeRegress.pdf: age residual on each health measure
# 4. ModelCombinedBoxPlotPC*Age.pdf: predicted age on each health measure
for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Making boxplot on $clock vs each health measurement"
    Rscript ${homeDir}/5_Resources/HealthBoxPlot.R \
        "${homeDir}/1_RawData/pDataSortedCombined.RData" \
        "${homeDir}/4_Results/Age/PCClockAgeCombined.txt" \
        "${homeDir}/4_Results/Module/ModelCombined" \
        F \
        $clock

done


for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Making boxplot for wave2 on $clock vs each health measurement"
    Rscript ${homeDir}/5_Resources/HealthBoxPlot.R \
        "${homeDir}/1_RawData/pDataSortedWave2.RData" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/4_Results/Module/ModelWave2" \
        T \
        $clock
done


