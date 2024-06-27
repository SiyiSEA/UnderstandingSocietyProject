#!/bin/sh
#SBATCH --export=ALL # export all environment variables to the batch job.
#SBATCH -p mrcq # submit to the serial queue
#SBATCH --time=24:00:00 # Maximum wall time for the job.
#SBATCH -A Research_Project-MRC190311 # research project to submit under. 
#SBATCH --nodes=1 # specify number of nodes.
#SBATCH --ntasks-per-node=16 # specify number of processors per node
#SBATCH --mail-type=END # send email at job completion 
#SBATCH --output=/lustre/home/sww208/USProject/demoPlan2Wave2%j.o
#SBATCH --error=/lustre/home/sww208/USProject/demoPlan2Wave2%j.e
#SBATCH --job-name=demoPlan2Wave2

set -e
module load R/4.2.1-foss-2022a
homeDir="/lustre/home/sww208/USProject"
clocksDir="${homeDir}/2_PCClocks/PC-Clocks/"
SD=5

# Due to the participants from wave2 have intact variables, 
# this script is for running a demo of Plan2 based on wave2 particpants.

## Step 1 - Format the data
# Aims:
# 1. extract pData only for wave2 and reorganise the data;
# 2. calculate the time span between the interview time and DNAm collection time;
# 3. cluster samples into three groups(a_wave1, b_wave2, not devided);
# 4. split data into three tables(bNDabHealth, aNDabHealth, abHealth);
# Inputs
# 1. 1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt: phenotype + variables of 2020
# 2. 4_Results/Age/PCClockAgeWave2.txt: clock aging table
# 3. 1_RawData/OMIC-49-SUGDEN_2020.gds: beta matrix of 2020 where contain wave3+2
# 4. 1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt: corrsponding phenotype of 2020
# Outputs
# 1. 6_Result_plan2/pDataWave2Plan2.txt: a_age_dv, b_age_dv and six health measures
# 2. 6_Result_plan2/bNDabHealth2217.txt
# 3. 6_Result_plan2/aNDabHealth2217.txt
# 4. 6_Result_plan2/abHealth2176.txt
Rscript ${homeDir}/5_Resources/DemoAgeDv.R

## Step 2 - Module Prediction with time span
# Aims:
# module one: aging ~ health measure + sex + confage + time span + barcode
# module one: aging ~ health measure + sex + confage + time span + barcode + cellcount
# Inputs
# 1. 6_Result_plan2/aNDabHealth2217.txt: covariates
# 2. 4_Results/Age/PCClockAgeWave2.txt: clock aging table
# Outputs
# 1. ModelXXX.pdf - plot(model), 4 plots for each model
# 2. ModelXXXCoeffSum.txt - Estimate  Std. Error  t value Pr(>|t|) with intercept
# 3. ModelXXXCoeffPvalueSum.txt - Estimate  Std. Error  t value Pr(>|t|) without intercept
# 4. ModelXXXResi.txt - sf1M1Resi sf1M2Resi sf1M3Resi sf1M4Resi scsf1M1Resi scsf1M2Resi scsf1M3Resi
# 5. ModelXXXlog.txt -log file for model

for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoModelPreWave2.R \
        "${homeDir}/6_Result_plan2/aNDabHealth2217.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/aNDabHealth2217" \
        $clock
done


for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoModelPreWave2.R \
        "${homeDir}/6_Result_plan2/bNDabHealth2217.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/bNDabHealth2217" \
        $clock
done



for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoModelPreWave2.R \
        "${homeDir}/6_Result_plan2/abHealth2176.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/abHealth2176" \
        $clock
done

# Step 3 - Module Prediction with health change
# Aims:
# module one: aging ~ change in health measure + sex + confage + time span + barcode
# module one: aging ~ change in health measure + sex + confage + time span + barcode + cellcount
# Inputs:
# 1. first arg - covariates - age, sex, cell type, health measure
# 2. second arg - health measure variables
# 3. thrid arg - PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
# Outputs:
# 1. ModelXXX.pdf - plot(model), 4 plots for each model
# 2. ModelXXXCoeffSum.txt - Estimate  Std. Error  t value Pr(>|t|) with intercept
# 3. ModelXXXCoeffPvalueSum.txt - Estimate  Std. Error  t value Pr(>|t|) without intercept
# 4. ModelXXXResi.txt - sf1M1Resi sf1M2Resi sf1M3Resi sf1M4Resi scsf1M1Resi scsf1M2Resi scsf1M3Resi
# 5. ModelXXXlog.txt -log file for model


for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoHealthChange.R \
        "${homeDir}/6_Result_plan2/aNDabHealth2217.txt" \
        "${homeDir}/6_Result_plan2/pDataWave2Plan2.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/HealthChange/aNDabHealth2217_HealthChange" \
        $clock
done


for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoHealthChange.R \
        "${homeDir}/6_Result_plan2/bNDabHealth2217.txt" \
        "${homeDir}/6_Result_plan2/pDataWave2Plan2.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/HealthChange/bNDabHealth2217_HealthChange" \
        $clock
done



for clock in PCPhenoAge PCGrimAge DunedinPACE
do 
    echo "Module Prediction on $clock"
    Rscript ${homeDir}/5_Resources/DemoHealthChange.R \
        "${homeDir}/6_Result_plan2/abHealth2176.txt" \
        "${homeDir}/6_Result_plan2/pDataWave2Plan2.txt" \
        "${homeDir}/4_Results/Age/PCClockAgeWave2.txt" \
        "${homeDir}/6_Result_plan2/HealthChange/abHealth2176_HealthChange" \
        $clock
done