# Load the Functions you need to calculate the PCClocks (you need to change the path to the directory 
#       where you installed the code)

# Load the file with your pheno data and methylation data in it (Here we used the example data included)
# load(paste(clocksDir,"Extras/Example_PCClock_Data_final.RData",sep=""))

# IMPORTANT FORMATTING NOTE: If you are not using the example methylation and Pheno data, you will need to have specific
#     formatting. Please ensure that your Methylation dataframe/ matrix is of the methylation beta values and row names
#     are sample names, and column names are CpGs.
#     For the pheno data, ensure that the data frame/ matrix has rows as samples, and columns as whatever phenotype
#     variables you have/ wish. This can also include the original CpG clocks if you used the online Horvath calculator
#     as well. HOWEVER, the pheno matrix MUST have a column named "Age", and a column named "Female" (capital required),
#     especially if you want to calculate GrimAge and its components. Female = 1 is F and Female = 0 is M.
#
#     If you don't have this information, you can also just set it so Females is all 1 (all samples labeled female) and
#     all the same Age. Just know that this won't be accurate for PCGrimAge or components, and that you can't run
#     the acceleration calculations with calcPCClock_Accel.
#
#     The code below is going to ask if you would like to check the order of your datPheno and datMeth samples to ensure
#     they line up. For this to work, you will need to type the column name of datPheno with the names of the samples or 
#     'skip'.

  arguments <- commandArgs(T)
  formattedData <- arguments[1]
  agePred <- arguments[2]
  clocksDir <- arguments[3]
  
  # load data and funtions
  message("Loading the data...")
  load(formattedData)
  
  source(paste0(clocksDir, "run_calcPCClocks.R", sep = ""))
  source(paste0(clocksDir, "run_calcPCClocks_Accel.R", sep = ""))
  
  datMeth = as.matrix(t(betas))
  datPheno = pData[,c('id', "Age_numeric")]
  colnames(datPheno) = c('id', 'Age')
  datPheno$Female = ifelse(pData$Sex_factor == 'female', 1, 0)
    
  # Get the PC Clocks values and the PC Clock Acceleration values
  message("Getting PC Clocks vlaues...")
  PCClock <- calcPCClocks(path_to_PCClocks_directory = clocksDir, datMeth = datMeth, datPheno = datPheno)
  
  #in order to calculate Acceleration below, you will need a column called "Age", just as was needed for PCGrimAge.
  # PCClock_DNAmAge <- calcPCClocks_Accel(PCClock_DNAmAge)
  
  # Get the DunedinPACE value
  message("Getting DunedinPACE vlaues...")
  
  # update DunedinPACE
  # remotes::install_github("danbelsky/DunedinPACE", force = TRUE)
  library(DunedinPACE)
  PCClock$DunedinPACE = PACEProjector(betas, proportionOfProbesRequired=0.7)$DunedinPACE
  
  # Save the clocks result 
  message("Saving the age predicted clocks...")
  write.table(PCClock, file=agePred, row=F, col=T, qu=F)
  
  message("Successfully done for the calculation on the PCClocks and DunedinPACE.")


