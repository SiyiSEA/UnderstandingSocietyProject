# Format the data

  arguments <- commandArgs(T)
  betas <- arguments[1]
  pData <- arguments[2]
  formattedData <- arguments[3]
  
  message("Loading the data...")
  load(betas)
  pData <- read.delim(pData, sep='\t', stringsAsFactors = FALSE)
  
  message("Dealing with betas matrix...")
  datMeth <- as.matrix(t(betas))
  
  # to add the health measurement
  message("Dealing with pheno data...")
  pData = pData[,c('id', 'nsex', 'confage', 'rackbarcode')]
  pData$Female = ifelse(pData$nsex == 'female', 1, 0)
  pData$Age = pData$confage
  
  message("Matching the data...")
  m <- match(pData$id, rownames(datMeth))
  datMeth <- datMeth[m,]
  
  message("Saving the formated data...")
  save(datMeth, pData, file = formattedData)
  
  message("Successfully done for the phase the data.")

