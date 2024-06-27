# Combine on 2017 and 2020

main <- function() {

  arguments <- commandArgs(T)
  formattedData2017 <- arguments[1]
  formattedData2020 <- arguments[2]
  formattedDataCombine <- arguments[3]

  message("Loading the data...")
  load(formattedData2017)
  load(formattedData2020)

  message("Combining the data...")
  datMeth = rbind(datMeth2017, datMeth2020)
  pData = rbind(pData2017, pData2020)
  
  message("Classifying variable types ...")
  pData$Female = ifelse(pData$nsex == 'female', 1, 0)
  pData$Age = as.numeric(pData$confage)
  pData$rackbarcode = as.factor(pData$rackbarcode)

  message("Saving the combined methlyation data and pheno data ...")
  save(datMeth, pData, file = formattedDataCombine)

}