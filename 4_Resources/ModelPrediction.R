library(dplyr)
library(plyr)

# parse the covariates for building the modules ####

arguments <- commandArgs(T)
# covariates - age, sex, cell type, health measure
pData <- arguments[1]
# PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
PCClock <- arguments[2]
# smokPred - estimated smoking score txt file with header
smokPred <- arguments[3]
# residOutput - path for output file 
residOutput <- arguments[4]
# wave2 - identify if the input files are all belongs to wave2 or not
wave2 <- arguments[5]
# whichClock - identify the models are built for specify clcok
whichClock <- arguments[6]


load(pData)
residOutput <- paste0(residOutput, whichClock)
PCClock = read.table(PCClock, stringsAsFactors = FALSE, header = T)
smokPred = read.table(smokPred, stringsAsFactors = FALSE, header = T)
CellCount = pData[, c("id", "cd8t", "cd4t", "nk", "bcell", "mono")]
Health = pData[, c("id", "sf1", "scsf1", "scsf2a", "scsf2b", "scsf3a", "scsf3b", "scsf5", "scsf7")]
Health[Health == ""] <- NA
SexAge = pData[,c("id", "Age_numeric", "Sex_factor", "Rackbarcode_factor")]
PCSexAge = merge(SexAge, PCClock[,c("id", whichClock)], by.y = "id", by.x = "id")
colnames(PCSexAge)[which(colnames(PCSexAge)==whichClock)] = "PredAge"

if (wave2 == T){
  SmokingWave = pData[,c("id", "smcigs", "smnow")]
  SmokingWave[SmokingWave == ""] <- NA
  SmokingWave$smcigsValue = as.numeric(revalue(SmokingWave[,"smcigs"], c("smoke them only occasionally"=2, "smoked regularly, at least one per day"=1,
                                                                         "spontaneous never really smoked cigarettes, just tried them once or twice"=3)))
  SmokingWave$smnowValue = as.numeric(revalue(SmokingWave[,"smnow"], c("no"=2, "yes"=1)))
  SmokingWave = SmokingWave[,c('id', 'smcigsValue', 'smnowValue')]
}else{
  SmokingWave=NULL
}


# Build Models for each health measure ####

# outputs:
# 1. ModelXXX.pdf - plot(model), 4 plots for each model
# 2. ModelXXXCoeffSum.txt - Estimate  Std. Error  t value Pr(>|t|) with intercept
# 3. ModelXXXCoeffPvalueSum.txt - Estimate  Std. Error  t value Pr(>|t|) without intercept
# 4. ModelXXXResi.txt - sf1M1Resi sf1M2Resi sf1M3Resi sf1M4Resi scsf1M1Resi scsf1M2Resi scsf1M3Resi
# 5. ModelXXXlog.txt -log file for model

BuildModels <- function(Health, CellCount, Smoking, SmokingWave=NULL, PCSexAge, whichClock, output){
  
  coeffSum =c()
  coeffpvalueSum = c()
  ResiTable <- PCSexAge[, c('id', "PredAge")]
  pdf(paste0(output, '.pdf'), width=12, height=12)
  sink(paste0(output, 'log.txt'), append = F)
  
  for (HealthMeasure in colnames(Health)[-1]) {
    print(HealthMeasure)
    PCSexAgeHealth = merge(PCSexAge, Health[, c('id', HealthMeasure)], by.x = 'id', by.y = 'id')
    index = which(colnames(PCSexAgeHealth) == HealthMeasure)
    
    if (HealthMeasure %in% c('sf1','scsf1')) {
      value = c("excellent"=1, "very good"=2, "good"=3, "fair"=4,"poor"=5)
    } else if (HealthMeasure %in% c('scsf2a','scsf2b')) {
      value = c("no, not limited at all"=3, "yes, limited a little"=2, "yes, limited a lot"=1)
    } else if (HealthMeasure %in% c('scsf3a','scsf3b','scsf7')) {
      value = c("none of the time"=5, "a little of the time"=4, "some of the time"=3,"most of the time"=2, "all of the time"=1)
    } else {
      value = c("extremely"=5, "quite a bit"=4, "moderately"=3,"a little bit"=2, "not at all"=1)
    }
    
    PCSexAgeHealth$Value = revalue(PCSexAgeHealth[,c(HealthMeasure)], value)
    PCSexAgeHealth$Value = as.numeric(PCSexAgeHealth$Value)
    PCSexAgeHealth = PCSexAgeHealth[-c(index)]
    
    par(mfrow=c(2,2))
    
    message(paste0(whichClock,' ~ ', HealthMeasure,' + age + sex + barcode'))
    M1 = lm(PredAge ~ ., data = PCSexAgeHealth[, -1], na.action = na.exclude)
    coeffM1 = summary(M1)$coefficients[c('(Intercept)', 'Value'),]
    coeffpvalueM1 = summary(M1)$coefficients[c('Value'),]
    plot(M1, main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + sex + barcode'))
    ResiTable[,paste0(HealthMeasure,'M1Resi')] = residuals(M1)
    print(summary(M1))
    
    message(paste0(whichClock,' ~ ',  HealthMeasure,' + age + sex + barcode + cellcount'))
    M2table = merge(PCSexAgeHealth, CellCount, by.x = 'id', by.y = 'id')
    M2 = lm(PredAge ~ ., data = M2table[-1], na.action = na.exclude)
    coeffM2 = summary(M2)$coefficients[c('(Intercept)', 'Value'),]
    coeffpvalueM2 = summary(M2)$coefficients[c('Value'),]
    plot(M2,  main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + sex + barcode + cellcount'))
    ResiTable[,paste0(HealthMeasure,'M2Resi')] = residuals(M2)
    print(summary(M2))
    
    message(paste0(whichClock,' ~ ', HealthMeasure,' + age + sex + barcode + estimated smoking'))
    M3table = merge(PCSexAgeHealth, Smoking, by.x = 'id', by.y = 'id')
    M3 = lm(PredAge ~ ., data = M3table[,-1], na.action = na.exclude)
    coeffM3 = summary(M3)$coefficients[c('(Intercept)', 'Value'),]
    coeffpvalueM3 = summary(M3)$coefficients[c('Value'),]
    plot(M3, main=paste0(whichClock,' ~ ', HealthMeasure,' + age + sex + barcode + estimated smoking'))
    ResiTable[,paste0(HealthMeasure,'M3Resi')] = residuals(M3)
    print(summary(M3))
    
    coeffTemp = rbind(coeffM1, coeffM2, coeffM3)
    rownames(coeffTemp) = c(paste0(HealthMeasure, 'M1Intercept'), paste0(HealthMeasure, 'M1'),
                           paste0(HealthMeasure, 'M2Intercept'), paste0(HealthMeasure, 'M2'),
                           paste0(HealthMeasure, 'M3Intercept'), paste0(HealthMeasure, 'M3'))
    coeffSum = rbind(coeffSum, coeffTemp)
    
    coeffpvalue = rbind(coeffpvalueM1, coeffpvalueM2, coeffpvalueM3)
    rownames(coeffpvalue) = c(paste0(HealthMeasure, 'M1'),
                              paste0(HealthMeasure, 'M2'),
                              paste0(HealthMeasure, 'M3'))
    
    
    if (!(is.null(SmokingWave))) {
      message(paste0(whichClock, ' ~ ', HealthMeasure,' + age + sex + barcode + (smcigs smnow) smoking'))
      M4table = merge(PCSexAgeHealth, SmokingWave, by.x = 'id', by.y = 'id')
      M4 = lm(PredAge ~ ., data = M4table[,-1], na.action = na.exclude)
      coeffM4 = summary(M4)$coefficients[c('(Intercept)', 'Value'),]
      coeffpvalueM4 = summary(M4)$coefficients[c('Value'),]
      rownames(coeffM4) = c(paste0(HealthMeasure, 'M4Intercept'), paste0(HealthMeasure, 'M4'))
      plot(M4, main=paste0(whichClock,' ~ ', HealthMeasure,' + age + sex + barcode + (smcigs smnow) smoking'))
      ResiTable[,paste0(HealthMeasure,'M4Resi')] = residuals(M4)
      print(summary(M4))
      
      coeffSum = rbind(coeffSum, coeffM4)
      coeffpvalue = rbind(coeffpvalueM1, coeffpvalueM2, coeffpvalueM3, coeffpvalueM4)
      rownames(coeffpvalue) = c(paste0(HealthMeasure, 'M1'),paste0(HealthMeasure, 'M2'),
                                paste0(HealthMeasure, 'M3'),paste0(HealthMeasure, 'M4'))
      
    }
    coeffpvalueSum = rbind(coeffpvalueSum, coeffpvalue)
    
    
    write.table(coeffpvalueSum, file = paste0(output, 'CoeffPvalueSum.txt'), row=T, col=T, qu=F, sep = '\t')
    write.table(coeffSum, file = paste0(output, 'CoeffSum.txt'), row=T, col=T, qu=F, sep = '\t')
    write.table(ResiTable, file = paste0(output, 'Resi.txt'), row=F, col=T, qu=F)
  }
  dev.off()
  sink()
}


BuildModels(Health=Health, CellCount=CellCount, Smoking=smokPred, 
            SmokingWave=SmokingWave, PCSexAge=PCSexAge, 
            whichClock = whichClock, output = residOutput)

