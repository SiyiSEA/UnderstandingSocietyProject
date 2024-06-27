library(dplyr)
library(plyr)

arguments <- commandArgs(T)
# covariates - age, sex, cell type, health measure
filename <- arguments[1]
# PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
PCClock <- arguments[2]
# residOutput - path for output file 
residOutput <- arguments[3]
# whichClock - identify the models are built for specify clcok
whichClock <- arguments[4]


pData = read.table(filename, sep = '\t', header = T)
pData <- as.data.frame(apply(pData, 2, function(x) gsub("^\\s+|\\s+$", "",x)))
PCClock = read.table(PCClock, stringsAsFactors = FALSE, header = T)
residOutput <- paste0(residOutput, whichClock)

CellCount = pData[, c("id", "cd8t", "cd4t", "nk", "bcell", "mono")]
Health = pData[, c("id", "generalHealth", "limitsAct", "limitsStairs", "limitsAmountWork", "limitsTypeWork", "painWork")]
Health[Health == ""] <- NA
SexAge = pData[,c("id", "confage", "timeSpan","nsex", "rackbarcode")]
colnames(SexAge) = c("id", "Age_numeric", "TimeSpan_numeric","Sex_factor", "Rackbarcode_factor")
PCSexAge = merge(SexAge, PCClock[,c("id", whichClock)], by.y = "id", by.x = "id")
colnames(PCSexAge)[which(colnames(PCSexAge)==whichClock)] = "PredAge"

BuildModels <- function(Health, CellCount, PCSexAge, whichClock, output){
  
  coeffSum =c()
  coeffpvalueSum = c()
  ResiTable <- PCSexAge[, c('id', "PredAge")]
  pdf(paste0(output, '.pdf'), width=12, height=12)
  sink(paste0(output, 'log.txt'), append = F)
  
  for (HealthMeasure in colnames(Health)[-1]) {
    print(HealthMeasure)
    PCSexAgeHealth = merge(PCSexAge, Health[, c('id', HealthMeasure)], by.x = 'id', by.y = 'id')
    index = which(colnames(PCSexAgeHealth) == HealthMeasure)
    
    if (HealthMeasure %in% c('generalHealth')) {
      value = c("excellent"=1, "very good"=2, "good"=3, "fair"=4,"poor"=5, "or Poor?"=5)
    } else if (HealthMeasure %in% c("limitsAct", "limitsStairs")) {
      value = c("no, not limited at all"=3, "yes, limited a little"=2, "yes, limited a lot"=1)
    } else if (HealthMeasure %in% c("limitsAmountWork", "limitsTypeWork")) {
      value = c("none of the time"=5, "a little of the time"=4, "some of the time"=3,"most of the time"=2, "all of the time"=1)
    } else {
      value = c("extremely"=5, "quite a bit"=4, "moderately"=3,"a little bit"=2, "not at all"=1)
    }
    
    PCSexAgeHealth$Value = revalue(PCSexAgeHealth[,c(HealthMeasure)], value)
    PCSexAgeHealth$Value = as.factor(PCSexAgeHealth$Value)
    PCSexAgeHealth$Age_numeric = as.numeric(PCSexAgeHealth$Age_numeric)
    PCSexAgeHealth$TimeSpan_numeric = as.numeric(PCSexAgeHealth$TimeSpan_numeric)
    PCSexAgeHealth$Sex_factor = as.factor(PCSexAgeHealth$Sex_factor)
    PCSexAgeHealth = PCSexAgeHealth[-c(index)]
    
    CellCount$cd8t = as.numeric(CellCount$cd8t)
    CellCount$cd4t = as.numeric(CellCount$cd4t)
    CellCount$nk = as.numeric(CellCount$nk)
    CellCount$bcell = as.numeric(CellCount$bcell)
    CellCount$mono = as.numeric(CellCount$mono)
    
    par(mfrow=c(2,2))
    
    message(paste0(whichClock,' ~ ', HealthMeasure,' + age + time span + sex + barcode'))
    M1 = lm(PredAge ~ ., data = PCSexAgeHealth[, -1], na.action = na.exclude)
    
    coeffM1 = as.data.frame(summary(M1)$coefficients) %>%
      dplyr::filter(row.names(as.data.frame(summary(M1)$coefficients)) %in% 
                      c('(Intercept)', 'Value1','Value2', 'Value3', 'Value4', 'Value5'))
    coeffpvalueM1 = coeffM1[-1,]
    plot(M1, main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode'))
    ResiTable[,paste0(HealthMeasure,'M1Resi')] = residuals(M1)
    print(summary(M1))
    
    message(paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
    M2table = merge(PCSexAgeHealth, CellCount, by.x = 'id', by.y = 'id')
    M2 = lm(PredAge ~ ., data = M2table[-1], na.action = na.exclude)
    coeffM2 = as.data.frame(summary(M2)$coefficients) %>%
      dplyr::filter(row.names(as.data.frame(summary(M2)$coefficients)) %in% 
                      c('(Intercept)', 'Value1','Value2', 'Value3', 'Value4', 'Value5'))
    coeffpvalueM2 = coeffM2[-1,]
    plot(M2,  main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
    ResiTable[,paste0(HealthMeasure,'M2Resi')] = residuals(M2)
    print(summary(M2))
    
    i = 1
    for (valuename in rownames(coeffM1)){
      rownames(coeffM1)[i] = paste0(HealthMeasure, 'M1', valuename)
      rownames(coeffM2)[i] = paste0(HealthMeasure, 'M2', valuename)
      i = i + 1
    } 
    
    coeffTemp = rbind(coeffM1, coeffM2)
    coeffSum = rbind(coeffSum, coeffTemp)
    
    i = 1
    for (valuename in rownames(coeffpvalueM1)){
      rownames(coeffpvalueM1)[i] = paste0(HealthMeasure, 'M1', valuename)
      rownames(coeffpvalueM2)[i] = paste0(HealthMeasure, 'M2', valuename)
      i = i + 1
    } 
    coeffpvalue = rbind(coeffpvalueM1, coeffpvalueM2)
    coeffpvalueSum = rbind(coeffpvalueSum, coeffpvalue)
    
    
    write.table(coeffpvalueSum, file = paste0(output, 'CoeffPvalueSum.txt'), row=T, col=T, qu=F, sep = '\t')
    write.table(coeffSum, file = paste0(output, 'CoeffSum.txt'), row=T, col=T, qu=F, sep = '\t')
    write.table(ResiTable, file = paste0(output, 'Resi.txt'), row=F, col=T, qu=F)
  }
  dev.off()
  sink()
}

BuildModels(Health=Health, CellCount=CellCount, PCSexAge=PCSexAge, 
            whichClock = whichClock, output = residOutput)

