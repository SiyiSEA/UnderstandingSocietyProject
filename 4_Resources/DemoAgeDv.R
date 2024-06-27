# a demo for wave2 pData comes from batch2020

# Select necessary for wave2 pData for Plan2 ####
pDataRaw2020 <- read.delim("1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE)
extractVariable <- c('id', 'confage', 'a_age_dv','b_age_dv','nsex', 'rackbarcode', 'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                     'a_sf1','b_sf1','c_sf1','b_scsf1','c_scsf1', 'a_sf2a','b_scsf2a','c_scsf2a',
                     'a_sf2b','b_scsf2b','c_scsf2b','a_sf3a','b_scsf3a', 'c_scsf3a', 
                     'a_sf3b','b_scsf3b','c_scsf3b','a_sf5','b_scsf5', 'c_scsf5')
pDataWave2Plan2 <- pDataRaw2020[which(pDataRaw2020$wave == 2), extractVariable]
pDataWave2Plan2[pDataWave2Plan2 == ""] <- NA
write.table(pDataWave2Plan2, "./6_Result_plan2/pDataWave2Plan2.txt", row=T, col=T, qu=F, sep = '\t')

length(which(pDataWave2Plan2$c_sf1 == pDataWave2Plan2$c_scsf1))
# 1550/2234 samples have the same answer on c_sf1 and c_scsf1

length(which(pDataWave2Plan2$b_sf1 == pDataWave2Plan2$b_scsf1))
# 1630/2234 samples have the same answer on the b_sf1 and b_scsf1

# options for the first model of the general health status
# 1. c_sf1 - a_sf1 either b_sf1
# 2. c_scsf1 - a_sf1 either b_sf1
# 3. c_sf1 - a_sf1 either b_scsf1
# 4. c_scsf1 - a_sf1 either b_scsf1

timeSpanTable <- pDataWave2Plan2[,c("id", "confage", 'a_age_dv','b_age_dv')]
timeSpanTable$confage_a = timeSpanTable$confage - timeSpanTable$a_age_dv
timeSpanTable$confage_b = timeSpanTable$confage - timeSpanTable$b_age_dv

group_allocation <- function(a,b) {
  if (a == 0){
    return("exclude")
  }
  
  if ( a == "NA"){
    if ( b == "NA" | b == 0){
      return("exclude")
    }
  }
  
  if (a >= 1 & b >= 1){
    if (a == b){
      return("not decided")
    }
  }
  
  if (a >= 1 ){
    if ( b == "NA" | b == 0){
      return("a_wave1")
    }
  }
  
  if (a == "NA" & b >=1 ){
    return("b_wave2")
  }else if (a > b & b >=1){
    return("b_wave2")
  }
  
}

timeSpanTable[is.na(timeSpanTable)] = "NA"
timeSpanTable$ageGroup = "NA"

library(magrittr)
timeSpanTable <- timeSpanTable %>%
  dplyr::rowwise() %>% #REMOVE if this doesn't work
  dplyr::mutate(ageGroup = group_allocation(confage_a, confage_b))

timeSpanTable[timeSpanTable =="NA"] = NA
timeSpanTable$confage = as.numeric(timeSpanTable$confage)
timeSpanTable$a_age_dv = as.numeric(timeSpanTable$a_age_dv)
timeSpanTable$confage_a = as.numeric(timeSpanTable$confage_a)
timeSpanTable$b_age_dv = as.numeric(timeSpanTable$b_age_dv)
timeSpanTable$confage_b = as.numeric(timeSpanTable$confage_b)
timeSpanTable$ageGroup = as.factor(timeSpanTable$ageGroup)


summary(timeSpanTable)
#       id            confage         a_age_dv       b_age_dv       confage_a       confage_b             ageGroup   
# Min.   :    91   Min.   :16.00   Min.   :14.0   Min.   :16.00   Min.   :1.000   Min.   :0.0000   a_wave1    :1327  
# 1st Qu.: 32333   1st Qu.:39.00   1st Qu.:38.0   1st Qu.:38.00   1st Qu.:1.000   1st Qu.:0.0000   b_wave2    : 849  
# Median : 63302   Median :51.00   Median :50.0   Median :51.00   Median :1.000   Median :0.0000   exclude    :  17  
# Mean   : 64053   Mean   :50.03   Mean   :48.8   Mean   :49.63   Mean   :1.411   Mean   :0.3988   not decided:  41  
# 3rd Qu.: 96639   3rd Qu.:63.00   3rd Qu.:61.0   3rd Qu.:62.00   3rd Qu.:2.000   3rd Qu.:1.0000                     
# Max.   :127987   Max.   :83.00   Max.   :81.0   Max.   :82.00   Max.   :3.000   Max.   :2.0000                     
#                                  NA's   :21                     NA's   :21                                         

clearTimeSpanTable = timeSpanTable[which(timeSpanTable$ageGroup != "exclude"),]
summary(clearTimeSpanTable)
#       id            confage         a_age_dv       b_age_dv       confage_a       confage_b             ageGroup   
# Min.   :    91   Min.   :16.00   Min.   :14.0   Min.   :16.00   Min.   :1.000   Min.   :0.0000   a_wave1    :1327  
# 1st Qu.: 32331   1st Qu.:39.00   1st Qu.:38.0   1st Qu.:39.00   1st Qu.:1.000   1st Qu.:0.0000   b_wave2    : 849  
# Median : 63293   Median :51.00   Median :50.0   Median :51.00   Median :1.000   Median :0.0000   exclude    :   0  
# Mean   : 63990   Mean   :50.16   Mean   :48.8   Mean   :49.76   Mean   :1.411   Mean   :0.4019   not decided:  41  
# 3rd Qu.: 96422   3rd Qu.:63.00   3rd Qu.:61.0   3rd Qu.:62.00   3rd Qu.:2.000   3rd Qu.:1.0000                     
# Max.   :127987   Max.   :83.00   Max.   :81.0   Max.   :82.00   Max.   :3.000   Max.   :2.0000                     
#                                  NA's   :4                      NA's   :4                    

boxplot(clearTimeSpanTable[,c("confage","a_age_dv", "b_age_dv")], main = "wave2 samples come from batch2020")
boxplot(clearTimeSpanTable[,c("confage_a", "confage_b")], main = "wave2 samples come from batch2020")

# for not deceided sample, check if they have the same health value for each status ####
notdecidedSamplelist = clearTimeSpanTable[which(clearTimeSpanTable$ageGroup == "not decided"), "id"]
notdecidedSampleHealthTable = pDataWave2Plan2[which(pDataWave2Plan2$id %in% notdecidedSamplelist$id),]

notdecidedSampleHealthTable[is.na(notdecidedSampleHealthTable)] = "NA"
if (all(notdecidedSampleHealthTable$a_sf1 == notdecidedSampleHealthTable$b_sf1) == F) {
  print("a_sf1 and b_sf1 are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf1 == notdecidedSampleHealthTable$b_scsf1) == F){
  print("a_sf1 and b_scsf1 are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf2a == notdecidedSampleHealthTable$b_scsf2a) == F){
  print("a_sf2a and b_scsf2a are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf2b == notdecidedSampleHealthTable$b_scsf2b) == F){
  print("a_sf2b and b_scsf2b are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf3a == notdecidedSampleHealthTable$b_scsf3a) == F){
  print("a_sf3a and b_scsf3a are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf3b == notdecidedSampleHealthTable$b_scsf3b) == F){
  print("a_sf3b and b_scsf3b are not identical")
}
if (all(notdecidedSampleHealthTable$a_sf5 == notdecidedSampleHealthTable$b_scsf5) == F){
  print("a_sf5 and b_scsf5 are not identical")
}

# due to none of the health status for samples in not decide group are the same, I will split the table into two. ####
# (aNDab)table 1 will contain: samples from a_wave1 and b_wave2 + assign a_wave1 to all the samples with not decided;
# (bNDab)table 2 will contain: samples from a_wave1 and b_wave2 + assign b_wave2 to all the samples with not decided;
# (ab)table 3 will only contain: samples from a_wave1 and b_wave2 (for comparision)

filterConfageHealth <- pDataWave2Plan2[which(pDataWave2Plan2$id %in% clearTimeSpanTable$id),]
filterConfageHealth <- cbind(clearTimeSpanTable[,c('ageGroup', 'confage_a', 'confage_b')], filterConfageHealth)

health_allocation_fora = function(ageGroup, a_health, b_health){
  if (ageGroup != 'b_wave2'){
    return(a_health)
  }else{
    return(b_health)
  }
  
}

library(magrittr)
aNDabHealth <- filterConfageHealth %>%
  dplyr::rowwise() %>% #REMOVE if this doesn't work
  dplyr::mutate("generalHealth" = health_allocation_fora(ageGroup, a_sf1, b_sf1)) %>%
  dplyr::mutate("limitsAct" = health_allocation_fora(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsStairs" = health_allocation_fora(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsAmountWork" = health_allocation_fora(ageGroup, a_sf3a, b_scsf3a)) %>%
  dplyr::mutate("limitsTypeWork" = health_allocation_fora(ageGroup, a_sf3b, b_scsf3b)) %>%
  dplyr::mutate("painWork" = health_allocation_fora(ageGroup, a_sf5, b_scsf5)) %>%
  dplyr::mutate("timeSpan" = health_allocation_fora(ageGroup, confage_a, confage_b)) %>%
  dplyr::select(id, ageGroup, timeSpan, confage,generalHealth, limitsAct,limitsStairs,limitsAmountWork,limitsTypeWork,painWork,
                nsex, rackbarcode, cd8t, cd4t, nk, bcell, mono)

write.table(aNDabHealth, "./6_Result_plan2/aNDabHealth2217.txt", row=T, col=T, qu=F, sep = '\t')

health_allocation_forb = function(ageGroup, a_health, b_health){
  if (ageGroup != 'a_wave1'){
    return(b_health)
  }else{
    return(a_health)
  }
  
}

library(magrittr)
bNDabHealth <- filterConfageHealth %>%
  dplyr::rowwise() %>% #REMOVE if this doesn't work
  dplyr::mutate("generalHealth" = health_allocation_forb(ageGroup, a_sf1, b_sf1)) %>%
  dplyr::mutate("limitsAct" = health_allocation_forb(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsStairs" = health_allocation_forb(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsAmountWork" = health_allocation_forb(ageGroup, a_sf3a, b_scsf3a)) %>%
  dplyr::mutate("limitsTypeWork" = health_allocation_forb(ageGroup, a_sf3b, b_scsf3b)) %>%
  dplyr::mutate("painWork" = health_allocation_forb(ageGroup, a_sf5, b_scsf5)) %>%
  dplyr::mutate("timeSpan" = health_allocation_forb(ageGroup, confage_a, confage_b)) %>%
  dplyr::select(id, ageGroup,timeSpan, confage,generalHealth, limitsAct,limitsStairs,limitsAmountWork,limitsTypeWork,painWork,
                nsex, rackbarcode, cd8t, cd4t, nk, bcell, mono)

write.table(bNDabHealth, "./6_Result_plan2/bNDabHealth2217.txt", row=T, col=T, qu=F, sep = '\t')


a1b1_id = subset(timeSpanTable, ageGroup %in% c("a_wave1", "b_wave2"))$id
filterConfageHealth <- pDataWave2Plan2[which(pDataWave2Plan2$id %in% a1b1_id),]
filterConfageHealth <- cbind(subset(timeSpanTable, ageGroup %in% c("a_wave1", "b_wave2"))[,c('ageGroup', 'confage_a', 'confage_b')], filterConfageHealth)
health_allocation = function(ageGroup, a_health, b_health){
  if (ageGroup == 'a_wave1'){
    return(a_health)
  }else{
    return(b_health)
  }
  
}

library(magrittr)
abHealth <- filterConfageHealth %>%
  dplyr::rowwise() %>% #REMOVE if this doesn't work
  dplyr::mutate("generalHealth" = health_allocation(ageGroup, a_sf1, b_sf1)) %>%
  dplyr::mutate("limitsAct" = health_allocation(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsStairs" = health_allocation(ageGroup, a_sf2a, b_scsf2a)) %>%
  dplyr::mutate("limitsAmountWork" = health_allocation(ageGroup, a_sf3a, b_scsf3a)) %>%
  dplyr::mutate("limitsTypeWork" = health_allocation(ageGroup, a_sf3b, b_scsf3b)) %>%
  dplyr::mutate("painWork" = health_allocation(ageGroup, a_sf5, b_scsf5)) %>%
  dplyr::mutate("timeSpan" = health_allocation(ageGroup, confage_a, confage_b)) %>%
  dplyr::select(id, ageGroup,timeSpan, confage,generalHealth, limitsAct,limitsStairs,limitsAmountWork,limitsTypeWork,painWork,
                nsex, rackbarcode, cd8t, cd4t, nk, bcell, mono)

write.table(abHealth, "./6_Result_plan2/abHealth2176.txt", row=T, col=T, qu=F, sep = '\t')


# Here is the models with time span ####
PCClock = read.table("./4_Results/Age/PCClockAgeWave2.txt", stringsAsFactors = FALSE, header = T)

# aNDabHealth ####
whichClock = "DunedinPACE"
residOutput = "./6_Result_plan2/aNDabHealth"
residOutput <- paste0(residOutput, whichClock)
aNDabHealth <- as.data.frame(apply(aNDabHealth, 2, function(x) gsub("^\\s+|\\s+$", "",x)))
CellCount = aNDabHealth[, c("id", "cd8t", "cd4t", "nk", "bcell", "mono")]
Health = aNDabHealth[, c("id", "generalHealth", "limitsAct", "limitsStairs", "limitsAmountWork", "limitsTypeWork", "painWork")]
Health[Health == ""] <- NA
SexAge = aNDabHealth[,c("id", "confage", "timeSpan","nsex", "rackbarcode")]
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
      coeff_list = c('(Intercept)','Value2', 'Value3', 'Value4', 'Value5')
    } else if (HealthMeasure %in% c("limitsAct", "limitsStairs")) {
      value = c("no, not limited at all"=3, "yes, limited a little"=2, "yes, limited a lot"=1)
      coeff_list = c('(Intercept)', 'Value1', 'Value2')
    } else if (HealthMeasure %in% c("limitsAmountWork", "limitsTypeWork")) {
      value = c("none of the time"=5, "a little of the time"=4, "some of the time"=3,"most of the time"=2, "all of the time"=1)
      coeff_list = c('(Intercept)', 'Value1', 'Value2', 'Value5', 'Value3')
    } else {
      value = c("extremely"=5, "quite a bit"=4, "moderately"=3,"a little bit"=2, "not at all"=1)
      coeff_list = c('(Intercept)', 'Value1', 'Value3', 'Value4', 'Value5')
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
    coeffM1 = summary(M1)$coefficients[coeff_list,]
    coeffpvalueM1 = summary(M1)$coefficients[coeff_list[-1],]
    plot(M1, main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode'))
    ResiTable[,paste0(HealthMeasure,'M1Resi')] = residuals(M1)
    print(summary(M1))
    
    message(paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
    M2table = merge(PCSexAgeHealth, CellCount, by.x = 'id', by.y = 'id')
    M2 = lm(PredAge ~ ., data = M2table[-1], na.action = na.exclude)
    coeffM2 = summary(M2)$coefficients[coeff_list,]
    coeffpvalueM2 = summary(M2)$coefficients[coeff_list[-1],]
    plot(M2,  main=paste0(whichClock,' ~ ',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
    ResiTable[,paste0(HealthMeasure,'M2Resi')] = residuals(M2)
    print(summary(M2))
    
    i = 1
    for (valuename in coeff_list){
      rownames(coeffM1)[i] = paste0(HealthMeasure, 'M1', valuename)
      rownames(coeffM2)[i] = paste0(HealthMeasure, 'M2', valuename)
      i = i + 1
    } 
    
    coeffTemp = rbind(coeffM1, coeffM2)
    coeffSum = rbind(coeffSum, coeffTemp)
    
    i = 1
    for (valuename in coeff_list[-1]){
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
library(dplyr)
library(plyr)



# boxplot - leave it 

# Step - 3 calculate the changes in Health measure
wave2Table = pDataRaw2020[,c('id', 'c_sf1', 'c_scsf2a', 'c_scsf2b', 'c_scsf3a', 'c_scsf3b', 'c_scsf5')]
wave2Table <- as.data.frame(apply(wave2Table, 2, function(x) gsub("^\\s+|\\s+$", "",x)))
wave2Table[wave2Table == ""] <- NA

for (HealthMeasure in colnames(wave2Table)[-1]){
  if (HealthMeasure %in% c('c_sf1')) {
    value = c("excellent"=1, "very good"=2, "good"=3, "fair"=4,"poor"=5)
  } else if (HealthMeasure %in% c("c_scsf2a", "c_scsf2b")) {
    value = c("no, not limited at all"=3, "yes, limited a little"=2, "yes, limited a lot"=1)
  } else if (HealthMeasure %in% c("c_scsf3a", "c_scsf3b")) {
    value = c("none of the time"=5, "a little of the time"=4, "some of the time"=3,"most of the time"=2, "all of the time"=1)
  } else {
    value = c("extremely"=5, "quite a bit"=4, "moderately"=3,"a little bit"=2, "not at all"=1)
  }
  
  wave2Table$C_Value = revalue(wave2Table[,c(HealthMeasure)], value)
  wave2Table$C_Value = as.numeric(wave2Table$C_Value)
  
}


whichClock = "DunedinPACE"
residOutput = "./6_Result_plan2/aNDabHealthChangeHealth"
residOutput <- paste0(residOutput, whichClock)
aNDabHealth <- as.data.frame(apply(aNDabHealth, 2, function(x) gsub("^\\s+|\\s+$", "",x)))

HealthChange = read.table("./6_Result_plan2/pDataWave2Plan2.txt", sep = "\t", header = T)
HealthChange <- as.data.frame(apply(HealthChange, 2, function(x) gsub("^\\s+|\\s+$", "",x)))

CellCount = aNDabHealth[, c("id", "cd8t", "cd4t", "nk", "bcell", "mono")]
Health = aNDabHealth[, c("id", "generalHealth", "limitsAct", "limitsStairs", "limitsAmountWork", "limitsTypeWork", "painWork")]
Health[Health == ""] <- NA

HealthChange = HealthChange[,c('id', 'c_sf1', 'c_scsf2a', 'c_scsf2b', 'c_scsf3a', 'c_scsf3b', 'c_scsf5')]
colnames(HealthChange) = c('id', 'c_generalHealth', 'c_limitsAct', 'c_limitsStairs', 
                           'c_limitsAmountWork', 'c_limitsTypeWork', 'c_painWork')
HealthChange[HealthChange == ""] <- NA

SexAge = aNDabHealth[,c("id", "confage", "timeSpan","nsex", "rackbarcode")]
colnames(SexAge) = c("id", "Age_numeric", "TimeSpan_numeric","Sex_factor", "Rackbarcode_factor")
PCSexAge = merge(SexAge, PCClock[,c("id", whichClock)], by.y = "id", by.x = "id")
colnames(PCSexAge)[which(colnames(PCSexAge)==whichClock)] = "PredAge"

BuildModels_change <- function(Health, HealthChange, CellCount, PCSexAge, whichClock, output){
  
  coeffSum =c()
  coeffpvalueSum = c()
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
    HealthChange$C_Value = revalue(HealthChange[,c(paste0("c_", HealthMeasure))], value)
    PCSexAgeHealth$Value = as.numeric(PCSexAgeHealth$Value)
    HealthChange$C_Value = as.numeric(HealthChange$C_Value)
    
    PCSexAgeHealthChange = merge(PCSexAgeHealth, HealthChange[,c('id', 'C_Value')], by.x = 'id', by.y = 'id')
    PCSexAgeHealthChange$HealthChange_value = PCSexAgeHealthChange$C_Value - PCSexAgeHealthChange$Value
    
    
    HealthChange_allocation = function(HealthChange_value){
      if (is.na(HealthChange_value)){
        return("Missing")
      }else if (HealthChange_value > 0){
        return("worse_health")
      }else if (HealthChange_value == 0){
        return("no_change")
      }else{
        return("better_health")}
    }
    
    PCSexAgeHealthChange <- PCSexAgeHealthChange %>%
      dplyr::rowwise() %>%
      dplyr::mutate("HealthChange_factor" = HealthChange_allocation(HealthChange_value))
    
    PCSexAgeHealthChange$HealthChange_factor = as.factor(PCSexAgeHealthChange$HealthChange_factor)
    message("There are ", count(PCSexAgeHealthChange[PCSexAgeHealthChange == 'Missing'])$freq," Missing value for the change of ", HealthMeasure)
    
    PCSexAgeHealthChange = subset(PCSexAgeHealthChange, HealthChange_factor != 'Missing')
    ResiTable <- PCSexAgeHealthChange[, c('id', "PredAge")]
    
    PCSexAgeHealthChange$Age_numeric = as.numeric(PCSexAgeHealthChange$Age_numeric)
    PCSexAgeHealthChange$TimeSpan_numeric = as.numeric(PCSexAgeHealthChange$TimeSpan_numeric)
    PCSexAgeHealthChange$Sex_factor = as.factor(PCSexAgeHealthChange$Sex_factor)
    PCSexAgeHealthChange = PCSexAgeHealthChange[-c(index)]
    PCSexAgeHealthChange = subset(PCSexAgeHealthChange, select = -c(Value, C_Value, HealthChange_value))
    
    CellCount$cd8t = as.numeric(CellCount$cd8t)
    CellCount$cd4t = as.numeric(CellCount$cd4t)
    CellCount$nk = as.numeric(CellCount$nk)
    CellCount$bcell = as.numeric(CellCount$bcell)
    CellCount$mono = as.numeric(CellCount$mono)
    
    par(mfrow=c(2,2))
    
    message(paste0(whichClock,' ~ change in ', HealthMeasure,' + age + time span + sex + barcode'))
    M1 = lm(PredAge ~ ., data = PCSexAgeHealthChange[, -1], na.action = na.exclude)
    
    coeffM1 = as.data.frame(summary(M1)$coefficients) %>%
      dplyr::filter(row.names(as.data.frame(summary(M1)$coefficients)) %in% 
                      c('(Intercept)', 'no_change','better_health', 'worse_health'))
    coeffpvalueM1 = coeffM1[-1,]
    plot(M1, main=paste0(whichClock,' ~ change in',  HealthMeasure,' + age + time span + sex + barcode'))
    ResiTable[,paste0(HealthMeasure,'M1Resi')] = residuals(M1)
    print(summary(M1))
    
    message(paste0(whichClock,' ~ change in',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
    M2table = merge(PCSexAgeHealthChange, CellCount, by.x = 'id', by.y = 'id')
    M2 = lm(PredAge ~ ., data = M2table[-1], na.action = na.exclude)
    coeffM2 = as.data.frame(summary(M2)$coefficients) %>%
      dplyr::filter(row.names(as.data.frame(summary(M2)$coefficients)) %in% 
                      c('(Intercept)', 'no_change','better_health', 'worse_health'))
    coeffpvalueM2 = coeffM2[-1,]
    plot(M2,  main=paste0(whichClock,' ~ change in',  HealthMeasure,' + age + time span + sex + barcode + cellcount'))
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
library(dplyr)
library(plyr)
