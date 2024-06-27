# HealthBoxPlot.R
library(ggplot2)

arguments <- commandArgs(T)
# covariates - age, sex, cell type, health measure
pData <- arguments[1]
# PCclock - PCPhenoAge, PCGrimAge, DunedinPACE
PCClock <- arguments[2]
# resiPath - fetch folder path to residual 
resiPath <- arguments[3]
# wave2 - identify if the input files are all belongs to wave2 or not
wave2 <- arguments[4]
# whichClock - identify the models are built for specify clcok
whichClock <- arguments[5]

# pData <- "1_RawData/pDataSortedCombined.RData"
# PCClock <- "4_Results/Age/PCClockAgeCombined.txt"
# coeffSum <- paste0("4_Results/Module/ModelCombined", whichClock ,"CoeffSum.txt")
# resid <- paste0("4_Results/Module/ModelCombined")  
# residpdf <- paste0("4_Results/BoxPlot/ModelCombinedBoxPlot")  
# wave2 <- F
# whichClock <- "PCGrimAge"


# pData <- "1_RawData/pDataSortedWave2.RData"
# PCClock <- "4_Results/Age/PCClockAgeWave2.txt"
# whichClock <- "DunedinPACE"
# coeffSum <- paste0("4_Results/Module/ModelWave2", whichClock ,"CoeffSum.txt")
# resid <- paste0("4_Results/Module/ModelWave2", whichClock ,"Resi.txt")
# residpdf <- paste0("4_Results/BoxPlot/ModelWave2BoxPlot", whichClock)
# wave2 <- T

setwd("/lustre/home/sww208/USProject")
homedir = getwd()
coeffSum = paste0(resiPath, whichClock ,"CoeffSum.txt")
resid = paste0(resiPath, whichClock ,"Resi.txt") 
residpdf = paste0(homedir,"/", resiPath, whichClock, "BoxPlot.pdf") 


load(pData)
PCClock = read.table(PCClock, stringsAsFactors = FALSE, header = T)
coeffSum = read.table(coeffSum, header = T, sep = '\t')
resid = read.table(resid, stringsAsFactors = FALSE, header = T)

if (wave2 == T){
  Health = pData[, c("id", "sf1", "scsf1", "scsf2a", "scsf2b", "scsf3a", "scsf3b", "scsf5", "scsf7", "smcigs", "smnow")]
}else {
  Health = pData[, c("id", "sf1", "scsf1", "scsf2a", "scsf2b", "scsf3a", "scsf3b", "scsf5", "scsf7")]
}

Health[Health == ""] <- NA

# Boxplot on initial DunedinPACE ####

HealthBoxPlot <- function(Health, PCClock_DNAmAge, whichClock, outputpdf) {
  
  pdf(file = paste0(outputpdf, ".pdf"), width=10, height=3)
  par(mfrow=c(2,1))

  HealthAge <- merge(Health, PCClock_DNAmAge[,c('id', whichClock)], by.x = 'id', by.y = 'id')
  colnames(HealthAge)[which(colnames(HealthAge)==whichClock)] = "PredAge"
  
  for (HealthMeasure in colnames(Health)[-1]){
    
    if (HealthMeasure %in% c('sf1','scsf1')) {
      value = c("excellent"="1-excellent", "very good"="2-very good", "good"="3-good", "fair"="4-fair","poor"="5-poor")
    } else if (HealthMeasure %in% c('scsf2a','scsf2b')) {
      value = c("no, not limited at all"="3-no, not limited at all", "yes, limited a little"="2-yes, limited a little", "yes, limited a lot"="1-yes, limited a lot")
    } else if (HealthMeasure %in% c('scsf3a','scsf3b','scsf7')) {
      value = c("none of the time"="5-none of the time", "a little of the time"="4-a little of the time", "some of the time"="3-some of the time","most of the time"="2-most of the time", "all of the time"="1-all of the time")
    } else if (HealthMeasure == "smcigs"){
      value = c("smoke them only occasionally"="2-smoke them only occasionally", "smoked regularly, at least one per day"="1-smoked regularly, at least one per day",
                "spontaneous never really smoked cigarettes, just tried them once or twice"="3-spontaneous never really smoked cigarettes, just tried them once or twice")
    }else if (HealthMeasure == "smnow"){
      value = c("no"="2-no", "yes"="1-yes")
    }else {
      value = c("extremely"="5-extremely", "quite a bit"="4-quite a bit", "moderately"="3-moderately","a little bit"="2-a little bit", "not at all"="1-not at all")
    }
    
    print(HealthMeasure)
    HealthAge$Value = revalue(HealthAge[,c(HealthMeasure)], value)
    
    if (HealthMeasure == 'sf1'){
      xlabel = "sf1 General Health: In general, would you say NAME's health is…"
    } else if (HealthMeasure == 'scsf1'){
      xlabel = "scsf1 General Health: In general, would you say your health is…"
    } else if (HealthMeasure == 'scsf2a'){
      xlabel = "scsf2a Health limits moderate activities"
    } else if (HealthMeasure == 'scsf2b'){
      xlabel = "scsf2b Health limits several flights of stairs"
    } else if (HealthMeasure == 'scsf3a'){
      xlabel = "scsf3a Last 4 weeks: Physical health limits amount of work"
    } else if (HealthMeasure == 'scsf3b'){
      xlabel = "scsf3b Last 4 weeks: Physical health limits kind of work"
    } else if (HealthMeasure == 'scsf5'){
      xlabel = "scsf5 Last 4 weeks: Pain interfered with work"
    } else if (HealthMeasure == "smcigs"){
      xlabel = "How regular you smoked cigarettes"
    } else if (HealthMeasure == "smnow"){
      xlabel = "Do you smoke cigarettes at all nowadays?"
    } else {
      xlabel = "scsf7 Last 4 weeks: Physical or mental health interfered with social life"
    }
    
    p <- ggplot(HealthAge, aes(x=HealthAge$Value, y=PredAge)) + 
      geom_violin() + geom_boxplot(width=0.1) +
      xlab(xlabel) +
      ylab(whichClock)
    print(p)
  }
  
  dev.off()
  
}

HealthBoxPlot(Health = Health, PCClock_DNAmAge = PCClock, 
              whichClock = whichClock, outputpdf = residpdf)




# Boxplot on residual DunedinPACE ####

RegressHealthBoxPlot <- function(Health, resid, coeffSum, whichClock, outputpdf, wave2) {
  
  if (wave2 == T){
    M = c('M1', 'M2', 'M3', 'M4')
  }else {
    M = c('M1', 'M2', 'M3')
  }
  
  
  pdf(file = paste0(outputpdf, "Regress.pdf"),width=10, height=3)
  
  for (HealthMeasure in colnames(Health)[-c(1,10,11)]) {
    print(HealthMeasure)
    for (Module in M){
      print(Module)
      regress = merge(Health[,c('id', HealthMeasure)], 
                         resid[,c('id', paste0(HealthMeasure, Module, 'Resi'))], 
                         by.y = 'id', by.x = 'id')
      
      
      if (HealthMeasure %in% c('sf1','scsf1')) {
        label = c("excellent"="1-excellent", "very good"="2-very good", "good"="3-good", "fair"="4-fair","poor"="5-poor")
        value = c("excellent"=1, "very good"=2, "good"=3, "fair"=4,"poor"=5)
      } else if (HealthMeasure %in% c('scsf2a','scsf2b')) {
        label = c("no, not limited at all"="3-no, not limited at all", "yes, limited a little"="2-yes, limited a little", "yes, limited a lot"="1-yes, limited a lot")
        value = c("no, not limited at all"=3, "yes, limited a little"=2, "yes, limited a lot"=1)
      } else if (HealthMeasure %in% c('scsf3a','scsf3b','scsf7')) {
        label = c("none of the time"="5-none of the time", "a little of the time"="4-a little of the time", "some of the time"="3-some of the time","most of the time"="2-most of the time", "all of the time"="1-all of the time")
        value = c("none of the time"=5, "a little of the time"=4, "some of the time"=3,"most of the time"=2, "all of the time"=1)
      } else {
        label = c("extremely"="5-extremely", "quite a bit"="4-quite a bit", "moderately"="3-moderately","a little bit"="2-a little bit", "not at all"="1-not at all")
        value = c("extremely"=5, "quite a bit"=4, "moderately"=3,"a little bit"=2, "not at all"=1)
      }
      
      
      if (HealthMeasure == 'sf1'){
        xlabel = "sf1 General Health: In general, would you say NAME's health is…"
      } else if (HealthMeasure == 'scsf1'){
        xlabel = "scsf1 General Health: In general, would you say your health is…"
      } else if (HealthMeasure == 'scsf2a'){
        xlabel = "scsf2a Health limits moderate activities"
      } else if (HealthMeasure == 'scsf2b'){
        xlabel = "scsf2b Health limits several flights of stairs"
      } else if (HealthMeasure == 'scsf3a'){
        xlabel = "scsf3a Last 4 weeks: Physical health limits amount of work"
      } else if (HealthMeasure == 'scsf3b'){
        xlabel = "scsf3b Last 4 weeks: Physical health limits kind of work"
      } else if (HealthMeasure == 'scsf5'){
        xlabel = "scsf5 Last 4 weeks: Pain interfered with work"
      } else {
        xlabel = "scsf7 Last 4 weeks: Physical or mental health interfered with social life"
      }
      
      regress$Value = as.numeric(revalue(regress[,c(HealthMeasure)], value))
      regress$label = revalue(regress[,c(HealthMeasure)], label)
      
      regress$regress = regress[,c(paste0(HealthMeasure, Module, 'Resi'))] + 
        regress$Value * coeffSum[paste0(HealthMeasure, Module),1] + 
        coeffSum[paste0(HealthMeasure, Module, 'Intercept'),1]
      
      
      p <- ggplot(regress, aes(x=label, y=regress)) + 
        geom_violin() + geom_boxplot(width=0.1) +
        xlab(xlabel) + 
        ylab(paste0(Module, 'Resi', whichClock))
      print(p)
      
    }
  }
  
  dev.off()
}


RegressHealthBoxPlot(Health = Health, resid = resid, 
                     coeffSum = coeffSum, outputpdf = residpdf, 
                     whichClock = whichClock, wave2 = wave2)


