# plot function
age.plot = function(cname, cAge, pAge, SD=5, ageValid){
  
  if (ageValid == T){
    message("The correlation between predicted ", cname, " and actual age is ", cor(cAge, pAge,use="pair"))
    plot(cAge, pAge, cex.main=1.2, cex=1.2, 
         main=paste("correlation between", cname, "\nand actual age=",signif(cor(cAge, pAge,use="pair"),5), sep=""), 
         xlab = "Chronological Age (years)", ylab = cname)
    abline(lm(pAge ~ cAge))
  }else{
    message("No correlation between predicted ", cname, " and actual age.")
  }
  
  hist(pAge, xlab="", main=paste(cname,"\n(N=", length(which(!is.na(pAge))),")",sep=""), cex.main=1.2)
  abline(v=mean(pAge, na.rm=T)-SD*sd(pAge, na.rm=T), lty=2)
  abline(v=mean(pAge,na.rm=T)+SD*sd(pAge, na.rm=T), lty=2)
  
  qqnorm(pAge, main=paste(cname, "\n(N=", length(which(!is.na(pAge))),"; shapiroP=",signif(as.numeric(shapiro.test(pAge)[2]),5),")",sep=""),cex.main=1.2)
  qqline(pAge)
  message("Done plotting for ", cname)
}



  
  arguments <- commandArgs(T)
  PCClock2017 <- arguments[1]
  PCClock2020 <- arguments[2]
  PCClock <- arguments[3]
  agePlot<-arguments[4]

  
  clocks <- c('PCHorvath1', 'PCHorvath2', 'PCHannum', 'PCPhenoAge', 'PCGrimAge', 'DunedinPACE')
  
  message("Loading the data...")
  PCClock2017 = read.table(PCClock2017, stringsAsFactors = FALSE,header = T)
  PCClock2020 = read.table(PCClock2020, stringsAsFactors = FALSE,header = T)
  PCClockCombined = rbind(PCClock2017, PCClock2020)
  write.table(PCClockCombined, file=PCClock, row=F, col=T, qu=F)
  
  age_valid = TRUE
  if (mean(PCClockCombined$Age) == 1 ) {
    age_valid == FALSE
  }
  
  pdf(agePlot, width=12, height=20)
  
  if (age_valid == FALSE) {
    par(mfrow=c(6,2))
  }else{
    par(mfrow=c(6,3))
  }
  
  for (clock in clocks) {
    age.plot(cname = clock, cAge = PCClockCombined$Age, 
             pAge = PCClockCombined[,clock], ageValid = age_valid)
  
  }
  
  quiet <- dev.off()


