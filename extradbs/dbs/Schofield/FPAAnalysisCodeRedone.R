#Setting up the appropriate libraries
library(foreign)
library(lme4)

#Importing the original data
climate<-read.csv("ClimateChangeAttendanceFPA.csv", header=TRUE)

#Importing the imputed values
climatedat1<-read.csv("ClimateImputationsRR12181.csv",header=TRUE)
climatedat2<-read.csv("ClimateImputationsRR12182.csv",header=TRUE)
climatedat3<-read.csv("ClimateImputationsRR12183.csv",header=TRUE)
climatedat4<-read.csv("ClimateImputationsRR12184.csv",header=TRUE)
climatedat5<-read.csv("ClimateImputationsRR12185.csv",header=TRUE)
  
#Setting up the variables for estimation
distance1000<-climate$distance/1000
ldel<-log(climate$Delegates0+1)
gdpsquared1<-(climatedat1$logGDPpercap)^2
gdpsquared2<-(climatedat2$logGDPpercap)^2
gdpsquared3<-(climatedat3$logGDPpercap)^2
gdpsquared4<-(climatedat4$logGDPpercap)^2
gdpsquared5<-(climatedat5$logGDPpercap)^2
  
envcpiahalf1<-climatedat1$envcpia.2/2
envcpiahalf2<-climatedat2$envcpia.2/2
envcpiahalf3<-climatedat3$envcpia.2/2
envcpiahalf4<-climatedat4$envcpia.2/2
envcpiahalf5<-climatedat5$envcpia.2/2
  
EnvTreat1<-climatedat1$EnvTreaty
EnvTreat2<-climatedat2$EnvTreaty
EnvTreat3<-climatedat3$EnvTreaty
EnvTreat4<-climatedat4$EnvTreaty
EnvTreat5<-climatedat5$EnvTreaty
  
for (i in 1:4011){
  if (climatedat1$EnvTreaty[i]<0){
      EnvTreat1[i]<-0}
  if (climatedat2$EnvTreaty[i]<0){
      EnvTreat2[i]<-0}
  if (climatedat3$EnvTreaty[i]<0){
      EnvTreat3[i]<-0}
  if (climatedat4$EnvTreaty[i]<0){
      EnvTreat4[i]<-0}
  if (climatedat5$EnvTreaty[i]<0){
      EnvTreat5[i]<-0}
}
  
popsquared1<-(climatedat1$logpop)^2
popsquared2<-(climatedat2$logpop)^2
popsquared3<-(climatedat3$logpop)^2
popsquared4<-(climatedat4$logpop)^2
popsquared5<-(climatedat5$logpop)^2
  
#ALL COUNTRIES ANALYSIS: TABLE 1
    
#FULL MODEL POPULATION SQUARED
#TABLE 1 COLUMN 1

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel~climatedat1$logGDPpercap+gdpsquared1+climatedat1$logpop+popsquared1+climatedat1$Polity2+climatedat1$RegQual+climate$G20+climatedat1$UNSC+climatedat1$IO+climatedat1$WBEB+distance1000+climatedat1$GEFCouncil+climatedat1$logBilaterialAidDonor+climatedat1$logGEFDon+EnvTreat1+climatedat1$logBilaterialAidRecip+climatedat1$CDMDonor+climatedat1$CDMHost+climatedat1$GEFFundsDummy+climatedat1$logCO2percap+climatedat1$OPECDummy+climatedat1$logbiodiversity+climatedat1$FoodProd+climatedat1$AOSISMembers+climatedat1$logNatDiaster+envcpiahalf1+climatedat1$EnvMinistry+climatedat1$LeftExec+climatedat1$RightExec+climatedat1$NoExec+climatedat1$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r2<-lmer(ldel~climatedat2$logGDPpercap+gdpsquared2+climatedat2$logpop+popsquared2+climatedat2$Polity2+climatedat2$RegQual+climate$G20+climatedat2$UNSC+climatedat2$IO+climatedat2$WBEB+distance1000+climatedat2$GEFCouncil+climatedat2$logBilaterialAidDonor+climatedat2$logGEFDon+EnvTreat2+climatedat2$logBilaterialAidRecip+climatedat2$CDMDonor+climatedat2$CDMHost+climatedat2$GEFFundsDummy+climatedat2$logCO2percap+climatedat2$OPECDummy+climatedat2$logbiodiversity+climatedat2$FoodProd+climatedat2$AOSISMembers+climatedat2$logNatDiaster+envcpiahalf2+climatedat2$EnvMinistry+climatedat2$LeftExec+climatedat2$RightExec+climatedat2$NoExec+climatedat2$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r3<-lmer(ldel~climatedat3$logGDPpercap+gdpsquared3+climatedat3$logpop+popsquared3+climatedat3$Polity2+climatedat3$RegQual+climate$G20+climatedat3$UNSC+climatedat3$IO+climatedat3$WBEB+distance1000+climatedat3$GEFCouncil+climatedat3$logBilaterialAidDonor+climatedat3$logGEFDon+EnvTreat3+climatedat3$logBilaterialAidRecip+climatedat3$CDMDonor+climatedat3$CDMHost+climatedat3$GEFFundsDummy+climatedat3$logCO2percap+climatedat3$OPECDummy+climatedat3$logbiodiversity+climatedat3$FoodProd+climatedat3$AOSISMembers+climatedat3$logNatDiaster+envcpiahalf3+climatedat3$EnvMinistry+climatedat3$LeftExec+climatedat3$RightExec+climatedat3$NoExec+climatedat3$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r4<-lmer(ldel~climatedat4$logGDPpercap+gdpsquared4+climatedat4$logpop+popsquared4+climatedat4$Polity2+climatedat4$RegQual+climate$G20+climatedat4$UNSC+climatedat4$IO+climatedat4$WBEB+distance1000+climatedat4$GEFCouncil+climatedat4$logBilaterialAidDonor+climatedat4$logGEFDon+EnvTreat4+climatedat4$logBilaterialAidRecip+climatedat4$CDMDonor+climatedat4$CDMHost+climatedat4$GEFFundsDummy+climatedat4$logCO2percap+climatedat4$OPECDummy+climatedat4$logbiodiversity+climatedat4$FoodProd+climatedat4$AOSISMembers+climatedat4$logNatDiaster+envcpiahalf4+climatedat4$EnvMinistry+climatedat4$LeftExec+climatedat4$RightExec+climatedat4$NoExec+climatedat4$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r5<-lmer(ldel~climatedat5$logGDPpercap+gdpsquared5+climatedat5$logpop+popsquared5+climatedat5$Polity2+climatedat5$RegQual+climate$G20+climatedat5$UNSC+climatedat5$IO+climatedat5$WBEB+distance1000+climatedat5$GEFCouncil+climatedat5$logBilaterialAidDonor+climatedat5$logGEFDon+EnvTreat5+climatedat5$logBilaterialAidRecip+climatedat5$CDMDonor+climatedat5$CDMHost+climatedat5$GEFFundsDummy+climatedat5$logCO2percap+climatedat5$OPECDummy+climatedat5$logbiodiversity+climatedat5$FoodProd+climatedat5$AOSISMembers+climatedat5$logNatDiaster+envcpiahalf5+climatedat5$EnvMinistry+climatedat5$LeftExec+climatedat5$RightExec+climatedat5$NoExec+climatedat5$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
  
#COMBINING RESULTS AS PER RUBIN (1987)
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER
print(cbind(mnreg,sqrt(varreg),tv,pval)[2:28,])
aicave
  
#FULL MODEL POPULATION SQUARED INTERACTION OF OPEC AND CO2PERCAP WITH POLITY2 — INTEREST GROUPS WITH ECONOMIC INTEREST
#TABLE 1 COLUMN 2

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel~climatedat1$logGDPpercap+gdpsquared1+climatedat1$logpop+popsquared1+climatedat1$Polity2+climatedat1$RegQual+climate$G20+climatedat1$UNSC+climatedat1$IO+climatedat1$WBEB+distance1000+climatedat1$GEFCouncil+climatedat1$logBilaterialAidDonor+climatedat1$logGEFDon+EnvTreat1+climatedat1$logBilaterialAidRecip+climatedat1$CDMDonor+climatedat1$CDMHost+climatedat1$GEFFundsDummy+climatedat1$logCO2percap+climatedat1$OPECDummy+climatedat1$logbiodiversity+climatedat1$FoodProd+climatedat1$AOSISMembers+climatedat1$logNatDiaster+envcpiahalf1+climatedat1$EnvMinistry+climatedat1$Polity2:(climatedat1$logCO2percap+climatedat1$OPECDummy)+climatedat1$LeftExec+climatedat1$RightExec+climatedat1$NoExec+climatedat1$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r2<-lmer(ldel~climatedat2$logGDPpercap+gdpsquared2+climatedat2$logpop+popsquared2+climatedat2$Polity2+climatedat2$RegQual+climate$G20+climatedat2$UNSC+climatedat2$IO+climatedat2$WBEB+distance1000+climatedat2$GEFCouncil+climatedat2$logBilaterialAidDonor+climatedat2$logGEFDon+EnvTreat2+climatedat2$logBilaterialAidRecip+climatedat2$CDMDonor+climatedat2$CDMHost+climatedat2$GEFFundsDummy+climatedat2$logCO2percap+climatedat2$OPECDummy+climatedat2$logbiodiversity+climatedat2$FoodProd+climatedat2$AOSISMembers+climatedat2$logNatDiaster+envcpiahalf2+climatedat2$EnvMinistry+climatedat2$Polity2:(climatedat2$logCO2percap+climatedat2$OPECDummy)+climatedat2$LeftExec+climatedat2$RightExec+climatedat2$NoExec+climatedat2$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r3<-lmer(ldel~climatedat3$logGDPpercap+gdpsquared3+climatedat3$logpop+popsquared3+climatedat3$Polity2+climatedat3$RegQual+climate$G20+climatedat3$UNSC+climatedat3$IO+climatedat3$WBEB+distance1000+climatedat3$GEFCouncil+climatedat3$logBilaterialAidDonor+climatedat3$logGEFDon+EnvTreat3+climatedat3$logBilaterialAidRecip+climatedat3$CDMDonor+climatedat3$CDMHost+climatedat3$GEFFundsDummy+climatedat3$logCO2percap+climatedat3$OPECDummy+climatedat3$logbiodiversity+climatedat3$FoodProd+climatedat3$AOSISMembers+climatedat3$logNatDiaster+envcpiahalf3+climatedat3$EnvMinistry+climatedat3$Polity2:(climatedat3$logCO2percap+climatedat3$OPECDummy)+climatedat3$LeftExec+climatedat3$RightExec+climatedat3$NoExec+climatedat3$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r4<-lmer(ldel~climatedat4$logGDPpercap+gdpsquared4+climatedat4$logpop+popsquared4+climatedat4$Polity2+climatedat4$RegQual+climate$G20+climatedat4$UNSC+climatedat4$IO+climatedat4$WBEB+distance1000+climatedat4$GEFCouncil+climatedat4$logBilaterialAidDonor+climatedat4$logGEFDon+EnvTreat4+climatedat4$logBilaterialAidRecip+climatedat4$CDMDonor+climatedat4$CDMHost+climatedat4$GEFFundsDummy+climatedat4$logCO2percap+climatedat4$OPECDummy+climatedat4$logbiodiversity+climatedat4$FoodProd+climatedat4$AOSISMembers+climatedat4$logNatDiaster+envcpiahalf4+climatedat4$EnvMinistry+climatedat4$Polity2:(climatedat4$logCO2percap+climatedat4$OPECDummy)+climatedat4$LeftExec+climatedat4$RightExec+climatedat4$NoExec+climatedat4$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r5<-lmer(ldel~climatedat5$logGDPpercap+gdpsquared5+climatedat5$logpop+popsquared5+climatedat5$Polity2+climatedat5$RegQual+climate$G20+climatedat5$UNSC+climatedat5$IO+climatedat5$WBEB+distance1000+climatedat5$GEFCouncil+climatedat5$logBilaterialAidDonor+climatedat5$logGEFDon+EnvTreat5+climatedat5$logBilaterialAidRecip+climatedat5$CDMDonor+climatedat5$CDMHost+climatedat5$GEFFundsDummy+climatedat5$logCO2percap+climatedat5$OPECDummy+climatedat5$logbiodiversity+climatedat5$FoodProd+climatedat5$AOSISMembers+climatedat5$logNatDiaster+envcpiahalf5+climatedat5$EnvMinistry+climatedat5$Polity2:(climatedat5$logCO2percap+climatedat5$OPECDummy)+climatedat5$LeftExec+climatedat5$RightExec+climatedat5$NoExec+climatedat5$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER  
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:28,53:54),])
aicave
  
#FULL. MODEL POPULATION SQUARED CIVIL SOCIETY MECHANISM AND VULNERABILITY
#TABLE 1 COLUMN 3

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel~climatedat1$logGDPpercap+gdpsquared1+climatedat1$logpop+popsquared1+climatedat1$Polity2+climatedat1$RegQual+climate$G20+climatedat1$UNSC+climatedat1$IO+climatedat1$WBEB+distance1000+climatedat1$GEFCouncil+climatedat1$logBilaterialAidDonor+climatedat1$logGEFDon+EnvTreat1+climatedat1$logBilaterialAidRecip+climatedat1$CDMDonor+climatedat1$CDMHost+climatedat1$GEFFundsDummy+climatedat1$logCO2percap+climatedat1$OPECDummy+climatedat1$logbiodiversity+climatedat1$FoodProd+climatedat1$AOSISMembers+climatedat1$logNatDiaster+envcpiahalf1+climatedat1$EnvMinistry+climatedat1$Polity2*(climatedat1$logBilaterialAidRecip+climatedat1$GEFFundsDummy+climatedat1$logbiodiversity+climatedat1$FoodProd+climatedat1$AOSISMembers+climatedat1$logNatDiaster)+climatedat1$LeftExec+climatedat1$RightExec+climatedat1$NoExec+climatedat1$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r2<-lmer(ldel~climatedat2$logGDPpercap+gdpsquared2+climatedat2$logpop+popsquared2+climatedat2$Polity2+climatedat2$RegQual+climate$G20+climatedat2$UNSC+climatedat2$IO+climatedat2$WBEB+distance1000+climatedat2$GEFCouncil+climatedat2$logBilaterialAidDonor+climatedat2$logGEFDon+EnvTreat2+climatedat2$logBilaterialAidRecip+climatedat2$CDMDonor+climatedat2$CDMHost+climatedat2$GEFFundsDummy+climatedat2$logCO2percap+climatedat2$OPECDummy+climatedat2$logbiodiversity+climatedat2$FoodProd+climatedat2$AOSISMembers+climatedat2$logNatDiaster+envcpiahalf2+climatedat2$EnvMinistry+climatedat2$Polity2*(climatedat2$logBilaterialAidRecip+climatedat2$GEFFundsDummy+climatedat2$logbiodiversity+climatedat2$FoodProd+climatedat2$AOSISMembers+climatedat2$logNatDiaster)+climatedat2$LeftExec+climatedat2$RightExec+climatedat2$NoExec+climatedat2$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r3<-lmer(ldel~climatedat3$logGDPpercap+gdpsquared3+climatedat3$logpop+popsquared3+climatedat3$Polity2+climatedat3$RegQual+climate$G20+climatedat3$UNSC+climatedat3$IO+climatedat3$WBEB+distance1000+climatedat3$GEFCouncil+climatedat3$logBilaterialAidDonor+climatedat3$logGEFDon+EnvTreat3+climatedat3$logBilaterialAidRecip+climatedat3$CDMDonor+climatedat3$CDMHost+climatedat3$GEFFundsDummy+climatedat3$logCO2percap+climatedat3$OPECDummy+climatedat3$logbiodiversity+climatedat3$FoodProd+climatedat3$AOSISMembers+climatedat3$logNatDiaster+envcpiahalf3+climatedat3$EnvMinistry+climatedat3$Polity2*(climatedat3$logBilaterialAidRecip+climatedat3$GEFFundsDummy+climatedat3$logbiodiversity+climatedat3$FoodProd+climatedat3$AOSISMembers+climatedat3$logNatDiaster)+climatedat3$LeftExec+climatedat3$RightExec+climatedat3$NoExec+climatedat3$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r4<-lmer(ldel~climatedat4$logGDPpercap+gdpsquared4+climatedat4$logpop+popsquared4+climatedat4$Polity2+climatedat4$RegQual+climate$G20+climatedat4$UNSC+climatedat4$IO+climatedat4$WBEB+distance1000+climatedat4$GEFCouncil+climatedat4$logBilaterialAidDonor+climatedat4$logGEFDon+EnvTreat4+climatedat4$logBilaterialAidRecip+climatedat4$CDMDonor+climatedat4$CDMHost+climatedat4$GEFFundsDummy+climatedat4$logCO2percap+climatedat4$OPECDummy+climatedat4$logbiodiversity+climatedat4$FoodProd+climatedat4$AOSISMembers+climatedat4$logNatDiaster+envcpiahalf4+climatedat4$EnvMinistry+climatedat4$Polity2*(climatedat4$logBilaterialAidRecip+climatedat4$GEFFundsDummy+climatedat4$logbiodiversity+climatedat4$FoodProd+climatedat4$AOSISMembers+climatedat4$logNatDiaster)+climatedat4$LeftExec+climatedat4$RightExec+climatedat4$NoExec+climatedat4$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
tab1r5<-lmer(ldel~climatedat5$logGDPpercap+gdpsquared5+climatedat5$logpop+popsquared5+climatedat5$Polity2+climatedat5$RegQual+climate$G20+climatedat5$UNSC+climatedat5$IO+climatedat5$WBEB+distance1000+climatedat5$GEFCouncil+climatedat5$logBilaterialAidDonor+climatedat5$logGEFDon+EnvTreat5+climatedat5$logBilaterialAidRecip+climatedat5$CDMDonor+climatedat5$CDMHost+climatedat5$GEFFundsDummy+climatedat5$logCO2percap+climatedat5$OPECDummy+climatedat5$logbiodiversity+climatedat5$FoodProd+climatedat5$AOSISMembers+climatedat5$logNatDiaster+envcpiahalf5+climatedat5$EnvMinistry+climatedat5$Polity2*(climatedat5$logBilaterialAidRecip+climatedat5$GEFFundsDummy+climatedat5$logbiodiversity+climatedat5$FoodProd+climatedat5$AOSISMembers+climatedat5$logNatDiaster)+climatedat5$LeftExec+climatedat5$RightExec+climatedat5$NoExec+climatedat5$logtourarriv+as.factor(climate$Year)+(1|climate$CountryCode))
 
#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER  
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:28,53:58),])
aicave
  
#TABLE 2
#ANNEX 1 COUNTRIES ANALYSIS 

#FULL MODEL POPULATION SQUARED   
#TABLE 2 COLUMN 1

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel[climate$Annex1Dummy==1]~climatedat1$logGDPpercap[climate$Annex1Dummy==1]+gdpsquared1[climate$Annex1Dummy==1]+climatedat1$logpop[climate$Annex1Dummy==1]+popsquared1[climate$Annex1Dummy==1]+climatedat1$Polity2[climate$Annex1Dummy==1]+climatedat1$RegQual[climate$Annex1Dummy==1]+climatedat1$WBEB[climate$Annex1Dummy==1]+distance1000[climate$Annex1Dummy==1]+climatedat1$GEFCouncil[climate$Annex1Dummy==1]+climatedat1$logCO2percap[climate$Annex1Dummy==1]+climatedat1$logbiodiversity[climate$Annex1Dummy==1]+climatedat1$FoodProd[climate$Annex1Dummy==1]+climatedat1$logNatDiaster[climate$Annex1Dummy==1]+envcpiahalf1[climate$Annex1Dummy==1]+climatedat1$EnvMinistry[climate$Annex1Dummy==1]+climatedat1$LeftExec[climate$Annex1Dummy==1]+climatedat1$RightExec[climate$Annex1Dummy==1]+climatedat1$NoExec[climate$Annex1Dummy==1]+climate$G20[climate$Annex1Dummy==1]+climatedat1$UNSC[climate$Annex1Dummy==1]+climatedat1$IO[climate$Annex1Dummy==1]+climatedat1$logtourarriv[climate$Annex1Dummy==1]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==1]+climatedat1$logGEFDon[climate$Annex1Dummy==1]+EnvTreat1[climate$Annex1Dummy==1]+climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==1]+climatedat1$CDMDonor[climate$Annex1Dummy==1]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==1]+as.factor(climate$Year[climate$Annex1Dummy==1])+(1|climate$CountryCode[climate$Annex1Dummy==1]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==1]~climatedat2$logGDPpercap[climate$Annex1Dummy==1]+gdpsquared2[climate$Annex1Dummy==1]+climatedat2$logpop[climate$Annex1Dummy==1]+popsquared2[climate$Annex1Dummy==1]+climatedat2$Polity2[climate$Annex1Dummy==1]+climatedat2$RegQual[climate$Annex1Dummy==1]+climatedat2$WBEB[climate$Annex1Dummy==1]+distance1000[climate$Annex1Dummy==1]+climatedat2$GEFCouncil[climate$Annex1Dummy==1]+climatedat2$logCO2percap[climate$Annex1Dummy==1]+climatedat2$logbiodiversity[climate$Annex1Dummy==1]+climatedat2$FoodProd[climate$Annex1Dummy==1]+climatedat2$logNatDiaster[climate$Annex1Dummy==1]+envcpiahalf2[climate$Annex1Dummy==1]+climatedat2$EnvMinistry[climate$Annex1Dummy==1]+climatedat2$LeftExec[climate$Annex1Dummy==1]+climatedat2$RightExec[climate$Annex1Dummy==1]+climatedat2$NoExec[climate$Annex1Dummy==1]+climate$G20[climate$Annex1Dummy==1]+climatedat2$UNSC[climate$Annex1Dummy==1]+climatedat2$IO[climate$Annex1Dummy==1]+climatedat2$logtourarriv[climate$Annex1Dummy==1]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==1]+climatedat2$logGEFDon[climate$Annex1Dummy==1]+EnvTreat2[climate$Annex1Dummy==1]+climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==1]+climatedat2$CDMDonor[climate$Annex1Dummy==1]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==1]+as.factor(climate$Year[climate$Annex1Dummy==1])+(1|climate$CountryCode[climate$Annex1Dummy==1]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==1]~climatedat3$logGDPpercap[climate$Annex1Dummy==1]+gdpsquared3[climate$Annex1Dummy==1]+climatedat3$logpop[climate$Annex1Dummy==1]+popsquared3[climate$Annex1Dummy==1]+climatedat3$Polity2[climate$Annex1Dummy==1]+climatedat3$RegQual[climate$Annex1Dummy==1]+climatedat3$WBEB[climate$Annex1Dummy==1]+distance1000[climate$Annex1Dummy==1]+climatedat3$GEFCouncil[climate$Annex1Dummy==1]+climatedat3$logCO2percap[climate$Annex1Dummy==1]+climatedat3$logbiodiversity[climate$Annex1Dummy==1]+climatedat3$FoodProd[climate$Annex1Dummy==1]+climatedat3$logNatDiaster[climate$Annex1Dummy==1]+envcpiahalf3[climate$Annex1Dummy==1]+climatedat3$EnvMinistry[climate$Annex1Dummy==1]+climatedat3$LeftExec[climate$Annex1Dummy==1]+climatedat3$RightExec[climate$Annex1Dummy==1]+climatedat3$NoExec[climate$Annex1Dummy==1]+climate$G20[climate$Annex1Dummy==1]+climatedat3$UNSC[climate$Annex1Dummy==1]+climatedat3$IO[climate$Annex1Dummy==1]+climatedat3$logtourarriv[climate$Annex1Dummy==1]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==1]+climatedat3$logGEFDon[climate$Annex1Dummy==1]+EnvTreat3[climate$Annex1Dummy==1]+climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==1]+climatedat3$CDMDonor[climate$Annex1Dummy==1]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==1]+as.factor(climate$Year[climate$Annex1Dummy==1])+(1|climate$CountryCode[climate$Annex1Dummy==1]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==1]~climatedat4$logGDPpercap[climate$Annex1Dummy==1]+gdpsquared4[climate$Annex1Dummy==1]+climatedat4$logpop[climate$Annex1Dummy==1]+popsquared4[climate$Annex1Dummy==1]+climatedat4$Polity2[climate$Annex1Dummy==1]+climatedat4$RegQual[climate$Annex1Dummy==1]+climatedat4$WBEB[climate$Annex1Dummy==1]+distance1000[climate$Annex1Dummy==1]+climatedat4$GEFCouncil[climate$Annex1Dummy==1]+climatedat4$logCO2percap[climate$Annex1Dummy==1]+climatedat4$logbiodiversity[climate$Annex1Dummy==1]+climatedat4$FoodProd[climate$Annex1Dummy==1]+climatedat4$logNatDiaster[climate$Annex1Dummy==1]+envcpiahalf4[climate$Annex1Dummy==1]+climatedat4$EnvMinistry[climate$Annex1Dummy==1]+climatedat4$LeftExec[climate$Annex1Dummy==1]+climatedat4$RightExec[climate$Annex1Dummy==1]+climatedat4$NoExec[climate$Annex1Dummy==1]+climate$G20[climate$Annex1Dummy==1]+climatedat4$UNSC[climate$Annex1Dummy==1]+climatedat4$IO[climate$Annex1Dummy==1]+climatedat4$logtourarriv[climate$Annex1Dummy==1]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==1]+climatedat4$logGEFDon[climate$Annex1Dummy==1]+EnvTreat4[climate$Annex1Dummy==1]+climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==1]+climatedat4$CDMDonor[climate$Annex1Dummy==1]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==1]+as.factor(climate$Year[climate$Annex1Dummy==1])+(1|climate$CountryCode[climate$Annex1Dummy==1]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==1]~climatedat5$logGDPpercap[climate$Annex1Dummy==1]+gdpsquared5[climate$Annex1Dummy==1]+climatedat5$logpop[climate$Annex1Dummy==1]+popsquared5[climate$Annex1Dummy==1]+climatedat5$Polity2[climate$Annex1Dummy==1]+climatedat5$RegQual[climate$Annex1Dummy==1]+climatedat5$WBEB[climate$Annex1Dummy==1]+distance1000[climate$Annex1Dummy==1]+climatedat5$GEFCouncil[climate$Annex1Dummy==1]+climatedat5$logCO2percap[climate$Annex1Dummy==1]+climatedat5$logbiodiversity[climate$Annex1Dummy==1]+climatedat5$FoodProd[climate$Annex1Dummy==1]+climatedat5$logNatDiaster[climate$Annex1Dummy==1]+envcpiahalf5[climate$Annex1Dummy==1]+climatedat5$EnvMinistry[climate$Annex1Dummy==1]+climatedat5$LeftExec[climate$Annex1Dummy==1]+climatedat5$RightExec[climate$Annex1Dummy==1]+climatedat5$NoExec[climate$Annex1Dummy==1]+climate$G20[climate$Annex1Dummy==1]+climatedat5$UNSC[climate$Annex1Dummy==1]+climatedat5$IO[climate$Annex1Dummy==1]+climatedat5$logtourarriv[climate$Annex1Dummy==1]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==1]+climatedat5$logGEFDon[climate$Annex1Dummy==1]+EnvTreat5[climate$Annex1Dummy==1]+climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==1]+climatedat5$CDMDonor[climate$Annex1Dummy==1]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==1]+as.factor(climate$Year[climate$Annex1Dummy==1])+(1|climate$CountryCode[climate$Annex1Dummy==1]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER
print(cbind(mnreg,sqrt(varreg),tv,pval)[2:11,])
aicave
  
#NON- ANNEX 1 COUNTRIES ANALYSIS 

#FULL MODEL POPULATION SQUARED 
#TABLE 2 COLUMN 2

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel[climate$Annex1Dummy==0]~climatedat1$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared1[climate$Annex1Dummy==0]+climatedat1$logpop[climate$Annex1Dummy==0]+popsquared1[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]+climatedat1$RegQual[climate$Annex1Dummy==0]+climatedat1$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0]+climatedat1$logbiodiversity[climate$Annex1Dummy==0]+climatedat1$FoodProd[climate$Annex1Dummy==0]+climatedat1$AOSISMembers[climate$Annex1Dummy==0]+climatedat1$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf1[climate$Annex1Dummy==0]+climatedat1$EnvMinistry[climate$Annex1Dummy==0]+climatedat1$LeftExec[climate$Annex1Dummy==0]+climatedat1$RightExec[climate$Annex1Dummy==0]+climatedat1$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat1$UNSC[climate$Annex1Dummy==0]+climatedat1$IO[climate$Annex1Dummy==0]+climatedat1$logtourarriv[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat1$logGEFDon[climate$Annex1Dummy==0]+EnvTreat1[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat1$CDMHost[climate$Annex1Dummy==0]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==0]~climatedat2$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared2[climate$Annex1Dummy==0]+climatedat2$logpop[climate$Annex1Dummy==0]+popsquared2[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]+climatedat2$RegQual[climate$Annex1Dummy==0]+climatedat2$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0]+climatedat2$logbiodiversity[climate$Annex1Dummy==0]+climatedat2$FoodProd[climate$Annex1Dummy==0]+climatedat2$AOSISMembers[climate$Annex1Dummy==0]+climatedat2$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf2[climate$Annex1Dummy==0]+climatedat2$EnvMinistry[climate$Annex1Dummy==0]+climatedat2$LeftExec[climate$Annex1Dummy==0]+climatedat2$RightExec[climate$Annex1Dummy==0]+climatedat2$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat2$UNSC[climate$Annex1Dummy==0]+climatedat2$IO[climate$Annex1Dummy==0]+climatedat2$logtourarriv[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat2$logGEFDon[climate$Annex1Dummy==0]+EnvTreat2[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat2$CDMHost[climate$Annex1Dummy==0]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==0]~climatedat3$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared3[climate$Annex1Dummy==0]+climatedat3$logpop[climate$Annex1Dummy==0]+popsquared3[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]+climatedat3$RegQual[climate$Annex1Dummy==0]+climatedat3$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat3$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0]+climatedat3$logbiodiversity[climate$Annex1Dummy==0]+climatedat3$FoodProd[climate$Annex1Dummy==0]+climatedat3$AOSISMembers[climate$Annex1Dummy==0]+climatedat3$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf3[climate$Annex1Dummy==0]+climatedat3$EnvMinistry[climate$Annex1Dummy==0]+climatedat3$LeftExec[climate$Annex1Dummy==0]+climatedat3$RightExec[climate$Annex1Dummy==0]+climatedat3$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat3$UNSC[climate$Annex1Dummy==0]+climatedat3$IO[climate$Annex1Dummy==0]+climatedat3$logtourarriv[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat3$logGEFDon[climate$Annex1Dummy==0]+EnvTreat3[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat3$CDMHost[climate$Annex1Dummy==0]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==0]~climatedat4$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared4[climate$Annex1Dummy==0]+climatedat4$logpop[climate$Annex1Dummy==0]+popsquared4[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]+climatedat4$RegQual[climate$Annex1Dummy==0]+climatedat4$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat4$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0]+climatedat4$logbiodiversity[climate$Annex1Dummy==0]+climatedat4$FoodProd[climate$Annex1Dummy==0]+climatedat4$AOSISMembers[climate$Annex1Dummy==0]+climatedat4$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf4[climate$Annex1Dummy==0]+climatedat4$EnvMinistry[climate$Annex1Dummy==0]+climatedat4$LeftExec[climate$Annex1Dummy==0]+climatedat4$RightExec[climate$Annex1Dummy==0]+climatedat4$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat4$UNSC[climate$Annex1Dummy==0]+climatedat4$IO[climate$Annex1Dummy==0]+climatedat4$logtourarriv[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat4$logGEFDon[climate$Annex1Dummy==0]+EnvTreat4[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat4$CDMHost[climate$Annex1Dummy==0]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==0]~climatedat5$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared5[climate$Annex1Dummy==0]+climatedat5$logpop[climate$Annex1Dummy==0]+popsquared5[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]+climatedat5$RegQual[climate$Annex1Dummy==0]+climatedat5$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat5$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0]+climatedat5$logbiodiversity[climate$Annex1Dummy==0]+climatedat5$FoodProd[climate$Annex1Dummy==0]+climatedat5$AOSISMembers[climate$Annex1Dummy==0]+climatedat5$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf5[climate$Annex1Dummy==0]+climatedat5$EnvMinistry[climate$Annex1Dummy==0]+climatedat5$LeftExec[climate$Annex1Dummy==0]+climatedat5$RightExec[climate$Annex1Dummy==0]+climatedat5$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat5$UNSC[climate$Annex1Dummy==0]+climatedat5$IO[climate$Annex1Dummy==0]+climatedat5$logtourarriv[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat5$logGEFDon[climate$Annex1Dummy==0]+EnvTreat5[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat5$CDMHost[climate$Annex1Dummy==0]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER  
print(cbind(mnreg,sqrt(varreg),tv,pval)[2:12,])
aicave
  
#FULL. MODEL POPULATION SQUARED INTERACTION OF CO2PERCAP WITH POLITY2 — PRO- EMISSIONS INTEREST GROUPS WITH ECONOMIC INTEREST
#TABLE 2 COLUMN 3

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel[climate$Annex1Dummy==0]~climatedat1$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared1[climate$Annex1Dummy==0]+climatedat1$logpop[climate$Annex1Dummy==0]+popsquared1[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]+climatedat1$RegQual[climate$Annex1Dummy==0]+climatedat1$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]:(climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0])+climatedat1$logbiodiversity[climate$Annex1Dummy==0]+climatedat1$FoodProd[climate$Annex1Dummy==0]+climatedat1$AOSISMembers[climate$Annex1Dummy==0]+climatedat1$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf1[climate$Annex1Dummy==0]+climatedat1$EnvMinistry[climate$Annex1Dummy==0]+climatedat1$LeftExec[climate$Annex1Dummy==0]+climatedat1$RightExec[climate$Annex1Dummy==0]+climatedat1$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat1$UNSC[climate$Annex1Dummy==0]+climatedat1$IO[climate$Annex1Dummy==0]+climatedat1$logtourarriv[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat1$logGEFDon[climate$Annex1Dummy==0]+EnvTreat1[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat1$CDMHost[climate$Annex1Dummy==0]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==0]~climatedat2$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared2[climate$Annex1Dummy==0]+climatedat2$logpop[climate$Annex1Dummy==0]+popsquared2[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]+climatedat2$RegQual[climate$Annex1Dummy==0]+climatedat2$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]:(climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0])+climatedat2$logbiodiversity[climate$Annex1Dummy==0]+climatedat2$FoodProd[climate$Annex1Dummy==0]+climatedat2$AOSISMembers[climate$Annex1Dummy==0]+climatedat2$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf2[climate$Annex1Dummy==0]+climatedat2$EnvMinistry[climate$Annex1Dummy==0]+climatedat2$LeftExec[climate$Annex1Dummy==0]+climatedat2$RightExec[climate$Annex1Dummy==0]+climatedat2$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat2$UNSC[climate$Annex1Dummy==0]+climatedat2$IO[climate$Annex1Dummy==0]+climatedat2$logtourarriv[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat2$logGEFDon[climate$Annex1Dummy==0]+EnvTreat2[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat2$CDMHost[climate$Annex1Dummy==0]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==0]~climatedat3$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared3[climate$Annex1Dummy==0]+climatedat3$logpop[climate$Annex1Dummy==0]+popsquared3[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]+climatedat3$RegQual[climate$Annex1Dummy==0]+climatedat3$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat3$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]:(climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0])+climatedat3$logbiodiversity[climate$Annex1Dummy==0]+climatedat3$FoodProd[climate$Annex1Dummy==0]+climatedat3$AOSISMembers[climate$Annex1Dummy==0]+climatedat3$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf3[climate$Annex1Dummy==0]+climatedat3$EnvMinistry[climate$Annex1Dummy==0]+climatedat3$LeftExec[climate$Annex1Dummy==0]+climatedat3$RightExec[climate$Annex1Dummy==0]+climatedat3$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat3$UNSC[climate$Annex1Dummy==0]+climatedat3$IO[climate$Annex1Dummy==0]+climatedat3$logtourarriv[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat3$logGEFDon[climate$Annex1Dummy==0]+EnvTreat3[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat3$CDMHost[climate$Annex1Dummy==0]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==0]~climatedat4$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared4[climate$Annex1Dummy==0]+climatedat4$logpop[climate$Annex1Dummy==0]+popsquared4[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]+climatedat4$RegQual[climate$Annex1Dummy==0]+climatedat4$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat4$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]:(climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0])+climatedat4$logbiodiversity[climate$Annex1Dummy==0]+climatedat4$FoodProd[climate$Annex1Dummy==0]+climatedat4$AOSISMembers[climate$Annex1Dummy==0]+climatedat4$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf4[climate$Annex1Dummy==0]+climatedat4$EnvMinistry[climate$Annex1Dummy==0]+climatedat4$LeftExec[climate$Annex1Dummy==0]+climatedat4$RightExec[climate$Annex1Dummy==0]+climatedat4$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat4$UNSC[climate$Annex1Dummy==0]+climatedat4$IO[climate$Annex1Dummy==0]+climatedat4$logtourarriv[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat4$logGEFDon[climate$Annex1Dummy==0]+EnvTreat4[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat4$CDMHost[climate$Annex1Dummy==0]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==0]~climatedat5$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared5[climate$Annex1Dummy==0]+climatedat5$logpop[climate$Annex1Dummy==0]+popsquared5[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]+climatedat5$RegQual[climate$Annex1Dummy==0]+climatedat5$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat5$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]:(climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0])+climatedat5$logbiodiversity[climate$Annex1Dummy==0]+climatedat5$FoodProd[climate$Annex1Dummy==0]+climatedat5$AOSISMembers[climate$Annex1Dummy==0]+climatedat5$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf5[climate$Annex1Dummy==0]+climatedat5$EnvMinistry[climate$Annex1Dummy==0]+climatedat5$LeftExec[climate$Annex1Dummy==0]+climatedat5$RightExec[climate$Annex1Dummy==0]+climatedat5$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat5$UNSC[climate$Annex1Dummy==0]+climatedat5$IO[climate$Annex1Dummy==0]+climatedat5$logtourarriv[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat5$logGEFDon[climate$Annex1Dummy==0]+EnvTreat5[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat5$CDMHost[climate$Annex1Dummy==0]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER  
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:12,52:53),])
aicave
  
#FULL. MODEL POPULATION SQUARED INTERACTION OF CO2PERCAP WITH BUREAUCRACY 1 AND ECONOMIC INTEREST
#TABLE 2 COLUMN 4

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel[climate$Annex1Dummy==0]~climatedat1$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared1[climate$Annex1Dummy==0]+climatedat1$logpop[climate$Annex1Dummy==0]+popsquared1[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]+climatedat1$RegQual[climate$Annex1Dummy==0]+climatedat1$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0]+(envcpiahalf1[climate$Annex1Dummy==0]+climatedat1$EnvMinistry[climate$Annex1Dummy==0])*(climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0])+climatedat1$logbiodiversity[climate$Annex1Dummy==0]+climatedat1$FoodProd[climate$Annex1Dummy==0]+climatedat1$AOSISMembers[climate$Annex1Dummy==0]+climatedat1$logNatDiaster[climate$Annex1Dummy==0]+climatedat1$LeftExec[climate$Annex1Dummy==0]+climatedat1$RightExec[climate$Annex1Dummy==0]+climatedat1$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat1$UNSC[climate$Annex1Dummy==0]+climatedat1$IO[climate$Annex1Dummy==0]+climatedat1$logtourarriv[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat1$logGEFDon[climate$Annex1Dummy==0]+EnvTreat1[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat1$CDMHost[climate$Annex1Dummy==0]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==0]~climatedat2$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared2[climate$Annex1Dummy==0]+climatedat2$logpop[climate$Annex1Dummy==0]+popsquared2[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]+climatedat2$RegQual[climate$Annex1Dummy==0]+climatedat2$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0]+(envcpiahalf2[climate$Annex1Dummy==0]+climatedat2$EnvMinistry[climate$Annex1Dummy==0])*(climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0])+climatedat2$logbiodiversity[climate$Annex1Dummy==0]+climatedat2$FoodProd[climate$Annex1Dummy==0]+climatedat2$AOSISMembers[climate$Annex1Dummy==0]+climatedat2$logNatDiaster[climate$Annex1Dummy==0]+climatedat2$LeftExec[climate$Annex1Dummy==0]+climatedat2$RightExec[climate$Annex1Dummy==0]+climatedat2$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat2$UNSC[climate$Annex1Dummy==0]+climatedat2$IO[climate$Annex1Dummy==0]+climatedat2$logtourarriv[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat2$logGEFDon[climate$Annex1Dummy==0]+EnvTreat2[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat2$CDMHost[climate$Annex1Dummy==0]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==0]~climatedat3$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared3[climate$Annex1Dummy==0]+climatedat3$logpop[climate$Annex1Dummy==0]+popsquared3[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]+climatedat3$RegQual[climate$Annex1Dummy==0]+climatedat3$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat3$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0]+(envcpiahalf3[climate$Annex1Dummy==0]+climatedat3$EnvMinistry[climate$Annex1Dummy==0])*(climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0])+climatedat3$logbiodiversity[climate$Annex1Dummy==0]+climatedat3$FoodProd[climate$Annex1Dummy==0]+climatedat3$AOSISMembers[climate$Annex1Dummy==0]+climatedat3$logNatDiaster[climate$Annex1Dummy==0]+climatedat3$LeftExec[climate$Annex1Dummy==0]+climatedat3$RightExec[climate$Annex1Dummy==0]+climatedat3$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat3$UNSC[climate$Annex1Dummy==0]+climatedat3$IO[climate$Annex1Dummy==0]+climatedat3$logtourarriv[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat3$logGEFDon[climate$Annex1Dummy==0]+EnvTreat3[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat3$CDMHost[climate$Annex1Dummy==0]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==0]~climatedat4$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared4[climate$Annex1Dummy==0]+climatedat4$logpop[climate$Annex1Dummy==0]+popsquared4[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]+climatedat4$RegQual[climate$Annex1Dummy==0]+climatedat4$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat4$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0]+(envcpiahalf4[climate$Annex1Dummy==0]+climatedat4$EnvMinistry[climate$Annex1Dummy==0])*(climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0])+climatedat4$logbiodiversity[climate$Annex1Dummy==0]+climatedat4$FoodProd[climate$Annex1Dummy==0]+climatedat4$AOSISMembers[climate$Annex1Dummy==0]+climatedat4$logNatDiaster[climate$Annex1Dummy==0]+climatedat4$LeftExec[climate$Annex1Dummy==0]+climatedat4$RightExec[climate$Annex1Dummy==0]+climatedat4$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat4$UNSC[climate$Annex1Dummy==0]+climatedat4$IO[climate$Annex1Dummy==0]+climatedat4$logtourarriv[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat4$logGEFDon[climate$Annex1Dummy==0]+EnvTreat4[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat4$CDMHost[climate$Annex1Dummy==0]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==0]~climatedat5$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared5[climate$Annex1Dummy==0]+climatedat5$logpop[climate$Annex1Dummy==0]+popsquared5[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]+climatedat5$RegQual[climate$Annex1Dummy==0]+climatedat5$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat5$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0]+(envcpiahalf5[climate$Annex1Dummy==0]+climatedat5$EnvMinistry[climate$Annex1Dummy==0])*(climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0])+climatedat5$logbiodiversity[climate$Annex1Dummy==0]+climatedat5$FoodProd[climate$Annex1Dummy==0]+climatedat5$AOSISMembers[climate$Annex1Dummy==0]+climatedat5$logNatDiaster[climate$Annex1Dummy==0]+climatedat5$LeftExec[climate$Annex1Dummy==0]+climatedat5$RightExec[climate$Annex1Dummy==0]+climatedat5$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat5$UNSC[climate$Annex1Dummy==0]+climatedat5$IO[climate$Annex1Dummy==0]+climatedat5$logtourarriv[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat5$logGEFDon[climate$Annex1Dummy==0]+EnvTreat5[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat5$CDMHost[climate$Annex1Dummy==0]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER  
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:12,53),])
aicave
  
#FULL. MODEL POPULATION SQUARED CIVIL SOCIETY MECHANISM AND VULERNABILITY
#TABLE 2 COLUMN 5

#RUNNING EACH DATA SET ONCE
tab1r1<-lmer(ldel[climate$Annex1Dummy==0]~climatedat1$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared1[climate$Annex1Dummy==0]+climatedat1$logpop[climate$Annex1Dummy==0]+popsquared1[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]+climatedat1$RegQual[climate$Annex1Dummy==0]+climatedat1$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]*(climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==0]+climatedat1$logbiodiversity[climate$Annex1Dummy==0]+climatedat1$AOSISMembers[climate$Annex1Dummy==0]+climatedat1$FoodProd[climate$Annex1Dummy==0]+climatedat1$logNatDiaster[climate$Annex1Dummy==0])+envcpiahalf1[climate$Annex1Dummy==0]+climatedat1$EnvMinistry[climate$Annex1Dummy==0]+climatedat1$LeftExec[climate$Annex1Dummy==0]+climatedat1$RightExec[climate$Annex1Dummy==0]+climatedat1$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat1$UNSC[climate$Annex1Dummy==0]+climatedat1$IO[climate$Annex1Dummy==0]+climatedat1$logtourarriv[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat1$logGEFDon[climate$Annex1Dummy==0]+EnvTreat1[climate$Annex1Dummy==0]+climatedat1$CDMHost[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==0]~climatedat2$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared2[climate$Annex1Dummy==0]+climatedat2$logpop[climate$Annex1Dummy==0]+popsquared2[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]+climatedat2$RegQual[climate$Annex1Dummy==0]+climatedat2$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]*(climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==0]+climatedat2$logbiodiversity[climate$Annex1Dummy==0]+climatedat2$AOSISMembers[climate$Annex1Dummy==0]+climatedat2$FoodProd[climate$Annex1Dummy==0]+climatedat2$logNatDiaster[climate$Annex1Dummy==0])+envcpiahalf2[climate$Annex1Dummy==0]+climatedat2$EnvMinistry[climate$Annex1Dummy==0]+climatedat2$LeftExec[climate$Annex1Dummy==0]+climatedat2$RightExec[climate$Annex1Dummy==0]+climatedat2$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat2$UNSC[climate$Annex1Dummy==0]+climatedat2$IO[climate$Annex1Dummy==0]+climatedat2$logtourarriv[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat2$logGEFDon[climate$Annex1Dummy==0]+EnvTreat2[climate$Annex1Dummy==0]+climatedat2$CDMHost[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==0]~climatedat3$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared3[climate$Annex1Dummy==0]+climatedat3$logpop[climate$Annex1Dummy==0]+popsquared3[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]+climatedat3$RegQual[climate$Annex1Dummy==0]+climatedat3$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat3$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]*(climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==0]+climatedat3$logbiodiversity[climate$Annex1Dummy==0]+climatedat3$AOSISMembers[climate$Annex1Dummy==0]+climatedat3$FoodProd[climate$Annex1Dummy==0]+climatedat3$logNatDiaster[climate$Annex1Dummy==0])+envcpiahalf3[climate$Annex1Dummy==0]+climatedat3$EnvMinistry[climate$Annex1Dummy==0]+climatedat3$LeftExec[climate$Annex1Dummy==0]+climatedat3$RightExec[climate$Annex1Dummy==0]+climatedat3$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat3$UNSC[climate$Annex1Dummy==0]+climatedat3$IO[climate$Annex1Dummy==0]+climatedat3$logtourarriv[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat3$logGEFDon[climate$Annex1Dummy==0]+EnvTreat3[climate$Annex1Dummy==0]+climatedat3$CDMHost[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==0]~climatedat4$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared4[climate$Annex1Dummy==0]+climatedat4$logpop[climate$Annex1Dummy==0]+popsquared4[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]+climatedat4$RegQual[climate$Annex1Dummy==0]+climatedat4$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat4$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]*(climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==0]+climatedat4$logbiodiversity[climate$Annex1Dummy==0]+climatedat4$AOSISMembers[climate$Annex1Dummy==0]+climatedat4$FoodProd[climate$Annex1Dummy==0]+climatedat4$logNatDiaster[climate$Annex1Dummy==0])+envcpiahalf4[climate$Annex1Dummy==0]+climatedat4$EnvMinistry[climate$Annex1Dummy==0]+climatedat4$LeftExec[climate$Annex1Dummy==0]+climatedat4$RightExec[climate$Annex1Dummy==0]+climatedat4$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat4$UNSC[climate$Annex1Dummy==0]+climatedat4$IO[climate$Annex1Dummy==0]+climatedat4$logtourarriv[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat4$logGEFDon[climate$Annex1Dummy==0]+EnvTreat4[climate$Annex1Dummy==0]+climatedat4$CDMHost[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==0]~climatedat5$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared5[climate$Annex1Dummy==0]+climatedat5$logpop[climate$Annex1Dummy==0]+popsquared5[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]+climatedat5$RegQual[climate$Annex1Dummy==0]+climatedat5$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat5$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]*(climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==0]+climatedat5$logbiodiversity[climate$Annex1Dummy==0]+climatedat5$AOSISMembers[climate$Annex1Dummy==0]+climatedat5$FoodProd[climate$Annex1Dummy==0]+climatedat5$logNatDiaster[climate$Annex1Dummy==0])+envcpiahalf5[climate$Annex1Dummy==0]+climatedat5$EnvMinistry[climate$Annex1Dummy==0]+climatedat5$LeftExec[climate$Annex1Dummy==0]+climatedat5$RightExec[climate$Annex1Dummy==0]+climatedat5$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat5$UNSC[climate$Annex1Dummy==0]+climatedat5$IO[climate$Annex1Dummy==0]+climatedat5$logtourarriv[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat5$logGEFDon[climate$Annex1Dummy==0]+EnvTreat5[climate$Annex1Dummy==0]+climatedat5$CDMHost[climate$Annex1Dummy==0]+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER    
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:12,52:55),])
aicave
  
#FULL. MODEL POPULATION SQUARED: CIVIL SOCIETY WITH GREEN POLITICS
#TABLE 2 COLUMN 6

#RUNNING EACH DATA SET ONCE    
tab1r1<-lmer(ldel[climate$Annex1Dummy==0]~climatedat1$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared1[climate$Annex1Dummy==0]+climatedat1$logpop[climate$Annex1Dummy==0]+popsquared1[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]+climatedat1$RegQual[climate$Annex1Dummy==0]+climatedat1$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logCO2percap[climate$Annex1Dummy==0]+climatedat1$OPECDummy[climate$Annex1Dummy==0]+climatedat1$logbiodiversity[climate$Annex1Dummy==0]+climatedat1$FoodProd[climate$Annex1Dummy==0]+climatedat1$AOSISMembers[climate$Annex1Dummy==0]+climatedat1$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf1[climate$Annex1Dummy==0]+climatedat1$EnvMinistry[climate$Annex1Dummy==0]+climatedat1$LeftExec[climate$Annex1Dummy==0]+climatedat1$RightExec[climate$Annex1Dummy==0]+climatedat1$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat1$UNSC[climate$Annex1Dummy==0]+climatedat1$IO[climate$Annex1Dummy==0]+climatedat1$logtourarriv[climate$Annex1Dummy==0]+climatedat1$Polity2[climate$Annex1Dummy==0]*(climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat1$logGEFDon[climate$Annex1Dummy==0]+EnvTreat1[climate$Annex1Dummy==0]+climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat1$CDMHost[climate$Annex1Dummy==0]+climatedat1$GEFFundsDummy[climate$Annex1Dummy==0])+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r2<-lmer(ldel[climate$Annex1Dummy==0]~climatedat2$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared2[climate$Annex1Dummy==0]+climatedat2$logpop[climate$Annex1Dummy==0]+popsquared2[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]+climatedat2$RegQual[climate$Annex1Dummy==0]+climatedat2$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logCO2percap[climate$Annex1Dummy==0]+climatedat2$OPECDummy[climate$Annex1Dummy==0]+climatedat2$logbiodiversity[climate$Annex1Dummy==0]+climatedat2$FoodProd[climate$Annex1Dummy==0]+climatedat2$AOSISMembers[climate$Annex1Dummy==0]+climatedat2$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf2[climate$Annex1Dummy==0]+climatedat2$EnvMinistry[climate$Annex1Dummy==0]+climatedat2$LeftExec[climate$Annex1Dummy==0]+climatedat2$RightExec[climate$Annex1Dummy==0]+climatedat2$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat2$UNSC[climate$Annex1Dummy==0]+climatedat2$IO[climate$Annex1Dummy==0]+climatedat2$logtourarriv[climate$Annex1Dummy==0]+climatedat2$Polity2[climate$Annex1Dummy==0]*(climatedat2$GEFCouncil[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat2$logGEFDon[climate$Annex1Dummy==0]+EnvTreat2[climate$Annex1Dummy==0]+climatedat2$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat2$CDMHost[climate$Annex1Dummy==0]+climatedat2$GEFFundsDummy[climate$Annex1Dummy==0])+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r3<-lmer(ldel[climate$Annex1Dummy==0]~climatedat3$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared3[climate$Annex1Dummy==0]+climatedat3$logpop[climate$Annex1Dummy==0]+popsquared3[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]+climatedat3$RegQual[climate$Annex1Dummy==0]+climatedat3$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logCO2percap[climate$Annex1Dummy==0]+climatedat3$OPECDummy[climate$Annex1Dummy==0]+climatedat3$logbiodiversity[climate$Annex1Dummy==0]+climatedat3$FoodProd[climate$Annex1Dummy==0]+climatedat3$AOSISMembers[climate$Annex1Dummy==0]+climatedat3$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf3[climate$Annex1Dummy==0]+climatedat3$EnvMinistry[climate$Annex1Dummy==0]+climatedat3$LeftExec[climate$Annex1Dummy==0]+climatedat3$RightExec[climate$Annex1Dummy==0]+climatedat3$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat3$UNSC[climate$Annex1Dummy==0]+climatedat3$IO[climate$Annex1Dummy==0]+climatedat3$logtourarriv[climate$Annex1Dummy==0]+climatedat3$Polity2[climate$Annex1Dummy==0]*(climatedat3$GEFCouncil[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat3$logGEFDon[climate$Annex1Dummy==0]+EnvTreat3[climate$Annex1Dummy==0]+climatedat3$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat3$CDMHost[climate$Annex1Dummy==0]+climatedat3$GEFFundsDummy[climate$Annex1Dummy==0])+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r4<-lmer(ldel[climate$Annex1Dummy==0]~climatedat4$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared4[climate$Annex1Dummy==0]+climatedat4$logpop[climate$Annex1Dummy==0]+popsquared4[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]+climatedat4$RegQual[climate$Annex1Dummy==0]+climatedat4$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logCO2percap[climate$Annex1Dummy==0]+climatedat4$OPECDummy[climate$Annex1Dummy==0]+climatedat4$logbiodiversity[climate$Annex1Dummy==0]+climatedat4$FoodProd[climate$Annex1Dummy==0]+climatedat4$AOSISMembers[climate$Annex1Dummy==0]+climatedat4$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf4[climate$Annex1Dummy==0]+climatedat4$EnvMinistry[climate$Annex1Dummy==0]+climatedat4$LeftExec[climate$Annex1Dummy==0]+climatedat4$RightExec[climate$Annex1Dummy==0]+climatedat4$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat4$UNSC[climate$Annex1Dummy==0]+climatedat4$IO[climate$Annex1Dummy==0]+climatedat4$logtourarriv[climate$Annex1Dummy==0]+climatedat4$Polity2[climate$Annex1Dummy==0]*(climatedat4$GEFCouncil[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat4$logGEFDon[climate$Annex1Dummy==0]+EnvTreat4[climate$Annex1Dummy==0]+climatedat4$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat4$CDMHost[climate$Annex1Dummy==0]+climatedat4$GEFFundsDummy[climate$Annex1Dummy==0])+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))
tab1r5<-lmer(ldel[climate$Annex1Dummy==0]~climatedat5$logGDPpercap[climate$Annex1Dummy==0]+gdpsquared5[climate$Annex1Dummy==0]+climatedat5$logpop[climate$Annex1Dummy==0]+popsquared5[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]+climatedat5$RegQual[climate$Annex1Dummy==0]+climatedat5$WBEB[climate$Annex1Dummy==0]+distance1000[climate$Annex1Dummy==0]+climatedat1$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logCO2percap[climate$Annex1Dummy==0]+climatedat5$OPECDummy[climate$Annex1Dummy==0]+climatedat5$logbiodiversity[climate$Annex1Dummy==0]+climatedat5$FoodProd[climate$Annex1Dummy==0]+climatedat5$AOSISMembers[climate$Annex1Dummy==0]+climatedat5$logNatDiaster[climate$Annex1Dummy==0]+envcpiahalf5[climate$Annex1Dummy==0]+climatedat5$EnvMinistry[climate$Annex1Dummy==0]+climatedat5$LeftExec[climate$Annex1Dummy==0]+climatedat5$RightExec[climate$Annex1Dummy==0]+climatedat5$NoExec[climate$Annex1Dummy==0]+climate$G20[climate$Annex1Dummy==0]+climatedat5$UNSC[climate$Annex1Dummy==0]+climatedat5$IO[climate$Annex1Dummy==0]+climatedat5$logtourarriv[climate$Annex1Dummy==0]+climatedat5$Polity2[climate$Annex1Dummy==0]*(climatedat5$GEFCouncil[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidDonor[climate$Annex1Dummy==0]+climatedat5$logGEFDon[climate$Annex1Dummy==0]+EnvTreat5[climate$Annex1Dummy==0]+climatedat5$logBilaterialAidRecip[climate$Annex1Dummy==0]+climatedat5$CDMHost[climate$Annex1Dummy==0]+climatedat5$GEFFundsDummy[climate$Annex1Dummy==0])+as.factor(climate$Year[climate$Annex1Dummy==0])+(1|climate$CountryCode[climate$Annex1Dummy==0]))

#COMBINING RESULTS AS PER RUBIN (1987)  
reg1<-tab1r1
reg2<-tab1r2
reg3<-tab1r3
reg4<-tab1r4
reg5<-tab1r5
n<-dim(summary(reg1)$coefficients)[1]
  
mnreg<-rep(NA,n)
varreg<-rep(NA,n)
pval<-rep(NA,n)
  
for (i in 1:n){
  mnreg[i]<-mean(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))
  varreg[i]<-var(c(summary(reg1)$coefficients[i,1],summary(reg2)$coefficients[i,1],summary(reg3)$coefficients[i,1],summary(reg4)$coefficients[i,1],summary(reg5)$coefficients[i,1]))*6/5+mean(c(summary(reg1)$coefficients[i,2]^2,summary(reg2)$coefficients[i,2]^2,summary(reg3)$coefficients[i,2]^2,summary(reg4)$coefficients[i,2]^2,summary(reg5)$coefficients[i,2]^2))
}
  
grouplevvar<-mean(c(as.data.frame(VarCorr(reg1))[1,4],as.data.frame(VarCorr(reg2))[1,4], as.data.frame(VarCorr(reg3))[1,4], as.data.frame(VarCorr(reg4))[1,4], as.data.frame(VarCorr(reg5))[1,4]))
indlevvar<-mean(c(as.data.frame(VarCorr(reg1))[2,4],as.data.frame(VarCorr(reg2))[2,4], as.data.frame(VarCorr(reg3))[2,4], as.data.frame(VarCorr(reg4))[2,4], as.data.frame(VarCorr(reg5))[2,4]))
aicave<-mean(c(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5)))
  
defr<-4011-n-2
tv<-mnreg/sqrt(varreg)
for (i in 1:n){
  if (tv[i]>0){
    pval[i]<-pt(tv[i],defr,lower.tail=FALSE)*2}
  if (tv[i]<0){
    pval[i]<-pt(tv[i],defr)*2}
}

#PRINTING ONLY RESULTS SHOWN IN THE PAPER    
print(cbind(mnreg,sqrt(varreg),tv,pval)[c(2:12,56,58),])
aicave

