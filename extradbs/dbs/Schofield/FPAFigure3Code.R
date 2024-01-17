#FIGURE 3 R CODE

#SETTING UP THE APPROPRIATE LIBRARIES
library(foreign)
library(lme4)

#IMPORTING DATA
climate<-read.csv("ClimateChangeAttendanceFPA.csv", header=TRUE)
climatedat1<-read.csv("ClimateImputationsRR12181.csv",header=TRUE)
climatedat2<-read.csv("ClimateImputationsRR12182.csv",header=TRUE)
climatedat3<-read.csv("ClimateImputationsRR12183.csv",header=TRUE)
climatedat4<-read.csv("ClimateImputationsRR12184.csv",header=TRUE)
climatedat5<-read.csv("ClimateImputationsRR12185.csv",header=TRUE)

#SETTING UP VARIABLES
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

#PLOT RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES) FOR ALL COUNTRIES
#SET UP RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES)
loggdppercap<-climatedat1$logGDPpercap
logpop<-climatedat1$logpop
leftexec<-climatedat1$LeftExec
rightexec<-climatedat1$RightExec
noexec<-climatedat1$NoExec
polity2<-climatedat1$Polity2
regqual<-climatedat1$RegQual
logtourarriv<-climatedat1$logtourarriv
g20<-climate$G20
unsc<-climatedat1$UNSC
io<-climatedat1$IO
wbeb<-climatedat1$WBEB
gefcouncil<-climatedat1$GEFCouncil
logbiaiddonor<-climatedat1$logBilaterialAidDonor
logbiodiversity<-climatedat1$logbiodiversity
aosismember<-climatedat1$AOSISMembers
foodprod<-climatedat1$FoodProd
logbiaidrecip<-climatedat1$logBilaterialAidRecip
lognatdisaster<-climatedat1$logNatDiaster
logco2percap<-climatedat1$logCO2percap
opec<-climatedat1$OPECDummy
eu<-climate$EU
envministry<-climatedat1$EnvMinistry
year<-as.factor(climate$Year)
country<-climate$CountryCode

tab1r1pred<-lmer(ldel~loggdppercap+gdpsquared1+logpop+leftexec+rightexec+noexec+polity2+regqual+distance1000+logtourarriv+g20+unsc+io+wbeb+EnvTreat1+gefcouncil+logbiaiddonor+logbiodiversity+aosismember+foodprod+logbiaidrecip+lognatdisaster+logco2percap+opec+eu+envcpiahalf1+envministry+year+(1|country))

#SET UP NEW DATA WHERE ALL VARIABLES EXCEPT GDP ARE EVALUATED AT THEIR MEAN
new.data1<-data.frame(loggdppercap=seq(1.1,5.4,0.01), gdpsquared1=seq(1.1,5.4,0.01)^2, logpop=rep(mean(climatedat1$logpop), 431), leftexec=rep(mean(climatedat1$LeftExec), 431), rightexec=rep(mean(climatedat1$RightExec),431), noexec=rep(mean(climatedat1$NoExec),431), polity2=rep(mean(climatedat1$Polity2),431), regqual=rep(mean(climatedat1$RegQual),431), distance1000=rep(mean(distance1000),431), logtourarriv=rep(mean(climatedat1$logtourarriv),431), g20=rep(mean(climate$G20),431), unsc=rep(mean(climatedat1$UNSC),431), io=rep(mean(climatedat1$IO),431), wbeb=rep(mean(climatedat1$WBEB),431), EnvTreat1=rep(mean(EnvTreat1),431), gefcouncil=rep(mean(climatedat1$GEFCouncil),431), logbiaiddonor=rep(mean(climatedat1$logBilaterialAidDonor),431), logbiodiversity=rep(mean(climatedat1$logbiodiversity),431), aosismember=rep(mean(climatedat1$AOSISMembers),431), foodprod=rep(mean(climatedat1$FoodProd),431), logbiaidrecip=rep(mean(climatedat1$logBilaterialAidRecip),431), lognatdisaster=rep(mean(climatedat1$logNatDiaster),431), logco2percap=rep(mean(climatedat1$logCO2percap),431), opec=rep(mean(climatedat1$OPECDummy),431), eu=rep(mean(climate$EU),431), envcpiahalf1=rep(mean(envcpiahalf1),431),envministry=rep(mean(climatedat1$EnvMinistry),431), year=rep("2000", 431), country=rep("HRV", 431))

#GET PREDICTED VALUES FOR ALL COUNTIRES
yhats<-predict(tab1r1pred, newdata=new.data1)
lgdppercap<-seq(1.1,5.4,0.01)

#PLOT RELATIONSHIP FOR ALL COUNTRIES
plot(lgdppercap,yhats, type="l", lwd=2, lty=1, xlab="Log(GDPperCap)", ylab="Log(Delegates)", ylim=c(0,3))

#PLOT RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES) FOR ALL ANNEX 1 COUNTRIES
#SET UP RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES)

ldela1<-ldel[climate$Annex1Dummy==1]
gdpsquared1a1<-gdpsquared1[climate$Annex1Dummy==1]
loggdppercapa1<-climatedat1$logGDPpercap[climate$Annex1Dummy==1]
logpopa1<-climatedat1$logpop[climate$Annex1Dummy==1]
leftexeca1<-climatedat1$LeftExec[climate$Annex1Dummy==1]
rightexeca1<-climatedat1$RightExec[climate$Annex1Dummy==1]
noexeca1<-climatedat1$NoExec[climate$Annex1Dummy==1]
polity2a1<-climatedat1$Polity2[climate$Annex1Dummy==1]
regquala1<-climatedat1$RegQual[climate$Annex1Dummy==1]
distance1000a1<-distance1000[climate$Annex1Dummy==1]
logtourarriva1<-climatedat1$logtourarriv[climate$Annex1Dummy==1]
g20a1<-climate$G20[climate$Annex1Dummy==1]
unsca1<-climatedat1$UNSC[climate$Annex1Dummy==1]
ioa1<-climatedat1$IO[climate$Annex1Dummy==1]
wbeba1<-climatedat1$WBEB[climate$Annex1Dummy==1]
EnvTreat1a1<-EnvTreat1[climate$Annex1Dummy==1]
gefcouncila1<-climatedat1$GEFCouncil[climate$Annex1Dummy==1]
logbiaiddonora1<-climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==1]
logbiodiversitya1<-climatedat1$logbiodiversity[climate$Annex1Dummy==1]
foodproda1<-climatedat1$FoodProd[climate$Annex1Dummy==1]
logbiaidrecipa1<-climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==1]
lognatdisastera1<-climatedat1$logNatDiaster[climate$Annex1Dummy==1]
logco2percapa1<-climatedat1$logCO2percap[climate$Annex1Dummy==1]
eua1<-climate$EU[climate$Annex1Dummy==1]
envcpiahalf1a1<-envcpiahalf1[climate$Annex1Dummy==1]
envministrya1<-climatedat1$EnvMinistry[climate$Annex1Dummy==1]
yeara1<-as.factor(climate$Year[climate$Annex1Dummy==1])
countrya1<-climate$CountryCode[climate$Annex1Dummy==1]

tab1r1preda1<-lmer(ldela1~loggdppercapa1+gdpsquared1a1+logpopa1+leftexeca1+rightexeca1+noexeca1+polity2a1+regquala1+distance1000a1+logtourarriva1+g20a1+unsca1+ioa1+wbeba1+EnvTreat1a1+gefcouncila1+logbiaiddonora1+logbiodiversitya1+foodproda1+logbiaidrecipa1+lognatdisastera1+logco2percapa1+eua1+envcpiahalf1a1+envministrya1+yeara1+(1|countrya1))

#SET UP NEW DATA WHERE ALL VARIABLES EXCEPT GDP ARE EVALUATED AT THEIR MEAN
new.data1a1<-data.frame(loggdppercapa1=seq(1.1,5.4,0.01), gdpsquared1a1=seq(1.1,5.4,0.01)^2, logpopa1=rep(mean(climatedat1$logpop), 431), leftexeca1=rep(mean(climatedat1$LeftExec), 431), rightexeca1=rep(mean(climatedat1$RightExec),431), noexeca1=rep(mean(climatedat1$NoExec),431), polity2a1=rep(mean(climatedat1$Polity2),431), regquala1=rep(mean(climatedat1$RegQual),431), distance1000a1=rep(mean(distance1000),431), logtourarriva1=rep(mean(climatedat1$logtourarriv),431), g20a1=rep(mean(climate$G20),431), unsca1=rep(mean(climatedat1$UNSC),431), ioa1=rep(mean(climatedat1$IO),431), wbeba1=rep(mean(climatedat1$WBEB),431), EnvTreat1a1=rep(mean(EnvTreat1),431), gefcouncila1=rep(mean(climatedat1$GEFCouncil),431), logbiaiddonora1=rep(mean(climatedat1$logBilaterialAidDonor),431), logbiodiversitya1=rep(mean(climatedat1$logbiodiversity),431), foodproda1=rep(mean(climatedat1$FoodProd),431), logbiaidrecipa1=rep(mean(climatedat1$logBilaterialAidRecip),431), lognatdisastera1=rep(mean(climatedat1$logNatDiaster),431), logco2percapa1=rep(mean(climatedat1$logCO2percap),431), eua1=rep(mean(climate$EU),431), envcpiahalf1a1=rep(mean(envcpiahalf1),431),envministrya1=rep(mean(climatedat1$EnvMinistry),431), yeara1=rep("2000", 431), countrya1=rep("HRV", 431))

#GET PREDICTED VALUES FOR ALL ANNEX 1 COUNTIRES
yhatsa1<-predict(tab1r1preda1, newdata=new.data1a1)
lgdppercap<-seq(1.1,5.4,0.01)

#ADD ANNEX 1 COUNTRIES RELATIONSHIP TO THE PLOT
lines(lgdppercap,yhatsa1, type="l", lwd=2, lty=2, col="red")

#PLOT RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES) FOR ALL NON-ANNEX 1 COUNTRIES
#SET UP RELATIONSHIP BETWEEN LOG(GDPPERCAP) AND LOG(DELEGATES)

ldelna1<-ldel[climate$Annex1Dummy==0]
gdpsquared1na1<-gdpsquared1[climate$Annex1Dummy==0]
loggdppercapna1<-climatedat1$logGDPpercap[climate$Annex1Dummy==0]
logpopna1<-climatedat1$logpop[climate$Annex1Dummy==0]
leftexecna1<-climatedat1$LeftExec[climate$Annex1Dummy==0]
rightexecna1<-climatedat1$RightExec[climate$Annex1Dummy==0]
noexecna1<-climatedat1$NoExec[climate$Annex1Dummy==0]
polity2na1<-climatedat1$Polity2[climate$Annex1Dummy==0]
regqualna1<-climatedat1$RegQual[climate$Annex1Dummy==0]
distance1000na1<-distance1000[climate$Annex1Dummy==0]
logtourarrivna1<-climatedat1$logtourarriv[climate$Annex1Dummy==0]
g20na1<-climate$G20[climate$Annex1Dummy==0]
unscna1<-climatedat1$UNSC[climate$Annex1Dummy==0]
iona1<-climatedat1$IO[climate$Annex1Dummy==0]
wbebna1<-climatedat1$WBEB[climate$Annex1Dummy==0]
EnvTreat1na1<-EnvTreat1[climate$Annex1Dummy==0]
gefcouncilna1<-climatedat1$GEFCouncil[climate$Annex1Dummy==0]
logbiaiddonorna1<-climatedat1$logBilaterialAidDonor[climate$Annex1Dummy==0]
logbiodiversityna1<-climatedat1$logbiodiversity[climate$Annex1Dummy==0]
aosismemberna1<-climatedat1$AOSISMembers[climate$Annex1Dummy==0]
foodprodna1<-climatedat1$FoodProd[climate$Annex1Dummy==0]
logbiaidrecipna1<-climatedat1$logBilaterialAidRecip[climate$Annex1Dummy==0]
lognatdisasterna1<-climatedat1$logNatDiaster[climate$Annex1Dummy==0]
logco2percapna1<-climatedat1$logCO2percap[climate$Annex1Dummy==0]
opecna1<-climatedat1$OPECDummy[climate$Annex1Dummy==0]
envcpiahalf1na1<-envcpiahalf1[climate$Annex1Dummy==0]
envministryna1<-climatedat1$EnvMinistry[climate$Annex1Dummy==0]
yearna1<-as.factor(climate$Year[climate$Annex1Dummy==0])
countryna1<-climate$CountryCode[climate$Annex1Dummy==0]

tab1r1predna1<-lmer(ldelna1~loggdppercapna1+gdpsquared1na1+logpopna1+leftexecna1+rightexecna1+noexecna1+polity2na1+regqualna1+distance1000na1+logtourarrivna1+g20na1+unscna1+iona1+wbebna1+EnvTreat1na1+gefcouncilna1+logbiaiddonorna1+logbiodiversityna1+aosismemberna1+foodprodna1+logbiaidrecipna1+lognatdisasterna1+logco2percapna1+opecna1+envcpiahalf1na1+envministryna1+yearna1+(1|countryna1))

#SET UP NEW DATA WHERE ALL VARIABLES EXCEPT GDP ARE EVALUATED AT THEIR MEAN
new.data1na1<-data.frame(loggdppercapna1=seq(1.1,5.4,0.01), gdpsquared1na1=seq(1.1,5.4,0.01)^2, logpopna1=rep(mean(climatedat1$logpop), 431), leftexecna1=rep(mean(climatedat1$LeftExec), 431), rightexecna1=rep(mean(climatedat1$RightExec),431), noexecna1=rep(mean(climatedat1$NoExec),431), polity2na1=rep(mean(climatedat1$Polity2),431), regqualna1=rep(mean(climatedat1$RegQual),431), distance1000na1=rep(mean(distance1000),431), logtourarrivna1=rep(mean(climatedat1$logtourarriv),431), g20na1=rep(mean(climate$G20),431), unscna1=rep(mean(climatedat1$UNSC),431), iona1=rep(mean(climatedat1$IO),431), wbebna1=rep(mean(climatedat1$WBEB),431), EnvTreat1na1=rep(mean(EnvTreat1),431), gefcouncilna1=rep(mean(climatedat1$GEFCouncil),431), logbiaiddonorna1=rep(mean(climatedat1$logBilaterialAidDonor),431), logbiodiversityna1=rep(mean(climatedat1$logbiodiversity),431), aosismemberna1=rep(mean(climatedat1$AOSISMembers),431), foodprodna1=rep(mean(climatedat1$FoodProd),431), logbiaidrecipna1=rep(mean(climatedat1$logBilaterialAidRecip),431), lognatdisasterna1=rep(mean(climatedat1$logNatDiaster),431), logco2percapna1=rep(mean(climatedat1$logCO2percap),431), opecna1=rep(mean(climatedat1$OPECDummy),431), envcpiahalf1na1=rep(mean(envcpiahalf1),431),envministryna1=rep(mean(climatedat1$EnvMinistry),431), yearna1=rep("2000", 431), countryna1=rep("BWA", 431))

#GET PREDICTED VALUES FOR ALL NON-ANNEX 1 COUNTIRES
yhatsna1<-predict(tab1r1predna1, newdata=new.data1na1)
lgdppercap<-seq(1.1,5.4,0.01)

#ADD NON-ANNEX 1 COUNTRIES TO THE PLOT
lines(lgdppercap,yhatsna1, type="l", lwd=2, lty=3, col="blue")

#ADD LEGEND AND TITLE
legend("topleft",legend=c("All Countries", "Annex 1 Countries", "Non-Annex 1 Countries"), col=c("black", "red", "blue"), lty=c(1, 2, 3), lwd=c(2, 2, 2))
title("Log(Delegates) by Log(GDPperCap) by Country Type")

