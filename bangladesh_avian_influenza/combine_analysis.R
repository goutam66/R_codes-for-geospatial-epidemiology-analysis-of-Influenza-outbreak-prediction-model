detach()
rm(list=ls())     # clear everything out of memory
rm(list=ls(all=TRUE))

setwd("C:\\2011\\banglaUCMG\\dec_2011_new_analysis")

  # set to wherever your data directory is located
getwd()           # check that you are in the correct directory


D_new <- read.table("D_complete", sep=" ",header=T)
str(D_new)
head(D_new)
dim(D_new)

survey_subdistrict <- read.table("summary_survey_subdistrict.txt", header=T)
dim(survey_subdistrict)
names(survey_subdistrict)
head(survey_subdistrict)
str(survey_subdistrict)
summary(survey_subdistrict)

size_subdistrict <- read.table("size_per_subdistrict.txt", header=T)
dim(size_subdistrict)
names(size_subdistrict)
head(size_subdistrict)
str(size_subdistrict)
summary(size_subdistrict)

all_data <- merge(D_new,survey_subdistrict,by.x="FID",by.y="id",all.x=T)
all_data <- merge(all_data,size_subdistrict,by.x="FID",by.y="SubDistrict_ID",all.x=T)


head(all_data)
all_data
dim(all_data)
names(all_data)

library(epibasix)
univar(survey_subdistrict$NumberOfFarms)
univar(survey_subdistrict$NumberOfMarkets)
univar(survey_subdistrict$NumberOfHatchery)      
univar(survey_subdistrict$NumberOfLayerFarms)
univar(survey_subdistrict$NumberOfBroilerFarms)

summary(survey_subdistrict$NumberOfFarms)
summary(survey_subdistrict$NumberOfMarkets)
summary(survey_subdistrict$NumberOfHatchery)      
summary(survey_subdistrict$NumberOfLayerFarms)
summary(survey_subdistrict$NumberOfBroilerFarms)

cor(survey_subdistrict)

summary(size_subdistrict$Layerno.m)
summary(size_subdistrict$Broilerno.m)
summary(size_subdistrict$Ducksno.m)      
summary(size_subdistrict$other_birds_no.m)

write.table(all_data, file="all_data_size_number.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)




######FINAL########

model.logit=glm(OUTBR~  POP2001_log2 + BRLA_log + Roads + ArT, data = all_data,family=binomial)
summary(model.logit)
                                                                   
########
model.logit=glm(OUTBR~  POP2001_log2 + BRLA_log + Roads + ArT+ NumberOfLayerFarms+Layerno.m, data = all_data,family=binomial)
summary(model.logit)


model.logit=glm(OUTBR~  NumberOfLayerFarms + CROPMEAN +RICECROPME+Roads, data = all_data,family=binomial)
summary(model.logit)


###### Describe the spatial correlation ########
library(gstat)
all_data$X_KM2 = all_data$X_KM^2
all_data$Y_KM2 = all_data$Y_KM^2

LagInterval = 10
qv <- variogram(OUTBR ~ 1, width = LagInterval, locations = ~ X_KM + Y_KM, data = D, cutoff = 150)
qv.fit = fit.variogram(qv, model = vgm(0.14, "Sph", 100, 0.06), fit.method = 2)
plot(qv$dist, qv$gamma, ylim = c(0,0.25),xlab = "Lag (km)", ylab="Semivariance") # plot the result
myModel = variogramLine(qv.fit,maxdist = 150)
lines(myModel$dist,myModel$gamma)

myNug = qv.fit[1,2]
mySill = qv.fit[2,2]
myRange = qv.fit[2,3]
text(5,0.04, paste("Nugget :", format(myNug, digits = 3)), pos = 4)
text(5,0.025, paste("Sill :", format(mySill, digits = 3)), pos = 4)
text(5,0.010, paste("Range :", format(myRange, digits = 3), "km"), pos = 4)
text(80,0.025, paste("Lag interval :", format(LagInterval, digits = 1), "m"), pos = 4)
text(80,0.010, paste("Min. pairs / lag :", min(qv$np)), pos = 4)




########### Function to estimate the autoregressive term ###########
myLib.AutoRegressiveQuant = function (myDepVec, myXVec, myYVec, myMaxDist) {
#myDepVec = myD$OUTBR
#myXVec = myD$X_COORD
#myYVec = myD$Y_COORD
#myMaxDist = 0.4
myARVec = myDepVec * 0
nRec =length(myARVec)
for (i in 1:nRec) {
  myDistVec = (((myXVec[i] - myXVec)^2)+((myYVec[i] - myYVec)^2))^0.5

  myDistVecS0 = subset(myDistVec, myDistVec != 0)
  myART = sum(1/myDistVecS0)

  mySGrid = cbind(myDistVec, myDepVec)
  mySSGrid = subset(mySGrid, myDistVec !=0 & myDistVec < myMaxDist)
  if (nrow(mySSGrid) > 0) {
  myWeigths = 1/(mySSGrid[,1])
  mySum = sum(myWeigths * mySSGrid[,2])
  myARVec[i] = mySum/myART
  }
   if (nrow(mySSGrid) == 0) myARVec[i] = 0
}
return(myARVec)
}


####### Build the autoregressive term ########
all_data$ArT = myLib.AutoRegressiveQuant(all_data$Boutbreaks, all_data$X_KM, all_data$Y_KM, 78.9)

summary(all_data$ArT)
str(all_data)
names(all_data)


################## Single vari test####################
model.No_households=glm(OUTBR~  No_households, data = all_data,family=binomial)
summary(model.NumberOfFarms)
 
model.No_Villages=glm(OUTBR~  No_Villages, data = all_data,family=binomial)
summary(model.No_Villages)
 
model.No_Mauza=glm(OUTBR~  No_Mauza, data = all_data,family=binomial)
summary(model.No_Mauza)
 
model.No_union=glm(OUTBR~  No_union, data = all_data,family=binomial)
summary(model.No_union)
 
model.POP2001=glm(OUTBR~  POP2001, data = all_data,family=binomial)
summary(model.POP2001)
 
model.T_CHICKEN=glm(OUTBR~  T_CHICKEN, data = all_data,family=binomial)
summary(model.T_CHICKEN)
 
model.T_DUCKS=glm(OUTBR~  T_DUCKS, data = all_data,family=binomial)
summary(model.T_DUCKS)
 
model.FIRMS_LYR=glm(OUTBR~  FIRMS_LYR, data = all_data,family=binomial)
summary(model.FIRMS_LYR)
 
model.BIRDS_LYR=glm(OUTBR~  BIRDS_LYR, data = all_data,family=binomial)
summary(model.BIRDS_LYR)
 
model.FIRMS_BRO=glm(OUTBR~  FIRMS_BRO, data = all_data,family=binomial)
summary(model.FIRMS_BRO)
 
model.BIRDS_BRO=glm(OUTBR~  BIRDS_BRO, data = all_data,family=binomial)
summary(model.BIRDS_BRO)
 
model.T_CATTLE=glm(OUTBR~  T_CATTLE, data = all_data,family=binomial)
summary(model.T_CATTLE)
 
model.T_BUFFALO=glm(OUTBR~  T_BUFFALO, data = all_data,family=binomial)
summary(model.T_BUFFALO)
 
model.T_GOAT=glm(OUTBR~  T_GOAT, data = all_data,family=binomial)
summary(model.T_GOAT)
 
model.T_SHEEP=glm(OUTBR~  T_SHEEP, data = all_data,family=binomial)
summary(model.T_SHEEP)
 
model.Towns=glm(OUTBR~  Towns, data = all_data,family=binomial)
summary(model.Towns)
 
model.Roads=glm(OUTBR~  Roads, data = all_data,family=binomial)
summary(model.Roads)
 
model.R_lenght=glm(OUTBR~  R_lenght, data = all_data,family=binomial)
summary(model.R_lenght)
 
model.AREAKMN45=glm(OUTBR~  AREAKMN45, data = all_data,family=binomial)
summary(model.AREAKMN45)
 
model.DUDN=glm(OUTBR~  DUDN, data = all_data,family=binomial)
summary(model.DUDN)
 
model.CHDN=glm(OUTBR~  CHDN, data = all_data,family=binomial)
summary(model.CHDN)
 
model.CROPMEAN=glm(OUTBR~  CROPMEAN, data = all_data,family=binomial)
summary(model.CROPMEAN)
 
model.RICECROPME=glm(OUTBR~  RICECROPME, data = all_data,family=binomial)
summary(model.RICECROPME)
 
model.CROPSUM=glm(OUTBR~  CROPSUM, data = all_data,family=binomial)
summary(model.CROPSUM)
 
model.RICECROPSU=glm(OUTBR~  RICECROPSU, data = all_data,family=binomial)
summary(model.RICECROPSU)
 
model.D_CHICKEN=glm(OUTBR~  D_CHICKEN, data = all_data,family=binomial)
summary(model.D_CHICKEN)
 
model.D_DUCKS=glm(OUTBR~  D_DUCKS, data = all_data,family=binomial)
summary(model.D_DUCKS)
 
model.D_FARM_LYR=glm(OUTBR~  D_FARM_LYR, data = all_data,family=binomial)
summary(model.D_FARM_LYR)
 
model.D_LYR=glm(OUTBR~  D_LYR, data = all_data,family=binomial)
summary(model.D_LYR)
 
model.D_FARM_BRO=glm(OUTBR~  D_FARM_BRO, data = all_data,family=binomial)
summary(model.D_FARM_BRO)
 
model.D_BRO=glm(OUTBR~  D_BRO, data = all_data,family=binomial)
summary(model.D_BRO)
 
model.D_CATTLE=glm(OUTBR~  D_CATTLE, data = all_data,family=binomial)
summary(model.D_CATTLE)
 
model.D_BUFFALO=glm(OUTBR~  D_BUFFALO, data = all_data,family=binomial)
summary(model.D_BUFFALO)
 
model.D_GOAT=glm(OUTBR~  D_GOAT, data = all_data,family=binomial)
summary(model.D_GOAT)
 
model.D_SHEEP=glm(OUTBR~  D_SHEEP, data = all_data,family=binomial)
summary(model.D_SHEEP)
 
model.D_No_housh=glm(OUTBR~  D_No_housh, data = all_data,family=binomial)
summary(model.D_No_housh)
 
model.D_No_Villages=glm(OUTBR~  D_No_Villages, data = all_data,family=binomial)
summary(model.D_No_Villages)
 
model.D_No_Mauza=glm(OUTBR~  D_No_Mauza, data = all_data,family=binomial)
summary(model.D_No_Mauza)
 
model.D_No_union=glm(OUTBR~  D_No_union, data = all_data,family=binomial)
summary(model.D_No_union)
 
model.D_POP2001=glm(OUTBR~  D_POP2001, data = all_data,family=binomial)
summary(model.D_POP2001)
 
model.D_Towns=glm(OUTBR~  D_Towns, data = all_data,family=binomial)
summary(model.D_Towns)
 
model.D_Roads=glm(OUTBR~  D_Roads, data = all_data,family=binomial)
summary(model.D_Roads)
 
model.D_R_lenght=glm(OUTBR~  D_R_lenght, data = all_data,family=binomial)
summary(model.D_R_lenght)
 
model.D_CROPSUM=glm(OUTBR~  D_CROPSUM, data = all_data,family=binomial)
summary(model.D_CROPSUM)
 
model.D_RICECROPSU=glm(OUTBR~  D_RICECROPSU, data = all_data,family=binomial)
summary(model.D_RICECROPSU)
 
model.BRLA=glm(OUTBR~  BRLA, data = all_data,family=binomial)
summary(model.BRLA)
 
model.BY=glm(OUTBR~  BY, data = all_data,family=binomial)
summary(model.BY)
 
model.D_BRLA=glm(OUTBR~  D_BRLA, data = all_data,family=binomial)
summary(model.D_BRLA)

model.T_CHICKEN_log=glm(OUTBR~  T_CHICKEN_log, data = all_data,family=binomial)
summary(model.T_CHICKEN_log)

model.T_DUCKS_log=glm(OUTBR~  T_DUCKS_log, data = all_data,family=binomial)
summary(model.T_DUCKS_log)

model.BRLA_log=glm(OUTBR~  BRLA_log, data = all_data,family=binomial)
summary(model.BRLA_log)

model.D_DUCKS_log=glm(OUTBR~  D_DUCKS_log, data = all_data,family=binomial)
summary(model.D_DUCKS_log)

model.ArT=glm(OUTBR~  ArT, data = all_data,family=binomial)
summary(model.ArT)


# nuber of farms#
model.NumberOfFarms=glm(OUTBR~  NumberOfFarms, data = all_data,family=binomial)
summary(model.NumberOfFarms)

model.NumberOfMarkets=glm(OUTBR~  NumberOfMarkets, data = all_data,family=binomial)
summary(model.NumberOfMarkets)

model.NumberOfHatchery=glm(OUTBR~  NumberOfHatchery, data = all_data,family=binomial)
summary(model.NumberOfHatchery)

model.NumberOfLayerFarms=glm(OUTBR~  NumberOfLayerFarms, data = all_data,family=binomial)
summary(model.NumberOfLayerFarms)

model.NumberOfBroilerFarms=glm(OUTBR~  NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.NumberOfBroilerFarms)

## farm size## 
model.Layerno.md=glm(OUTBR~  Layerno.md, data = all_data,family=binomial)
summary(model.Layerno.md)

model.Broilerno.m=glm(OUTBR~  Broilerno.m, data = all_data,family=binomial)
summary(model.Broilerno.m)

model.Ducksno.m=glm(OUTBR~  Ducksno.m, data = all_data,family=binomial)
summary(model.Ducksno.m)

model.other_birds_no.m=glm(OUTBR~  other_birds_no.m, data = all_data,family=binomial)
summary(model.other_birds_no.m)

model.Layerno.md=glm(OUTBR~  Layerno.md, data = all_data,family=binomial)
summary(model.Layerno.md)

model.Broilerno.md=glm(OUTBR~  Broilerno.md, data = all_data,family=binomial)
summary(model.Broilerno.md)

model.Ducksno.md=glm(OUTBR~  Ducksno.md, data = all_data,family=binomial)
summary(model.Ducksno.md)

model.other_birds_no.md=glm(OUTBR~  other_birds_no.md, data = all_data,family=binomial)
summary(model.other_birds_no.md)

########## significant variables ##############

No_households
No_Villages
No_Mauza
No_union
POP2001
T_CHICKEN
FIRMS_LYR
BIRDS_LYR
FIRMS_BRO
BIRDS_BRO
T_CATTLE
T_BUFFALO
T_GOAT
T_SHEEP
Roads
R_lenght
D_CHICKEN
D_No_Mauza
D_No_union

BRLA
BY
D_BRLA

NumberOfFarms
NumberOfMarkets
NumberOfHatchery
NumberOfLayerFarms
NumberOfBroilerFarms

Layerno.md
Broilerno.m
Ducksno.m
other_birds_no.m


# model test#########
model.old=glm(OUTBR~  No_households + 
No_Villages+
No_Mauza  +
No_union  +
POP2001     +  
T_CHICKEN    +
FIRMS_LYR     +
BIRDS_LYR      +
FIRMS_BRO       +
BIRDS_BRO        +
T_CATTLE          +
T_BUFFALO          +
T_GOAT              +
T_SHEEP              +
Roads                 +
R_lenght               +
D_CHICKEN               +
D_No_Mauza               +
D_No_union                +
 
D_BRLA                        +
                              
NumberOfFarms                   +
NumberOfMarkets                  +
NumberOfHatchery                  +
NumberOfLayerFarms                 +
NumberOfBroilerFarms                

, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA                        +
                              
NumberOfFarms                   +
NumberOfMarkets                  +
NumberOfHatchery                  +
NumberOfLayerFarms                 +
NumberOfBroilerFarms   , data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA                        +
NumberOfHatchery              
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA                        +
NumberOfHatchery    +
Layerno.md              
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA                        +
NumberOfHatchery  +
RICECROPSU            
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.m+Layerno.md+Layerno.s+
Broilerno.md+Broilerno.s+Ducksno.m+
Ducksno.md+Ducksno.s
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.m+Layerno.md+Layerno.s+
Broilerno.md+Broilerno.s+Ducksno.m+
Ducksno.md+Ducksno.s
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.m+Layerno.md+Layerno.s+
Broilerno.md+Broilerno.s
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.m+Layerno.md+Layerno.s
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.md
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  NumberOfHatchery+NumberOfLayerFarms+Layerno.md
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA  +
NumberOfHatchery+NumberOfLayerFarms+Layerno.md
, data = all_data,family=binomial)
summary(model.old)

model.old=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA  +
NumberOfHatchery+NumberOfLayerFarms
, data = all_data,family=binomial)
summary(model.old)


model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms
, data = all_data,family=binomial)
summary(model.old)


model.final=glm(OUTBR~  No_households + 
No_Villages+  
POP2001     +  
BIRDS_BRO        +
Roads                 +
D_BRLA  +
NumberOfHatchery+NumberOfLayerFarms+Layerno.md
, data = all_data,family=binomial)
summary(model.final)

?glm
?predict.glm

model.final

newpredict <- predict.glm(model.final, type="response")
newpredict <- data.frame(names(newpredict),newpredict)  
dim(newpredict)
names(newpredict)<-c("id","predicts")
names(newpredict)
newpredict
length(newpredict)
?cbind
predict.glm(model.final, type="response")
data.frame(predict.glm(model.final, type="response"))

names(all_data)

all_data <- merge(all_data,newpredict,by.x="FID",by.y="id",all.x=T)

all_data$predicts

predict.glm(model.final, type="response")
data.frame(predict.glm(model.final, type="response"))
dim(data.frame(predict.glm(model.final, type="response")))
all_data$predicts <- predict.glm(model.final, type="response")
all_data$predicts[which(is.na(all_data$predicts))]<-0

all_data$predicts
str(newpredicts)
names(newpredicts)

str(all_data)


######ROC curve This works well!!!!########

ROC( test = all_data$predicts,
     stat = all_data$OUTBR,
     form = NULL,
     plot = c("sp", "ROC"),         
       PV = TRUE,             
       MX = TRUE,             
       MI = TRUE,             
      AUC = TRUE,             
     grid = seq(0,100,10),
 col.grid = gray( 0.9 ),
     cuts = NULL,
      lwd = 2)


ROC( test = all_data$predicts,
     stat = all_data$OUTBR,
     form = NULL,
     plot = c("sp", "ROC"),         
       PV = FALSE,             
       MX = FALSE,             
       MI = FALSE,             
      AUC = TRUE,             
     grid = seq(0,100,10),
 col.grid = gray( 0.9 ),
     cuts = NULL,
      lwd = 2)

########################

## try remove NA from numberOflayerfarms to run the model
dim(all_data)
dim(all_data[which(is.na(all_data$NumberOfFarms)),])
dim(all_data[which(!is.na(all_data$NumberOfFarms)),])
sub_all_data <-  all_data[which(!is.na(all_data$NumberOfFarms)),]
dim(sub_all_data)

model.old=glm(OUTBR~  NumberOfFarms + 
NumberOfMarkets+NumberOfHatchery+NumberOfLayerFarms+NumberOfBroilerFarms
, data = sub_all_data,family=binomial)
summary(model.old)




######Initial MODEL with 26 vars + 5 new survey number of farms used in the paper########
model.old=glm(OUTBR~  POP2001+POP2001_log+POP2001_2+POP2001_log2+D_POP2001_log2+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BIRDS_BRO+BRLA+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT, data = all_data,family=binomial)
summary(model.old)

######Initial MODEL with 26 vars + 5 new survey number of farms used in the paper########


model.26_5=glm(OUTBR~  POP2001+POP2001_log+POP2001_2+POP2001_log2+D_POP2001_log2+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BIRDS_BRO+BRLA+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT+NumberOfFarms+NumberOfMarkets+NumberOfHatchery+
                             NumberOfLayerFarms+NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.26_5)

## model + 2 Layerno.m+Ducksno.m #
model.26_5=glm(OUTBR~  POP2001+POP2001_log+POP2001_2+POP2001_log2+D_POP2001_log2+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BIRDS_BRO+BRLA+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT+NumberOfFarms+NumberOfMarkets+NumberOfHatchery+
                             NumberOfLayerFarms+NumberOfBroilerFarms+Layerno.m, data = all_data,family=binomial)
summary(model.26_5)

model.26_5=glm(OUTBR~ POP2001_log2+Roads+D_BRLA_log+ NumberOfLayerFarms+Layerno.m, data = all_data,family=binomial)
summary(model.26_5)

model.old=glm(OUTBR~  D_POP2001_log2+Roads+BIRDS_LYR+BRLA_log+D_BRLA_log+ArT, data = all_data,family=binomial)
summary(model.old)

model.logit_real_1=glm(OUTBR~  Roads+BIRDS_LYR+BRLA_log+D_BRLA_log+ArT+NumberOfHatchery+Layerno.m+Ducksno.m, data = all_data,family=binomial)
summary(model.logit_real_1)

model.logit_real_1=glm(OUTBR~  BIRDS_LYR+BRLA_log+D_BRLA_log+ArT+Ducksno.m, data = all_data,family=binomial)
summary(model.logit_real_1)

model.logit=glm(OUTBR~  POP2001_log2 + BRLA_log + Roads + ArT+NumberOfHatchery, data = all_data,family=binomial)
summary(model.logit)

model.logit_real_1=glm(OUTBR~  Roads+D_BRLA_log+NumberOfHatchery+ArT, data = all_data,family=binomial)
summary(model.logit_real_1)

model.logit_real_2=glm(OUTBR~  NumberOfFarms+NumberOfMarkets+NumberOfHatchery+
                             NumberOfLayerFarms+NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.logit_real_2)

model.logit_real_3=glm(OUTBR~  POP2001_log2+Roads+D_BRLA_log+NumberOfFarms+ArT, data = all_data,family=binomial)
summary(model.logit_real_3)

model.logit_real_3=glm(OUTBR~  POP2001_log2+Roads+D_BRLA_log+NumberOfFarms+NumberOfMarkets+NumberOfHatchery+
                             NumberOfLayerFarms+NumberOfBroilerFarms+ArT, data = all_data,family=binomial)
summary(model.logit_real_3)

model.logit_real4=glm(OUTBR~  POP2001+POP2001_log+POP2001_2+POP2001_log2+D_POP2001_log2+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BIRDS_BRO+BRLA+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT+NumberOfFarms+NumberOfMarkets+
                             NumberOfLayerFarms+NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.logit_real4)

model.logit_real4=glm(OUTBR~  POP2001_log+POP2001_log2+D_POP2001_log2+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT+NumberOfFarms+NumberOfMarkets+
                             NumberOfLayerFarms+NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.logit_real4)

model.logit_real4=glm(OUTBR~  POP2001+T_CHICKEN+T_DUCKS+FIRMS_LYR+D_POP2001+Roads+R_lenght+
                             Towns+T_CHICKEN+BY+FIRMS_LYR+BIRDS_LYR+FIRMS_BRO+BRLA_log+
                             D_CHICKEN+D_FARM_LYR+D_LYR+D_FARM_BRO+D_BRO+D_BRLA+D_CHICKEN_log+D_BY_log+
                             D_BRLA_log+ArT+NumberOfFarms+NumberOfMarkets+
                             NumberOfLayerFarms+NumberOfBroilerFarms, data = all_data,family=binomial)
summary(model.logit_real4)

#### Problems: cannot generate the final model by backstep wise methods#
# the population is not significant #
model.logit_real_1=glm(OUTBR~  POP2001_log2+Roads+BIRDS_LYR+BRLA_log+D_BRLA_log+ArT, data = all_data,family=binomial)
summary(model.logit_real_1)

model.logit_real_2=glm(OUTBR~  Roads+BIRDS_LYR+BRLA_log++ArT, data = all_data,family=binomial)
summary(model.logit_real_2)

# new model ##
model.new=glm(OUTBR~  POP2001_log2+Roads+BIRDS_LYR+BRLA_log+Layerno.md+ArT, data = all_data,family=binomial)
summary(model.new)

names(all_data)


  
#######old r code from Final R code 325.R. Loth's old code  
               ######## Full model #######
?glm
model.logit=glm(OUTBR~ +BRLA+ T_CHICKEN + BY + T_DUCKS + FIRMS_LYR + BIRDS_LYR +
FIRMS_BRO + T_CATTLE + T_BUFFALO + T_GOAT + T_SHEEP + Towns	+ Roads + R_lenght + D_CHICKEN + D_DUCKS	+ D_FARM_LYR + D_LYR +
D_FARM_BRO + D_BRO + D_CATTLE + D_BUFFALO + D_GOAT + D_SHEEP + T_CHICKEN_log + T_DUCKS_log + D_BRLA + BRLA_log + No_househo +
No_Village + No_Mauza + No_union + D_No_housh + D_No_Mauza + D_No_union +
D_Towns + D_Roads + D_R_lenght + POP2001_log2+ DUDN + CHDN + CROPMEAN + RICECROPME + 
CROPSUM +	RICECROPSU + D_CROPSUM + D_RICECROP + NumberOfFa +ArT, data = all_data,family=binomial)
summary(model.logit)

######FINAL_New########

model.logit_new=glm(OUTBR~  POP2001_log2 + BRLA_log + Roads + NumberOfFa + ArT, data = D,family=binomial)
summary(model.logit_new)

########


######FINAL########

model.logit=glm(OUTBR~  POP2001_log2 + BRLA_log + Roads + NumberOfFa + ArT, data = D,family=binomial)
summary(model.logit)

########


all_data$predicts <- predict.glm(model.final, type="response")

str(all_data)


######ROC curve This works well!!!!########

ROC( test = all_data$predicts,
     stat = all_data$OUTBR,
     form = NULL,
     plot = c("sp", "ROC"),         
       PV = TRUE,             
       MX = TRUE,             
       MI = TRUE,             
      AUC = TRUE,             
     grid = seq(0,100,10),
 col.grid = gray( 0.9 ),
     cuts = NULL,
      lwd = 2)


ROC( test = all_data$predicts,
     stat = all_data$OUTBR,
     form = NULL,
     plot = c("sp", "ROC"),         
       PV = FALSE,             
       MX = FALSE,             
       MI = FALSE,             
      AUC = TRUE,             
     grid = seq(0,100,10),
 col.grid = gray( 0.9 ),
     cuts = NULL,
      lwd = 2)

########################

library(utils)