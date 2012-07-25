detach()
rm(list=ls())     # clear everything out of memory
rm(list=ls(all=TRUE))

setwd("C:\\2011\\banglaUCMG\\dec_2011_new_analysis")

  # set to wherever your data directory is located 
getwd()           # check that you are in the correct directory

B <- read.table("GIS Data_All_Farm and LBM_08_02_11_removed_1049_locations_short_R.txt", header=TRUE, sep="\t", na.strings="NA",dec=".")  # read text data file
str(B)
summary(B)
names(B)
head(B) 
dim(B)  
dim(table(B$SubDistrict_ID))

#calculate Layerno Broilerno Ducksno other_birds_no per subdistrict
?aggregate
aggregate(B$Layerno,list(B$SubDistrict_ID),mean,na.rm=TRUE)
aggregate(B$Broilerno,list(B$SubDistrict_ID),mean,na.rm=TRUE)
aggregate(B$Ducksno,list(B$SubDistrict_ID),mean,na.rm=TRUE)
aggregate(B$other_birds_no,list(B$SubDistrict_ID),mean,na.rm=TRUE)

aggregate(B$Layerno,list(B$SubDistrict_ID),median,na.rm=TRUE)
aggregate(B$Broilerno,list(B$SubDistrict_ID),median,na.rm=TRUE)
aggregate(B$Ducksno,list(B$SubDistrict_ID),median,na.rm=TRUE)
aggregate(B$other_birds_no,list(B$SubDistrict_ID),median,na.rm=TRUE)

aggregate(B$Layerno,list(B$SubDistrict_ID),sd,na.rm=TRUE)
aggregate(B$Broilerno,list(B$SubDistrict_ID),sd,na.rm=TRUE)
aggregate(B$Ducksno,list(B$SubDistrict_ID),sd,na.rm=TRUE)
aggregate(B$other_birds_no,list(B$SubDistrict_ID),sd,na.rm=TRUE)

library(psych)
describe.by(B$Layerno, B$SubDistrict_ID)

#This method is best to summary statistics by group
library(doBy)
sizebysubdistrict <- summaryBy(Layerno+Broilerno + Ducksno + other_birds_no~ SubDistrict_ID, data = B, 
 	 FUN = function(x) { c(m = mean(x,na.rm=TRUE), md= median(x,na.rm=TRUE), s = sd(x,na.rm=TRUE)) } )

sizebysubdistrict 	 
write.table(sizebysubdistrict, file="size_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)

names(sizebysubdistrict)
nrow(sizebysubdistrict)
sum(!is.na(sizebysubdistrict$Layerno.m))
sum(!is.na(sizebysubdistrict$Layerno.m))/nrow(sizebysubdistrict)

sum(!is.na(sizebysubdistrict$Broilerno.m))
sum(!is.na(sizebysubdistrict$Broilerno.m))/nrow(sizebysubdistrict)

sum(!is.na(sizebysubdistrict$Ducksno.m))
sum(!is.na(sizebysubdistrict$Ducksno.m))/nrow(sizebysubdistrict)

sum(!is.na(sizebysubdistrict$other_birds_no.m))
sum(!is.na(sizebysubdistrict$other_birds_no.m))/nrow(sizebysubdistrict)



# visit
B$visit <- 1
visits_per_subdistrict <- tapply(B$visit,B$SubDistrict_ID,sum)
visits_per_subdistrict <- data.frame(names(visits_per_subdistrict),visits_per_subdistrict)
visits_per_subdistrict
names(visits_per_subdistrict) <- c("id","NumberOfFarms")
names(visits_per_subdistrict)


# market type
B$Type <- factor(B$Type,labels=c('Farm','Market'))
summary(B$Type) 
Typetable <- table(B$Type)
Typetable
lbls <- paste(names(Typetable), "\n", Typetable, sep="")
pie(Typetable,col=rainbow(length(Typetable)),labels = lbls)
summary(B$Type=='Market')                          
B$Market <- 0
B$Market[B$Type=='Market'] <- 1
markets_per_subdistrict <- tapply(B$Market,B$SubDistrict_ID,sum)
markets_per_subdistrict <- data.frame(names(markets_per_subdistrict),markets_per_subdistrict)
markets_per_subdistrict
names(markets_per_subdistrict) <- c("id","NumberOfMarkets")
names(markets_per_subdistrict)
write.table(markets_per_subdistrict, file="markets_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)
summary(markets_per_subdistrict)
head(markets_per_subdistrict)
dim(markets_per_subdistrict)

# hatchery
summary(B$Farmtype_hatchery)
B$hatchery <- 0
B$hatchery[B$Farmtype_hatchery==T] <- 1 
table(B$hatchery)
hatchery_per_subdistrict <- tapply(B$hatchery,B$SubDistrict_ID,sum)
hatchery_per_subdistrict <- data.frame(names(hatchery_per_subdistrict),hatchery_per_subdistrict)
hatchery_per_subdistrict
table(hatchery_per_subdistrict)
names(hatchery_per_subdistrict) <- c("id","NumberOfHatchery")
write.table(hatchery_per_subdistrict, file="hatchery_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)
head(hatchery_per_subdistrict)
dim(hatchery_per_subdistrict)

# layers farms
summary(B$Farmtype_layers)
B$Flayers <- 0
B$Flayers[B$Farmtype_layers==T] <- 1 
table(B$Flayers)
Flayers_per_subdistrict <- tapply(B$Flayers,B$SubDistrict_ID,sum)
Flayers_per_subdistrict <- data.frame(names(Flayers_per_subdistrict),Flayers_per_subdistrict)
Flayers_per_subdistrict
names(Flayers_per_subdistrict) <- c("id","NumberOfLayerFarms")
write.table(Flayers_per_subdistrict, file="Flayers_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)
head(Flayers_per_subdistrict)
dim(Flayers_per_subdistrict)

# broilers farms
summary(B$Farmtype_broilers)
B$Fbroilers <- 0
B$Fbroilers[B$Farmtype_broilers==T] <- 1 
table(B$Fbroilers)
Fbroilers_per_subdistrict <- tapply(B$Fbroilers,B$SubDistrict_ID,sum)
Fbroilers_per_subdistrict <- data.frame(names(Fbroilers_per_subdistrict),Fbroilers_per_subdistrict)
Fbroilers_per_subdistrict
names(Fbroilers_per_subdistrict) <- c("id","NumberOfBroilerFarms")
write.table(Fbroilers_per_subdistrict, file="Fbroilers_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)
head(Fbroilers_per_subdistrict)
names(Fbroilers_per_subdistrict)
dim(Fbroilers_per_subdistrict)

m_data <- merge(visits_per_subdistrict,markets_per_subdistrict,by="id")
m_data <- merge(m_data,hatchery_per_subdistrict,by="id")
m_data <- merge(m_data,Flayers_per_subdistrict,by="id")
m_data <- merge(m_data,Fbroilers_per_subdistrict,by="id")
dim(m_data)
head(m_data)
write.table(m_data, file="survey_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=F)


# Original report farm type
B$Farm_Type <- NA
B$Farm_Type[B$Farmtype_broilers==T] <- 'Broilers'
B$Farm_Type[B$Farmtype_layers==T] <- 'Layers'
B$Farm_Type[B$Farmtype_hatchery==T] <- 'Hatchery'    
B$Farm_Type[B$Farmtype_hatchery==T & B$Farmtype_layers==T] <- 'Hatchery & Layers'
B$Farm_Type[B$Farmtype_hatchery==T & B$Farmtype_broilers==T] <- 'Hatchery & Broilers'
B$Farm_Type[B$Farmtype_layers==T & B$Farmtype_broilers==T] <- 'Layers & Broilers'
B$Farm_Type[B$Farmtype_hatchery==T & B$Farmtype_layers==T & B$Farmtype_broilers==T] <- 'Hatchery & Layers & Broilers'
B$Farm_Type <- factor(B$Farm_Type)
data.frame(summary(B$Farm_Type))
write.table(data.frame(summary(B$Farm_Type)), file="original_FarmType.txt", append=FALSE, quote=F, sep="\t", col.names=TRUE, row.names=T)
#Draw a pie chart
B$Farm_Type <- NA
B$Farm_Type[B$Farmtype_broilers==T] <- 'Broilers'
B$Farm_Type[B$Farmtype_layers==T] <- 'Layers'
B$Farm_Type[B$Farmtype_hatchery==T] <- 'Hatchery'
B$Farm_Type <- factor(B$Farm_Type)
summary(B$Farm_Type)       
Farm_table <- table(B$Farm_Type)
Farm_table                      
lbls <- paste(names(Farm_table), "\n", Farm_table, sep="")
pie(Farm_table,col=rainbow(length(Farm_table)),labels = lbls)

# Add broiler and layer together to get total chicken
str(B$Layerno)
str(B$Broilerno)                      
B$T_Chicken <- B$Layerno+B$Broilerno
str(B$T_Chicken)
summary(B$T_Chicken) 

# Find the real farm with number > 0
B$LayerFarm <- F
B$LayerFarm[B$Layerno > 0] <- T
summary(B$LayerFarm) 

B$BroilerFarm <- F
B$BroilerFarm[B$Broilerno > 0] <- T
summary(B$BroilerFarm)

B$Duck_Farm <- F
B$Duck_Farm[B$Ducksno>0] <- T
summary(B$Duck_Farm)

B$other_birds_Farm <- F
B$other_birds_Farm[B$other_birds_no>0] <- T
summary(B$other_birds_Farm)

# No data farm is huge 25305 no poultry number, 39097 has number when visit
summary(B$LayerFarm == T | B$BroilerFarm==T | B$Duck_Farm==T | B$other_birds_Farm==T)

B$Farm_Type2 <- NA
B$Farm_Type2[B$LayerFarm == T] <- 'Layer'
B$Farm_Type2[B$BroilerFarm == T] <- 'Broiler'
B$Farm_Type2[B$Duck_Farm == T] <- 'Duck'
B$Farm_Type2[B$other_birds_Farm == T] <- 'other_birds'
B$Farm_Type2  <- factor(B$Farm_Type2)
summary(B$Farm_Type2)
data.frame(summary(B$Farm_Type2))
write.table(data.frame(summary(B$Farm_Type2)), file="from_number_FarmType.txt", append=FALSE, quote=F, sep="\t", col.names=TRUE, row.names=T)
sum(summary(B$Farm_Type2))    

# subset only none zero farms
summary(subset(B, (B$LayerFarm == T | B$BroilerFarm==T | B$Duck_Farm==T | B$other_birds_Farm)))
names(B)
sub_farm <- subset(B, (B$LayerFarm == T | B$BroilerFarm==T | B$Duck_Farm==T | B$other_birds_Farm), select=c("ID","Date_visit","Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no","SubDistrict_ID","Farm_Type2")) 
names(sub_farm)

Farm_Type2table <- table(sub_farm$Farm_Type2)
Farm_Type2table
lbls <- paste(names(Farm_Type2table), "\n", Farm_Type2table, sep="")
pie(Farm_Type2table,col=rainbow(length(Farm_Type2table)),labels = lbls)

# Farm size analysis
summary(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")]) 
data.frame(summary(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")]) )
write.table(data.frame(summary(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")])), file="farmSize.txt", append=FALSE, quote=F, sep="\t", col.names=TRUE, row.names=F)
sapply(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")], sd, na.rm=TRUE)
data.frame(sapply(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")], sd, na.rm=TRUE))
write.table(data.frame(sapply(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")], sd, na.rm=TRUE)), file="farmSize_sd.txt", append=FALSE, quote=F, sep="\t", col.names=F, row.names=T)

boxplot(sub_farm[,c("Layerno","Broilerno","T_Chicken","Ducksno","other_birds_no")])

# Farm number per District ID
B$farm <- 1     
farms_per_subdistrict <- data.frame(tapply(B$farm,B$SubDistrict_ID,sum))
summary(farms_per_subdistrict)
write.table(farms_per_subdistrict, file="farms_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

write.table(B, file="GIS Data_All_Farm and LBM_08_02_11_removed_1049_locations_short_after_R.txt", append=FALSE, quote=F, sep="\t", col.names=TRUE, row.names=F)

ID <- as.factor(levels(as.factor(B$Leo_ID)))
str(ID)

Farm <- as.vector(tapply(B$Farms,B$Leo_ID,sum))
str(Farm)
Market <- as.vector(tapply(B$Market,B$Leo_ID,sum))
str(Market)
Duck_Farm <- as.vector(tapply(B$Duck_Farm,B$Leo_ID,sum))
str(Duck_Farm)

Layerno <- as.vector(tapply(B$Layerno,B$Leo_ID,sum))
str(Layerno)
Broilerno <- as.vector(tapply(B$Broilerno,B$Leo_ID,sum))
str(Broilerno)
T_Chicken <- as.vector(tapply(B$T_Chicken,B$Leo_ID,sum))
str(T_Chicken)
Ducksno <- as.vector(tapply(B$Ducksno,B$Leo_ID,sum))
str(Ducksno)

new_data <- data.frame(ID)
new_data$Farm <- Farm
new_data$Market <- Market
new_data$Duck_Farm <- Duck_Farm
new_data$Layerno <- Layerno
new_data$Broilerno <- Broilerno
new_data$T_Chicken <- T_Chicken
new_data$Ducksno <- Ducksno

str(new_data)
head(new_data)

D <- read.table("C:\\2011\\banglaUCMG\\old_data\\BanglaUCMG1.txt", header=TRUE, sep="\t", na.strings="NA",dec=".")  # read text data file
names(D)
str(D)
str(new_data)

B_D <- merge(new_data,D,by.x='ID',by.y='FID')

plot(B_D$T_Chicken,B_D$T_CHICKEN)


B_D_subset <- subset(B_D, select=c(ID,Farm,Market,Duck_Farm,Layerno,Broilerno,T_Chicken,Ducksno,T_CHICKEN,T_DUCKS))

write.table (B_D_subset, file="B_D_subset.txt", append=FALSE, quote=TRUE, sep="\t", col.names=TRUE, row.names=FALSE)
