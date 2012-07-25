detach()
rm(list=ls())     # clear everything out of memory
rm(list=ls(all=TRUE))

setwd("C:\\2011\\banglaUCMG\\dec_2011_new_analysis")

  # set to wherever your data directory is located 
getwd()           # check that you are in the correct directory

outbreaks <- read.table("outbreaks_subdistrictID_correct_remove_19.txt", header=TRUE, sep="\t", na.strings="NA",dec=".")  # read text data file
str(outbreaks)
summary(outbreaks)
names(outbreaks)
head(outbreaks) 
dim(outbreaks)

outbreaks$outbreak <-1
outbreak_per_subdistrict <- data.frame(tapply(outbreaks$outbreak,outbreaks$SubDistrictID,sum))
outbreak_per_subdistrict
summary(outbreak_per_subdistrict)
write.table(outbreak_per_subdistrict, file="outbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

phaseI <- subset(outbreaks, outbreaks$Phases=='I')
phaseI
phaseIoutbreak_per_subdistrict <- data.frame(tapply(phaseI$outbreak,phaseI$SubDistrictID,sum))
phaseIoutbreak_per_subdistrict
str(phaseIoutbreak_per_subdistrict)
summary(phaseIoutbreak_per_subdistrict)
write.table(phaseIoutbreak_per_subdistrict, file="phaseIoutbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

phaseII <- subset(outbreaks, outbreaks$Phases=='II')
phaseII
phaseIIoutbreak_per_subdistrict <- data.frame(tapply(phaseII$outbreak,phaseII$SubDistrictID,sum))
phaseIIoutbreak_per_subdistrict
summary(phaseIIoutbreak_per_subdistrict)
write.table(phaseIIoutbreak_per_subdistrict, file="phaseIIoutbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

phaseIII <- subset(outbreaks, outbreaks$Phases=='III')
phaseIII
phaseIIIoutbreak_per_subdistrict <- data.frame(tapply(phaseIII$outbreak,phaseIII$SubDistrictID,sum))
phaseIIIoutbreak_per_subdistrict
summary(phaseIIIoutbreak_per_subdistrict)
write.table(phaseIIIoutbreak_per_subdistrict, file="phaseIIIoutbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

phaseIV <- subset(outbreaks, outbreaks$Phases=='IV')
phaseIV
phaseIVoutbreak_per_subdistrict <- data.frame(tapply(phaseIV$outbreak,phaseIV$SubDistrictID,sum))
phaseIVoutbreak_per_subdistrict
summary(phaseIVoutbreak_per_subdistrict)
write.table(phaseIVoutbreak_per_subdistrict, file="phaseIVoutbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

phaseV <- subset(outbreaks, outbreaks$Phases=='V')
phaseV
phaseVoutbreak_per_subdistrict <- data.frame(tapply(phaseV$outbreak,phaseV$SubDistrictID,sum))
phaseVoutbreak_per_subdistrict
summary(phaseVoutbreak_per_subdistrict)
write.table(phaseVoutbreak_per_subdistrict, file="phaseVoutbreak_per_subdistrict.txt", append=FALSE, quote=F, sep="\t", col.names=T, row.names=T)

