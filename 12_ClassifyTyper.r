library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(mgcv)
library(Amelia)
library(gamm4)
library(plyr)

load("EachFileExports/10-02_FullDatasetWithReversals.rdata")
load("EachFileExports/07-02_AbortionVariablesReCategorized.rdata")

unconfirmed <- abs(log(abs(coroutsets$corlistimp)/abs(coroutsets$corlistdir)))<.10 | coroutsets$topcor<.05
checkthese <- avplus[avplus$UniqueID %in% coroutsets$UniqueID[is.na(unconfirmed) | unconfirmed],]


fg3 <- with(fg2, data.frame(StudyID, UniqueID, respID, dt, weight, Global="Global", Sex, AgeCategories, State, Region, Division, Education, RaceEthnicity, Partisanship, Religion, Religionmin, Religevang, Religfund, FreqAttend, BirthDecade, PresidentialApproval, majorclass2, bfsets5, fullclassification, majorclass, bigcats, wccat, out01rev, stdzoutrev, classdir, confirmclasses))


fgunif <- with(fg2, data.frame(StudyID, UniqueID, dt, majorclass2, bfsets5, fullclassification, majorclass, bigcats, wccat, classdir, confirmclasses, circumstances, pregnancyterm))

oneper <- fgunif[!duplicated(fg2$UniqueID),]

demoglist <- c("Global", "Sex", "AgeCategories", "State", "Region", "Division", "Education", "RaceEthnicity", "Partisanship", "Religion",  "Religionmin", "Religevang", "Religfund", "FreqAttend", "BirthDecade", "PresidentialApproval")

#globtab <- as.data.frame(xtabs(eval(weight*out01rev)~UniqueID, data=fg3))
#globtabstdz <- as.data.frame(xtabs(eval(weight*stdzoutrev)~UniqueID, data=fg3))
#globtabn <- as.data.frame(xtabs(weight~UniqueID, data=fg3))

#globtab$fg3...x. <- "Global"
#globtabstdz$fg3...x. <- "Global"
#globtabn$fg3...x. <- "Global"

tabberset <- lapply(demoglist, function(x) as.data.frame(t(xtabs(eval(weight*out01rev)~fg3[,x]+UniqueID, data=fg3))))
tabbersetstdz <- lapply(demoglist, function(x) as.data.frame(t(xtabs(eval(weight*stdzoutrev)~fg3[,x]+UniqueID, data=fg3))))
nerset <- lapply(demoglist, function(x) as.data.frame(t(xtabs(weight~fg3[,x]+UniqueID, data=fg3))))

tabbersetsex <- lapply(demoglist[demoglist!="Sex"], function(x) as.data.frame(xtabs(eval(weight*out01rev)~fg3[,x]+Sex+UniqueID, data=fg3)))
tabbersetstdzsex <- lapply(demoglist[demoglist!="Sex"], function(x) as.data.frame(xtabs(eval(weight*stdzoutrev)~fg3[,x]+Sex+UniqueID, data=fg3)))
nersetsex <- lapply(demoglist[demoglist!="Sex"], function(x) as.data.frame(xtabs(weight~fg3[,x]+Sex+UniqueID, data=fg3)))

tabbersetparty <- lapply(demoglist[demoglist!="Partisanship"], function(x) as.data.frame(xtabs(eval(weight*out01rev)~fg3[,x]+Partisanship+UniqueID, data=fg3)))
tabbersetstdzparty <- lapply(demoglist[demoglist!="Partisanship"], function(x) as.data.frame(xtabs(eval(weight*stdzoutrev)~fg3[,x]+Partisanship+UniqueID, data=fg3)))
nersetparty <- lapply(demoglist[demoglist!="Partisanship"], function(x) as.data.frame(xtabs(weight~fg3[,x]+Partisanship+UniqueID, data=fg3)))

tbs <- rbindlist(c(tabberset, tabbersetsex, tabbersetparty), use.names=TRUE, fill=TRUE)
tbsstdz <- rbindlist(c(tabbersetstdz, tabbersetstdzsex, tabbersetstdzparty), use.names=TRUE, fill=TRUE)
nbs <- rbindlist(c(nerset, nersetsex, nersetparty), use.names=TRUE, fill=TRUE)

tbsmin <- with(tbs, data.frame(UniqueID, Var=paste("v", gsub("-NA", "", gsub(" ", "_", paste(fg3...x., Sex, Partisanship, sep="-"))), sep=""), Freq))
tbsstdzmin <- with(tbsstdz, data.frame(UniqueID, Var=paste("v", gsub("-NA", "", gsub(" ", "_", paste(fg3...x., Sex, Partisanship, sep="-"))), sep=""), Freq))
nbsmin <- with(nbs, data.frame(UniqueID, Var=paste("v", gsub("-NA", "", gsub(" ", "_", paste(fg3...x., Sex, Partisanship, sep="-"))), sep=""), Freq))


tbs2 <- dcast(tbsmin, Var~UniqueID, sum, value.var="Freq")
tbs2stdz <- dcast(tbsstdzmin, formula=Var~UniqueID, sum, value.var="Freq")
nbs2 <- dcast(nbsmin, formula=Var~UniqueID, sum, value.var="Freq")

mnset <- tbs2[,-1]/nbs2[,-1]
mnsetstdz <- tbs2stdz[,-1]/nbs2[,-1]

mnsout <- as.data.frame(t(mnset))
mnsoutstdz <- as.data.frame(t(mnsetstdz))
mnswt <- as.data.frame(t(nbs2[,-1]))

colnames(mnsout) <- paste("Mean01", gsub(" ", "_", as.character(as.data.frame(tbs2)[,1])), sep="_")
colnames(mnsoutstdz) <- paste("MeanStdz", gsub(" ", "_", as.character(as.data.frame(tbs2)[,1])), sep="_")
colnames(mnswt) <- paste("Ns01", gsub(" ", "_", as.character(as.data.frame(tbs2)[,1])), sep="_")

mnsout$UniqueID <- rownames(mnsout)
mnsoutstdz$UniqueID <- rownames(mnsoutstdz)
mnswt$UniqueID <- rownames(mnswt)

jn1 <- join(oneper, mnsout)
jn2 <- join(jn1, mnsoutstdz)
jn3 <- join(jn2, mnswt)

justdts <- data.frame(dt=jn3$dt)

directionaldata <- jn3[jn3$classdir=="Directional" & !jn3$confirmclasses & !is.na(jn3$classdir),]
importancedata <- jn3[jn3$classdir=="Importance" & !jn3$confirmclasses & !is.na(jn3$classdir),]
importancedata$bfsets5 <- drop.levels(importancedata$bfsets5)
importancedata$wccat <- drop.levels(importancedata$wccat)
importancedata$circumstances <- drop.levels(importancedata$circumstances)
importancedata$pregnancyterm <- drop.levels(importancedata$pregnancyterm)

glob01preddir <- gamm4(Mean01_vGlobal~s(dt, k=20), random=~(1|bfsets5)+(1|wccat)+(1|circumstances)+(1|pregnancyterm), weights=sqrt(directionaldata$Ns01_vGlobal), data=directionaldata)
glob01predimp <- gamm4(Mean01_vGlobal~s(dt, k=10), random=~(1|bfsets5)+(1|wccat), weights=sqrt(importancedata$Ns01_vGlobal), data=importancedata)

justdts2 <- data.frame(dt=jn3$dt, bfsets5="Importance")


predictedglobdir <- predict(glob01preddir$gam, newdata=justdts)
mnshiftdir <- jn3$Mean01_vGlobal-predictedglobdir
predictedglobimp <- predict(glob01predimp$gam, newdata=justdts2)
mnshiftimp <- jn3$Mean01_vGlobal-predictedglobimp

mnshift <- mnshiftdir
mnshift[jn3$classdir=="Importance" & !is.na(jn3$classdir)] <- mnshiftimp[jn3$classdir=="Importance" & !is.na(jn3$classdir)]

jn4 <- apply(jn3[,grepl("Mean01", colnames(jn3))], 2, function(x) x-mnshift)
colnames(jn4) <- gsub("Mean01", "AdjMn01", colnames(jn4))

fd <- cbind(jn3, jn4)

fdDemset <- grepl("Democrat$", colnames(fd)) & !grepl("vDemocrat$", colnames(fd))
fdRepset <- grepl("Republican$", colnames(fd)) & !grepl("vRepublican$", colnames(fd))

fdFemaleset <- grepl("Female$", colnames(fd)) & !grepl("vFemale$", colnames(fd))
fdMaleset <- grepl("Male$", colnames(fd)) & !grepl("vMale$", colnames(fd))

partydifs <- fd[,fdDemset]-fd[,fdRepset]
partydifNs <- sqrt(fd[,fdDemset]^2+fd[,fdRepset]^2)
partydifs[grepl("Ns01", colnames(partydifs))] <- partydifNs[grepl("Ns01", colnames(partydifs))]
colnames(partydifs) <- gsub("Democrat", "PartyDifs", colnames(partydifs))

sexdifs <- fd[,fdFemaleset]-fd[,fdMaleset]
sexdifNs <- sqrt(fd[,fdFemaleset]^2+fd[,fdMaleset]^2)
sexdifs[grepl("Ns01", colnames(sexdifs))] <- sexdifNs[grepl("Ns01", colnames(sexdifs))]
colnames(sexdifs) <- gsub("Female", "SexDifs", colnames(sexdifs))

dm <- cbind(fd, partydifs, sexdifs)

demogsets <- sapply(demoglist, function(x) gsub(" ", "_", levels(fg3[,x])))

save(dm, demogsets, file="EachFileExports/12-01_DemographicsByDataset.rdata")

plot(dm$dt[dm$classdir=="Directional"], dm$Mean01_vGlobal[dm$classdir=="Directional"], col="blue")

plot(dm$dt[dm$classdir=="Directional"], dm$AdjMn01_vGlobal[dm$classdir=="Directional"], col="blue")


plot(dm$dt[dm$classdir=="Directional"], dm$AdjMn01_vMale[dm$classdir=="Directional"], col="blue")
lines(dm$dt[dm$classdir=="Directional"], dm$AdjMn01_vFemale[dm$classdir=="Directional"], col="red", type="p")


plot(dm$dt[dm$classdir=="Directional"], dm$AdjMn01_vSexDifs[dm$classdir=="Directional"], col="red", type="p")
plot(dm$dt[dm$classdir=="Directional"], dm$MeanStdz_vGlobal-SexDifs[dm$classdir=="Directional"], col="red", type="p")


plot(dm$dt[dm$classdir=="Directional"], dm$MeanStdz_vSexDifs[dm$classdir=="Directional"], col="red", type="p", pch=20, cex=sqrt(dm$Ns01_vSexDifs)/25)
