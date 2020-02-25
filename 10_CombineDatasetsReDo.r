library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(Amelia)
library(MASS)

load("EachFileExports/04-01_MergedDataFiles.rdata")
#load("EachFileExports/07-01_AbortionVariablesCategorized.rdata")
load("EachFileExports/07-02_AbortionVariablesReCategorized.rdata")
load("EachFileExports/08-01_CovariatesCategorized.rdata")
load("EachFileExports/09-01_ImputationPreds.rdata")


# Clean up and add on imputed data
impvers <- primaryoutcomeimps$imputations$imp1
names(impvers) <- paste("imp", names(impvers), sep="")

impvers$impAge[impvers$impAge<18] <- 18
impvers$impAge[impvers$impAge>99] <- 99

impversmin <- with(impvers, data.frame(respID=imprespID, impSex, impAge, impDivision, impEducation, impRaceEthnicity, impPartisanship, impReligion, impReligfund, impFreqAttend))

gpdimp <- merge(gpd, impversmin, all.x=TRUE)

gpdcutpre <- lapply(usablestudies, function(x) gpdimp[gpdimp$StudyID==x,])
names(gpdcutpre) <- usablestudies

gpdcut <- gpdcutpre[sapply(gpdcutpre, dim)[1,]>0]
usablestud2 <- usablestudies[sapply(gpdcutpre, dim)[1,]>0]

# Limit to data that is available that is either directional or strength measure

avus <- avplus[(avplus$StudyID %in% names(outcomedatasets)) & avplus$datatype!="Topline" & !(avplus$bfsets5 %in% c("Uncategorized", "Excluded")),]

outcomemasterlistpre1 <- apply(avus, 1, function(n) try(data.frame(outcome=outcomedatasets[[as.character(n["StudyID"])]][,as.character(n["Varname"])], gpdcut[[as.character(n["StudyID"])]], UniqueID=paste(n["StudyID"], n["Varname"], sep="~"))))
names(outcomemasterlistpre1) <- avus$UniqueID

# These Ones Didn't Work Properly
write.csv(avus[sapply(outcomemasterlistpre1, function(x) class(x)[1]=="try-error"),], file="Troubleshooting/10-01_ImproperImports.csv")



outcomemasterlist <- outcomemasterlistpre1[!sapply(outcomemasterlistpre1, function(x) class(x)[1]=="try-error")]

avds <- avus[!sapply(outcomemasterlistpre1, function(x) class(x)[1]=="try-error"),]

abortvarwpcts <- lapply(outcomemasterlist, function(x) wpct(x[,1], x$weight))

# Figure Out What Answers Rarely Get Chosen

avwp <- abortvarwpcts[sapply(abortvarwpcts, length)<20]

avnom <- unlist(sapply(avwp, names))
avval <- unlist(avwp)

nomvecs <- lapply(unique(avnom), function(x) avval[avnom==x])
names(nomvecs) <- unique(avnom)

nvo <- nomvecs[order(sapply(nomvecs, length))]

nomquantiles <- sapply(nvo, function(x) quantile(x, .5))

likelydrops <- names(nvo)[nomquantiles<.05] # This is the proportion of responses that we consider reasonable on average



keeprespers <- lapply(abortvarwpcts, function(x) x[!(names(x) %in% likelydrops)])

# Limit to Questions For Which >80% give a moderate probability answer (of 5% or more)
krsets <- keeprespers[sapply(keeprespers, sum)>.8]
krnoms <- lapply(krsets, function(x) sort(names(x)))

nombinds <- sapply(krnoms, function(x) paste(x, collapse="; "))

ordinalitytrainer1 <- rev(sort(table(nombinds)))#[table(nombinds)>5]
ordstrsplit <- sapply(names(ordinalitytrainer1), function(x) try(strsplit(x, "; ", fixed=TRUE)))
ordstrcomb <- t(sapply(ordstrsplit[sapply(ordstrsplit, length)>2 & sapply(ordstrsplit, length)<21], function(x) c(x, rep("", 20-length(x)))))
colnames(ordstrcomb) <- paste("Opt", 1:20, sep="")
ordinalityshow <- data.frame(ordinalitytrainer1[sapply(ordstrsplit, length)>2 & sapply(ordstrsplit, length)<21], ordstrcomb)


twocatvars <- nombinds[nombinds %in% names(ordinalitytrainer1[sapply(ordstrsplit, length)==2])]
tcvordinal <- strsplit(twocatvars, "; ", fixed=TRUE)
names(tcvordinal) <- twocatvars
tcvordinalmin <- tcvordinal[!duplicated(tcvordinal)]
tcvcheckpre <- t(sapply(tcvordinalmin, function(x) (x)))
tcvcheck <- data.frame(nombinds=names(tcvordinalmin), tcvcheckpre)

write.csv(ordinalityshow, "Fixordinality/TrainingPossibilities.csv")
write.csv(tcvcheck[ordered(tcvcheck)], "Fixordinality/Check2Cats.csv")

## Trained - Responses With Known Ordinality and Direction (>2 response options)
## UnknownDir - Responses With Known Ordinality and Unknown Direction (>2 response options)

trained <- read.csv("Fixordinality/Trained.csv", stringsAsFactors=FALSE)
unknowndir <- read.csv("Fixordinality/OrderedDirectionUnknown.csv", stringsAsFactors=FALSE)
twocats <- read.csv("Fixordinality/Use2Cats.csv", stringsAsFactors=FALSE)

tns <- nombinds[nombinds %in% trained$nombinds]
udns <- nombinds[nombinds %in% unknowndir$nombinds]
tcns <- nombinds[nombinds %in% twocats$nombinds]
uncategorized <- rev(sort(table(nombinds[!(nombinds %in% c(tns, udns, tcns))])))

write.csv(uncategorized, "Troubleshooting/10-02_CurrentlyUncategorizedResponseOptions.csv")

write.csv(ordinalityshow[ordinalityshow$nombinds %in% names(uncategorized),], "Fixordinality/Untrained.csv")


# Build Global Dataset

nombindmerge <- data.frame(UniqueID=names(nombinds), nombinds=nombinds)
avjoin <- merge(avus, nombindmerge, by="UniqueID", all.x=TRUE)

fullcombinedglobalpre <- rbindlist(outcomemasterlist)
fullcombinedglobal <- merge(fullcombinedglobalpre[,-c("StudyID", "sample", "mode")], avjoin, by="UniqueID", all.x=TRUE)

fullcombinedglobal$dt <- as.numeric(fullcombinedglobal$date)/20000
fullcombinedglobal$dt2 <- fullcombinedglobal$dt^2
fullcombinedglobal$dt3 <- fullcombinedglobal$dt^3
fullcombinedglobal$dt4 <- fullcombinedglobal$dt^4
fullcombinedglobal$dt5 <- fullcombinedglobal$dt^5
fullcombinedglobal$impagesq <- fullcombinedglobal$impAge*fullcombinedglobal$impAge

TrainSet <- fullcombinedglobal$nombinds %in% trained$nombinds
NoDirection <- fullcombinedglobal$nombinds %in% c(udns, twocatvars)

fullcombinedglobal$trainstatus <- NA
fullcombinedglobal$trainstatus[NoDirection] <- "NoDirection"
fullcombinedglobal$trainstatus[TrainSet] <- "Training Set"
fullcombinedglobal$trainstatus <- as.factor(fullcombinedglobal$trainstatus)

trainedordinal <- apply(trained[,grepl("Opt", names(trained))], 1, function(x) as.character(x[!is.na(x) & x!=""]))
nondirordinal <- apply(unknowndir[,grepl("Opt", names(unknowndir))], 1, function(x) as.character(x[!is.na(x) & x!=""]))
tcvordinal <- lapply(1:(dim(twocats)[1]), function(x) as.character(twocats[x,grepl("Opt", names(twocats))]))

ordnames <- c(as.character(trained$nombinds), as.character(unknowndir$nombinds), as.character(twocats$nombinds))

ordinalsetpre <- c(trainedordinal, nondirordinal, tcvordinal)
ordinalset <- ordinalsetpre[!duplicated(ordnames)]
onommer <- ordnames[!duplicated(ordnames)]

names(ordinalset) <- onommer

osmerge <- lapply(1:length(ordinalset), function(x) data.frame(nombinds=names(ordinalset)[x], outcome=ordinalset[[x]], out01=nalevs(ordered(ordinalset[[x]], levels=ordinalset[[x]]))))
osm <- rbindlist(osmerge)

fcg <- merge(fullcombinedglobal, osm, by=c("nombinds", "outcome"), all.x=TRUE)

fg <- fcg[!is.na(fcg$trainstatus),]

save(fcg, fg, file="EachFileExports/10-01_IndividualMasterPreReversalsB.rdata")
