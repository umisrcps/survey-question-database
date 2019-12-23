## New Analysis File As Of 5/24/19 ##

# Load Necessary Libraries #

library(foreign)
library(reshape)
library(weights)
library(asciiSetupReader)

# Identify All Current Data Files #

allfiles <- dir("../Data/", recursive = TRUE, include.dirs = TRUE)
afpre <- allfiles[!grepl("donotread", allfiles)]
af <- afpre[!grepl("Thumbs.db", afpre)]
nondirs <- dir("../Data/", recursive = TRUE, include.dirs = FALSE)
ndspre <- nondirs[!grepl("donotread", nondirs)]
nds <- ndspre[!grepl("Thumbs.db", ndspre)]
dirs <- af[!(af %in% nds)]

filesplitspre <- strsplit(dirs, "/", fixed=TRUE)
filesplits <- sapply(filesplitspre, function(x) x[length(x)])
#rm(list=c("filesplitspre", "allfiles", "afpre", "nondirs", "ndspre"))

## FINDS ANY DUPLICATED DIRECTORY NAMES AND RENAMES THEM BY ADDING PRIOR FOLDER NAME IN FRONT ##

duplicatedirectorynames <- try(names(table(filesplits))[table(filesplits)>1])

if(class(duplicatedirectorynames)!="try-error"){
    newnamers <- filesplitspre[filesplits %in% duplicatedirectorynames]
    newfinalnames <- sapply(newnamers, function(x) paste(paste(x[-length(x)], collapse="/"), "/", x[length(x)-1], "_", x[length(x)], sep=""))
}

#olddirnames <- dirs[filesplits %in% duplicatedirectorynames]
#newdirnames <- newfinalnames

#sapply(1:length(olddirnames), function(i) file.rename(paste("../Data/", olddirnames[i], sep=""), paste("../Data/", newdirnames[i], sep="")))

## FIXES ERRORS WHERE FILES LABELLED SPSS.PORTABLE DO NOT RUN BY RELABELING TO SPSS.POR

sapply(allfiles[grepl("spss.portable", allfiles)], function(i) file.rename(paste("../Data/", i, sep=""), gsub("spss.portable", "spss.por", paste("../Data/", i, sep=""))))

#allfiles[grepl("spss.portable", allfiles)]


## RERUNS IDENTIFICATION OF ALL CURRENT DATA FILES AFTER DUPLICATES REMOVED ##

allfiles <- dir("../Data/", recursive = TRUE, include.dirs = TRUE)
afpre <- allfiles[!grepl("donotread", allfiles)]
af <- afpre[!grepl("Thumbs.db", afpre)]
nondirs <- dir("../Data/", recursive = TRUE, include.dirs = FALSE)
ndspre <- nondirs[!grepl("donotread", nondirs)]
nds <- ndspre[!grepl("Thumbs.db", ndspre)]
dirs <- af[!(af %in% nds)]

filesplitspre <- strsplit(dirs, "/", fixed=TRUE)
filesplits <- sapply(filesplitspre, function(x) x[length(x)])


nondircheck <- strsplit(nds, "/", fixed=TRUE)
studynames <- sapply(nondircheck, function(x) x[length(x)-1])

uniquestudiespre <- unlist(unique(studynames)[sapply(unique(studynames), length)>0])
uniquestudies <- uniquestudiespre[!uniquestudiespre %in% duplicatedirectorynames]

dirsets <- lapply(uniquestudies, function(i) dirs[grepl(paste("/", i, "/", sep=""), paste("/", dirs, "/", sep=""))])

## Classify Files By Type and Study

studyfiletypes <- tolower(sapply(strsplit(nds, ".", fixed=TRUE), function(x) x[length(x)]))
studyfolder <- sapply(strsplit(nds, "/"), function(x) paste(x[-length(x)], collapse="/"))

datafiles <- studyfiletypes %in% c("fwf", "dat", "data", "sav", "por", "portable", "rdata", "tab", "rda", "sps", "dta")
ropertoplines <- grepl("report.csv", tolower(nds), fixed=TRUE)
parserfile <- grepl("parse", tolower(nds), fixed=TRUE)

studyhasdata <- sapply(unique(studyfolder), function(x) sum(datafiles[studyfolder==x]))
studyhastopline <- sapply(unique(studyfolder), function(x) sum(ropertoplines[studyfolder==x]))
names(studyhasdata) <- names(studyhastopline) <-unique(studyfolder)

eachfileinfo <- data.frame(file=nds, folder=studyfolder, type=studyfiletypes, topline=ropertoplines, parser=parserfile, isdatafile=datafiles)

## Generate Estimates at the Study/Folder Level

eachfolderinfopre <- t(sapply(unique(eachfileinfo$folder), function(x) c(totalfiles=sum(eachfileinfo$folder==x), table(eachfileinfo$type[eachfileinfo$folder==x]), hastopline=sum(eachfileinfo$topline[eachfileinfo$folder==x])>0, hasparser=sum(eachfileinfo$parser[eachfileinfo$folder==x])>0, hasdatafile=sum(eachfileinfo$isdatafile[eachfileinfo$folder==x])>0)))

eachfolderinfo <- eachfolderinfopre[,colSums(eachfolderinfopre)>3]
rownames(eachfolderinfo) <- as.character(unique(eachfileinfo$folder))
 
summaryinfo <- c("Total", colSums(eachfolderinfo))

write.csv(rbind(summaryinfo, cbind(unique(eachfileinfo$folder), eachfolderinfo)), "SummaryInformation/StudyFiles.csv")


eachfolderinfo <- as.data.frame(eachfolderinfo)

eachfolderinfo$StudyID <- paste("ds", tolower(sapply(strsplit(rownames(eachfolderinfo), "/", fixed=TRUE), function(x) x[length(x)])), sep="_")
eachfileinfo$StudyID <- paste("ds",tolower(sapply(strsplit(as.character(eachfileinfo$folder), "/", fixed=TRUE), function(x) x[length(x)])), sep="_")


save(eachfileinfo, eachfolderinfo, file="EachFileExports/01_FileInformation.rdata")




