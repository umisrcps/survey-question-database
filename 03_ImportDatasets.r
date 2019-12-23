## New Analysis File As Of 5/24/19 ##

# Load Necessary Libraries #

library(foreign)
library(reshape)
library(weights)
library(data.table)
library(asciiSetupReader)
library(memisc)
library(haven)

# Load File Information
load("EachFileExports/02-01_ParseFiles.rdata")
load("EachFileExports/02-02_ToplineFiles.rdata")
load("EachFileExports/02-03_MetaInformation.rdata")

alldatafiles <- eachfileinfo[eachfileinfo$isdatafile,]

# Import Rdata Files

rdafiles <- alldatafiles[alldatafiles$type %in% c("rda", "rdata"),]

rdacomb <- as.list(rep(NA, length(rdafiles$file)))
names(rdacomb) <- as.character(rdafiles$file)
for(i in as.character(rdafiles$file)){
    currfiles <- c(ls(), "currfiles")
    load(paste("../Data/", i, sep=""))
    newfiles <- ls()[!(ls() %in% currfiles)]
    if(length(newfiles)==1)
        eval(parse(text=paste("rdacomb[[i]] <-", newfiles)))
    if(length(newfiles)>1)
        eval(parse(text=paste("rdacomb[[i]] <- list(", paste(newfiles, collapse=", "), ")", sep="")))
    rm(newfiles)
}
names(rdacomb) <- as.character(rdafiles$StudyID)

save(rdacomb, file="EachFileExports/03-01_RDAFiles.rdata")

# Import SPS Files and Run on Datasets

spsfiles <- alldatafiles[alldatafiles$type=="sps",]
txtforspspre <- eachfileinfo[eachfileinfo$type=="txt" & (eachfileinfo$folder %in% spsfiles$folder),]
txtforsps <- txtforspspre[!grepl("codebook|manifest|literature|readme", tolower(txtforspspre$file)),]

tfsmin <- with(txtforsps, data.frame(StudyID=StudyID, folder=as.character(folder), textfile=as.character(file), stringsAsFactors=FALSE))
spsmin <- with(spsfiles, data.frame(folder=as.character(folder), spsfile=as.character(file)))

spsimportsets <- merge(tfsmin, spsmin)
spsimportsets$spsfile <- as.character(spsimportsets$spsfile)

spsimports <- lapply(1:dim(spsimportsets)[1], function(x) try(spss_ascii_reader(paste("../Data/", spsimportsets[x,]$textfile, sep=""), paste("../Data/", spsimportsets[x,]$spsfile, sep="")), silent=TRUE))

spsimps <- spsimports[sapply(spsimports, class)!="try-error"]
names(spsimps) <- spsimportsets$StudyID[sapply(spsimports, class)!="try-error"]

spsparserspre <- lapply(spsfiles$file, function(x) try(parse_setup(paste("../Data/", x, sep="")), silent=TRUE))

spsparsers <- spsparserspre[sapply(spsparserspre, class)!="try-error"]
names(spsparsers) <- spsfiles$StudyID[sapply(spsparserspre, class)!="try-error"]

write.csv(spsimportsets[sapply(spsimports, class)=="try-error",], "Troubleshooting/03-01_runthesespsfilesmanually.csv")
write.csv(spsfiles[sapply(spsparserspre, class)=="try-error",], "Troubleshooting/03-02_spsfilesnotrunning.csv")

save(spsimps, spsparsers, file="EachFileExports/03-02_SPSFiles.rdata")

# Import SPSS and Portable Files

spssfiles <- alldatafiles[alldatafiles$type %in% c("sav", "portable", "por"),]

newspssimports <- lapply(spssfiles$file, function(x) try(read_spss(paste("../Data/", x, sep="")), silent=TRUE))

names(newspssimports) <- spssfiles$StudyID

spssnotimported <- spssfiles[sapply(newspssimports, class)=="try-error",]

spssimps <- newspssimports[sapply(newspssimports, class)!="try-error"]

newspssimports2 <- lapply(spssnotimported$file, function(x) try(read.spss(paste("../Data/", x, sep=""), duplicated.value.labels="condense", max.value.labels=60, to.data.frame=TRUE), silent=TRUE))

newspssimports2alt <- lapply(spssnotimported$file, function(x) try(read.spss(paste("../Data/", x, sep="")), silent=TRUE))
spssattributespre <- lapply(newspssimports2alt, function(x) attributes(x))

spssimps2 <- newspssimports2[sapply(newspssimports2, class)!="try-error"]
spssattributes2 <- spssattributespre[sapply(newspssimports2, class)!="try-error"]
names(spssimps2) <- names(spssattributes2) <- spssnotimported$StudyID[sapply(newspssimports2, class)!="try-error"]

spssstillnotimported <- spssnotimported[sapply(newspssimports2, class)=="try-error",]

spssmemiscs <- lapply(spssstillnotimported$file, function(x) try(spss.portable.file(paste("../Data/", x, sep="")), silent=TRUE))

spssmemiscimps <- lapply(spssmemiscs[sapply(spssmemiscs, class)!="try-error"], function(x) as.data.set(x))
names(spssmemiscimps) <- spssstillnotimported$StudyID[sapply(spssmemiscs, class)!="try-error"]

spssimportfailures <- spssstillnotimported[sapply(spssmemiscimps, class)=="try-error",]

#spssimports <- lapply(spssfiles$file, function(x) try(read.spss(paste("../Data/", x, sep=""), duplicated.value.labels="condense", max.value.labels=60, to.data.frame=TRUE), silent=TRUE))

#spssimportsalt <- lapply(spssfiles$file, function(x) try(read.spss(paste("../Data/", x, sep="")), silent=TRUE))
#spssattributespre <- lapply(spssimportsalt, function(x) attributes(x))

#spssimps <- spssimports[sapply(spssimports, class)!="try-error"]
#spssattributes <- spssattributespre[sapply(spssimports, class)!="try-error"]
#names(spssimps) <- names(spssattributes) <- spssfiles$StudyID[sapply(spssimports, class)!="try-error"]

#spssnotimported <- spssfiles[sapply(spssimports, class)=="try-error",]

#spssmemiscs <- lapply(spssnotimported$file, function(x) try(spss.portable.file(paste("../Data/", x, sep="")), silent=TRUE))

#spssmemisc <- spssmemiscs[sapply(spssmemiscs, class)!="try-error"]

#spssmemiscimps <- lapply(spssmemisc, function(x) as.data.set(x))
#names(spssmemiscimps) <- spssnotimported$StudyID[sapply(spssmemiscs, class)!="try-error"]

#spssremaining <- spssnotimported[sapply(spssmemiscs, class)=="try-error",]

write.csv(spssimportfailures, "Troubleshooting/03-03_spssfilesnotimported.csv")

save(spssimps, spssimps2, spssattributes2, spssmemiscimps, file="EachFileExports/03-03_SPSSfiles.rdata")


# Import Stata Files

statafiles <- alldatafiles[alldatafiles$type=="dta",]

stataimports <- lapply(statafiles$file, function(x) try(read.dta(paste("../Data/", x, sep="")), silent=TRUE))
names(stataimports) <- statafiles$StudyID
#write.csv(statafiles$file[sapply(stataimports, class)=="try-error",], "Troubleshooting/03-04_statafilesnotimported.csv")

save(stataimports, file="EachFileExports/03-04_Statafiles.rdata")

# Import Tab Files

tabfiles <- alldatafiles[alldatafiles$type=="tab",]

tabimports <- lapply(tabfiles$file, function(x) try(read.csv(paste("../Data/", x, sep=""), sep="\t"), silent=TRUE))

names(tabimports) <- tabfiles$StudyID
#write.csv(tabfiles$file[sapply(tabimports, class)=="try-error",], "Troubleshooting/03-05_tabdelimfilesnotimported.csv")

save(tabimports, file="EachFileExports/03-05_TabDelimfiles.rdata")


# Import Parsed Roper Files

parsefile <- function(parser, datafile){
  parser <- parser[order(parser$Starting),]
  col_positions = parser$Starting
  widths = parser$Lengthwidth
  parser$qs = paste("Q", parser$Question.number, sep="")
  filepath = datafile
  card_read = parser$Card
  cards = parser$Total.cards[1]
  parser <- parser[!is.na(parser$Question.number) & parser$Question.number!="",]
  parser$End[is.na(parser$End)] <- parser$Lengthwidth[is.na(parser$End)]+parser$Starting[is.na(parser$End)]-1
  if(is.null(cards)){
    cards <- 1
    card_read <- rep(1, length(parser$qs))
  }
  if(is.na(cards) | cards==""){
    cards <- 1
    card_read <- rep(1, length(parser$qs))
  }
  out <- NULL
  for(i in sort(unique(card_read))){
    pi <- parser[card_read==i & !is.na(card_read),]
    start <- c(0, pi$Starting)
    ends <- c(pi$Starting[1]-1, pi$End)
    widths <- ends-start+1
    gaps <- start[-1]-start[-length(start)]
    extras <- widths[-length(start)]-gaps
    cardselect <- rep(FALSE, cards)
    cardselect[i] <- TRUE
    finwidths <- c(as.vector(rbind(widths[-length(widths)], extras)), widths[length(widths)])
    fw <- finwidths[finwidths!=0 & !is.na(finwidths)]
    fw[1] <- -fw[1]+1
    if(fw[1]==0)
      fw <- fw[-1]
    datapre <- try(read.fwf(file=datafile, widths=fw, col.names=pi$qs))
    if(class(datapre)=="try-error"){
        datapre <- read.fwf(file=datafile, widths=fw, col.names=pi$qs, fileEncoding="latin1")
    }
    data <- as.data.frame(datapre[rep(cardselect, dim(datapre)[1]/cards),])
    colnames(data) <- colnames(datapre)
    if(!is.null(out)){
        if(dim(out)[1]==(dim(data)[1]+1))
            out <- cbind(out[1:dim(data),], data)
        else
            out <- cbind(out, data)
        }
    if(is.null(out))
      out <- data
  }
  out
}

datfiles <- alldatafiles[alldatafiles$type %in% c("dat", "data"),]

dtf <- with(datfiles, data.frame(dat=file, StudyID))
psf <- with(parsers, data.frame(parser=file, StudyID))

dtcomb <- merge(dtf, psf)
dtcomb$StudyID <- as.character(dtcomb$StudyID)

needsparser <- datfiles[!(datfiles$StudyID %in% dtcomb$StudyID),]
write.csv(needsparser, "Troubleshooting/03-06_datafileswithoutparsers.csv")

parseddata <- lapply(dtcomb$StudyID, function(x) try(parsefile(importparsers[[x]], paste("../Data/", dtcomb$dat[dtcomb$StudyID==x], sep="")), silent=TRUE))
names(parseddata) <- dtcomb$StudyID

names(importparsers)[!(names(importparsers) %in% dtcomb$StudyID)]
dtcomb$StudyID[!(dtcomb$StudyID %in% names(importparsers))]

datimports <- parseddata[sapply(parseddata, class)=="data.frame"]

parsingfailures <- dtcomb[!(dtcomb$StudyID %in% names(parseddata)[sapply(parseddata, class)=="data.frame"]),]

notparsederrors <- parseddata[sapply(parseddata, class)!="data.frame"]
#parserswitherrors <- importparsers[sapply(parseddata, class)!="data.frame"]

#notparsederrors[[37]][1]
#notparsederrors[[11]][1]
#parser <- parserswitherrors[[37]]
#datafile <- paste("../Data/", datfiles$file[datfiles$StudyID==names(parserswitherrors)[37]], sep="")

#parsefile(parser, datafile)

parsingfailures$reason <- NA
parsingfailures$reason[sapply(notparsederrors, function(x) is.null(x))] <- "Parser File Missing Key Data"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("Error in read.table", x[1])))==TRUE] <- "Data File Mismatched"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("Error in file", x[1])))==TRUE] <- "Multiple Data Files"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("Error in rep_len", x[1])))==TRUE] <- "Check Parser File for Location Typos or Lack of Card Number - Type 1"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("Error in scan", x[1])))==TRUE] <- "Check Parser File for Location Typos or Lack of Card Number - Type 2"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("missing value where TRUE/FALSE needed", x[1])))==TRUE] <- "Check Parser File for Location Typos or Lack of Card Number - Type 3"
parsingfailures$reason[sapply(notparsederrors, function(x) try(grepl("arguments imply differing number of rows", x[1])))==TRUE] <- "Probably the Wrong Number of Cards - Check for Translation Matrix At Beginning"


write.csv(needsparser, "Troubleshooting/03-06_datafileswithoutparsers.csv")
write.csv(parsingfailures, "Troubleshooting/03-07_datafileswithparsingfailures.csv")


save(datimports, file="EachFileExports/03-06_FixedWidthParsedfiles.rdata")

