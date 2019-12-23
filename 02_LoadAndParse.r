## New Analysis File As Of 5/24/19 ##

# Load Necessary Libraries #

library(foreign)
library(reshape)
library(weights)
library(data.table)
library(asciiSetupReader)

# Load File Information

load("EachFileExports/01_FileInformation.rdata")

# Import Metadata Files

keywordfiles <- read.csv("../Track/keyword_abortion_DATASETS_1.17.18.csv")
topicfiles <- read.csv("../Track/topic_abortion_QUESTIONS_1.17.18.csv")
addmeta <- read.csv("../Analysis/Metadata/additionalmetadata.csv", header=TRUE)

kwmeta <- keywordfiles[keywordfiles$COUNTRY_NAME=="United States",]
kwmeta$StudyID <- paste("ds", tolower(kwmeta$ARCHNO), sep="_")

topicfiles$StudyID <- paste("ds", tolower(topicfiles$ARCHNO), sep="_")
topicfiles$StudyID[topicfiles$ARCHNO==""] <-  paste("ds", tolower(topicfiles$STDY_ID[topicfiles$ARCHNO==""]), sep="_")

addmeta$StudyID <- paste("ds", tolower(addmeta$Subfolder.name), sep="_")

# Get Metadata from Toplines and Parsers

toplinefiles <- eachfileinfo[eachfileinfo$topline,]
parsers <- eachfileinfo[eachfileinfo$parser,]

importparsers <- lapply(parsers$file, function(x) try(read.csv(paste("../Data/", x, sep=""))))
names(importparsers) <- parsers$StudyID

importtoplines <- lapply(toplinefiles$file, function(x) try(read.csv(paste("../Data/", x, sep=""))))
names(importtoplines) <- toplinefiles$StudyID

parsermeta <- rbindlist(lapply(importparsers[!sapply(importparsers, class)=="try-error"], function(x) x[1,]), fill=TRUE)
toplinemeta <- rbindlist(lapply(importtoplines[!sapply(importtoplines, class)=="try-error"], function(x) x[1,]), fill=TRUE)


#parsers[parsers$StudyID %in% names(table(names(importparsers))[table(names(importparsers))>1]),]

rownames(parsermeta) <- names(importparsers)[!sapply(importparsers, class)=="try-error"]
rownames(toplinemeta) <- names(importtoplines)[!sapply(importtoplines, class)=="try-error"]

parsermeta$StudyID <- paste("ds",tolower(sapply(strsplit(rownames(parsermeta), "/", fixed=TRUE), function(x) x[length(x)-1])), sep="_")
toplinemeta$StudyID <- paste("ds",tolower(sapply(strsplit(rownames(toplinemeta), "/", fixed=TRUE), function(x) x[length(x)-1])), sep="_")

save(importparsers, parsermeta, parsers, file="EachFileExports/02-01_ParseFiles.rdata")
save(importtoplines, toplinemeta, toplinefiles, file="EachFileExports/02-02_ToplineFiles.rdata")

write.csv(names(importparsers)[sapply(importparsers, class)=="try-error"], "Troubleshooting/02-01_Unable to Use These Parser Files.csv")
write.csv(names(importtoplines)[sapply(importtoplines, class)=="try-error"], "Troubleshooting/02-02_Unable to Use These Topline Files.csv")

generateUniqueIDs <- unlist(sapply(names(importparsers), function(x) paste(x, importparsers[[x]]$Question.number, sep="~")))

write.csv(table(generateUniqueIDs)[table(generateUniqueIDs)>1], "Troubleshooting/02-03_DuplicateQuestionNumbersInParserFile.csv")

table(eachfolderinfo$StudyID %in% c(parsermeta$StudyID, toplinemeta$StudyID, kwmeta$StudyID, topicfiles$StudyID, addmeta$StudyID))

nometainfo <- eachfolderinfo$StudyID[eachfolderinfo$hasdatafile & !(eachfolderinfo$StudyID %in% c(parsermeta$StudyID, toplinemeta$StudyID, kwmeta$StudyID, topicfiles$StudyID, addmeta$StudyID))]

write.csv(nometainfo, "Troubleshooting/02-04_Studies With No Metadata.csv")

## Clean and Merge Metadata

pmdate <- function(x){
    d1 <- strsplit(as.character(x), "/", fixed=TRUE)
    d3 <- sapply(d1, function(x) x[3])
    d3n <- sapply(d3, nchar)
    dtpre <- sapply(d1, function(x) paste(x[3], x[1], x[2], sep="-"))
    dtpre[d3n==2 & d3>30 & !is.na(d3)] <- paste("19", dtpre[d3n==2 & d3>30 & !is.na(d3)], sep="")
    dtpre[d3n==2 & d3<=30 & !is.na(d3)] <- paste("20", dtpre[d3n==2 & d3<=30 & !is.na(d3)], sep="")
    dt <- as.Date(dtpre)
    dt
}

dateset <- function(x){
  monthers <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  x <- x[x!=""]
  endyear <- as.numeric(x[length(x)])
  endyear[endyear==34] <- NA
  eye <- endyear
  endyear[endyear<1000 & !is.na(endyear)] <- endyear[endyear<1000 & !is.na(endyear)]+1900
  endyear[endyear<1930 & !is.na(endyear)] <- endyear[endyear<1930 & !is.na(endyear)]+100
  endyear[endyear>2030& !is.na(endyear)] <- NA
  x <- x[-length(x)]
  months <- x[x %in% monthers]
  endmonth <- months[length(months)]
  startmonth <- months[1]
  x <- x[!(x %in% monthers)]
  startyear <- NA
  if(length(x)>2){
    yearpossible <- x[2:(length(x)-1)]
    if((eye-1) %in% yearpossible)
      startyear <- eye-1
    startyear[startyear==34] <- NA
    startyear[startyear<1000 & !is.na(startyear)] <- startyear[startyear<1000 & !is.na(startyear)]+1900
    startyear[startyear<1930 & !is.na(startyear)] <- startyear[startyear<1930 & !is.na(startyear)]+100
    startyear[startyear>2030& !is.na(startyear)] <- NA
    x <- x[c(1,length(x))]
  }
  if(is.na(startyear))
    startyear <- endyear
  if(length(x)>0){
    startday <- x[1]
    endday <- x[length(x)]
  }
  if(length(x)==0){
    startday <- 1
    endday <- 28
  }
  startdate <- as.Date(paste(startmonth, startday, startyear), format="%b %d %Y")
  enddate <- as.Date(paste(endmonth, endday, endyear), format="%b %d %Y")
  c(startdate, enddate)
}


pmpre <- parsermeta[!duplicated(parsermeta$StudyID), c("StudyID", "Country", "Survey.organization", "Sponsor", "Start.date", "End.date", "Sample", "Sample.size")]
tmpre <- toplinemeta[!duplicated(toplinemeta$StudyID), c("StudyID", "SurveyOrg", "SurveySponsor", "BegDate", "EndDate", "ExactDates", "SampleDesc", "SampleSize", "IntMethod", "StudyNote")]
kmpre <- kwmeta[!duplicated(kwmeta$StudyID),]
tfmpre <- topicfiles[!duplicated(topicfiles$StudyID), c("StudyID", "STDY_ID", "RPR_STDY_ID", "CHG_DATE", "BEG_DATE_TP", "END_DATE_DATE", "BEG_DATE_DATE", "SAMPLE_DESC", "SAMPLE_SIZE", "RLSE_DATE_DATE", "COPY_DATE", "SURVEY_ORG", "SURVEY_SPONSOR", "SOURCE_DOC", "STITLE", "STUDYDATE", "STUDY_SDA_STATUS", "S_REXPSTATUS", "DECADE")]
ampre <- addmeta[!duplicated(addmeta$StudyID), c("StudyID", "Survey.firm", "Start.date", "End.date", "Interview.method", "Sample", "Notes", "Include")]



pm <- with(pmpre, data.frame(StudyID, sd=pmdate(Start.date), ed=pmdate(End.date), country=Country, sponsor=Sponsor, surveyorg=Survey.organization, sample=Sample, N=as.numeric(gsub(",| ", "", as.character(Sample.size)))))

tm <- with(tmpre, data.frame(StudyID, sd=pmdate(BegDate), ed=pmdate(EndDate), sponsor=SurveySponsor, surveyorg=SurveyOrg, sample=SampleDesc, N=as.numeric(gsub(",| ", "", as.character(SampleSize))), mode=IntMethod, notes=StudyNote))

studydatechar1 <- gsub("through", "", gsub("and", "", gsub("(see", "", gsub("note)", "", gsub("&", "", gsub(";", "", gsub(".", "", gsub(" ", "", gsub(",,", ",", gsub(" ", ", ", as.character(kmpre$STUDYDATE))), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE)))
studydatechar <- gsub("January", "Jan", gsub("February", "Feb", gsub("March", "Mar", gsub("April", "Apr", gsub("June", "Jun", gsub("July", "Jul", gsub("August", "Aug", gsub("September", "Sep", gsub("October", "Oct", gsub("November", "Nov", gsub("December", "Dec", studydatechar1)))))))))))
splstudydate <- strsplit(studydatechar, split = ",|-|–")

newdates <- sapply(splstudydate, dateset)
startdate1 <- as.Date(newdates[1,], origin="1970-01-01")
enddate1 <- as.Date(newdates[2,], origin="1970-01-01")

startdate1[!is.na(startdate1) & enddate1<"1900-01-01"] <- NA
enddate1[!is.na(enddate1) & enddate1<"1900-01-01"] <- NA

altstartdate <- as.Date(kmpre$STUDYDATE1)
startdate1[is.na(startdate1)] <- altstartdate[is.na(startdate1)]
enddate1[is.na(enddate1)] <- altstartdate[is.na(enddate1)]

km <- with(kmpre, data.frame(StudyID, sd=startdate1, ed=enddate1, country=COUNTRY_NAME, sponsor=SPONSOR, surveyorg=ORGANIZATION, sample=SAMPLE, Title=STITLE, iPoll=IN_IPOLL=="Y"))

tfm <- with(tfmpre, data.frame(StudyID, sd=as.Date(BEG_DATE_DATE), ed=as.Date(END_DATE_DATE), sponsor=SURVEY_SPONSOR, surveyorg=SURVEY_ORG, sample=SAMPLE_DESC, N=as.numeric(as.character(SAMPLE_SIZE)), Title=STITLE))

am <- with(ampre, data.frame(StudyID, sd=as.Date(Start.date), ed=as.Date(End.date), sponsor=Survey.firm, mode=Interview.method, sample=Sample, notes=Notes, include=Include))


pmtm <- merge(pm, tm, by="StudyID", all=TRUE)
pmkm <- merge(pmtm, km, by="StudyID", all=TRUE)
pmtfm <- merge(pmkm, tfm, by="StudyID", all=TRUE)
pmam <- merge(pmtfm, am, by="StudyID", all=TRUE)

names(pmam)

nommer <- strsplit(names(pmam), ".", fixed=TRUE)
uniquenamespmam <- sapply(strsplit(names(pmam), ".", fixed=TRUE), function(x) x[1])
eachnom <- unique(uniquenamespmam)

pullmedian <- function(x){
    if(length(x[!is.na(x)])>0)
        out <- sort(x[!is.na(x)])[ceiling(length(x[!is.na(x)])/2)]
    if(length(x[!is.na(x)])==0)
        out <- NA
    out
}

cm <- as.data.frame(sapply(eachnom, function(i) unlist(apply(as.matrix(pmam[,uniquenamespmam==i]), 1, function(x) pullmedian(x)))))

cm$sd <- as.Date(as.character(cm$sd))
cm$ed <- as.Date(as.character(cm$ed))
cm$N <- as.numeric(as.character(cm$N))

summary(eachfileinfo)

eachfolderinfo$folder <- rownames(eachfolderinfo)

cm$Tracked <- TRUE

eachfileinfoplus <- merge(eachfileinfo, cm, by="StudyID", all.x=TRUE)
eachfolderinfoplus <- merge(eachfolderinfo, cm, by="StudyID", all.x=TRUE)

missingdates <- eachfolderinfoplus$folder[is.na(eachfolderinfoplus$sd) & !is.na(eachfolderinfoplus$Tracked)]
missingsampleinfo <- eachfolderinfoplus$folder[(is.na(eachfolderinfoplus$sample) | eachfolderinfoplus$sample=="") & !is.na(eachfolderinfoplus$Tracked)]

write.csv(rbind(cbind(missingdates, "Date"), cbind(missingsampleinfo, "Sample Information")), "Troubleshooting/02-04_StudiesMissingImportantMetadata.csv")


eachavailablestudy <- cm[cm$StudyID %in% eachfolderinfo$StudyID,]

save(cm, eachavailablestudy, eachfolderinfo, eachfileinfo, file="EachFileExports/02-03_MetaInformation.rdata")
write.csv(cm, "SummaryInformation/CombinedMetadata.csv")


