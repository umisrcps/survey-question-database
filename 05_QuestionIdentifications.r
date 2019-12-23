library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)

## Load Key Files

load("EachFileExports/02-02_ToplineFiles.rdata")
load("EachFileExports/04-02_QuestionInformationFromDataFiles.rdata")

toplinetransform <- function(x){
    qspre <- as.character(unique(x$QuestionID))
    qs <- qspre[!grepl("Number of items", qspre) & !is.na(qspre)]
    t(sapply(qs, function(g) c(QuestionTLT=g, wordingTLT=as.character(x$QuestionTxt[x$QuestionID==g][1]), respsTLT=paste(as.character(sort(x$RespTxt[x$QuestionID==g])), collapse="; "))))
}

tlts <- lapply(1:length(importtoplines), function(x) data.frame(StudyID=names(importtoplines)[x], toplinetransform(importtoplines[[x]]), stringsAsFactors=FALSE))
names(tlts) <- names(importtoplines)

tltmerge <- rbindlist(tlts, fill=TRUE)
tltmerge$datatypeTLT <- "Topline"

vardatabase <- rbindlist(allinfopervar, fill=TRUE)
vardatabase$datatype <- "Microdata"

vdbpre <- vardatabase[!duplicated(vardatabase$UniqueID),]
vdbpre2 <- vdbpre[!(is.na(vdbpre$Varname) | vdbpre$Varname=="QNA"),]

vdbpre2$vn <- gsub("_", " ", tolower(vdbpre2$Varname))
tltmerge$vntlt <- gsub("_", " ", sapply(strsplit(tolower(as.character(tltmerge$Question)), ".", fixed=TRUE), function(x) x[length(x)]))

vdbpre2$UniqueIDlower <- tolower(vdbpre2$UniqueID)

tltmerge$UniqueIDlower <- tolower(paste(tltmerge$StudyID, tltmerge$vntlt, sep="~"))
tltmerge$UniqueIDlower[tltmerge$vntlt==""] <- NA

duplicatedQuestionNames <- sort(table(vdbpre2$UniqueID)[table(vdbpre2$UniqueID)>1])

write.csv(duplicatedQuestionNames, "Troubleshooting/05-01_DuplicatedQuestionNames_FixInOriginals.csv")

tltpre <- tltmerge[!duplicated(tltmerge$UniqueIDlower),]

mgd <- merge(vdbpre2, tltpre, by=c("UniqueIDlower", "StudyID"), all.x=TRUE)

tltpre2 <- tltpre[!(tltpre$UniqueIDlower %in% mgd$UniqueIDlower),]

tltorigwording <- tltpre2$wording
tltorigwording <- gsub("[.*]", "", tltorigwording)
tltsplitw1 <- sapply(strsplit(tltorigwording, " "), function(x) x[1])
tltdropfirstword <- (grepl(".", tltsplitw1, fixed=TRUE) | grepl("[0-9]", tltsplitw1) | grepl(":", tltsplitw1, fixed=TRUE)) & nchar(tltsplitw1)<15
tltorigwording[tltdropfirstword] <- gsub("^ ", "", gsub("  ", " ", sapply((1:length(tltorigwording))[tltdropfirstword], function(x) gsub(tltsplitw1[x], "", tltorigwording[x], fixed=TRUE))))


tltpre2$wordingsimp <- gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", tltorigwording))))), fixed=TRUE), fixed=TRUE)


mgdorigwording <- gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", mgd$wording))))), fixed=TRUE), fixed=TRUE)
mgdorigwording <- gsub("[.*]", "", mgdorigwording)
mgdsplitw1 <- sapply(strsplit(mgdorigwording, " "), function(x) x[1])
mgddropfirstword <- (grepl(".", mgdsplitw1, fixed=TRUE) | grepl("[0-9]", mgdsplitw1) | grepl(":", mgdsplitw1, fixed=TRUE)) & nchar(mgdsplitw1)<15
mgdorigwording[mgddropfirstword] <- gsub("^ ", "", gsub("  ", " ", sapply((1:length(mgdorigwording))[mgddropfirstword], function(x) gsub(mgdsplitw1[x], "", mgdorigwording[x], fixed=TRUE))))

mgd$wordingsimp <- mgdorigwording#gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", mgdorigwording))))), fixed=TRUE), fixed=TRUE)

mgd2 <- merge(mgd, tltpre2, by=c("StudyID", "wordingsimp"), all=TRUE)
mgd2$UniqueIDlower <- mgd2$UniqueIDlower.x
mgd2$UniqueIDlower[is.na(mgd2$UniqueIDlower)] <- mgd2$UniqueIDlower.y[is.na(mgd2$UniqueIDlower)]
mgd2$QuestionTLT <- mgd2$QuestionTLT.x
mgd2$QuestionTLT[is.na(mgd2$QuestionTLT)] <- mgd2$QuestionTLT.y[is.na(mgd2$QuestionTLT)]
mgd2$wordingTLT <- mgd2$wordingTLT.x
mgd2$wordingTLT[is.na(mgd2$wordingTLT)] <- mgd2$wordingTLT.y[is.na(mgd2$wordingTLT)]
mgd2$respsTLT <- mgd2$respsTLT.x
mgd2$respsTLT[is.na(mgd2$respsTLT)] <- mgd2$respsTLT.y[is.na(mgd2$respsTLT)]
mgd2$datatypeTLT <- mgd2$datatypeTLT.x
mgd2$datatypeTLT[is.na(mgd2$datatypeTLT)] <- mgd2$datatypeTLT.y[is.na(mgd2$datatypeTLT)]
mgd2$vntlt <- mgd2$vntlt.x
mgd2$vntlt[is.na(mgd2$vntlt)] <- mgd2$vntlt.y[is.na(mgd2$vntlt)]
mgd2$datatype <- factor((mgd2$datatype=="Microdata" & !is.na(mgd2$datatype)) + 2*(mgd2$datatypeTLT=="Topline" & !is.na(mgd2$datatypeTLT)), 1:3, c("Microdata", "Topline", "Both"))

mgd2$wordingsimp[mgd2$wordingsimp==" "|is.na(mgd2$wordingsimp)] <- ""
mgd2$resps[is.na(mgd2$resps)] <- mgd2$respsTLT[is.na(mgd2$resps)]
mgd2$resps[is.na(mgd2$resps)] <- ""
mgd2$vn[is.na(mgd2$vn)] <- mgd2$vntlt[is.na(mgd2$vn)]
mgd2$vn[is.na(mgd2$vn)] <- ""

mgd3 <- mgd2[!duplicated(mgd2$UniqueIDlower),]

md <- with(mgd3, data.frame(StudyID, UniqueID, UniqueIDlower, Varname, vn, vntlt, wordingsimp, wording, wordingTLT, resps, respsTLT, type, datatype, stringsAsFactors=FALSE))

md$wordingsimp[is.na(md$wordingsimp)] <- gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", md$wording))))), fixed=TRUE), fixed=TRUE)[is.na(md$wordingsimp)]
md$wordingsimp[is.na(md$wordingsimp)] <- gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", md$wordingTLT))))), fixed=TRUE), fixed=TRUE)[is.na(md$wordingsimp)]

md$respssimp <- gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\xf1", "n", gsub("\xfa", "u", gsub("\xed", "i", gsub("\xe9", "e", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94|\xaf", "", md$resps))))))))), fixed=TRUE), fixed=TRUE)

md$wordingresponses <- paste(md$vn, md$wordingsimp, md$respssimp, sep="; ")
md$vnames <- paste(md$vn, md$wordingsimp, sep="; ")

save(md, file="EachFileExports/05-01_AllQuestionWordingResponses.rdata")

