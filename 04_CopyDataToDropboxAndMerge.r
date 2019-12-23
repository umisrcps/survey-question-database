library(weights)
library(foreign)
library(memisc)
library(reshape)
library(dplyr)

#source("/Volumes/data/abort/Analysis2/01_GetAllFileInfo.r", echo=TRUE)
#source("/Volumes/data/abort/Analysis2/02_LoadAndParse.r", echo=TRUE)
#source("/Volumes/data/abort/Analysis2/03_ImportDatasets.r", echo=TRUE)

## UNCOMMENT THE BELOW LINES TO COPY DATA TO DROPBOX

#if(file.exists("/Volumes/data/abort/Analysis2/EachFileExports/"))
#    file.copy("/Volumes/data/abort/Analysis2/EachFileExports/", "./", overwrite=TRUE, recursive=TRUE)


## Load All RData Files

for(i in dir("EachFileExports/", full.names=TRUE)[grepl("01-|02-|03-", dir("EachFileExports/", full.names=TRUE))])
    load(i)

## Build Parsed Datasets

#ds_usabc1985-8825


tobuild <- names(datimports)[names(datimports) %in% names(importparsers)]

numericlean <- function(x){
    out <- as.character(as.numeric(as.character(x)))
    out[is.na(out)] <- as.character(x)[is.na(out)]
    out
}

parsebuilderrors <- NULL

for(d in tobuild){
    print(d)
    dsout <- datimports[[d]]
    questionnumber <- paste("Q", importparsers[[d]]$Question.number, sep="")
    questiontype <- as.character(importparsers[[d]]$Variable.type)
    questiontext <- as.character(importparsers[[d]]$Question.text)
    respinfo <- strsplit(as.character(importparsers[[d]]$Response.options), "; ", fixed=TRUE)
    resptext <- strsplit(as.character(importparsers[[d]]$Response.text), "; ", fixed=TRUE)
    names(resptext) <- names(respinfo) <- names(questiontext) <- names(questiontype) <- questionnumber
    for(i in colnames(dsout)){
        rt <- resptext[[i]]
        if(length(rt)==length(respinfo[[i]]) & length(rt)>0){
            names(rt) <- numericlean(respinfo[[i]])
            rt <- rt[names(rt)!=""]
            dsout[,i] <- recode(numericlean(dsout[,i]), !!!rt)
        }
        if(length(rt)!=length(respinfo[[i]]))
            parsebuilderrors <- rbind(parsebuilderrors, c(d, i, "Number of Response Options in Parser File Does Not Match Number of Values"))
    }
    attributes(dsout)$variable.labels <- questiontext
    attributes(dsout)$variable.type <- questiontype
    datimports[[d]] <- dsout
}

if(!is.null(dim(parsebuilderrors)))
    colnames(parsebuilderrors) <- c("Dataset", "Variable", "Problem")

write.csv(parsebuilderrors, "Troubleshooting/04-01_ParseBuildErrors.csv")


## Combine All Datasets

memiscversions <- lapply(spssmemiscimps, function(x) try(as.data.frame(x, stringsAsFactors=FALSE)))

memiscspssers <- memiscversions[sapply(memiscversions, class)!="try-error"]

write.csv(names(memiscversions)[sapply(memiscversions, class)=="try-error"], "Troubleshooting/04-02_CannotExtractSPSSDataDueToDuplicateLevelNames.csv")

dataframesets <- c(datimports, rdacomb, spsimps, spssimps, spssimps2, memiscspssers, stataimports, tabimports)

dftypestrack <- c(rep("dat", length(datimports)), rep("rds", length(rdacomb)), rep("sps", length(spsimps)), rep("spss", length(spssimps)), rep("spss2", length(spssimps2)), rep("memisc", length(memiscspssers)), rep("stata", length(stataimports)), rep("tab", length(tabimports)))

table(dftypestrack[!duplicated(names(dataframesets))])

sum(table(table(names(dataframesets))))

dfs <- dataframesets[!duplicated(names(dataframesets))]

save(dfs, file="EachFileExports/04-01_MergedDataFiles.rdata")

#unique(lapply(dfs, function(x) names(attributes(x))))

# Identify Question Wordings and Response Options For All Variables

vnattributes <- lapply(dfs, function(x) attributes(x)$variable.labels)
lattributes <- lapply(dfs, function(x) attributes(x)$label)
vlattributes <- lapply(dfs, function(x) attributes(x)$var.labels)
vtypes <- lapply(dfs, function(x) attributes(x)$variable.type)

workingvars <- lapply(dfs, function(x) sum(!is.na(x)))

write.csv(names(workingvars)[workingvars==0], "Troubleshooting/04-03_SeeminglyCorruptDataImports.csv")

touse <- unlist(sapply(dfs, function(x) dim(x)[1]))
emptydataframes <- names(dfs)[!(names(dfs) %in% names(touse))]
write.csv(emptydataframes, "Troubleshooting/04-04_EmptyDataFrames.csv")

allinfopervar <- allrespspervar <- as.list(rep(NA, length(touse)))
names(allinfopervar) <- names(allrespspervar) <- names(touse)
for(i in names(touse)){
    print(i)
    if(workingvars[i]>0){
        uniqueresps <- apply(dfs[[i]], 2, function(x) sort(unique(x)))
        names(uniqueresps) <- paste(i, colnames(dfs[[i]]), sep="~")
    }
    if(workingvars[i]==0)
        uniqueresps <- ""
    allcols <- data.frame(StudyID=i, Varname=colnames(dfs[[i]]), UniqueID=paste(i, colnames(dfs[[i]]), sep="~"), resps=sapply(uniqueresps, function(x) paste(x, collapse="; ")), stringsAsFactors=FALSE)
    attrib1 <- cbind(StudyID=i, Varname=names(vnattributes[[i]]), UniqueID=paste(i, names(vnattributes[[i]]), sep="~"), wording=as.character(vnattributes[[i]]))
    attrib2 <- cbind(StudyID=i, Varname=names(vlattributes[[i]]), UniqueID=paste(i, names(vlattributes[[i]]), sep="~"), wording=as.character(vlattributes[[i]]))
    attrib3 <- cbind(StudyID=i, Varname=names(lattributes[[i]]), UniqueID=paste(i, names(lattributes[[i]]), sep="~"), wording=as.character(lattributes[[i]]))
    types <- cbind(StudyID=i, Varname=names(vtypes[[i]]), UniqueID=paste(i, names(vtypes[[i]]), sep="~"), type=vtypes[[i]])
    pslist <- list(allcols, attrib1, attrib2, attrib3, types)
    varinfo <- merge_recurse(pslist)
    if(!is.null(varinfo$wording))
        varinfo$wording <- as.character(varinfo$wording)
    allinfopervar[[i]] <- varinfo
    allrespspervar[[i]] <- uniqueresps
}

#unx <- sapply(allinfopervar, function(x) unique(names(x)))

save(allinfopervar, allrespspervar, file="EachFileExports/04-02_QuestionInformationFromDataFiles.rdata")
