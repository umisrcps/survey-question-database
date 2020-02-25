library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(Amelia)
library(MASS)
library(plyr)

load("EachFileExports/10-01_IndividualMasterPreReversals.rdata")

# Split into outcomes per dataset

minfg <- with(fg, data.frame(out01, out01rev=out01, respID, StudyID, UniqueID))
fgsplit <- split(minfg, minfg$UniqueID)
#names(fgsplit) <- unique(fg$UniqueID)
fgsstdz <- lapply(fgsplit, function(x) stdz(x$out01))

minfgr <- data.frame(rbindlist(fgsplit), stdzout=unlist(fgsstdz))
minfgr2 <- with(minfgr, data.frame(respID, UniqueID, stdzout))

fg2p <- merge(fg, minfgr2, by=c("UniqueID", "respID"))
fg2 <- fg2p

fg2$proposeddir <- rep("Unknown", length(fg2$majorclass))
fg2$proposeddir[fg2$majorclass2=="Directional" | fg2$majorclass=="Directional Attitude"] <- "Directional Half"
fg2$proposeddir[fg2$majorclass2=="Importance" | fg2$majorclass=="Attitude Strength"] <- "Importance Half"
fg2$proposeddir[fg2$majorclass2=="Directional" & fg2$majorclass=="Directional Attitude"] <- "Directional"
fg2$proposeddir[fg2$majorclass2=="Importance" & fg2$majorclass=="Attitude Strength"] <- "Importance"
fg2$proposeddir[fg2$majorclass2=="Directional" & fg2$majorclass=="Attitude Strength"] <- "Mixed"
fg2$proposeddir[fg2$majorclass2=="Importance" & fg2$majorclass=="Directional Attitude"] <- "Mixed"
fg2$proposeddir[grepl("Excl", fg2$wccat) | grepl("Excl", fg2$bfsets5)] <- NA

fctregdirectional <- lm(stdzout~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*(dt+dt2+dt3+dt4+dt5), data=fg2, subset=(trainstatus=="Training Set" & proposeddir=="Directional"))

fctregimportance <- lm(stdzout~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*(dt+dt2+dt3+dt4+dt5), data=fg2, subset=(trainstatus=="Training Set" & proposeddir=="Importance"))

fg2$fctpreddir <- predict(fctregdirectional, newdata=fg2)
fg2$fctpredimp <- predict(fctregimportance, newdata=fg2)

# Identify Variables To Be Reversed

fg2split <- split(fg2, fg2$UniqueID)
corlistdir <- sapply(fg2split, function(x) try(cor(x$fctpreddir, x$out01, use="pairwise.complete.obs"), silent=TRUE))
corlistimp <- sapply(fg2split, function(x) try(cor(x$fctpredimp, x$out01, use="pairwise.complete.obs"), silent=TRUE))
mc2chk <- sapply(fg2split, function(x) x$majorclass2[1])
mc1chk <- sapply(fg2split, function(x) x$majorclass[1])
dtchk <- sapply(fg2split, function(x) x$dt[1])

dirguess <- abs(corlistdir)>abs(corlistimp)
dirguess2 <- dirguess
dirguess2[abs(corlistdir)<.1 & abs(corlistimp)<.1] <- NA
#dirguess[abs(corlistdir)<.01 & abs(corlistimp)<.01] <- NA
#table(dirguess, mc1chk, useNA="ifany")
#table(dirguess, mc2chk, useNA="ifany")
#plot(dtchk, dirguess)

adddir <- dirguess
adddir[abs(corlistdir)<.1] <- FALSE

addimp <- !dirguess
addimp[abs(corlistimp)<.1] <- FALSE

fg2$proposeddir2 <- fg2$proposeddir
fg2$proposeddir2[fg2$proposeddir %in% c("Directional Half", "Mixed", "Unknown") & fg2$UniqueID %in% names(corlistdir)[adddir]] <- "Directional"
fg2$proposeddir2[fg2$proposeddir %in% c("Importance Half", "Mixed", "Unknown") & fg2$UniqueID %in% names(corlistdir)[addimp]] <- "Importance"

fg2$classdir <- factor(fg2$proposeddir2, c("Importance", "Directional"))
fg2$classdir[fg2$UniqueID %in% names(corlistdir)[is.na(dirguess2)]] <- NA

fg2minrev <- fg2$classdir[!duplicated(fg2$UniqueID)]

toreversedir <- names(corlistdir[corlistdir < -.1 & !is.na(corlistdir) & dirguess==TRUE & !is.na(fg2minrev) & fg2minrev=="Directional"])
toreverseimp <- names(corlistdir[corlistimp < -.1 & !is.na(corlistimp) & dirguess==FALSE & !is.na(fg2minrev) & fg2minrev=="Importance"])

fg2$out01rev <- fg2$out01
fg2$stdzoutrev <- fg2$stdzout
fg2$out01rev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% c(toreversedir, toreverseimp))] <- 1-fg2$out01[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% c(toreversedir, toreverseimp))]
fg2$stdzoutrev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% c(toreversedir, toreverseimp))] <- -fg2$stdzout[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% c(toreversedir, toreverseimp))]

## Rerun This A Couple of Times Without Subsetting
for(i in c(-.05, -.02, -.01, 0)){
    print(paste("Reversals", i))
    print("Run Predictions")
    fctregdirectional <- lm(stdzoutrev~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*(dt+dt2+dt3+dt4+dt5), data=fg2, subset=(classdir=="Directional"))
    fctregimportance <- lm(stdzoutrev~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*(dt+dt2+dt3+dt4+dt5), data=fg2, subset=(classdir=="Importance"))
    fg2$fctpreddir <- predict(fctregdirectional, newdata=fg2)
    fg2$fctpredimp <- predict(fctregimportance, newdata=fg2)
    fg2split <- split(fg2, fg2$UniqueID)
    print("Run Correlations")
    corlistdir <- sapply(fg2split, function(x) try(cor(x$fctpreddir, x$stdzoutrev, use="pairwise.complete.obs"), silent=TRUE))
    corlistimp <- sapply(fg2split, function(x) try(cor(x$fctpredimp, x$stdzoutrev, use="pairwise.complete.obs"), silent=TRUE))
    mc2chk <- sapply(fg2split, function(x) x$majorclass2[1])
    mc1chk <- sapply(fg2split, function(x) x$majorclass[1])
    dtchk <- sapply(fg2split, function(x) x$dt[1])
    dirguess <- abs(corlistdir)>abs(corlistimp)
    dirguess2 <- dirguess
    dirguess2[abs(corlistdir)<.05 & abs(corlistimp)<.05] <- NA
    adddir <- dirguess
    adddir[abs(corlistdir)<.02] <- FALSE
    adddir[abs(corlistdir)>.15 & dirguess & (abs(corlistdir)/2)>abs(corlistimp)] <- TRUE
    addimp <- !dirguess
    addimp[abs(corlistimp)<.02] <- FALSE
    addimp[abs(corlistimp)>.15 & !dirguess & (abs(corlistimp)/2)>abs(corlistdir)] <- TRUE
    fg2$proposeddir3 <- fg2$proposeddir2
    fg2$proposeddir3[fg2$proposeddir2 %in% c("Directional Half", "Mixed", "Unknown") & fg2$UniqueID %in% names(corlistdir)[adddir]] <- "Directional"
    fg2$proposeddir3[fg2$proposeddir2 %in% c("Importance Half", "Mixed", "Unknown") & fg2$UniqueID %in% names(corlistdir)[addimp]] <- "Importance"
    fg2$classdir <- factor(fg2$proposeddir3, c("Importance", "Directional"))
    fg2$classdir[fg2$UniqueID %in% names(corlistdir)[is.na(dirguess2)]] <- NA
    fg2minrev <- fg2$classdir[!duplicated(fg2$UniqueID)]
    toreversedir <- names(corlistdir[corlistdir < i & !is.na(corlistdir) & dirguess==TRUE & !is.na(fg2minrev) & fg2minrev=="Directional"])
    toreverseimp <- names(corlistdir[corlistimp < i & !is.na(corlistimp) & dirguess==FALSE & !is.na(fg2minrev) & fg2minrev=="Importance"])
    print("Run Reversals")
    fg2$out01rev[(fg2$UniqueID %in% c(toreversedir, toreverseimp))] <- 1-fg2$out01rev[(fg2$UniqueID %in% c(toreversedir, toreverseimp))]
    fg2$stdzoutrev[(fg2$UniqueID %in% c(toreversedir, toreverseimp))] <- -fg2$stdzoutrev[(fg2$UniqueID %in% c(toreversedir, toreverseimp))]
}

topcor <- corlistdir
topcor[corlistimp>corlistdir & !is.na(corlistimp)] <- corlistimp[corlistimp>corlistdir & !is.na(corlistimp)]

fg2$confirmclasses <- fg2$UniqueID %in% names(topcor[topcor<.01])
fg2$classdir2 <- fg2$classdir
fg2$classdir2[fg2$confirmclasses] <- NA

coroutsets <- data.frame(UniqueID=names(corlistimp), corlistimp, corlistdir, topcor)

sepcats <- unique(names(table(fg2$bfsets5)))[!grepl("-", unique(names(table(fg2$bfsets5))))]
dummycats <- sapply(sepcats, function(x) grepl(x, fg2$bfsets5))

sepcubcats <- unique(names(table(fg2$fullclassification)))
sscats <- gsub("~~", "~", gsub(paste(unique(names(table(fg2$bfsets5))), collapse="|"), "", fg2$fullclassification))
sscs <- strsplit(unique(sscats), "~|-")
unqterms <- unique(unlist(sscs))
sscsbind <- sapply(sscs, function(x) paste(x, collapse="|"))
dummysubcatspre <- sapply(unqterms[unqterms!=""], function(x) sapply(sscs, function(g) sum(x==g, na.rm=TRUE)))

fullsized <- data.frame(sscats)
minisized <- data.frame(sscats=unique(sscats), dummysubcatspre)

dummysubcats <- join(fullsized, minisized)

save(fg2, dummycats, dummysubcats, topcor, coroutsets, file="EachFileExports/10-02_FullDatasetWithReversals.rdata")



fgmin <- with(fg2, data.frame(StudyID, UniqueID, dt, wccat, bigcats, majorclass, fullclassification, bfsets5, majorclass2, stdzoutrev, classdir2))[!is.na(fg2$classdir2),]

fgstuds <- split(fgmin, fgmin$UniqueID)
fgoutrevs <- lapply(fgstuds, function(x) x$stdzoutrev)
fgr <- fgoutrevs[sapply(fgoutrevs, function(x) length(!is.na(x)))>0]
studsings <- rbindlist(lapply(fgstuds, function(x) x[1,]))[sapply(fgoutrevs, function(x) length(!is.na(x)))>0,]

studds <- lapply(unique(studsings$StudyID), function(x) try(data.frame(fgoutrevs[studsings$UniqueID[studsings$StudyID==x]])))
names(studds) <- unique(studsings$StudyID)


studcompares <- studds[sapply(studds, dim)[2,]>1]

studcors <- lapply(studcompares, wtd.cors)

scread <- function(x){
    rbindlist(lapply(1:(dim(x)[1]-1), function(a) data.frame(t(sapply((a+1):dim(x)[1], function(b) c(v1=rownames(x)[a], v2=rownames(x)[b], cor=x[a,b]))))))
}

scc <- as.data.frame(rbindlist(lapply(studcors, scread)))
scc$cor <- as.numeric(as.character(scc$cor))

sccmin <- scc[!is.na(scc$cor) & scc$cor<.99 & scc$cor>-.99,]

studsings1 <- studsings
colnames(studsings1) <- paste("v1", colnames(studsings), sep="_")

studsings2 <- studsings
colnames(studsings2) <- paste("v2", colnames(studsings), sep="_")

studsings1$v1 <- gsub("-|_", ".", gsub("~", ".", tolower(studsings1$v1_UniqueID), fixed=TRUE))
studsings2$v2 <- gsub("-|_", ".", gsub("~", ".", tolower(studsings2$v2_UniqueID), fixed=TRUE))
sccmin$v1 <- gsub("-|_", ".", gsub("~", ".", tolower(sccmin$v1), fixed=TRUE))
sccmin$v2 <- gsub("-|_", ".", gsub("~", ".", tolower(sccmin$v2), fixed=TRUE))

table(sccmin$v1 %in% studsings1$v1)

ss1 <- merge(sccmin, studsings1, all.x=TRUE)
ss2 <- merge(ss1, studsings2, all.x=TRUE)

nommerge <- function(x, y){
    nms <- apply(data.frame(as.character(x), as.character(y), stringsAsFactors=FALSE), 1, function(g) paste(sort(g), collapse=" X "))
    nms[!grepl("X", nms)] <- paste(nms[!grepl("X", nms)], "X Uncategorized")
    nms[nms==" X Uncategorized"] <- "Uncategorized X Uncategorized"
    nms
}
    
ss2$mcmerge1 <- nommerge(ss2$v1_majorclass, ss2$v2_majorclass)
ss2$mcmerge2 <- nommerge(ss2$v1_majorclass2, ss2$v2_majorclass2)
ss2$classdir <- nommerge(ss2$v1_classdir, ss2$v2_classdir)
ss2$bigcats <- nommerge(ss2$v1_bigcats, ss2$v2_bigcats)
ss2$bfsets5 <- nommerge(ss2$v1_bfsets5, ss2$v2_bfsets5)
ss2$wccat <- nommerge(ss2$v1_wccat, ss2$v2_wccat)
ss2$fullclassification <- nommerge(ss2$v1_fullclassification, ss2$v2_fullclassification)


wcdp <- function(x, nom="Type"){
    seps <- split(abs(ss2$cor), x)
    seps <- seps[sapply(seps, length)>4]
    mns <- sapply(seps, mean)
    sp <- seps[order(mns)]
    pdf(paste("Figures/", nom, ".pdf", sep=""), width=6+(max(sapply(names(sp), nchar))/10), height=4+(length(seps)/2.5))
    plot(c(-(max(sapply(names(sp), nchar))/60),1.2), c(.5,length(sp)+.5), type="n", ylab="Variable Types", xlab="Absolute Correlation", axes=FALSE, main=paste("Absolute Correlations Between Items Using", nom, "\n "))
    axis(1, at=seq(0,1,.1))
    axis(3, at=seq(0,1,.1))
    abline(v=c(0,1))
    abline(v=seq(0,1,.1), lty=3, col="light gray")
    for(i in 1:length(sp))
        lines(sp[[i]], jitter(rep(i, length(sp[[i]])), amount=.35), type="p", pch=20)
    lines(sapply(sp, mean), 1:length(sp), type="p", pch=23, col="red", cex=1.5, lwd=3)
    text(0, 1:length(sp), labels=names(sp), pos=2)
    text(1, 1:length(sp), labels=paste("N =", sapply(sp, length), "\nMean r =", rd(sapply(sp, mean))), pos=4)
    dev.off()
}

wcdp2d <- function(x, nom="Type"){
    seps <- split(ss2$cor, x)
    seps <- seps[sapply(seps, length)>4]
    mns <- sapply(seps, mean)
    sp <- seps[order(mns)]
    pdf(paste("Figures/", nom, "Bidirectional.pdf", sep=""), width=8+(max(sapply(names(sp), nchar))/8), height=4+(length(seps)/2.5))
    plot(c(-1-(max(sapply(names(sp), nchar))/40),1.4), c(.5,length(sp)+.5), type="n", ylab="Variable Types", xlab="Correlation", axes=FALSE, main=paste("Bidirectional Correlations Between Items Using", nom, "\n "))
    axis(1, at=seq(-1,1,.1))
    axis(3, at=seq(-1,1,.1))
    abline(v=c(-1,0,1))
    abline(v=seq(-1,1,.1), lty=3, col="light gray")
    for(i in 1:length(sp))
        lines(sp[[i]], jitter(rep(i, length(sp[[i]])), amount=.35), type="p", pch=20)
    lines(sapply(sp, mean), 1:length(sp), type="p", pch=23, col="red", cex=1.5, lwd=3)
    text(-1, 1:length(sp), labels=names(sp), pos=2)
    text(1, 1:length(sp), labels=paste("N =", sapply(sp, length), "\nMean r =", rd(sapply(sp, mean))), pos=4)
    dev.off()
}




wcdp(ss2$mcmerge1, "Majorclass")
wcdp(ss2$mcmerge2, "Majorclass2")
wcdp(ss2$classdir, "classdir")
wcdp(ss2$bigcats, "Big Categories")
wcdp(ss2$bfsets5, "BF Sets")
wcdp(ss2$wccat, "Wccats")
wcdp(ss2$fullclassification, "Full Classifications")

wcdp2d(ss2$mcmerge1, "Majorclass")
wcdp2d(ss2$mcmerge2, "Majorclass2")
wcdp2d(ss2$classdir, "classdir")
wcdp2d(ss2$bigcats, "Big Categories")
wcdp2d(ss2$bfsets5, "BF Sets")
wcdp2d(ss2$wccat, "Wccats")
wcdp2d(ss2$fullclassification, "Full Classifications")

ss2rev <- ss2
names(ss2rev) <- gsub("v1", "v3", names(ss2rev))
names(ss2rev) <- gsub("v2", "v1", names(ss2rev))
names(ss2rev) <- gsub("v3", "v2", names(ss2rev))

ssdoubled <- rbind(ss2, ss2rev)

cormat <- function(x, y){
    out <- sapply(levels(x), function(f) sapply(levels(y), function(g) try(mean(ssdoubled$cor[x==f & y==g], na.rm=TRUE))))
    colnames(out) <- levels(x)
    rownames(out) <- levels(y)
    out
}


classdirset <- cormat(ssdoubled$v1_classdir, ssdoubled$v2_majorclass2)
wccatset <- cormat(ssdoubled$v1_wccat, ssdoubled$v2_wccat)
classdirset <- cormat(ssdoubled$v1_classdir, ssdoubled$v2_majorclass2)
classdirset <- cormat(ssdoubled$v1_classdir, ssdoubled$v2_majorclass2)

dtval <- seq(1960, 2020, 5)
dtnums <- as.numeric(as.Date(paste(dtval, "-01-01", sep="")))/20000

plotsepsets <- function(x, nom="Topic"){
    sp <- x[sapply(x, length)>5]
    pdf(paste("Figures/", nom, "Dates.pdf", sep=""), width=8+(max(sapply(names(sp), nchar))/8), height=4+(length(sp)/2.5))
    plot(c(-.6-(max(sapply(names(sp), nchar))/110),1.4), c(.5,length(sp)+.5), type="n", ylab=nom, xlab="Dates", axes=FALSE, main=paste("Mentions of", nom, "by Date"))
    axis(1, at=dtnums, labels=dtval)
    axis(3, at=dtnums, labels=rep("", length(dtnums)))
    abline(v=dtnums, lty=3, col="light gray")
    for(i in 1:length(sp))
        lines(sp[[i]], jitter(rep(i, length(sp[[i]])), amount=.35), type="p", pch=20)
    #lines(sapply(sp, mean), 1:length(sp), type="p", pch=23, col="red", cex=1.5, lwd=3)
    text(-.2, 1:length(sp), labels=names(sp), pos=2)
    text(.95, 1:length(sp), labels=paste("N =", sapply(sp, length)), pos=4)
    dev.off()
}


#Frequency of Question Types Over Time

fg2oneper <- fg2[!duplicated(fg2$UniqueID),]
dumcatoneper <- dummycats[!duplicated(fg2$UniqueID),]
dumsubcatoneper <- dummysubcats[!duplicated(fg2$UniqueID),colnames(dummysubcats)!="sscats"]

dc1p <- dumcatoneper[,colSums(dumcatoneper)>0]
dc2p <- dumsubcatoneper[,colSums(dumsubcatoneper)>0]

eachdumcatone <- apply(dc1p, 2, function(x) fg2oneper$dt[x])
eachdumcattwo <- apply(dc2p, 2, function(x) fg2oneper$dt[x==1])
wccatset <- split(fg2oneper$dt, fg2oneper$wccat)
bigcatset <- split(fg2oneper$dt, fg2oneper$bigcats)
bfsets <- split(fg2oneper$dt, fg2oneper$bfsets5)
fullclassifications <- split(fg2oneper$dt, fg2oneper$fullclassification)
specwordings <- split(fg2oneper$dt, fg2oneper$wordingsimp)



plotsepsets(eachdumcatone, "Major Topic Dummies")
plotsepsets(eachdumcattwo, "SubTopic Dummies")
plotsepsets(wccatset, "Original Subcategories")
plotsepsets(bigcatset, "Original Major Categories")
plotsepsets(bfsets, "Alternate Categories")
plotsepsets(fullclassifications, "Full Classifications")
plotsepsets(specwordings, "Specific Wordings")







## Rebuild Data At Study Level To Assess Correlations Between Items By Types


#outcomemergeds <- lapply(unique(fg2$StudyID), function(x) data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) fg2$stdzoutrev[fg2$UniqueID==y])))

#outcometypes <- lapply(unique(fg2$StudyID), function(x) as.data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) as.character(fg2$bfsets5)[fg2$UniqueID==y][1])))

#outcomemajortypes <- lapply(unique(fg2$StudyID), function(x) as.data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) as.character(fg2$classdir)[fg2$UniqueID==y][1])))

#ocmrel <- outcomemergeds[sapply(outcomemergeds, dim)[2,]>1]
#octrel <- outcometypes[sapply(outcomemergeds, dim)[2,]>1]

#ocmcors <- lapply(ocmrel, function(x) cor(x, use="pairwise.complete.obs"))

#outcomes <- NULL
#for(i in 1:length(ocmcors)){
#    for(a in 1:(length(octrel[[i]])-1)){
#        for(b in (a+1):length(octrel[[i]])){
#            outcomes[[paste(i,a,b, sep="_")]] <- try(data.frame(Type=paste(sort(as.character(unlist(c(octrel[[i]][a], octrel[[i]][b])))), collapse="~"), Corr=ocmcors[[i]][a,b]))
#        }
#    }
#}

#corsets <- rbindlist(outcomes)
#cs <- corsets[abs(corsets$Corr)!=1 & !is.na(corsets$Corr),]

#cbl <- sapply(levels(cs$Type), function(x) abs(cs$Corr[cs$Type==x]))
#cbldecadecol <- sapply(levels(wcimin$typecombo), function(x) wcimin$decadecol[wcimin$typecombo==x])
#cblord <- order(sapply(cbl, function(x) mean(x, na.rm=TRUE)))

#pdf("Figures/CorrelationsBetweenVariableTypes2.pdf", width=12, height=40, bg="White")
#plot(0:1, c(1,length(cblord)), type="n", xlim=c(-.75,1), axes=FALSE, xlab="                                                                                    Absolute Correlation Coefficient", ylab="Major Types Compared", main="Correlations Between Types of Variables in Individual Surveys")
#axis(1, seq(0,1,.2))
#axis(3, seq(0,1,.2), labels=FALSE)
#abline(v=seq(0,1,.2), lty=3, col="light gray")
#abline(v=0:1)
#sapply(1:length(cblord), function(x) lines(cbl[[cblord[x]]], rep(x, length(cbl[[cblord[x]]])), type="p", pch=20))
#lines(sapply(cbl, function(x) mean(x, na.rm=TRUE))[cblord], 1:length(cblord), type="p", pch=18, col="red")
#lines(sapply(cbl, function(x) mean(x, na.rm=TRUE))[cblord], 1:length(cblord), type="p", pch=23, col="red")
#text(-.02, y=1:length(cblord), labels=levels(cs$Type)[cblord], pos=2, cex=.75)
#legend(x="bottomleft", legend=c("Correlation in survey", "Mean Correlation"), pch=c(20, 23), col=c("black", "red"), bg="White", cex=.8)
#legend(x="bottomleft", legend=c("Correlation in survey", "Mean Correlation"), pch=c(20, 18), col=c("black", "red"), bg="White", cex=.8)
#dev.off()
