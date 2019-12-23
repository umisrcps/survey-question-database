library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(mgcv)
library(Amelia)
library(gamm4)


load("EachFileExports/10-02_FullDatasetWithReversals.rdata")
load("EachFileExports/07-01_AbortionVariablesCategorized.rdata")
load("EachFileExports/02-02_ToplineFiles.rdata")


combineddata <- fg2
combineddata$numdt <- as.numeric(combineddata$date)
combineddata$dateadj <- combineddata$numdt/16000
dateplot <- as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))
numdateplot <- as.numeric(dateplot)/16000

combineddata <- combineddata[!is.na(combineddata$stdzoutrev) & !is.na(combineddata$dateadj),]
combineddata$ordrevstdz <- combineddata$stdzoutrev
combineddata$wt2 <- combineddata$weight

cdlim <- combineddata[!is.na(combineddata$ordrevstdz) & !is.na(combineddata$dateadj) & !is.na(combineddata$Partisanship) & !is.na(wccat),]

outcomeaggregate <- xtabs((combineddata$wt2*combineddata$out01rev)~combineddata$UniqueID)
naggregate <- xtabs(combineddata$wt2[!is.na(combineddata$out01rev)]~combineddata$UniqueID[!is.na(combineddata$out01rev)])

wtdmeanaggregate <- outcomeaggregate/naggregate

names(wtdmeanaggregate)[!(names(wtdmeanaggregate) %in% unique(combineddata$UniqueID))]
unique(combineddata$UniqueID)[!(unique(combineddata$UniqueID) %in% names(wtdmeanaggregate))]

dts <- data.frame(dateadj=combineddata$dateadj, UniqueID=combineddata$UniqueID, nombinds=combineddata$nombinds, wordingsimp=combineddata$wordingsimp, majorclass=combineddata$majorclass, bigcats=combineddata$bigcats)[!duplicated(combineddata$UniqueID),]
aggset <- data.frame(UniqueID=names(wtdmeanaggregate), Ns=naggregate, mean=wtdmeanaggregate)

ops <- merge(dts, aggset)
opsforpred <- ops
opsforpred$dateadj <- quantile(ops$dateadj, .5)










combineddata <- combineddata[!is.na(combineddata$stdzoutrev) & !is.na(combineddata$dateadj),]
combineddata$ordrevstdz <- combineddata$stdzoutrev
combineddata$wt2 <- combineddata$weight
combineddata$ordrev <- combineddata$out01rev

answerdists <- with(combineddata, lapply(unique(UniqueID), function(i) wpct(out01rev[UniqueID==i], wt2[UniqueID==i])))


answerdists <- with(combineddata, lapply(unique(UniqueID), function(i) c(wpct(outcome[UniqueID==i], wt2[UniqueID==i]), n=sum(wt2[UniqueID==i & !is.na(outcome)]))))
names(answerdists) <- with(combineddata, unique(UniqueID))

ads <- lapply(answerdists, function(x) x[names(x)!="n"])
adslim <- ads[sapply(ads, length)>0]

tlansewrdists <- NULL
for(i in 1:length(importtoplines)){
    print(names(importtoplines)[i])
    for(j in unique(importtoplines[[i]]$QuestionID)){
        tlansewrdists[[paste(names(importtoplines)[i], j, sep="~")]] <- importtoplines[[i]]$RespPct[importtoplines[[i]]$QuestionID==j]
        names(tlansewrdists[[paste(names(importtoplines)[i], j, sep="~")]]) <- importtoplines[[i]]$RespTxt[importtoplines[[i]]$QuestionID==j]
    }
}

sstile <- strsplit(names(tlansewrdists), "~")
qnsup <- sapply(sstile, function(x) strsplit(x[2], ".", fixed=TRUE))
qns <- tolower(sapply(qnsup, function(x) x[length(x)]))
ssds <- sapply(sstile, function(x) x[1])
names(tlansewrdists) <- tolower(paste(ssds, qns, sep="~"))

#load("../Analysis from shared drive/Analysis/Genfiles/ResponseTranslations.rdata")

#nonsubresps <- tm[,"respalls"][tm[,"ra"]=="nonsub"]

#nomclean <- function(x)
#    sapply(x, function(g) gsub("[[:punct:]]", "", tolower(g)))




avsetmatches <- tlansewrdists[names(tlansewrdists) %in% av$UniqueIDlower]
avsetnames <- lapply(avsetmatches, function(x) nomclean(names(x))[!(tolower(names(x)) %in% nonsubresps) & !grepl("(vol.)|(vol)", tolower(names(x)))])

allavcombo <- c(avsetnames, sapply(adslim, names))

avc <- allavcombo[sapply(allavcombo, length)<8 & sapply(allavcombo, length)>1]

avnamecollapse <- sapply(avc, function(x) paste(x, collapse=" | "))
avnamecollapse2 <- sapply(avc, function(x) paste(sort(x), collapse=" | "))

avtranspre <- avc[!duplicated(avnamecollapse2)]

avtransmat <- t(sapply(avtranspre, function(x) c(x, rep("", 8-length(x)))))


write.csv(avtransmat, "Fixordinality/ToplinesExport1.csv")

legtranspre <- avc[grepl("ill|legal", avnamecollapse2) & !grepl("hill|kill|imm|pill|bill|will|drug|still", avnamecollapse2)]
legalcollapse <- avnamecollapse2[grepl("ill|legal", avnamecollapse2) & !grepl("hill|kill|imm|pill|bill|will|drug|still", avnamecollapse2)]
legtranslim <- legtranspre[!duplicated(legalcollapse)]
legtransmat <- t(sapply(legtranslim, function(x) c(x, rep("", 8-length(x)))))

write.csv(legtransmat, "Fixordinality/LegalExport1.csv")

legtrans <- read.csv("Fixordinality/LegalImport1.csv")
ltlleng <- apply(legtrans, 1, function(x) length(x[!is.na(x) & x!=""])-1)
ltansresps <- apply(legtrans, 1, function(x) x[!is.na(x) & x!=""][-1])
ltabsvals <- sapply(ltlleng, function(x) seq(-1, 1, length.out=x))






avshow <- av[av$wccat!="Excluded" & !is.na(av$wccat),]
table(avshow$bigcats)

pdf("Figures/QuestionTypes.pdf", width=12, height=7, bg="white")
plot(avshow$sd, jitter(as.numeric(as.factor(avshow$bigcats))), pch=20, xlim=as.Date(c("1920-01-01", "2020-01-01")), axes=FALSE, ylab="Question Type", xlab="Date", main="Frequency of Primary Question Types Over Time")
axis(1, as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
axis(3, as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), labels=FALSE)
text(as.Date("1950-01-01", "1960-01-01"), as.numeric(unique(as.factor(avshow$bigcats))), unique(as.factor(avshow$bigcats)), pos=2)
for(b in levels(avshow$bigcats)){
    avbc <- avshow[avshow$bigcats==b,]
    avbc$wccat <- drop.levels(avbc$wccat)
    plot(avbc$sd, jitter(as.numeric(as.factor(avbc$wccat))), pch=20, xlim=as.Date(c("1920-01-01", "2020-01-01")), axes=FALSE, ylab="Question Type", xlab="Date", main=paste("Frequency of", b, "Questions Types Over Time"))
    axis(1, as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020))
    axis(3, as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), labels=FALSE)
    text(as.Date("1950-01-01"), as.numeric(unique(as.factor(avbc$wccat))), unique(as.factor(avbc$wccat)), pos=2)
}
dev.off()





topqs <- names(rev(sort(table(avshow$wordingresponses))))[1:30]




avsetshortened <- lapply(avsetmatches, function(x) x[x==as.numeric(as.character(x)) & as.numeric(as.character(x))>7 & !grepl("vol|t know|not sure", names(x)) & !(names(x) %in% c("don't know", "no opinion", "dont know", "dk"))])
avsetshortened[!duplicated(names(avsetshortened))]
shortanswers <- sapply(avsetshortened, function(x) try(paste(sort(tolower(names(x))), collapse="|")))

names(avsetshortened) <- tolower(names(avsetshortened))

commonanssets <- names(rev(sort(table(shortanswers))))[1:50]

metainfo <- NULL
ansdists <- NULL
for(i in commonanssets){
    dsnspre <- tolower(names(avsetshortened)[shortanswers==i])
    dsns <- dsnspre[dsnspre %in% av$UniqueIDlower]
    if(length(dsns)>0){
        metainfo[[i]] <- rbindlist(lapply(dsns, function(x) av[av$UniqueIDlower==x,][1,]))
        ansdistspre <- lapply(dsns, function(x) avsetshortened[names(avsetshortened)==x & !is.na(x)][[1]][!is.na(avsetshortened[names(avsetshortened)==x & !is.na(x)][[1]])])
        ansdists[[i]] <- t(sapply(ansdistspre[1:length(ansdistspre)], function(y) sapply(unlist(strsplit(i, "|", fixed=TRUE)), function(x) as.numeric(as.character(y))[tolower(names(y))==x & !is.na(x)])))
    }
}


plotseries <- NULL
megaseries <- NULL
q <- 1
for(i in commonanssets){
    ans <- ansdists[[i]]
    mama <- as.data.frame(metainfo[[i]])
    cats <- names(rev(sort(table(mama$wccat))))[rev(sort(table(mama$wccat)))>10]
    if(length(cats)>0)
        for(j in cats){
            plotseries[[paste(i, "~", j, sep="")]] <- data.frame(date=mama$sd, numdt=as.numeric(mama$sd), org=mama$surveyorg, nom=paste(j, "-", i), ans)[mama$wccat==j & !is.na(mama$wccat),]
            megaseries[[paste(i, "~", j, sep="")]] <- data.frame(mama, nom=paste(j, "-", i), ans)[mama$wccat==j & !is.na(mama$wccat),]
            q <- q+1
            }
}

pspre <- plotseries[!grepl("Excluded", names(plotseries))]
psp <- rev(pspre[order(sapply(pspre, dim)[1,])])
ps <- psp[sapply(psp, function(x) length(unique(x$date)))>5]

mspre <- megaseries[!grepl("Excluded", names(plotseries))]
msp <- rev(mspre[order(sapply(pspre, dim)[1,])])
mps <- msp[sapply(psp, function(x) length(unique(x$date)))>5]

colset <- c("blue", "red", "brown", "dark green", "cyan")

pdf("Figures/QuestionDistributions.pdf", width=12, height=7, bg="white")
for(i in 1:length(ps)){
    unqdt <- length(unique(ps[[i]]$date))
    if(unqdt>4){
    dtrng <- try(range(ps[[i]]$numdt, na.rm=TRUE))
    ylims <- try(max(ps[[i]][,5:(dim(ps[[i]])[2])], na.rm=TRUE)*1.25)
    if(class(ylims)!="try-error"){
    plot(dtrng, y=c(0, ylims), type="n", main=names(ps)[i], ylab="Proportion", xlab="Date", axes=FALSE)
    axis(1, at=as.numeric(as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))), seq(1960, 2020, 5))
    axis(2)
    axis(3, c(-999, 9999999))
    axis(2, c(-999, 9999999))
    axis(4, c(-999, 9999999))
    levs <- try(gsub(" |-", ".", strsplit(strsplit(names(ps)[i], "~", fixed=TRUE)[[1]][1], "|", fixed=TRUE)[[1]]))
    for(l in 1:length(levs)){
        lreg <- try(glm(as.vector(ps[[i]][,levs[l]])~numdt+eval(numdt^2)+eval(numdt)^3, data=ps[[i]]))
        try(lines(ps[[i]]$date, ps[[i]][,levs[l]], type="p", col=colset[l]))
        try(plotwtdinteraction(lreg, "numdt", linecol=colset[l], secol=colset[l], lty=l, density=30, add=TRUE, legend=FALSE))
    }
    legend(x="topleft", legend=levs, col=colset[1:l], lty=1:l, lwd=3)
    }
    }
}
dev.off()


psbind <- rbindlist(ps[!grepl("warm|import|democrat|gore|yes|likely|agree", names(ps))], fill=TRUE)

proset <- as.data.frame(psbind)[,grepl("legal|acceptable|available", colnames(psbind)) & !grepl("illeg|but|only|certain", colnames(psbind))]
antiset <- as.data.frame(psbind)[,grepl("illegal|wrong|not.be.permitted|not.permitted", colnames(psbind))]

psbind$pro <- rowSums(proset, na.rm=TRUE)
psbind$anti <- rowSums(antiset, na.rm=TRUE)
psbind$psnoms <- unlist(sapply(names(ps[!grepl("warm|import|democrat|gore|yes|likely|agree", names(ps))]), function(x) rep(x, dim(ps[[x]])[1])))
psb <- with(psbind[psbind$pro>0 & psbind$anti>0 & !is.na(psbind$pro) & !is.na(psbind$anti),], data.frame(date, numdt, org, nom, pro, anti, psnoms))

psb$MN <- (psb$pro-psb$anti)/200+.5

psb$admin <- cut(as.numeric(psb$date), as.numeric(as.Date(c("1960-11-04", "1963-11-12", "1968-11-04", "1974-08-09", "1976-11-04", "1980-11-04", "1988-11-04", "1992-11-04", "2000-11-04", "2008-11-04", "2016-11-04", "2020-11-04"))), c("Kennedy", "Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "W Bush", "Obama", "Trump"))

newdat <- with(psb, data.frame(numdt=seq(as.numeric(as.Date("1988-11-06")), max(numdt, na.rm=TRUE), length.out=50), psnoms=factor(names(rev(sort(table(psnoms))))[1], levels=levels(psnoms)), org=factor(names(rev(sort(table(org))))[1], levels=levels(org))))
newdat$admin <- cut(newdat$numdt, as.numeric(as.Date(c("1960-11-04", "1963-11-12", "1968-11-04", "1974-08-09", "1976-11-04", "1980-11-04", "1988-11-04", "1992-11-04", "2000-11-04", "2008-11-04", "2016-11-04", "2020-11-04"))), c("Kennedy", "Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "W Bush", "Obama", "Trump"))

library(gamm4)

gamout <- gamm4(MN~s(numdt)+admin, random=~(1|org)+(1|nom), data=psb)
as.data.frame(ranef(gamout$mer))
gampred <- predict(gamout$gam, newdata=newdat, se.fit=TRUE)

pdf("Figures/ByPresidentialControl.pdf", width=12, height=7, bg="white")
plot(psb$date, psb$MN, xlab="Date", ylab="Mean Abortion Attitude")
polygon(c(newdat$numdt, rev(newdat$numdt)), c(gampred$fit+1.96*gampred$se.fit, rev(gampred$fit-1.96*gampred$se.fit)), col="gray", density=60)
lines(newdat$numdt, gampred$fit, type="l", lwd=3)
abline(v=as.numeric(as.Date(c("1960-11-04", "1963-11-12", "1968-11-04", "1974-08-09", "1976-11-04", "1980-11-04", "1988-11-04", "1992-11-04", "2000-11-04", "2008-11-04", "2016-11-04", "2020-11-04"))))
text(as.numeric(as.Date(c("1978-11-04", "1984-11-04", "1990-11-04", "1996-11-04", "2004-11-04", "2012-11-04", "2018-02-04"))), .3, c("Carter", "Reagan", "Bush", "Clinton", "W Bush", "Obama", "Trump"))
dev.off()





legalitymps <- mps[grepl("~Legal", names(ps))]
legalitycombopre <- as.data.frame(rbindlist(legalitymps, fill=TRUE))
legalitycombo <- legalitycombopre[!(grepl("democrat|asian|hispanic|age|women|primary|black", legalitycombopre$sample) & !grepl("oversample", legalitycombopre$sample)),]
legalitycombo$LegalAll <- rowSums(legalitycombo[,grepl("all|always|any", names(legalitycombo)) & !grepl("ill", names(legalitycombo))], na.rm=TRUE)
legalitycombo$IllegalAll <- rowSums(legalitycombo[,grepl("all|always|any", names(legalitycombo)) & grepl("ill", names(legalitycombo))], na.rm=TRUE)
legalitycombo$MiddlingAll <- rowSums(legalitycombo[,!grepl("all|always|any", names(legalitycombo)) & grepl("leg", names(legalitycombo))], na.rm=TRUE)
legalitycombo$MN <- legalitycombo$LegalAll+.5*legalitycombo$MiddlingAll
legalitycombo$cases <- grepl("cases", legalitycombo$nom)
legalitycombo$nresps <- sapply(strsplit(as.character(legalitycombo$nom), "|", fixed=TRUE), length)==4
legalitycombo$numdt <- as.numeric(legalitycombo$sd)
    
legalitycombo$org <- legalitycombo$surveyorg
legalitycombo$org[is.na(legalitycombo$surveyorg)] <- legalitycombo$sponsor

orgmin <- rep("Other", length(legalitycombo$org))
orgmin[grepl("ABC|Post", legalitycombo$org)] <- "ABC/WaPo"
orgmin[grepl("SRBI|Ronca", legalitycombo$org)] <- "SRBI/Princeton"
orgmin[grepl("GfK|Knowledge Networks|Gfk", legalitycombo$org)] <- "Gfk"
orgmin[grepl("Quinnipiac", legalitycombo$org)] <- "Quinnipiac"
orgmin[grepl("Princeton", legalitycombo$org)] <- "SRBI/Princeton"
orgmin[grepl("Greenberg Quinlan", legalitycombo$org)] <- "Greenberg Quinlan"
orgmin[grepl("ICR", legalitycombo$org)] <- "ICR/SSRS"
orgmin[grepl("ORC", legalitycombo$org)] <- "ORC International"
orgmin[grepl("Social Science Research", legalitycombo$org)] <- "ICR/SSRS"
orgmin[grepl("Voter|Edison|Mitofsky", legalitycombo$org)] <- "Exit Polls"
orgmin[grepl("Gallup", legalitycombo$org)] <- "Gallup"
orgmin[grepl("PRRI|Public Religion", legalitycombo$org)] <- "PRRI"


#"Other"
orgmin <- as.factor(orgmin)

legalitycombo$StudyID[is.na(orgmin)]

library(gamm4)
legalityreg <- gamm4(MN~s(numdt)+cases+nresps, random=~(1|orgmin)+(1|mode), data=legalitycombo)
summary(legalityreg$gam)
ranef(legalityreg$mer)
refit <- as.data.frame(ranef(legalityreg$mer))
with(refit, cbind(as.character(grp), condval/condsd))


pdf("Figures/LegalityByQuestion.pdf", width=12, height=7, bg="white")
legalityps <- ps[grepl("~Legal", names(ps))]
fulldts <- lapply(legalityps, function(x) x$numdt)
fulllegalall <- lapply(legalityps, function(x) x[,grepl("all|always|any", names(x)) & !grepl("ill", names(x))])
ylims <- max(unlist(fulllegalall), na.rm=TRUE)*1.25
plot(range(fulldts, na.rm=TRUE), y=c(0, ylims), type="n", main="Combined Legal - Legal All or Any Cases", ylab="Proportion", xlab="Date", axes=FALSE)
axis(1, at=as.numeric(as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))), seq(1960, 2020, 5))
axis(2)
axis(3, c(-999, 9999999))
axis(2, c(-999, 9999999))
axis(4, c(-999, 9999999))
for(i in 1:length(fulllegalall)){
    ds <- data.frame(legal=fulllegalall[[i]], date=fulldts[[i]])
    lreg <- try(glm(legal~date+eval(date^2)+eval(date)^3, data=ds))
    try(lines(ds$date, ds$legal, type="p", col=colset[i]))
    try(plotwtdinteraction(lreg, "date", linecol=colset[i], secol=colset[i], lty=i, density=30, add=TRUE, legend=FALSE, seplot=FALSE))
}
legend(x="topleft", legend=names(fulllegalall), col=colset[1:i], lty=1:i, lwd=3, cex=.8)
fullillegalall <- lapply(legalityps, function(x) x[,grepl("all|always|any", names(x)) & grepl("ill", names(x))])
ylims <- max(unlist(fullillegalall), na.rm=TRUE)*1.25
plot(range(fulldts, na.rm=TRUE), y=c(0, ylims), type="n", main="Combined Legal - Illegal All or Any Cases", ylab="Proportion", xlab="Date", axes=FALSE)
axis(1, at=as.numeric(as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))), seq(1960, 2020, 5))
axis(2)
axis(3, c(-999, 9999999))
axis(2, c(-999, 9999999))
axis(4, c(-999, 9999999))
for(i in 1:length(fullillegalall)){
    ds <- data.frame(illegal=fullillegalall[[i]], date=fulldts[[i]])
    lreg <- try(glm(illegal~date+eval(date^2)+eval(date)^3, data=ds))
    try(lines(ds$date, ds$illegal, type="p", col=colset[i]))
    try(plotwtdinteraction(lreg, "date", linecol=colset[i], secol=colset[i], lty=i, density=30, add=TRUE, legend=FALSE, seplot=FALSE))
}
legend(x="topleft", legend=names(fulllegalall), col=colset[1:i], lty=1:i, lwd=3, cex=.8)
fulllegalmid <- lapply(legalityps, function(x) x[,!grepl("all|always|any", names(x)) & grepl("leg", names(x))])
ylims <- max(unlist(fulllegalmid), na.rm=TRUE)*1.25
plot(range(fulldts, na.rm=TRUE), y=c(0, ylims), type="n", main="Combined Legal - Middling Responses", ylab="Proportion", xlab="Date", axes=FALSE)
axis(1, at=as.numeric(as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))), seq(1960, 2020, 5))
axis(2)
axis(3, c(-999, 9999999))
axis(2, c(-999, 9999999))
axis(4, c(-999, 9999999))
for(i in 1:length(fulllegalmid)){
    ds <- data.frame(legal=rowSums(as.data.frame(fulllegalmid[[i]])), date=fulldts[[i]])
    lreg <- try(glm(legal~date+eval(date^2)+eval(date)^3, data=ds))
    try(lines(ds$date, ds$legal, type="p", col=colset[i]))
    try(plotwtdinteraction(lreg, "date", linecol=colset[i], secol=colset[i], lty=i, density=30, add=TRUE, legend=FALSE, seplot=FALSE))
}
legend(x="topleft", legend=names(fulllegalmid), col=colset[1:i], lty=1:i, lwd=3, cex=.8)
dev.off()



pclmps <- mps[grepl("~Pro-Choice", names(ps))][[1]]
pcl <- pclmps[!(grepl("democrat|asian|hispanic|age|women|primary|black", pclmps$sample) & !grepl("oversample", pclmps$sample)),]
pcl$surveyorg <- factor(grepl("Gallup", pcl$surveyorg)+2*grepl("Opinion Dy", pcl$surveyorg), 0:2, c("Other", "Gallup", "Opinion Dynamics"))
pcl$MN <- pcl$pro.choice/(pcl$pro.choice+pcl$pro.life)

pclreg <- gamm4(MN~s(numdt), random=~(1|orgs), data=pcl)
summary(pclreg$gam)
ranef(pclreg$mer)
refit <- as.data.frame(ranef(pclreg$mer))
with(refit, cbind(as.character(grp), condval/condsd))









# Survey and question info
summary(av[!duplicated(av$StudyID),])

svylev <- av[!duplicated(av$StudyID),]

table(grepl("Gallup", svylev$surveyorg))
table(grepl("Princeton", svylev$surveyorg))
table(grepl("Quinlan", svylev$surveyorg))
table(grepl("Angeles|LA", svylev$surveyorg))
table(grepl("ABC|Post", svylev$surveyorg))
table(grepl("CBS|New York Times|NYT", svylev$surveyorg))

rev(sort(table(svylev$surveyorg)))[1:30]

length(unique(av$wordingsimp))

length(unique(av$respssimp))


dim(av[!duplicated(av$UniqueID),])

svylev <- av[!duplicated(av$StudyID),]

table(grepl("Gallup", av$surveyorg))
table(grepl("Princeton", av$surveyorg))
table(grepl("Quinlan", av$surveyorg))
table(grepl("Angeles|LA", av$surveyorg))
table(grepl("ABC|Post", av$surveyorg))
table(grepl("CBS|New York Times|NYT", av$surveyorg))


