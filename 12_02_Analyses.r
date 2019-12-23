library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(mgcv)
library(Amelia)
library(gamm4)


load("EachFileExports/10-02_FullDatasetWithReversals.rdata")

combineddata <- fg2
combineddata$numdt <- as.numeric(combineddata$date)
combineddata$dateadj <- combineddata$numdt/16000
dateplot <- as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01"))
numdateplot <- as.numeric(dateplot)/16000

combineddata$Presidentdates <- cut(combineddata$dateadj, breaks=c(as.numeric(as.Date(c("1957-01-20", "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20")))/16000), c("Eisenhower", "Kennedy", "Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush I", "Clinton", "Bush II", "Obama", "Trump"))

combineddata$PresParty <- cut(combineddata$dateadj, breaks=c(as.numeric(as.Date(c("1957-01-20", "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20")))/16000), c("Republican", "Democrat", "Democrat", "Republican", "Republican", "Democrat", "Republican", "Republican", "Democrat", "Republican", "Democrat", "Republican"))

combineddata$Congdates <- cut(combineddata$dateadj, as.numeric(as.Date(c("1957-01-01", "1981-01-01", "1987-01-01", "1995-01-01", "2001-01-01", "2003-01-01", "2007-01-01", "2011-01-01", "2015-01-01", "2019-01-01", "2021-01-01")))/16000, c("Democrats", "Split", "Democrats", "Republicans", "Split", "Republicans", "Democrats", "Split", "Republicans", "Split"))


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

lmerset <- gamm4(mean.Freq~s(dateadj, k=floor(sqrt(length(unique(ops[!is.na(ops$bigcats) & !is.na(ops$majorclass),]$dateadj)))), by=majorclass)+bigcats, random=~(1|nombinds)+(1|wordingsimp), weights=ops[!is.na(ops$bigcats),]$Ns.Freq, data=ops[!is.na(ops$bigcats) & !is.na(ops$majorclass),])

wordingbaseline <- predict(lmerset$gam, newdata=opsforpred)
names(wordingbaseline) <- ops$UniqueID

wbaddon <- data.frame(UniqueID=ops$UniqueID, wordingbaseline=wordingbaseline)

cdplus <- merge(combineddata, wbaddon, by="UniqueID", all.x=TRUE, reorder=FALSE)

combineddata$ordrev <- cdplus$out01rev-cdplus$wordingbaseline+mean(cdplus$wordingbaseline, na.rm=TRUE)



sexparty <- paste(combineddata$Sex, combineddata$Partisanship, sep="x")
sexparty[is.na(combineddata$Sex) | is.na(combineddata$Partisanship)] <- NA
combineddata$sexparty <- as.factor(sexparty)

spgam <- gam(ordrev~s(dateadj, k=floor(sqrt(length(unique(combineddata$dateadj)))), by=sexparty)+sexparty, weight=sqrt(wt2), data=combineddata)


gamgroupcalcstdz <- function(groupid, subset=1){
    cdp <- combineddata[groupid & subset & !is.na(combineddata$ordrevstdz),]
    cd <- cdp[cdp$dateadj %in% names(table(cdp$dateadj))[table(cdp$dateadj)>10],]
    if(length(unique(cd$wccat))>1){
        reg <- gam(ordrevstdz~wccat+s(dateadj, k=floor(sqrt(length(unique(cd$dateadj))))), weight=sqrt(wt2), data=cd)
        predds <- with(cd, data.frame(wccat=names(rev(sort(table(wccat))))[1], dateadj=seq(min(dateadj, na.rm=TRUE), max(dateadj, na.rm=TRUE), length.out=100)))
        regse <- predict(reg, se.fit=TRUE, newdata=predds)
    }
    if(length(unique(cd$wccat))==1){
        reg <- gam(ordrevstdz~s(dateadj, k=floor(sqrt(length(unique(cd$dateadj))))), weight=sqrt(wt2), data=cd)
        predds <- with(cd, data.frame(dateadj=seq(min(dateadj, na.rm=TRUE), max(dateadj, na.rm=TRUE), length.out=100)))
        regse <- predict(reg, se.fit=TRUE, newdata=predds)
    }
    ptests <- as.data.frame(t(sapply(levels(drop.levels(as.factor(cd$UniqueID))), function(x) c(date=cd$dateadj[cd$UniqueID==x][1], mn=wtd.mean(cd$ordrevstdz[cd$UniqueID==x], cd$wt2[cd$UniqueID==x], na.rm=TRUE), n=sum(cd$wt2[cd$UniqueID==x & !is.na(cd$ordrevstdz)])))))
    out <- list(predds=predds, predict=regse, individuals=ptests)
    out
}

gamgroupcalcdir <- function(groupid, subset=1){
    cdp <- combineddata[groupid & subset & !is.na(combineddata$ordrevstdz) & !is.na(combineddata$dateadj),]
    cd <- cdp[cdp$dateadj %in% names(table(cdp$dateadj))[table(cdp$dateadj)>10],]
    if(length(unique(cd$wccat))>1){
        reg <- gam(ordrev~wccat+s(dateadj, k=floor(sqrt(length(unique(cd$dateadj))))), weight=sqrt(wt2), data=cd)
        predds <- with(cd, data.frame(wccat=names(rev(sort(table(wccat))))[1], dateadj=seq(min(dateadj, na.rm=TRUE), max(dateadj, na.rm=TRUE), length.out=100)))
        regse <- predict(reg, se.fit=TRUE, newdata=predds)
    }
    if(length(unique(cd$wccat))==1){
        reg <- gam(ordrev~s(dateadj, k=floor(sqrt(length(unique(cd$dateadj))))), weight=sqrt(wt2), data=cd)
        predds <- with(cd, data.frame(dateadj=seq(min(dateadj, na.rm=TRUE), max(dateadj, na.rm=TRUE), length.out=100)))
        regse <- predict(reg, se.fit=TRUE, newdata=predds)
    }
    ptests <- as.data.frame(t(sapply(levels(drop.levels(as.factor(cd$UniqueID))), function(x) c(date=cd$dateadj[cd$UniqueID==x][1], mn=wtd.mean(cd$ordrev[cd$UniqueID==x], cd$wt2[cd$UniqueID==x], na.rm=TRUE), n=sum(cd$wt2[cd$UniqueID==x & !is.na(cd$ordrev)])))))
    out <- list(predds=predds, predict=regse, individuals=ptests)
    out
}



plotsinglegam <- function(x, nom, showpts=TRUE, plotse=TRUE, col="Black", add=FALSE){
    title <- paste(nom, "\nN Resp =", round(sum(x$individuals$n)), "| N Questions =", length(x$individuals$n), "| N Studies =", length(unique(x$individuals$date)))
    plot(x$individuals$date, x$individuals$mn, ylab="Attitude Value", xlab="Date", main=title, axes=FALSE, type="n")
    axis(1, at=numdateplot, labels=format(dateplot, "%Y"))
    axis(3, at=numdateplot, labels=FALSE)
    axis(2)
    axis(2, c(-999, 999))
    axis(4, c(-999, 999))
    abline(h=0, lty=3, col="gray")
    if(showpts==TRUE)
        lines(x$individuals$date, x$individuals$mn, col=col, type="p")
    maxes <- x$predict$fit+1.96*x$predict$se.fit
    mins <- x$predict$fit-1.96*x$predict$se.fit
    if(plotse==TRUE)
        polygon(c(x$predds$dateadj, rev(x$predds$dateadj)), c(maxes, rev(mins)), density=40)
    lines(x$predds$dateadj, x$predict$fit, lwd=3)
}

plotsinglegamdif <- function(x, nom, showpts=TRUE, col="Black", add=FALSE, ylab="Attitude Difference"){
    title <- paste(nom)
    plot(x$individuals$date, x$individuals$mndif, ylab=ylab, xlab="Date", main=title, axes=FALSE, type="n")
    axis(1, at=numdateplot, labels=format(dateplot, "%Y"))
    axis(3, at=numdateplot, labels=FALSE)
    axis(2)
    axis(2, c(-999, 999))
    axis(4, c(-999, 999))
    abline(h=0, lty=3, col="gray")
    if(showpts==TRUE)
        lines(x$individuals$date, x$individuals$mndif, col=col, type="p")
    lines(x$predict$date, x$predict$dif, lwd=3)
}

plotmultiplegam <- function(xset, nom, showpts=FALSE, plotse=NULL, ylab="Attitude Difference", col=rep(c("Red", "Purple", "Blue", "Orange", "Black", "Dark Green", "Cyan"), 20), secol=rep(c("pink", "thistle1", "light blue", "bisque1", "light gray", "light green", "lightcyan1"), 20), lty=rep(1:6, 20), names=TRUE){
    dtrng <- range(unlist(sapply(xset, function(x) x$individuals$date)), na.rm=TRUE)
    dtrngx <- dtrng
    yrng <- range(unlist(sapply(xset, function(x) x$individuals$mn)), na.rm=TRUE)
    title <- paste(nom, "\nN Resp =", round(sum(sapply(xset, function(x) sum(x$individuals$n)))),  "| N Questions =", max(sapply(xset, function(x) length(x$individuals$n))), "| N Studies =", max(sapply(xset, function(x) length(unique(x$individuals$date)))))
    if(showpts==FALSE)
        yrng <- range(sapply(xset, function(x) x$predict$fit), na.rm=TRUE)
    if(names==TRUE)
        dtrngx[2] <- dtrng[2]+.2*(dtrng[2]-dtrng[1])
    plot(dtrngx, yrng, type="n", ylab=ylab, xlab="Date", main=title, axes=FALSE)
    axis(1, at=numdateplot, labels=format(dateplot, "%Y"))
    axis(3, at=numdateplot, labels=FALSE)
    axis(2)
    axis(2, c(-999, 999))
    axis(4, c(-999, 999))
    abline(h=0, lty=3, col="gray")
    if(showpts==TRUE)
        for(i in 1:length(xset))
            lines(xset[[i]]$individuals$date, xset[[i]]$individuals$mn, type="p", col=secol[i])
    if(length(xset)<=6 & is.null(plotse))
        plotse <- TRUE
    if(length(xset)>6 & is.null(plotse))
        plotse=FALSE
    if(plotse==TRUE)
        for(i in 1:length(xset))
            polygon(c(xset[[i]]$predds$dateadj, rev(xset[[i]]$predds$dateadj)), c(xset[[i]]$predict$fit+1.96*xset[[i]]$predict$se.fit, rev(xset[[i]]$predict$fit-1.96*xset[[i]]$predict$se.fit )), density=30, col=secol[i])
    for(i in 1:length(xset))
        lines(xset[[i]]$predds$dateadj, xset[[i]]$predict$fit, lwd=3, lty=lty[i], col=col[i])
    adjset <- rep(0, length(xset))
    if(length(xset)>20)
        adjset <- rep(c(0, .03, .06, .09, .12, .15), 20)
    if(names==TRUE)
        text(max(dtrng)+adjset[1:length(xset)], sapply(xset, function(x) rev(x$predict$fit)[1]), sapply(strsplit(names(xset), "="), function(x) paste(x[-1], collapse="-")), pos=4, cex=.85)
}

plotmultiplegamdif <- function(xset, nom, ylab="Attitude Difference", showpts=FALSE, col=rep(c("Red", "Purple", "Blue", "Orange", "Black", "Dark Green", "Cyan"), 20), secol=rep(c("pink", "thistle1", "light blue", "bisque1", "light gray", "light green", "lightcyan1"), 20), lty=rep(1:6, 20), names=TRUE){
    dtrng <- range(unlist(sapply(xset, function(x) x$individuals$date)), na.rm=TRUE)
    dtrngx <- dtrng
    yrng <- range(unlist(sapply(xset, function(x) x$individuals$mndif)), na.rm=TRUE)
    title <- paste(nom)
    if(showpts==FALSE)
        yrng <- range(sapply(xset, function(x) x$predict$dif), na.rm=TRUE)
    if(names==TRUE)
        dtrngx[2] <- dtrng[2]+.2*(dtrng[2]-dtrng[1])
    plot(dtrngx, yrng, type="n", ylab=ylab, xlab="Date", main=title, axes=FALSE)
    axis(1, at=numdateplot, labels=format(dateplot, "%Y"))
    axis(3, at=numdateplot, labels=FALSE)
    axis(2)
    axis(2, c(-999, 999))
    axis(4, c(-999, 999))
    abline(h=0, lty=3, col="gray")
    if(showpts==TRUE)
        for(i in 1:length(xset))
            lines(xset[[i]]$individuals$date, xset[[i]]$individuals$mndif, type="p", col=col[i])
    for(i in 1:length(xset))
        lines(xset[[i]]$predict$date, xset[[i]]$predict$dif, lwd=3, lty=lty[i], col=col[i])
    adjset <- rep(0, length(xset))
    if(length(xset)>20)
        adjset <- rep(c(0, .03, .06, .09, .12, .15), 20)
    if(names==TRUE)
        text(max(dtrng)+adjset[1:length(xset)], sapply(xset, function(x) rev(x$predict$dif)[1]), sapply(strsplit(names(xset), "="), function(x) paste(x[-1], collapse="-")), pos=4, cex=.85)
}




pdf("Figures/SingleVariablePlotsSTDZ.pdf", width=10, height=8, bg="white")
print("SINGLE")
singledemog <- NULL
for(c in c("Sex", "AgeCategories", "State", "Region", "Division", "Education", "RaceEthnicity", "Partisanship", "Religion", "Religionmin", "Religevang", "Religfund", "FreqAttend", "BirthDecade", "PresidentialApproval")){
    print(c)
    gid <- dummify(combineddata[[c]])
    noms <- paste(c, "=", colnames(gid), sep="")
    for(i in 1:length(noms)){
        print(noms[i])
        singledemog[[paste("Standardized Directional -", noms[i])]] <- try(gamgroupcalcstdz(gid[,i], subset=(combineddata$majorclass=="Directional Attitude")))
        singledemog[[paste("Unstandardized Directional -", noms[i])]] <- try(gamgroupcalcdir(gid[,i], subset=(combineddata$majorclass=="Directional Attitude")))
        try(plotsinglegam(singledemog[[paste("Standardized Directional -", noms[i])]], nom=paste("Standardized Directional -", noms[i])))
        try(plotsinglegam(singledemog[[paste("Unstandardized Directional -", noms[i])]], nom=paste("Unstandardized Directional -", noms[i])))
        singledemog[[paste("Standardized Strength -", noms[i])]] <- try(gamgroupcalcstdz(gid[,i], subset=(combineddata$majorclass=="Attitude Strength")))
        singledemog[[paste("Unstandardized Strength -", noms[i])]] <- try(gamgroupcalcdir(gid[,i], subset=(combineddata$majorclass=="Attitude Strength")))
        try(plotsinglegam(singledemog[[paste("Standardized Strength -", noms[i])]], nom=paste("Standardized Strength -", noms[i])))
        try(plotsinglegam(singledemog[[paste("Unstandardized Strength -", noms[i])]], nom=paste("Unstandardized Strength -", noms[i])))
    }
}
for(c in c('bigcats')){
    print(c)
    gid <- dummify(combineddata[[c]])
    noms <- paste(c, "=", colnames(gid), sep="")
    for(i in 1:length(noms)){
        print(noms[i])
        singledemog[[noms[i]]] <- try(gamgroupcalcdir(gid[,i]))
        try(plotsinglegam(singledemog[[noms[i]]], nom=noms[i]))
    }
}
dev.off()

singledemog <- singledemog[sapply(singledemog, class)!="try-error"]
save(singledemog, file="EachFileExports/11-01_SingleDemographicExports")

pdf("Figures/VariablePlotsSTDZ.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(singledemog), "="), function(x) x))
for(i in unique(sets[,1])){
    xset <- singledemog[grepl(paste(i, "=", sep=""), names(singledemog))]
    try(plotmultiplegam(xset, nom=i))
}
dev.off()

pdf("Figures/VariablePlotsSTDZpts.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(singledemog), "="), function(x) x))
for(i in unique(sets[,1])){
    xset <- singledemog[grepl(paste(i, "=", sep=""), names(singledemog))]
    try(plotmultiplegam(xset, nom=i, showpts=TRUE))
}
dev.off()



doubledemog <- NULL
for(c in c("Sex", "AgeCategories", "State", "Region", "Division", "Education", "RaceEthnicity", "Partisanship", "Religion", "Religionmin", "Religevang", "Religfund", "FreqAttend", "BirthDecade", "PresidentialApproval", "wccat", "bigcats")){
    print(c)
    gid <- dummify(combineddata[[c]])
    noms <- paste(c, "=", colnames(gid), sep="")
    for(i in 1:length(noms)){
        print(noms[i])
        for(d in c('Sex', 'Partisanship')){
            print(d)
            gid2 <- dummify(combineddata[[d]])
            noms2 <- paste(d, "=", colnames(gid2), sep="")
            for(j in 1:length(noms2)){
                print(noms2[j])
                doubledemog[[paste("Directional - ", noms[i], "; ", noms2[j], sep="")]] <- try(gamgroupcalcstdz(gid[,i]&gid2[,j], subset=(combineddata$majorclass=="Directional Attitude")))
                doubledemog[[paste("Strength - ", noms[i], "; ", noms2[j], sep="")]] <- try(gamgroupcalcstdz(gid[,i]&gid2[,j], subset=(combineddata$majorclass=="Attitude Strength")))
            }
        }
    }
}


doubledemog <- doubledemog[sapply(doubledemog, class)!="try-error"]
save(doubledemog, file="EachFileExports/10-02_DoubleDemographicExports")

partisandirdoubles <- doubledemog[grepl("; Partisanship", names(doubledemog)) & grepl("Directional", names(doubledemog))]
partisanstrdoubles <- doubledemog[grepl("; Partisanship", names(doubledemog)) & grepl("Strength", names(doubledemog))]
sexdirdoubles <- doubledemog[grepl("; Sex", names(doubledemog)) & grepl("Directional", names(doubledemog))]
sexstrdoubles <- doubledemog[grepl("; Sex", names(doubledemog)) & grepl("Strength", names(doubledemog))]


pdf("Figures/PartisanVariablePlotsSTDZ.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(partisandirdoubles), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- partisandirdoubles[grepl(paste(i, "=", sep=""), names(partisandirdoubles))]
    names(xset) <- gsub("; Partisanship=Republican", "-R", gsub("; Partisanship=Democrat", "-D", gsub("; Partisanship=Independent", "-I", names(xset))))
    try(plotmultiplegam(xset, nom=paste(i, "x Partisanship"), col=rep(c("red", "purple", "blue"), 100), lty=rep(1:4, 100), names=TRUE))
}
sets <- t(sapply(strsplit(names(partisanstrdoubles), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- partisanstrdoubles[grepl(paste(i, "=", sep=""), names(partisanstrdoubles))]
    names(xset) <- gsub("; partisan3=Republican", "-R", gsub("; partisan3=Democrat", "-D", gsub("; partisan3=Independent", "-I", names(xset))))
    try(plotmultiplegam(xset, nom=paste(i, "x Partisanship"), col=rep(c("red", "purple", "blue"), 100), lty=rep(1:4, 100), names=TRUE))
}
dev.off()

pdf("Figures/SexVariablePlotsSTDZ.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(sexdirdoubles), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- sexdirdoubles[grepl(paste(i, "=", sep=""), names(sexdirdoubles))]
    names(xset) <- gsub("; Sex=Male", "-M", gsub("; Sex=Female", "-F", names(xset)))
    try(plotmultiplegam(xset, nom=paste(i, "x Sex"), secol=rep(c("pink", "light blue", "bisque1", "lightcyan1", "thistle1", "light green", "light gray"), 20), col=rep(c("red", "blue"), 100), lty=rep(1:5, 100), names=TRUE))
}
sets <- t(sapply(strsplit(names(sexstrdoubles), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- sexstrdoubles[grepl(paste(i, "=", sep=""), names(sexstrdoubles))]
    names(xset) <- gsub("; Sex=Male", "-M", gsub("; Sex=Female", "-F", names(xset)))
    try(plotmultiplegam(xset, nom=paste(i, "x Sex"), secol=rep(c("pink", "light blue", "bisque1", "lightcyan1", "thistle1", "light green", "light gray"), 20), col=rep(c("red", "blue"), 100), lty=rep(1:5, 100), names=TRUE))
}
dev.off()



# Differences By Partisanship and Sex


# Sex Directional
sets <- t(sapply(strsplit(names(sexdirdoubles), "=|; "), function(x) x))
getfem <- sexdirdoubles[sets[,4]=="Female"]
getmal <- sexdirdoubles[sets[,4]=="Male"]
names(getfem) <- apply(sets[,1:2][sets[,4]=="Female",], 1, function(x) paste(x, collapse="="))
names(getmal) <- apply(sets[,1:2][sets[,4]=="Male",], 1, function(x) paste(x, collapse="="))
sexdifs <- lapply(names(getfem)[names(getfem) %in% names(getmal)], function(n) try(list(predict=data.frame(date=getfem[[n]]$predds$dateadj, dif=(getfem[[n]]$predict$fit-getmal[[n]]$predict$fit)), individuals=data.frame(date=getfem[[n]]$individuals$date, mndif=(getfem[[n]]$individuals$mn-getmal[[n]]$individuals$mn)))))
names(sexdifs) <- names(getfem)[names(getfem) %in% names(getmal)]

sdouts <- sexdifs[sapply(sexdifs, class)!="try-error"]

pdf("Figures/SexDifferencesDirectionalSTDZ.pdf", width=12, height=7, bg="white")
for(i in 1:length(sdouts))
    try(plotsinglegamdif(sdouts[[i]], paste("Sex Differences:", names(sdouts)[i]), ylab="Female - Male"))
dev.off()

pdf("Figures/SexDifferencesDirectionalSTDZCombined.pdf", width=12, height=7, bg="white")
for(i in unique(sets[,1]))
    try(plotmultiplegamdif(sdouts[grepl(paste(i, "=", sep=""), names(sdouts))], paste("Sex Differences:", i), ylab="Female - Male"))
dev.off()

#Sex Strength

sets <- t(sapply(strsplit(names(sexstrdoubles), "=|; "), function(x) x))
getfem <- sexstrdoubles[sets[,4]=="Female"]
getmal <- sexstrdoubles[sets[,4]=="Male"]
names(getfem) <- apply(sets[,1:2][sets[,4]=="Female",], 1, function(x) paste(x, collapse="="))
names(getmal) <- apply(sets[,1:2][sets[,4]=="Male",], 1, function(x) paste(x, collapse="="))
sexdifs <- lapply(names(getfem)[names(getfem) %in% names(getmal)], function(n) try(list(predict=data.frame(date=getfem[[n]]$predds$dateadj, dif=(getfem[[n]]$predict$fit-getmal[[n]]$predict$fit)), individuals=data.frame(date=getfem[[n]]$individuals$date, mndif=(getfem[[n]]$individuals$mn-getmal[[n]]$individuals$mn)))))
names(sexdifs) <- names(getfem)[names(getfem) %in% names(getmal)]

sdouts <- sexdifs[sapply(sexdifs, class)!="try-error"]

pdf("Figures/SexDifferencesStrengthSTDZ.pdf", width=12, height=7, bg="white")
for(i in 1:length(sdouts))
    try(plotsinglegamdif(sdouts[[i]], paste("Sex Differences:", names(sdouts)[i]), ylab="Female - Male"))
dev.off()

pdf("Figures/SexDifferencesStrengthSTDZCombined.pdf", width=12, height=7, bg="white")
for(i in unique(sets[,1]))
    try(plotmultiplegamdif(sdouts[grepl(paste(i, "=", sep=""), names(sdouts))], paste("Sex Differences:", i), ylab="Female - Male"))
dev.off()



# Partisan Directional
sets <- t(sapply(strsplit(names(partisandirdoubles), "=|; "), function(x) x))
getdem <- partisandirdoubles[sets[,4]=="Democrat"]
getrep <- partisandirdoubles[sets[,4]=="Republican"]
names(getdem) <- apply(sets[,1:2][sets[,4]=="Democrat",], 1, function(x) paste(x, collapse="="))
names(getrep) <- apply(sets[,1:2][sets[,4]=="Republican",], 1, function(x) paste(x, collapse="="))
partdifs <- lapply(names(getdem)[names(getdem) %in% names(getrep)], function(n) try(list(predict=data.frame(date=getdem[[n]]$predds$dateadj, dif=(getdem[[n]]$predict$fit-getrep[[n]]$predict$fit)), individuals=data.frame(date=getdem[[n]]$individuals$date, mndif=(getdem[[n]]$individuals$mn-getrep[[n]]$individuals$mn)))))
names(partdifs) <- names(getdem)[names(getdem) %in% names(getrep)]

pdouts <- partdifs[sapply(partdifs, class)!="try-error"]

pdf("Figures/PartisanDifferencesDirectionalSTDZ.pdf", width=12, height=7, bg="white")
for(i in 1:length(pdouts))
    try(plotsinglegamdif(pdouts[[i]], paste("Partisan Differences:", names(pdouts)[i]), ylab="Democrat - Republican"))
dev.off()

pdf("Figures/PartisanDifferencesDirectionalSTDZCombined.pdf", width=12, height=7, bg="white")
for(i in unique(sets[,1]))
    try(plotmultiplegamdif(pdouts[grepl(paste(i, "=", sep=""), names(pdouts))], paste("Partisan Differences:", i), ylab="Democrat - Republican"))
dev.off()

#Partisan Strength

sets <- t(sapply(strsplit(names(partisanstrdoubles), "=|; "), function(x) x))
getdem <- partisanstrdoubles[sets[,4]=="Democrat"]
getrep <- partisanstrdoubles[sets[,4]=="Republican"]
names(getdem) <- apply(sets[,1:2][sets[,4]=="Democrat",], 1, function(x) paste(x, collapse="="))
names(getrep) <- apply(sets[,1:2][sets[,4]=="Republican",], 1, function(x) paste(x, collapse="="))
partdifs <- lapply(names(getdem)[names(getdem) %in% names(getrep)], function(n) try(list(predict=data.frame(date=getdem[[n]]$predds$dateadj, dif=(getdem[[n]]$predict$fit-getrep[[n]]$predict$fit)), individuals=data.frame(date=getdem[[n]]$individuals$date, mndif=(getdem[[n]]$individuals$mn-getrep[[n]]$individuals$mn)))))
names(partdifs) <- names(getdem)[names(getdem) %in% names(getrep)]

pdouts <- partdifs[sapply(partdifs, class)!="try-error"]

pdf("Figures/PartisanDifferencesStrengthSTDZ.pdf", width=12, height=7, bg="white")
for(i in 1:length(pdouts))
    try(plotsinglegamdif(pdouts[[i]], paste("Partisan Differences:", names(pdouts)[i]), ylab="Democrat - Republican"))
dev.off()

pdf("Figures/PartisanDifferencesStrengthSTDZCombined.pdf", width=12, height=7, bg="white")
for(i in unique(sets[,1]))
    try(plotmultiplegamdif(pdouts[grepl(paste(i, "=", sep=""), names(pdouts))], paste("Partisan Differences:", i), ylab="Democrat - Republican"))
dev.off()








doubledemogdir <- NULL
for(c in c("Sex", "AgeCategories", "State", "Region", "Division", "Education", "RaceEthnicity", "Partisanship", "Religion", "Religionmin", "Religevang", "Religfund", "FreqAttend", "BirthDecade", "PresidentialApproval", "wccat", "bigcats")){
    print(c)
    gid <- dummify(combineddata[[c]])
    noms <- paste(c, "=", colnames(gid), sep="")
    for(i in 1:length(noms)){
        print(noms[i])
        for(d in c('Sex', 'Partisanship')){
            print(d)
            gid2 <- dummify(combineddata[[d]])
            noms2 <- paste(d, "=", colnames(gid2), sep="")
            for(j in 1:length(noms2)){
                print(noms2[j])
                doubledemogdir[[paste("Directional - ", noms[i], "; ", noms2[j], sep="")]] <- try(gamgroupcalcdir(gid[,i]&gid2[,j], subset=(combineddata$majorclass=="Directional Attitude")))
                doubledemogdir[[paste("Strength - ", noms[i], "; ", noms2[j], sep="")]] <- try(gamgroupcalcdir(gid[,i]&gid2[,j], subset=(combineddata$majorclass=="Attitude Strength")))
            }
        }
    }
}


doubledemogdir <- doubledemogdir[sapply(doubledemogdir, class)!="try-error"]
save(doubledemogdir, file="EachFileExports/10-02_doubledemogdirraphicExports")

partisandirdoublesdir <- doubledemogdir[grepl("; Partisanship", names(doubledemogdir)) & grepl("Directional", names(doubledemogdir))]
partisanstrdoublesdir <- doubledemogdir[grepl("; Partisanship", names(doubledemogdir)) & grepl("Strength", names(doubledemogdir))]
sexdirdoublesdir <- doubledemogdir[grepl("; Sex", names(doubledemogdir)) & grepl("Directional", names(doubledemogdir))]
sexstrdoublesdir <- doubledemogdir[grepl("; Sex", names(doubledemogdir)) & grepl("Strength", names(doubledemogdir))]


pdf("Figures/PartisanVariablePlotsDIR.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(partisandirdoublesdir), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- partisandirdoublesdir[grepl(paste(i, "=", sep=""), names(partisandirdoublesdir))]
    names(xset) <- gsub("; Partisanship=Republican", "-R", gsub("; Partisanship=Democrat", "-D", gsub("; Partisanship=Independent", "-I", names(xset))))
    try(plotmultiplegam(xset, nom=paste(i, "x Partisanship"), col=rep(c("red", "purple", "blue"), 100), lty=rep(1:4, 100), names=TRUE))
}
sets <- t(sapply(strsplit(names(partisanstrdoublesdir), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- partisanstrdoublesdir[grepl(paste(i, "=", sep=""), names(partisanstrdoublesdir))]
    names(xset) <- gsub("; partisan3=Republican", "-R", gsub("; partisan3=Democrat", "-D", gsub("; partisan3=Independent", "-I", names(xset))))
    try(plotmultiplegam(xset, nom=paste(i, "x Partisanship"), col=rep(c("red", "purple", "blue"), 100), lty=rep(1:4, 100), names=TRUE))
}
dev.off()

pdf("Figures/SexVariablePlotsDIR.pdf", width=12, height=7, bg="white")
sets <- t(sapply(strsplit(names(sexdirdoublesdir), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- sexdirdoublesdir[grepl(paste(i, "=", sep=""), names(sexdirdoublesdir))]
    names(xset) <- gsub("; Sex=Male", "-M", gsub("; Sex=Female", "-F", names(xset)))
    try(plotmultiplegam(xset, nom=paste(i, "x Sex"), secol=rep(c("pink", "light blue", "bisque1", "lightcyan1", "thistle1", "light green", "light gray"), 20), col=rep(c("red", "blue"), 100), lty=rep(1:5, 100), names=TRUE))
}
sets <- t(sapply(strsplit(names(sexstrdoublesdir), "=|; "), function(x) x))
for(i in unique(sets[,1])){
    xset <- sexstrdoublesdir[grepl(paste(i, "=", sep=""), names(sexstrdoublesdir))]
    names(xset) <- gsub("; Sex=Male", "-M", gsub("; Sex=Female", "-F", names(xset)))
    try(plotmultiplegam(xset, nom=paste(i, "x Sex"), secol=rep(c("pink", "light blue", "bisque1", "lightcyan1", "thistle1", "light green", "light gray"), 20), col=rep(c("red", "blue"), 100), lty=rep(1:5, 100), names=TRUE))
}
dev.off()
