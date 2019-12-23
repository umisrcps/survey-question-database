library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(mgcv)
library(Amelia)

load("EachFileExports/07-01_AbortionVariablesCategorized.rdata")

addQ <- with(av, data.frame(UniqueID, wordingsimp))

load("EachFileExports/09_IndividualMaster.rdata")
load("../Analysis from shared drive/Analysis/Genfiles/ResponseTranslations.rdata")

gomin <- globalbyoutcomeprimary[!is.na(globalbyoutcomeprimary$cleaned) & globalbyoutcomeprimary$wccat!="Excluded",]
gomin$indivID <- paste("i", 1:dim(gomin)[1], sep="_")
gm <- merge(gomin, addQ, all.x=TRUE)

gm$consistentresp <- as.character(gm$original)
for(i in 1:dim(tm)[1])
    gm$consistentresp[gm$original==tm[i,"respalls"]] <- as.character(tm[i,"ra"])

gm$consistentresp[grepl("dk/ref|0 inap|9 na; rf|8 dk|dont know|9 refused|dk/rf|9 rf|0 na inap", gm$consistentresp) | gm$consistentresp %in% c("refused", "refused ", "don", "na", "nra", "_na_", "__na__", "&", "")] <- "nonsub"

gmx <- gm[gm$consistentresp!="nonsub" & !is.na(gm$consistentresp),]

respsc <- sapply(unique(gmx$UniqueID), function(x) sort(unique(gmx$consistentresp[gm$UniqueID==x])))

respsccollapse <- sapply(respsc, function(x) paste(x, collapse=" | "))

load("../Analysis from shared drive/Analysis/scripts/ordinalities.rdata")



dsseps <- lapply(unique(gmx$UniqueID), function(x) gmx[gmx$UniqueID==x,])






set.seed(747474747)
primaryoutcomeimps <- amelia(gm[!is.na(gomin$numdt),], idvars=c('UniqueID', 'outcome', 'cleaned', 'original', 'wccat', 'bigcats', 'majorclass', 'sd', "hisp", "presapprove", "partystrength", "partisan", "region", "division", "indivID", "consistentresp"), ts='numdt', noms=c("Sex", "state", "relig", "Race", "marital", "religfund", "regionplus", "divisionplus", "partisan3", "religfreq"), ords=c("educ"), m=1, polytime=3)
save(primaryoutcomeimps, file="EachFileExports/Imputation1.rdata")
load("EachFileExports/Imputation1.rdata")

impvers <- primaryoutcomeimps$imputations$imp1
names(impvers) <- paste("imp", names(impvers), sep="")

studysds <- sapply(unique(impvers$impUniqueID), function(i) sd(impvers$impcleaned[impvers$impUniqueID==i]))
names(studysds) <- unique(impvers$impUniqueID)
impvers$impcleaned[impvers$impUniqueID %in% names(studysds[studysds==0])] <- NA


impvers$legaloutcome <- NA
impvers$legaloutcome[grepl("leg", impvers$imporiginal)] <- 1
impvers$legaloutcome[grepl("illeg|not leg|never", impvers$imporiginal)] <- 0

legalpred <- with(impvers, glm(legaloutcome~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt+eval(impnumdt^2)+eval(impnumdt^3)), family="binomial"))

legpredict <- predict(legalpred, newdata=impvers)


impvers$importoutcome <- NA
impvers$importoutcome[grepl("impo", impvers$imporiginal)] <- 1
impvers$importoutcome[grepl("somewhat|a little|slightly|one of many", impvers$imporiginal) & grepl("impo", impvers$imporiginal)] <- .5
impvers$importoutcome[grepl("not", impvers$imporiginal) & grepl("impo", impvers$imporiginal)] <- 0


importpred <- with(impvers, glm(importoutcome~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt)))

importpredict <- predict(importpred, newdata=impvers)


impvers$imporiginaldrops <- impvers$imporiginal
impvers$imporiginaldrops[grepl("dont| rf|dk|refused|inap|9 na|no opinion|no answer|no response", impvers$imporiginal)] <- NA

impvers$newstart <- NA
for(i in unique(impvers$impUniqueID)){
    print(i)
    if(length(levels(drop.levels(as.factor(impvers$imporiginaldrops[impvers$impUniqueID==i & !is.na(impvers$imporiginaldrops)])))))
        impvers$imporiginaldrops[impvers$impUniqueID==i & is.na(impvers$imporiginaldrops)] <- "ANOTHER ANSWER"
    eachresp <- sapply(levels(drop.levels(as.factor(impvers$imporiginaldrops[impvers$impUniqueID==i & !is.na(impvers$imporiginaldrops)]))), function(x) c(import=mean(importpredict[impvers$imporiginaldrops[impvers$impUniqueID==i]==x], na.rm=TRUE), legal=mean(legpredict[impvers$imporiginaldrops[impvers$impUniqueID==i]==x], na.rm=TRUE)))
    eachsd <- apply(eachresp, 1, sd)/c(sd(importpredict[impvers$impUniqueID==i], na.rm=TRUE), sd(legpredict[impvers$impUniqueID==i], na.rm=TRUE))
    biggervar <- names(eachsd)[eachsd==max(eachsd)]
    print(biggervar)
    srb <- sort(eachresp[biggervar,])
    print(srb)
    for(n in names(srb))
        impvers$newstart[impvers$impUniqueID==i & impvers$imporiginaldrops==n] <- srb[names(srb)==n]
}








impvers$availoutcome <- NA
impvers$availoutcome[grepl("available", impvers$imporiginal)] <- 1
impvers$availoutcome[grepl("available but", impvers$imporiginal)] <- .5
impvers$availoutcome[grepl("not permitted", impvers$imporiginal)] <- 0

with(impvers, summary(!is.na(importoutcome)|!is.na(availoutcome)|!is.na(legaloutcome)))


availpred <- with(impvers, glm(availoutcome~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt+eval(impnumdt^2)+eval(impnumdt^3))))

availpredict <- predict(availpred, newdata=impvers)




with(impvers, rev(sort(table(imporiginal[is.na(importoutcome) & is.na(legaloutcome)])))[1:30])


globalpred <- with(impvers, lm(impcleaned~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt+eval(impnumdt^2)+eval(impnumdt^3))))

gps <- predict(globalpred, newdata=impvers)


imprev <- impvers$impcleaned

for(i in names(studysds[studysds!=0])){
    print(i)
    print(try(cor(gps[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")))
    if(cor(gps[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")<0){
        imprev[impvers$impUniqueID==i] <- -imprev[impvers$impUniqueID==i]
        print("rev")
    }
}

globalpred2 <- with(impvers, lm(imprev~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt+eval(impnumdt^2)+eval(impnumdt^3))))

gps2 <- predict(globalpred2)

for(i in names(studysds[studysds!=0])){
    print(i)
    print(try(cor(gps2[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")))
    if(cor(gps2[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")<0){
        imprev[impvers$impUniqueID==i] <- -imprev[impvers$impUniqueID==i]
        print("rev")
    }
}

globalpred3 <- with(impvers, lm(imprev~(impSex+impdivisionplus+impRace+impeduc+impage+imppartisan3+impreligfund+imprelig+impreligfreq)*(impnumdt+eval(impnumdt^2)+eval(impnumdt^3))))

gps3 <- predict(globalpred2)

for(i in names(studysds[studysds!=0])){
    print(i)
    print(try(cor(gps3[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")))
    if(cor(gps3[impvers$impUniqueID==i], imprev[impvers$impUniqueID==i], use="pairwise.complete.obs")<0){
        imprev[impvers$impUniqueID==i] <- -imprev[impvers$impUniqueID==i]
        print("rev")
    }
}



findataset <- data.frame(gomin[(gomin$indivID %in% impvers$impindivID),], imprev)[(gomin$UniqueID %in% names(studysds[studysds!=0])),]
fds <- findataset[!is.na(findataset$imprev),]

save(fds, file="EachFileExports/10_IndividualPlusRecodes.rdata")





#reg1 <- with(globalbyoutcomeprimary, lm(cleaned~Sex*partisan3*(numdt+eval(numdt^2)+eval(numdt^3))))

#plotwtdinteraction(reg1, "numdt", "partisan3", "Sex", addat=TRUE)





