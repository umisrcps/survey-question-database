library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(Amelia)

load("EachFileExports/04-01_MergedDataFiles.rdata")
load("EachFileExports/07-01_AbortionVariablesCategorized.rdata")
load("EachFileExports/08-01_CovariatesCategorized.rdata")

avimppre <- av[av$datatype!="Topline",]

# Import Abortion Variable Individual Data

importabortvarspre <- lapply((1:dim(av)[1])[av$datatype!="Topline"], function(i) try(tolower(gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", as.character(unlist(dfs[[av[i,]$StudyID]][,av[i,]$Varname]))))))), fixed=TRUE), fixed=TRUE)), silent=TRUE))

importabortvars <- importabortvarspre[sapply(importabortvarspre, class)!="try-error"]
avimp <- avimppre[sapply(importabortvarspre, class)!="try-error",]

recomb <- rbindlist(list(avimp, infodf), fill=TRUE)

orderpreds <- function(outcome, predictor){ #PCA First Factor to Order Responses
    pred <- predictor[,sapply(predictor, function(x) length(unique(x[!is.na(x)]))>1)]
    if(length(levels(as.factor(outcome)))<20){
        ocpreds <- sapply(levels(as.factor(outcome)), function(y) try(predict(lm(eval(outcome==y & !is.na(y))~.,data=pred))))
        ocpredcors <- eigen(cor(ocpreds[,(apply(ocpreds, 2, sd)>0)], use="pairwise.complete.obs"))$vectors[,1]
        names(ocpredcors) <- levels(as.factor(outcome))[apply(ocpreds, 2, sd)>0]
        newout <- rep(NA, length(outcome))
        for(g in names(ocpredcors))
            newout[outcome==g] <- ocpredcors[names(ocpredcors)==g]
    }
    if(length(levels(as.factor(outcome)))>=20){
        abortdummy <- grepl("abort|pro choice|pro life|pro-life|pro-choice|unborn", outcome)
        if(sum(abortdummy)>0)
            newout <- factor(as.numeric(abortdummy), levels=0:1, labels=c("Not Mentioned", "Mentioned"))
        else
            newout <- outcome
    }      
    newout
}

load("../Analysis from shared drive/Analysis/Genfiles/ResponseTranslations.rdata")


cleanoutcome <- function(x){
    for(i in unique(x[x %in% tm[,"respalls"]]))
        x[x==i] <- tm[tm[,"respalls"]==i,"ra"]
    x[grepl("dk/ref|0 inap|9 na; rf|8 dk|dont know|9 refused|dk/rf|9 rf|0 na inap", x) | x %in% c("refused", "refused ", "don", "na", "nra", "_na_", "__na__", "&", "")] <- "nonsub"
    x[x=="nonsub"] <- NA
    x[x<1] <- NA
    x
}
        
# Build Study Level Datasets


keepstudiesinfo <- infodf[!(grepl("ohio|cali|ariz|caro|virgin|jersey|grades|class of|grades|york|wisc|flor|maryland|hampshire|illinois|indiana|miss|female|male|residents in|women|of men|students|iowa|minn|penn|texas|hispan|primary|priests|repub|democrat|black|afric|teen|latin|county|watched the|catholic|mormon|jewish|youth|parent|rhode|border|south|alab|georg|husband|delega|jews|elites|valley|kent|adult men|young|hispan|asian|angeles|catholic", tolower(infodf$sample)) & !grepl("oversample",  tolower(infodf$sample))) & !grepl("national hispanics age 16 and older including an oversample of 1240 hispanics ages 16-25", tolower(infodf$sample)),]

keepstudiesinfo$includer <- keepstudiesinfo$include==1 | is.na(keepstudiesinfo$include)

names(rev(sort(table(tolower(keepstudiesinfo$sample)))))
                          
usablestudies <- unique(avimp$StudyID[(avimp$StudyID %in% keepstudiesinfo$StudyID)])

studyleveldatasets <- predictordatasets <- outcomedatasets <- studylevelinfo <- as.list(rep(NA, length(usablestudies)))
names(studyleveldatasets) <- names(studylevelinfo) <- names(predictordatasets) <- names(outcomedatasets) <- usablestudies
for(i in usablestudies){
    print(i)
    studyleveldatasets[[i]] <- try(data.frame(importabortvars[avimp$StudyID==i], varset[infodf$StudyID==i]))
    predictordatasets[[i]] <- try(as.data.frame(varset[infodf$StudyID==i]))
    outcomedatasets[[i]] <- try(as.data.frame(lapply(importabortvars[avimp$StudyID==i], cleanoutcome)))
    colnames(studyleveldatasets[[i]]) <- try(recomb$Varname[recomb$StudyID==i])
    colnames(predictordatasets[[i]]) <- try(infodf$Varname[infodf$StudyID==i])
    colnames(outcomedatasets[[i]]) <- try(avimp$Varname[avimp$StudyID==i])
    studylevelinfo[[i]] <- try(recomb[recomb$StudyID==i,])
}

i <- avimp$StudyID[avimp$sd=="2018-07-17"][1]


predmerge <- function(x){
    classx <- sapply(x, class)[1][1]
    y <- x
    if(classx %in% c("numeric"))
        y <- apply(x, 1, function(g) quantile(g[!is.na(g)], .5, na.rm=TRUE))
    #if(classx %in% c("ordered")){
    #    levs <- levels(x[,1])
    #    med <- round(rowMeans(sapply(x, as.numeric), na.rm=TRUE), 0)
    #    y <- ordered(med, 1:length(levs), levs)
    #}
    if(classx %in% c("factor", "logical", "ordered")){
        y <- factor(as.character(apply(x, 1, function(g) randmax(g))), levels=levels(as.factor(unlist(x))))
        y[y=="NAX"] <- NA
        y <- drop.levels(y, reorder=FALSE)
    }
    y
}

pmdetermine <- function(x){
    if(dim(as.data.frame(as.list(x)))[2]>1){

        chx <- chx2 <- as.data.frame(as.list(x))
        out1 <- predmerge(chx)
        chx2[chx=="other"] <- NA
        out2 <-  predmerge(chx2)
        out <- as.character(out2)
        out[is.na(out2) & !is.na(out1)] <- as.character(out1[is.na(out2) & !is.na(out1)])
        }
    else
        out <- x
    out
}

randmax <- function(x){
    tbx <- table(c(x[!is.na(x)], x[!is.na(x)], "NAX"))
    sample(names(tbx[tbx==max(tbx)]), 1)
}

## Combine each type of predictor in each survey

scomb <- NULL
for(i in usablestudies){
    print(i)
    tempdir <- NULL
    for(c in unique(infodf$codedclass[infodf$StudyID==i])){
        print(c)
        tempdir[[c]] <- unlist(pmdetermine(varset[infodf$codedclass==c & infodf$StudyID==i]))
    }
    names(tempdir) <- unique(infodf$codedclass[infodf$StudyID==i])
    scomb[[i]] <- data.frame(StudyID=i, date=infodf$sd[infodf$StudyID==i][1], sample=infodf$sample[infodf$StudyID==i][1], mode=infodf$mode[infodf$StudyID==i][1], tempdir, stringsAsFactors=FALSE)
}

scombcatns <- lapply(scomb, function(x) data.frame(t(sapply(x, function(g) length(unique(g[!is.na(g)]))))))
catnsets <- rbindlist(scombcatns, fill=TRUE)
rownames(catnsets) <- names(scomb)

unqrace <- lapply(scomb, function(x) try(as.character(unique(x$Race))))

racbw <- sapply(unqrace, function(x) sum(x=="white")+sum(x=="black"))

dropreligfreqs <- rownames(catnsets)[catnsets$religfreq<4 & !is.na(catnsets$religfreq)]
dropages <- rownames(catnsets)[catnsets$age<20 & !is.na(catnsets$age)]
droprac <- names(racbw)[racbw<2 | is.na(racbw)]

for(i in droprac)
    scomb[[i]]$Race <- NA
for(i in dropages)
    scomb[[i]]$Age <- NA
for(i in dropreligfreqs)
    scomb[[i]]$religfreq <- NA
    



## Build Global Datset of Predictors

globalpreddatapre <- rbindlist(scomb, fill=TRUE)

globalpreddatapre$partisan3 <- ordered(grepl("Rep", globalpreddatapre$partisan)+2*grepl("Ind", globalpreddatapre$partisan)+3*grepl("Dem", globalpreddatapre$partisan), 1:3, c("Republican", "Independent", "Democrat"))

globalpreddatapre$numdt <- nalevs(as.numeric(globalpreddatapre$date))

globalpreddatapre$regionplus <- globalpreddatapre$region
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$division %in% c("East North Central", "West North Central"))] <- "Midwest"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$division %in% c("South Atlantic", "East South Central", "West South Central"))] <- "South"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$division %in% c("New England", "Middle Atlantic"))] <- "Northeast"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$division %in% c("Pacific", "Mountain"))] <- "West"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$state %in% c("WA", "MT", "ID", "OR", "WY", "CA", "NV", "UT", "CO", "AZ", "NM", "AK", "HI"))] <- "West"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$state %in%  c("ND", "SD", "MN", "NE", "IA", "KS", "MO", "WI", "MI", "IL", "IN", "OH"))] <- "Midwest"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$state %in% c("TX", "OK", "AR", "LA", "MS", "AL", "GA", "FL", "KY", "TN", "SC", "NC", "VA", "WV", "MD", "DE", "DC"))] <- "South"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$state %in% c("PA", "NJ", "NY", "CT", "MA", "RI", "VT", "NH", "ME"))] <- "Northeast"
globalpreddatapre$regionplus[is.na(globalpreddatapre$region) & (globalpreddatapre$state %in% c("WA", "MT", "ID", "OR", "WY", "CA", "NV", "UT", "CO", "AZ", "NM", "AK", "HI"))] <- "West"

globalpreddatapre$divisionplus <- globalpreddatapre$division
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("MT", "ID","WY","NV", "UT", "CO", "AZ", "NM"))] <- "Mountain"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("WA", "OR", "CA", "AK", "HI"))] <- "Pacific"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("WI", "MI", "IL", "IN", "OH"))] <- "East North Central"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("ND", "SD", "MN", "NE", "IA", "KS", "MO"))] <- "West North Central"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("TX", "OK", "AR", "LA"))] <- "West South Central"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("MS", "AL", "KY", "TN"))] <- "East South Central"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("GA", "FL", "SC", "NC", "VA", "WV", "MD", "DE", "DC"))] <- "South Atlantic"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("PA", "NJ", "NY"))] <- "Middle Atlantic"
globalpreddatapre$divisionplus[is.na(globalpreddatapre$division) & (globalpreddatapre$state %in% c("CT", "MA", "RI", "VT", "NH", "ME"))] <- "New England"

globalpreddatapre$agecats <- cut(as.numeric(as.character(globalpreddatapre$age)), c(17.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 999), c("Age 18-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64", "Age 65-74", "Age 75+"))

globalpreddatapre$birthyear <- as.numeric(format(globalpreddatapre$date, "%Y"))-as.numeric(as.character(globalpreddatapre$age))

globalpreddatapre$birthdecade <- cut(globalpreddatapre$birthyear, c(1800, seq(1890.5, 1990.5, 10), 2050), c("Before 1890", paste((189:198)*10, "s", sep=""), "After 1990"))

globalpreddatapre$PresidentialApproval <- factor((globalpreddatapre$presapprove %in% c("Approve", "Strongly Approve", "Somewhat Approve"))+2*(globalpreddatapre$presapprove %in% c("Disapprove", "Strongly Disapprove", "Somewhat Disapprove")), 1:2, c("Approve", "Disapprove"))

globalpreddatapre$wt <- globalpreddatapre$Weight
globalpreddatapre$wt[is.na(globalpreddatapre$Weight)] <- 1

globalpreddatapre$raceeth <- globalpreddatapre$Race
globalpreddatapre$raceeth[globalpreddatapre$hisp==TRUE & !is.na(globalpreddatapre$hisp)] <- "hispanic"

globalpreddatapre$religcombo <- as.character(globalpreddatapre$relig)
globalpreddatapre$religcombo[grepl("protest|cath|chris|other", globalpreddatapre$relig)] <- paste("Unspecified", as.character(globalpreddatapre$relig))[grepl("protest|cath|chris|other", globalpreddatapre$relig)]
globalpreddatapre$religcombo[globalpreddatapre$religfund=="Yes" & !is.na(globalpreddatapre$religfund) & grepl("protest|cath|chris|other", globalpreddatapre$relig)] <- paste("Evangelical", globalpreddatapre$relig)[globalpreddatapre$religfund=="Yes" & !is.na(globalpreddatapre$religfund) & grepl("protest|cath|chris|other", globalpreddatapre$relig)]
globalpreddatapre$religcombo[globalpreddatapre$religfund=="No" & !is.na(globalpreddatapre$religfund) & grepl("protest|cath|chris|other", globalpreddatapre$relig)] <- paste("Non-Evangelical", globalpreddatapre$relig)[globalpreddatapre$religfund=="No" & !is.na(globalpreddatapre$religfund) & grepl("protest|cath|chris|other", globalpreddatapre$relig)]
globalpreddatapre$religcombo[is.na(globalpreddatapre$relig)] <- NA
globalpreddatapre$religcombo <- as.factor(globalpreddatapre$religcombo)

globalpreddatapre$religevangtype <- as.character(globalpreddatapre$religcombo)
globalpreddatapre$religevang[grepl("Unspec", globalpreddatapre$religcombo)] <- NA
globalpreddatapre$religevang <- as.factor(globalpreddatapre$religevang)


# Dataset of Recoded Values for All Predictors - Will Need Updating If Additional Variables Come Online

gpd <- with(globalpreddatapre, data.frame(StudyID, date, sample, mode, weight=as.numeric(as.character(wt)), Sex=Sex, Age=as.numeric(as.character(age)), AgeCategories=agecats, State=state, Region=regionplus, Division=divisionplus, Education=ordered(educ, c("Less Than HS", "High School Graduate", "Some College", "College Graduate or More")), RaceEthnicity=raceeth, Partisanship=partisan3, Religion=religcombo, Religionmin=relig, Religevang=religevang, Religfund=religfund, FreqAttend=ordered(religfreq, c("Seldom to Never", "Yearly", "Monthly", "Weekly", "Multiple Times Per Week")), BirthDecade=birthdecade, PresidentialApproval=PresidentialApproval))

gpd$respID <- paste("resp", 1:(dim(gpd)[1]), sep="-")

set.seed(747474747)
primaryoutcomeimps <- amelia(gpd[!is.na(gpd$date),], idvars=c("StudyID", "sample", "mode",  "Religionmin", "Religevang",  "PresidentialApproval", "respID", "State", "AgeCategories", "BirthDecade"), ts="date", noms=c("Sex", "Region", "RaceEthnicity", "Partisanship", "Religion","Religfund", "Division", "FreqAttend", "Education"), m=1, polytime=3)
save(primaryoutcomeimps, gpd, usablestudies, avimp, outcomedatasets, file="EachFileExports/09-01_ImputationPreds.rdata")
