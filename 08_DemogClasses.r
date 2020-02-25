library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)

load("EachFileExports/04-01_MergedDataFiles.rdata")
load("EachFileExports/06-01_Flags.rdata")
load("EachFileExports/06-02_SelectedQuestionInfo.rdata")
source("99_TranslateResponses.r")

dmd <- qm[qm$datatype!="Topline" & qm$classmult!="abort",]



#importeachvar <- lapply(1:dim(dmd)[1], function(i) try(tolower(as.character(unlist(dfs[[dmd[i,]$StudyID]][,dmd[i,]$Varname]))), silent=TRUE))

importeachvar <- lapply(1:dim(dmd)[1], function(i) try(tolower(gsub(".", "", gsub("?", "", gsub("  ", " ", gsub(",|'", "", tolower(gsub("\x92", "'", gsub("\x85|\x97|\x91|\x92|\x96|\x93|\x94", "", as.character(unlist(dfs[[dmd[i,]$StudyID]][,dmd[i,]$Varname]))))))), fixed=TRUE), fixed=TRUE)), silent=TRUE))

# Weights

wtvarspre <- lapply(importeachvar[dmd$weightflag], function(x) try(as.numeric(x)))
wtvarsinfopre <- dmd[dmd$weightflag,]

weightvalidpotential <- sapply(wtvarspre, function(x) sum(!is.na(x))>.75*length(x))
weightvalidpotential[sapply(wtvarspre, class)=="try-error"] <- FALSE

wtvars <- lapply(wtvarspre[weightvalidpotential], function(x) try(x/mean(x, na.rm=TRUE)))
wtvarsinfo <- wtvarsinfopre[weightvalidpotential,]
wtvarsinfo$codedclass <- "Weight"

# Sex
sexvarspre <- importeachvar[dmd$sexflag]
sexvarsinfopre <- dmd[dmd$sexflag,]
sexvarstrans <- lapply(sexvarspre, sexfunc)

sexvarkeeps <- sapply(sexvarstrans, function(x) length(unique(x))>1 & sum(!is.na(x))/length(x)>.8)

sexvars <- sexvarstrans[sexvarkeeps]
sexvarinfo <- sexvarsinfopre[sexvarkeeps,]
sexvarinfo$codedclass <- "Sex"



# race
racevarspre <- importeachvar[dmd$raceflag]
racevarsinfopre <- dmd[dmd$raceflag,]
racevarstrans <- lapply(racevarspre, racefunc)

racevarkeeps <- sapply(racevarstrans, function(x) length(unique(x))>1 & sum(!is.na(x))/length(x)>.8)

racevars <- racevarstrans[racevarkeeps]
racevarinfo <- racevarsinfopre[racevarkeeps,]
racevarinfo$codedclass <- "Race"


# hisp
hispvarspre <- importeachvar[dmd$hispflag]
hispvarsinfopre <- dmd[dmd$hispflag,]
hispvarstrans <- lapply(hispvarspre, hispfunc)

hispvarkeeps <- sapply(hispvarstrans, function(x) length(unique(x))>2 & sum(!is.na(x))/length(x)>.8)

hispvars <- hispvarstrans[hispvarkeeps]
hispvarinfo <- hispvarsinfopre[hispvarkeeps,]
hispvarinfo$codedclass <- "hisp"


# state
statevarspre <- importeachvar[dmd$stateflag]
statevarsinfopre <- dmd[dmd$stateflag,]
statevarstrans <- lapply(statevarspre, stateclean)

statevarkeeps <- sapply(statevarstrans, function(x) length(unique(x))>20 & sum(!is.na(x))/length(x)>.8)

statevars <- statevarstrans[statevarkeeps]
statevarinfo <- statevarsinfopre[statevarkeeps,]
statevarinfo$codedclass <- "state"

# region
regionvarspre <- importeachvar[dmd$regionflag]
regionvarsinfopre <- dmd[dmd$regionflag,]
regionvarstrans <- lapply(regionvarspre, censregionclean)

regionvarkeeps <- sapply(regionvarstrans, function(x) length(unique(x[!is.na(x)]))>3 & sum(!is.na(x))/length(x)>.8)

regionvars <- regionvarstrans[regionvarkeeps]
regionvarinfo <- regionvarsinfopre[regionvarkeeps,]
regionvarinfo$codedclass <- "region"


# division
divisionvarspre <- importeachvar[dmd$divisionflag]
divisionvarsinfopre <- dmd[dmd$divisionflag,]
divisionvarstrans <- lapply(divisionvarspre, censdivisionclean)

divisionvarkeeps <- sapply(divisionvarstrans, function(x) length(unique(x[!is.na(x)]))>8 & sum(!is.na(x))/length(x)>.8)

divisionvars <- divisionvarstrans[divisionvarkeeps]
divisionvarinfo <- divisionvarsinfopre[divisionvarkeeps,]
divisionvarinfo$codedclass <- "division"


# educ
educvarspre <- importeachvar[dmd$educflag]
educvarsinfopre <- dmd[dmd$educflag,]
educvarstrans <- lapply(educvarspre, edufunc)

educvarkeeps <- sapply(educvarstrans, function(x) length(unique(x[!is.na(x)]))>3 & sum(!is.na(x))/length(x)>.8)

educvars1 <- educvarstrans[educvarkeeps]
educvarinfo1 <- educvarsinfopre[educvarkeeps,]
educvarinfo1$codedclass <- "educ"

educyrsvarspre <- importeachvar[dmd$educyrsflag]
educyrsvarsinfopre <- dmd[dmd$educyrsflag,]
educyrsvarstrans <- lapply(educyrsvarspre, edufuncyrs)

educyrsvarkeeps <- sapply(educyrsvarstrans, function(x) length(unique(x[!is.na(x)]))>3 & sum(!is.na(x))/length(x)>.8)

educvars2 <- educyrsvarstrans[educyrsvarkeeps]
educvarinfo2 <- educyrsvarsinfopre[educyrsvarkeeps,]
educvarinfo2$codedclass <- "educ"

educvars <- c(educvars1, educvars2)
educvarinfo <- rbind(educvarinfo1, educvarinfo2)


# relig
religvarspre <- importeachvar[dmd$religflag]
religvarsinfopre <- dmd[dmd$religflag,]
religvarstrans <- lapply(religvarspre, religfunc)

religvarkeeps <- sapply(religvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.8)

religvars <- religvarstrans[religvarkeeps]
religvarinfo <- religvarsinfopre[religvarkeeps,]
religvarinfo$codedclass <- "relig"

# religfund
religfundvarspre <- importeachvar[dmd$religfundflag]
religfundvarsinfopre <- dmd[dmd$religfundflag,]
religfundvarstrans <- lapply(religfundvarspre, relfundfind)

religfundvarkeeps <- sapply(religfundvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.4)

religfundvars <- religfundvarstrans[religfundvarkeeps]
religfundvarinfo <- religfundvarsinfopre[religfundvarkeeps,]
religfundvarinfo$codedclass <- "religfund"

# religfreq
religfreqvarspre <- importeachvar[dmd$religfreqflag]
religfreqvarsinfopre <- dmd[dmd$religfreqflag,]
religfreqvarstrans <- lapply(religfreqvarspre, relfreqfind)

religfreqvarkeeps <- sapply(religfreqvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.8)

religfreqvars <- lapply(religfreqvarstrans[religfreqvarkeeps], function(x) drop.levels(x, reorder=FALSE))
religfreqvarinfo <- religfreqvarsinfopre[religfreqvarkeeps,]
religfreqvarinfo$codedclass <- "religfreq"

# partisan
partisanvarspre <- importeachvar[dmd$partyflag]
partisanvarsinfopre <- dmd[dmd$partyflag,]
partisanvarstrans <- lapply(partisanvarspre, partsorter)

partisanvarkeeps <- sapply(partisanvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.8)

partisanvars <- lapply(partisanvarstrans[partisanvarkeeps], function(x) drop.levels(x, reorder=FALSE))
partisanvarinfo <- partisanvarsinfopre[partisanvarkeeps,]
partisanvarinfo$codedclass <- "partisan"

# partystrength
partystrengthvarspre <- importeachvar[dmd$partystrengthflag]
partystrengthvarsinfopre <- dmd[dmd$partystrengthflag,]
partystrengthvarstrans <- lapply(partystrengthvarspre, partystrength)

partystrengthvarkeeps <- sapply(partystrengthvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.4)

partystrengthvars <- partystrengthvarstrans[partystrengthvarkeeps]
partystrengthvarinfo <- partystrengthvarsinfopre[partystrengthvarkeeps,]
partystrengthvarinfo$codedclass <- "partystrength"

# presapprove
presapprovevarspre <- importeachvar[dmd$presapproveflag]
presapprovevarsinfopre <- dmd[dmd$presapproveflag,]
presapprovevarstrans <- lapply(presapprovevarspre, presapp)

presapprovevarkeeps <- sapply(presapprovevarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.8)

presapprovevars <- lapply(presapprovevarstrans[presapprovevarkeeps], function(x) drop.levels(x, reorder=FALSE))
presapprovevarinfo <- presapprovevarsinfopre[presapprovevarkeeps,]
presapprovevarinfo$codedclass <- "presapprove"


# age
agevarspre <- importeachvar[dmd$ageflag]
agevarsinfopre <- dmd[dmd$ageflag,]
agevarstrans <- lapply(agevarspre, function(x) try(agetrans(x), silent=TRUE))

agevarkeeps <- sapply(agevarstrans, function(x) length(unique(x[!is.na(x)]))>20 & sum(!is.na(x))/length(x)>.3)

agevarsunrev <- lapply(agevarstrans[agevarkeeps], function(x) as.numeric(unlist(x)))
rng <- sapply(agevarsunrev, function(x) range(x, na.rm=TRUE))
agevarsx <- agevarsunrev
agevarinfo <- agevarsinfopre[agevarkeeps,]
svyyrs <- as.numeric(format(as.Date(agevarinfo$sd, "%Y")))
svyyrs[is.na(svyyrs) & rng[1,]>1700] <- (rng[2,]+18)[is.na(svyyrs) & rng[1,]>1700]
agevarsx[rng[1,]>1700] <- lapply((1:length(agevarsx))[rng[1,]>1700], function(i) svyyrs[i]-agevarsunrev[[i]])
for(i in 1:length(agevarsx)){
    agevarsx[[i]][agevarsx[[i]]>130] <- NA
    agevarsx[[i]][agevarsx[[i]]>99] <- 99
}
agevars <- agevarsx
agevarinfo$codedclass <- "age"


# marital
maritalvarspre <- importeachvar[dmd$maritalflag]
maritalvarsinfopre <- dmd[dmd$maritalflag,]
maritalvarstrans <- lapply(maritalvarspre, maritaltrans)

maritalvarkeeps <- sapply(maritalvarstrans, function(x) length(unique(x[!is.na(x)]))>1 & sum(!is.na(x))/length(x)>.8)

maritalvars <- maritalvarstrans[maritalvarkeeps]
maritalvarinfo <- maritalvarsinfopre[maritalvarkeeps,]
maritalvarinfo$codedclass <- "marital"



infodf <- rbind(wtvarsinfo, sexvarinfo, racevarinfo, hispvarinfo, statevarinfo, regionvarinfo, divisionvarinfo, educvarinfo, religvarinfo, religfundvarinfo, religfreqvarinfo, partisanvarinfo, partystrengthvarinfo, presapprovevarinfo, agevarinfo, maritalvarinfo)
varset <- c(wtvars, sexvars, racevars, hispvars, statevars, regionvars, divisionvars, educvars, religvars, religfundvars, religfreqvars, partisanvars, partystrengthvars, presapprovevars, agevars, maritalvars)

save(infodf, varset, file="EachFileExports/08-01_CovariatesCategorized.rdata")
save(dmd, importeachvar, file="EachFileExports/08-02_AllCovariates.rdata")





summary(dmd[!(dmd$UniqueIDlower %in% infodf$UniqueIDlower),])

rev(sort(table(dmd$wordingsimp[!(dmd$UniqueIDlower %in% infodf$UniqueIDlower)])))[1:50]








