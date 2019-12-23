library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(Amelia)
library(MASS)

load("EachFileExports/10-01_IndividualMasterPreReversals.rdata")

# Split into outcomes per dataset

minfg <- with(fg, data.frame(out01, out01rev=out01, respID, StudyID, UniqueID))
fgsplit <- lapply(unique(minfg$UniqueID), function(x) minfg[minfg$UniqueID==x,])
names(fgsplit) <- unique(fg$UniqueID)
fgsstdz <- lapply(fgsplit, function(x) stdz(x$out01))

minfgr <- data.frame(rbindlist(fgsplit), stdzout=unlist(fgsstdz))
minfgr2 <- with(minfgr, data.frame(respID, UniqueID, stdzout))

fg2 <- merge(fg, minfgr2, by=c("UniqueID", "respID"))


## Use the trained data to produce a regression
fctreg <- lm(stdzout~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*majorclass*(dt+dt2+dt3+dt4+dt5), data=fg2, subset=(trainstatus=="Training Set"))

fg2$fctpred <- predict(fctreg, newdata=fg2)

# Identify Variables To Be Reversed

corlist <- sapply(unique(fg2$UniqueID), function(x) with(fg2[fg2$UniqueID==x,], try(cor(fctpred, out01, use="pairwise.complete.obs"))))
names(corlist) <- unique(fg2$UniqueID)
toreverse <- names(corlist[corlist < -.1 & !is.na(corlist)])

fg2$out01rev <- fg2$out01
fg2$stdzoutrev <- fg2$stdzout
fg2$out01rev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)] <- 1-fg2$out01[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)]
fg2$stdzoutrev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)] <- -fg2$stdzout[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)]

table(fg2$trainstatus, (fg2$UniqueID %in% toreverse))

## Rerun This A Couple of Times Without Subsetting
for(i in c(-.05, -.02, -.01)){
    print(paste("Reversals", i))
    fctreg <- lm(stdzoutrev~(impSex+impAge+impagesq+impDivision+impEducation+impRaceEthnicity+impPartisanship+impReligion+impFreqAttend)*majorclass*(dt+dt2+dt3+dt4+dt5), data=fg2)
    fg2$fctpred <- predict(fctreg, newdata=fg2)
    corlist <- corlist <- sapply(unique(fg2$UniqueID), function(x) with(fg2[fg2$UniqueID==x,], try(cor(fctpred, out01rev, use="pairwise.complete.obs"))))
    names(corlist) <- unique(fg2$UniqueID)
    threshold <- i
    toreverse <- names(corlist[corlist < threshold & !is.na(corlist)])
    fg2$out01rev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)] <- 1-fg2$out01rev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)]
    fg2$stdzoutrev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)] <- -fg2$stdzoutrev[fg2$trainstatus!="Training Set" & (fg2$UniqueID %in% toreverse)]
}

fg2$outmncent <- NA
for(i in unique(fg2$UniqueID)){
    print(i)
    fg2$outmncent[fg2$UniqueID==i] <- fg2$out01rev[fg2$UniqueID==i]-wtd.mean(fg2$out01rev[fg2$UniqueID==i], fg2$weight[fg2$UniqueID==i])
}

save(fg2, file="EachFileExports/10-02_FullDatasetWithReversals.rdata")




## Rebuild Data At Study Level To Assess Correlations Between Items By Types


outcomemergeds <- lapply(unique(fg2$StudyID), function(x) data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) fg2$stdzoutrev[fg2$UniqueID==y])))

outcometypes <- lapply(unique(fg2$StudyID), function(x) as.data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) as.character(fg2$bigcats)[fg2$UniqueID==y][1])))

outcomemajortypes <- lapply(unique(fg2$StudyID), function(x) as.data.frame(lapply(unique(fg2$UniqueID[fg2$StudyID==x & !is.na(fg2$stdzoutrev)]), function(y) as.character(fg2$majorclass)[fg2$UniqueID==y][1])))

ocmrel <- outcomemergeds[sapply(outcomemergeds, dim)[2,]>1]
octrel <- outcometypes[sapply(outcomemergeds, dim)[2,]>1]

ocmcors <- lapply(ocmrel, function(x) cor(x, use="pairwise.complete.obs"))

outcomes <- NULL
for(i in 1:length(ocmcors)){
    for(a in 1:(length(octrel[[i]])-1)){
        for(b in (a+1):length(octrel[[i]])){
            outcomes[[paste(i,a,b, sep="_")]] <- try(data.frame(Type=paste(sort(as.character(unlist(c(octrel[[i]][a], octrel[[i]][b])))), collapse="~"), Corr=ocmcors[[i]][a,b]))
        }
    }
}

corsets <- rbindlist(outcomes)
cs <- corsets[abs(corsets$Corr)!=1 & !is.na(corsets$Corr),]

cbl <- sapply(levels(cs$Type), function(x) abs(cs$Corr[cs$Type==x]))
#cbldecadecol <- sapply(levels(wcimin$typecombo), function(x) wcimin$decadecol[wcimin$typecombo==x])
cblord <- order(sapply(cbl, function(x) mean(x, na.rm=TRUE)))

pdf("Figures/CorrelationsBetweenVariableTypes.pdf", width=12, height=7, bg="White")
plot(0:1, c(1,length(cblord)), type="n", xlim=c(-.75,1), axes=FALSE, xlab="                                                                                    Absolute Correlation Coefficient", ylab="Major Types Compared", main="Correlations Between Types of Variables in Individual Surveys")
axis(1, seq(0,1,.2))
axis(3, seq(0,1,.2), labels=FALSE)
abline(v=seq(0,1,.2), lty=3, col="light gray")
abline(v=0:1)
sapply(1:length(cblord), function(x) lines(cbl[[cblord[x]]], rep(x, length(cbl[[cblord[x]]])), type="p", pch=20))
lines(sapply(cbl, function(x) mean(x, na.rm=TRUE))[cblord], 1:length(cblord), type="p", pch=18, col="red")
lines(sapply(cbl, function(x) mean(x, na.rm=TRUE))[cblord], 1:length(cblord), type="p", pch=23, col="red")
text(-.02, y=1:length(cblord), labels=levels(cs$Type)[cblord], pos=2, cex=.75)
legend(x="bottomleft", legend=c("Correlation in survey", "Mean Correlation"), pch=c(20, 23), col=c("black", "red"), bg="White", cex=.8)
legend(x="bottomleft", legend=c("Correlation in survey", "Mean Correlation"), pch=c(20, 18), col=c("black", "red"), bg="White", cex=.8)
dev.off()
