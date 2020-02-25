library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(glmnet)
library(qlcMatrix)
library(plyr)
library(stringr)
library(openxlsx)

grepelf <- function(pattern, str, fixed=FALSE)
    str_detect(str, pattern, fixed)

load("EachFileExports/07-01_AbortionVariablesCategorized.rdata")

avlearnmin <- with(av, data.frame(UniqueIDlower, vn, wordingsimp, respssimp, majorclass, bigcats, wccat, wordingresponses, stringsAsFactors=FALSE))

#findinvn <- sapply(stringpartstofind, function(x) str_count(gsub(" ", "_", as.character(avlearnmin$vn)), x))
#nscdict <- sapply(stringpartstofind, function(x) str_count(gsub(" ", "_", as.character(avlearnmin$wordingsimp)), x))
#nscresp <- sapply(stringpartstofind, function(x) str_count(gsub(" ", "_", as.character(avlearnmin$respssimp)), x))

rsnolong <- as.character(avlearnmin$respssimp)
rsnolong[sapply(strsplit(as.character(avlearnmin$respssimp), "; ", fixed=TRUE), length)>20] <- ""


dfmvn <- dfm(gsub("_", " ", as.character(avlearnmin$vn)), remove=c(stopwords("en"), ")", "(", "-"), stem=FALSE, ngrams=1:3)
dfmqw <- dfm(gsub("_", " ", paste(as.character(avlearnmin$vn), as.character(avlearnmin$wordingsimp))), remove=c(stopwords("en"), ")", "(", "-"), stem=FALSE, ngrams=1:3)
dfmvrespspre <- dfm(gsub("_", " ", rsnolong), remove=c(")", "(", "-"), stem=FALSE, ngrams=1:3)
dfmvresps <- dfmvrespspre[,!grepl(";", colnames(dfmvrespspre))]

overlaps <- colnames(dfmqw)[colnames(dfmqw) %in% colnames(dfmvresps)]

ovqs <- dfmqw[,overlaps]
ovrs <- dfmvresps[,overlaps]

ovset <- ovqs>0 & ovrs>0
ovout <- ovset[,colSums(ovset)>2]


writeflagger <- function(x){
    check <- flags[,x]
    dir.create(paste("AbortionCatsOutputs/", x, sep=""))
    write.csv(rev(sort(table(wdset[check]))), paste("AbortionCatsOutputs/", x, "/", x, "_StemMatches.csv", sep=""))
    write.csv(rev(sort(table(avlearnmin$wordingsimp[check]))), paste("AbortionCatsOutputs/", x, "/", x, "_Wordings.csv", sep=""))
    write.csv(rev(sort(colSums(dfmqw[check,])[colSums(dfmqw[check,])>0])), paste("AbortionCatsOutputs/", x, "/", x, "_Wds.csv", sep=""))
}

writerespflagger <- function(x){
    check <- respflags[,x]
    dir.create(paste("AbortionCatsOutputsResps/", x, sep=""))
    write.csv(rev(sort(table(wdset[check]))), paste("AbortionCatsOutputsResps/", x, "/", x, "_StemMatches.csv", sep=""))
    write.csv(rev(sort(table(avlearnmin$wordingsimp[check]))), paste("AbortionCatsOutputsResps/", x, "/", x, "_Wordings.csv", sep=""))
    write.csv(rev(sort(colSums(dfmqw[check,])[colSums(dfmqw[check,])>0])), paste("AbortionCatsOutputsResps/", x, "/", x, "_Wds.csv", sep=""))
}

spitwords <- write.csv(rev(sort(colSums(ovout))), "AbortionCatsOutputs/CommonWords.csv")

wdset <- apply(ovout, 1, function(x) paste(colnames(ovout)[x>0], collapse="; "))


legalresp <- rowSums(dfmvresps[,c("legal", "leg", "illegal", "alw_/_legl")])>=1
respflags <- data.frame(legalresp)
respflags$yesno <- rowSums(dfmvresps[,c("yes", "no", "chosen", "not_chosen")])>=1 & rowSums(dfmvresps[,c("no_ans", "no_answer", "no_opinion")])>=0
respflags$favopp <- rowSums(dfmvresps[,c("favor", "fav", "oppose")])>=1
respflags$numeric <- rowSums(dfmvresps[,as.character(c(1:9))])>=1
respflags$agree <- rowSums(dfmvresps[,c("agree")])>=1
respflags$import <- rowSums(dfmvresps[,c("important", "importnt", "very_imp")])>=1
respflags$warmcool <- rowSums(dfmvresps[,c("warm", "cool")])>=1
respflags$likelihood <- rowSums(dfmvresps[,c("likely")])>=1
respflags$shouldnot <- rowSums(dfmvresps[,c("should_not")])>=1
respflags$permitallow <- rowSums(dfmvresps[,c("permitted", "allow", "allowed", "prm_abor", "prevent", "prmt", "permit")])>=1
respflags$abortion <- rowSums(dfmvresps[,c("abortion")])>=1
respflags$choicelife <- rowSums(dfmvresps[,c("pro-life", "pro-choice", "prochoice", "prolife", "pro_choice", "pro_life")])>=1
respflags$wrong <- rowSums(dfmvresps[,c("wrong")])>=1
respflags$approve <- rowSums(dfmvresps[,c("approve")])>=1
respflags$candidates <- rowSums(dfmvresps[,c("bush", "gore", "obama", "trump", "kerry", "mccain", "clinton", "dole")])>=1
respflags$available <- rowSums(dfmvresps[,c("available")])>=1
respflags$appropriate <- rowSums(dfmvresps[,c("appropriate")])>=1
respflags$acceptable <- rowSums(dfmvresps[,c("acceptable")])>=1
respflags$effective <- rowSums(dfmvresps[,c("effective")])>=1
respflags$burden <- rowSums(dfmvresps[,c("burden")])>=1
respflags$closely <- rowSums(dfmvresps[,c("closely")])>=1
respflags$months <- rowSums(dfmvresps[,c("months")])>=1
respflags$repdem <- rowSums(dfmvresps[,c("republican", "democrat", "republicans", "democrats", "rep", "dem")])>=1
respflags$anyreason <- rowSums(dfmvresps[,c("any_reason")])>=1
respflags$failure <- rowSums(dfmvresps[,c("failure")])>=1
respflags$doubts <- rowSums(dfmvresps[,c("doubts")])>=1
respflags$difficulty <- rowSums(dfmvresps[,c("difficult", "easier", "less_strict", "more_strict", "stricter")])>=1
respflags$overturn <- rowSums(dfmvresps[,c("overturn")])>=1
respflags$overturned <- rowSums(dfmvresps[,c("overturned")])>=1
respflags$murder <- rowSums(dfmvresps[,c("murder", "murdering")])>=1
respflags$conservative <- rowSums(dfmvresps[,c("conservative")])>=1
respflags$votefor <- rowSums(dfmvresps[,c("vote_for")])>=1
respflags$against <- rowSums(dfmvresps[,c("against")])>=1
respflags$satisfied <- rowSums(dfmvresps[,c("satisfied")])>=1
respflags$problem <- rowSums(dfmvresps[,c("problem")])>=1
respflags$favorable <- rowSums(dfmvresps[,c("favorable")])>=1
respflags$care <- rowSums(dfmvresps[,c("care")])>=1
respflags$state <- rowSums(dfmvresps[,c("state", "states")])>=1
respflags$priority <- rowSums(dfmvresps[,c("priority")])>=1
respflags$restrictions <- rowSums(dfmvresps[,c("restrictions")])>=1
respflags$reasonable <- rowSums(dfmvresps[,c("reasonable", "extremists")])>=1
respflags$conception <- rowSums(dfmvresps[,c("conception")])>=1
respflags$did_not_want <- rowSums(dfmvresps[,c("did_not_want", "didnt_want")])>=1
respflags$goodbad <- rowSums(dfmvresps[,c("bad", "good")])>1
respflags$possible <- rowSums(dfmvresps[,c("possible")])>=1
respflags$safe <- rowSums(dfmvresps[,c("safe")])>=1
respflags$convincing <- rowSums(dfmvresps[,c("convincing")])>=1
respflags$womandoctor <- rowSums(dfmvresps[,c("woman", "doctor")])>1
respflags$womangovt <- rowSums(dfmvresps[,c("woman", "government")])>1
respflags$circumstances <- rowSums(dfmvresps[,c("most_circumstances")])>=1
respflags$toolittlemuch <- rowSums(dfmvresps[,c("too_little", "too_much")])>1
respflags$concern <- rowSums(dfmvresps[,c("concerned")])>=1
respflags$sin <- rowSums(dfmvresps[,c("sin")])>=1
respflags$moral <- rowSums(dfmvresps[,c("moral")])>=1
respflags$reason <- rowSums(dfmvresps[,c("major_reason")])>=1
respflags$helphurt <- rowSums(dfmvresps[,c("helps", "hurts")])>=1
respflags$publicfunds <- rowSums(dfmvresps[,c("public_funds")])>=1
respflags$banned <- rowSums(dfmvresps[,c("ban", "banned", "outlaw")])>=1
respflags$happenornot <- rowSums(dfmvresps[,c("not_happen")])>=1
respflags$persuasive <- rowSums(dfmvresps[,c("persuasive")])>=1
respflags$reduce <- rowSums(dfmvresps[,c("reduce")])>=1
respflags$truefalse <- rowSums(dfmvresps[,c("true")])>=1
respflags$inclination <- rowSums(dfmvresps[,c("inclined")])>=1
respflags$mainstream <- rowSums(dfmvresps[,c("the_mainstream")])>=1
respflags$restrictive <- rowSums(dfmvresps[,c("too_lenient", "too_restrictive")])>=1
respflags$involved <- rowSums(dfmvresps[,c("involved")])>=1
respflags$believe <- rowSums(dfmvresps[,c("believe")])>=1
respflags$negpos <- rowSums(dfmvresps[,c("negative", "positive")])>1
respflags$increased <- rowSums(dfmvresps[,c("increased", "decreased")])>1
respflags$neveralways <- rowSums(dfmvresps[,c("never", "always", "in_all", "under_all", "under_any", "general", "in_most", "in_a_few", "legal_all", "legal_any", "depends_on")])>1
respflags$prohibit <- rowSums(dfmvresps[,c("prohibit", "prohibited", "prevent")])>=1

uncategorized <- rowSums(respflags)==0
respflags$amt <- uncategorized & rowSums(dfmvresps[,c("very", "somewhat", "extremely", "not_at_all", "sometimes", "often", "a_lot", "nothing", "at_all", "the_time", "little")])>1
respflags$uncategorized <- rowSums(respflags)==0

lapply(names(respflags), writerespflagger)

crossoverresps <- sapply(respflags, function(x) sapply(respflags, function(y) sum(x & y)))


feeltherm <- rowSums(dfmqw[,c("warm", "cold", "rate_your_feelings", "thrm-opnents", "thrm-supptrs", "therm-anti-abortsts")])>1
flags <- data.frame(feeltherm)
flags$approvecand <- rowSums(dfmqw[,c("favorable", "approve", "very_satisfied")])>=1
flags$moral <- rowSums(dfmqw[,c("moral", "wrong", "sin", "murder", "immoral")])>=1 & rowSums(dfmqw[,c("thing_to_do")])==0
flags$roe <- rowSums(dfmqw[,c("roe", "1973")])>=1 & rowSums(dfmqw[,c("each_state", "likely")])==0
flags$amend <- rowSums(dfmqw[,c("amendment", "amend", "constitution", "constitutional")])>=1 & rowSums(dfmqw[,c("likely")])==0
flags$parentalconsent <- rowSums(dfmqw[,c("consent", "consnt", "permission", "tell", "conest")])>=1 & rowSums(dfmqw[,c("parent", "parental", "parents", "par", "parntl")])>=1
flags$parentalnotification <- rowSums(dfmqw[,c("notify", "notification", "tell", "be_told")])>=1 & rowSums(dfmqw[,c("parent", "parental", "parents", "teen")])>=1
flags$notifyspouse <- rowSums(dfmqw[,c("notify", "husband", "spouse")])>=1 & rowSums(dfmqw[,c("parent", "parental", "parents", "teen")])==0
flags$import <- rowSums(dfmqw[,c("important", "critical", "importance", "strength_of_feeling")])>=1   & rowSums(dfmqw[,c("places_greater_importance")])==0
flags$partialbirth <- rowSums(dfmqw[,c("partial_birth", "partial-birth", "late-term", "late_term", "late-term", "p-b", "dilation", "birth_abortion")])>=1
flags$legal <- rowSums(dfmqw[,c("legal", "illegal", "abortion-leg_/_illeg", "legality", "leg")])>=1 | rowSums(dfmvresps[,c("legal")])>=1 | rowSums(dfmvresps[,c("legal", "should_be_illegal", "abortions_illegal")])>=1
flags$circumst <- rowSums(dfmqw[,c("circumstances")])>=1
flags$provide <- rowSums(dfmqw[,c("provide")])>=1
flags$burden <- rowSums(dfmqw[,c("undue_burden")])>=1
flags$permit <- rowSums(dfmqw[,c("permit", "permitted")])>=1
flags$pills <- rowSums(dfmqw[,c("pill", "486", "ru-486", "morning_after", "r-u-4-86")])>=1
flags$candidates <- rowSums(dfmqw[,c("bush", "clinton", "dole", "obama", "trump", "mccain", "romney", "kerry", "gore", "giuliani", "palin", "perot", "house_cand", "hse_cand")])>=1
flags$demcands <- rowSums(dfmqw[,c("clinton", "obama", "kerry", "gore")])>=1
flags$repcands <- rowSums(dfmqw[,c("bush", "dole", "trump", "mccain", "romney")])>=1
flags$stricter <- rowSums(dfmqw[,c("stricter")])>=1
flags$repdem <- rowSums(dfmqw[,c("republican", "republicans", "democrat", "democratic", "democrats", "rep", "dem", "party_abortion_scale")])>1
flags$vote <- rowSums(dfmqw[,c("vote", "choice_for_president")])>=1
flags$rape <- rowSums(dfmqw[,c("rape")])>=1 & rowSums(dfmqw[,c("only_in", "except", "only_be", "in_all")])==0
flags$incest <- rowSums(dfmqw[,c("incest")])>=1 & rowSums(dfmqw[,c("only_in", "except", "only_be", "in_all")])==0
flags$afford <- rowSums(dfmqw[,c("afford", "low_income")])>=1 & rowSums(dfmqw[,c("public_funds", "govt_funds", "medicaid", "under_current_law", "government", "right-to-life", "candidate", "gambling", "tax")])==0
flags$interruptcareer <- rowSums(dfmqw[,c("her_career")])>=1
flags$doesntwant <- rowSums(dfmqw[,c("unwanted", "do_not_want", "does_not_want")])>=1
flags$womanshealth <- rowSums(dfmqw[,c("physical_health", "womans_health", "mothers_health", "w_to_die")])>=1
flags$healthrisk <- rowSums(dfmqw[,c("health", "hlth", "life")])>=1 & rowSums(dfmqw[,c("risk", "endanger", "endangered", "impaired", "would_hurt")])>=1
flags$mental <- rowSums(dfmqw[,c("mental")])>=1
flags$unmarried <- rowSums(dfmqw[,c("not_married", "unmarried", "unwed")])>=1
flags$anyreason <- rowSums(dfmqw[,c("any_reason")])>=1
flags$circumsex <- rowSums(dfmqw[,c("the_sex", "gender_wrong")])>=1
flags$circumadditional <- rowSums(dfmqw[,c("under_what_circumstances", "when_a_pregnancy", "be_legal_if", "abortn_legal_if")])>=1
flags$teen <- rowSums(dfmqw[,c("teen", "teenager", "under_18", "high_school")])>=1
flags$fetaldefect <- rowSums(dfmqw[,c("deformed", "defect", "impaired", "disabled", "defects")])>=1
flags$prolifechoice <- (rowSums(dfmqw[,c("pro-life", "pro-choice", "pro_choice", "pro_life", "right-to-life", "anti-abortion")])>=1 | rowSums(dfmvresps[,c("pro-life", "pro-choice", "pro_choice", "pro_life")])>=1) & rowSums(dfmqw[,c("know", "giuliani", "socially_acceptable", "candidates", "thermometer")])==0
flags$publicfunds <- rowSums(dfmqw[,c("public_funds", "government_funds", "subsidize", "should_pay", "government_should_help", "subsidies", "govt_funds", "be_funded_by")])>=1
flags$regulate <- rowSums(dfmqw[,c("regulate", "should_not_interfere")])>=1
flags$suprem <- rowSums(dfmqw[,c("supreme")])>=1
flags$court <- rowSums(dfmqw[,c("court")])>=1
flags$overturn <- rowSums(dfmqw[,c("overturn")])>=1
flags$congress <- rowSums(dfmqw[,c("congress")])>=1 & rowSums(dfmqw[,c("bill_in_congress")])>=0
flags$president <- rowSums(dfmqw[,c("president")])>=1
flags$nation <- rowSums(dfmqw[,c("nation", "country")])>=1
flags$disclose <- rowSums(dfmqw[,c("publicly_state", "divulge")])>=1
flags$justices <- rowSums(dfmqw[,c("alito", "roberts", "kagan", "sotomayor", "miers", "roberts", "thomas", "souter", "bork", "justice")])>=1
flags$criticalissue <- rowSums(dfmqw[,c("critical_issue")])>=1
flags$firsttrimester <- rowSums(dfmqw[,c("first_trimester", "first_three", "1st_three")])>=1
flags$secondtrimester <- rowSums(dfmqw[,c("second_trimester", "second_three", "4-6_months")])>=1
flags$thirdtrimester <- rowSums(dfmqw[,c("last_three", "third_trimester", "after_6", "sixth_month")])>=1
flags$weeksmonths <- rowSums(dfmqw[,c("weeks", "week", "month", "months")])>=1 & rowSums(dfmqw[,c("ended", "she_became_pregnant", "fetal_death", "pregnancy_occurred")])>=0
flags$morediff <- rowSums(dfmqw[,c("more_difficult", "make_it_harder", "more_strict", "new_limits", "make_it_difficult")])>=1 & rowSums(dfmqw[,c("think_it_is")])>=0
flags$righttochoose <- rowSums(dfmqw[,c("right_to_choose", "right_to_decide")])>=1
flags$beallowed <- rowSums(dfmqw[,c("be_allowed", "able_to_get", "able_to_have")])>=1
flags$doctor <- rowSums(dfmqw[,c("doctor", "physician")])>=1
flags$woman <- rowSums(dfmqw[,c("woman")])>=1
flags$prohibit <- rowSums(dfmqw[,c("prohibit", "prohibiting", "prevent", "preventing")])>=1
flags$ban <- rowSums(dfmqw[,c("ban", "banned")])>=1
flags$TRAP <- rowSums(dfmqw[,c("law_allowing", "24_hours", "24_hrs", "live_outside", "survive_outside", "laws_to_restrict", "not_developed_enough", "dificult_for_private", "medically_inaccurate", "hospitals", "medically_unnecessary_appointments", "ambulatory_surgical")])>=1
flags$plannedparenthood <- rowSums(dfmqw[,c("planned_parenthood")])>=1
flags$concern <- rowSums(dfmqw[,c("concerns_you_the", "of_most_concern")])>=1
flags$strong <- rowSums(dfmqw[,c("strong", "strongly")])>=1
flags$advice <- rowSums(dfmqw[,c("advice", "advise")])>=1
flags$oddmomentdrops <- rowSums(dfmqw[,c("gosnell", "bennett")])>=1
flags$christianstuff <- rowSums(dfmqw[,c("pope", "catholic", "place_of_worship", "church")])>=1
flags$coverage <- rowSums(dfmqw[,c("should_cover", "insurance")])>=1
flags$badsituation <- rowSums(dfmqw[,c("bad_situation")])>=1
flags$candplacement <- rowSums(dfmqw[,c("cand_placement", "party_placement", "president_placement", "placement_of")])>=1
flags$dropme <- rowSums(dfmqw[,c("sex_education", "teen_pregnancy", "redefine_rape", "before_this_pregnancy", "web_to_learn", "saw_heard", "saw_/_heard", "length_of", "baby_due", "miscarriage_/_stillbirth", "child_had_unwanted", "reduce_the_number", "birth_control", "casual_sex", "order_of_abortion", "happened_to_follow", "was_there_ever", "world_population", "news_coverage", "of_your_knowledge")])>=1 |
    rowSums(dfmqw[,c("life_begins", "using_a_method", "fetal_death", "#_months_between", "#_months_after", "ashcroft", "no_serious_contradiction", "last_12_months", "next_12_months", "douche", "doctor_interrupting_pregnancy", "this_past_month", "following_the_news", "how_many_abort", "last_six_months", "have_the_surgery")])>=1|
    rowSums(dfmqw[,c("until_pregnancy_occurred", "smoking", "because_you_were", "view_about_prenatal", "before_her_period", "why_no_method", "which_pregnancy_occurred", "under_which_rs", "your_immediate_family", "have_you_ever", "have_intercourse", "teach_teenagers", "child_abuse", "chances_of_avoiding")])>=1|
    rowSums(dfmqw[,c("children_will_be", "types_of_women", "what_percentage", "friend_who_became", "girls_will_become", "welfare", "seen_a_movie", "friend", "movie_or_tv")])>=1
flags$slowdrops <- grepelf("clinton on abortion", avlearnmin$wordingsimp)

flags$uncategorized <- rowSums(flags)==0

crossovers <- sapply(flags, function(x) sapply(flags, function(y) sum(x & y)))
                        
lapply(names(flags), writeflagger)

unplacedwords <- write.csv(rev(sort(colSums(ovout[flags$uncategorized,]))), "AbortionCatsOutputs/CommonWordsUnplaced.csv")


doubleuncategorized <- flags$uncategorized & respflags$uncategorized

write.csv(rev(sort(table(paste(avlearnmin$vn, avlearnmin$wordingsimp)[doubleuncategorized]))), "AbortionCatsOutputs/DoubleUncategorizedQuestions.csv")
write.csv(crossovers, "AbortionCatsOutputs/CrossoverCategories.csv")

flagsets <- apply(flags, 1, function(x) paste(names(flags)[x>0], collapse=";"))
respflagsets <- apply(respflags, 1, function(x) paste(names(respflags)[x>0], collapse=";"))

combosets <- paste(flagsets, "|", respflagsets)

rf <- respflags
colnames(rf) <- paste("R", colnames(respflags), sep="")
bothflags <- data.frame(flags, rf)

circcats <- with(bothflags, data.frame(circumst, rape, incest, afford, interruptcareer, doesntwant, womanshealth, healthrisk, mental, unmarried, circumsex, circumadditional, teen, fetaldefect, anyreason, Rcircumstances))
whenvars <- with(bothflags, data.frame(firsttrimester, secondtrimester, thirdtrimester, weeksmonths, partialbirth, Rconception, Rmonths))
courts <- with(bothflags, data.frame(roe, amend, suprem, court, justices, overturn, Roverturn))
imports <- with(bothflags, data.frame(import, criticalissue, strong, concern, Rimport, Rcare, Rconcern, Rpriority, Rclosely))
allowprohibregulate <- with(bothflags, data.frame(permit, regulate, beallowed, prohibit, ban, Rpermitallow, Ravailable))
easierharder <- with(bothflags, data.frame(burden, stricter, morediff, Rdifficulty, Rrestrictive, Rrestrictions, Rburden))
partisan <- with(bothflags, data.frame(approvecand, candidates, repdem, Rrepdem, Rcandidates))
notifcons <- with(bothflags, data.frame(parentalconsent, parentalnotification, notifyspouse))
legality <- with(bothflags, data.frame(legal, Rlegalresp))
morality <- with(bothflags, data.frame(moral, Rwrong, Rmoral, Rsin, Rmurder, Racceptable, Rappropriate, Rapprove))
pubfunds <- with(bothflags, data.frame(publicfunds, Rpublicfunds))
singissues <- with(bothflags, data.frame(pills, TRAP))
identgroups <- with(bothflags, data.frame(feeltherm, prolifechoice, plannedparenthood, Rchoicelife, Rmainstream, Rreasonable))
whoact <- with(bothflags, data.frame(congress, president, nation, doctor, woman, demcands, repcands, Rwomandoctor, Rstate))
directional <- with(bothflags, data.frame(advice, Rfavopp, Ragainst, righttochoose, Rnegpos, Rtoolittlemuch, Rhelphurt, Rreduce, overturn, Roverturn))
insurance <- with(bothflags, data.frame(coverage))

bflist <- list(Circumstances=circcats, Term=whenvars, Courts=courts, Import=imports, Regulation=allowprohibregulate, EasierHarder=easierharder, Partisanship=partisan, Notification=notifcons, Legality=legality, Morality=morality, PublicFunding=pubfunds, SpecificIssues=singissues, IdentityGroups=identgroups, Actors=whoact, Directional=directional, InsCov=insurance)

bfmaintyper <- as.data.frame(sapply(bflist, rowSums))
present <- rowSums(bfmaintyper)>0
numtypes <- rowSums(bfmaintyper>0)

conditions <- rowSums(data.frame(circcats, whenvars))>0 & !respflags$neveralways & !bothflags$Rwomandoctor
importance <- rowSums(imports)>0 & !respflags$favopp
directionalterms <- rowSums(data.frame(allowprohibregulate, easierharder, legality, morality, directional, singissues))>0
politicalmentions <- rowSums(data.frame(courts, partisan))>0

bfmtadd <- bfmaintyper[,!colnames(bfmaintyper) %in% c("Circumstances", "whenvars", "Import", "Regulation", "EasierHarder", "Legality", "Morality", "Directional", "Courts", "Partisanship", "Term")]>0

newmaj <- data.frame(Conditions=conditions, Importance=importance, DirectionalTerms=directionalterms, PoliticalMentions=politicalmentions, bfmtadd)

sapply(newmaj, function(x) sapply(newmaj, function(y) sum(x>0 & y>0)))

names(bothflags[!names(bothflags) %in% unique(unlist(sapply(bflist, colnames)))])
sort(sapply(bothflags[!present,], function(x) sum(!is.na(x) & x==TRUE)))[sort(sapply(bothflags[!present,], function(x) sum(!is.na(x) & x==TRUE)))>0]

bfsets <- apply(newmaj, 1, function(x) paste(colnames(newmaj)[x>0], collapse="-"))
bfsets[bfsets==""] <- "Uncategorized"

bfmaintyperdrops <- as.data.frame(newmaj>0)
bfmaintyperdrops$Actors[bfmaintyperdrops$Importance>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$PoliticalMentions>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$DirectionalTerms>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$Conditions>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$Notification>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$IdentityGroups>0] <- 0
bfmaintyperdrops$Actors[bfmaintyperdrops$PublicFunding>0] <- 0
bfmaintyperdrops$PoliticalMentions[bfmaintyperdrops$Notification>0] <- 0
bfmaintyperdrops$Importance[bfmaintyperdrops$Notification>0] <- 0
bfmaintyperdrops$Importance[bfmaintyperdrops$SpecificIssues>0] <- 0
bfmaintyperdrops$Importance[bfmaintyperdrops$PublicFunding>0] <- 0
bfmaintyperdrops$SpecificIssues[bfmaintyperdrops$Notification>0] <- 0
bfmaintyperdrops$Conditions[bfmaintyperdrops$SpecificIssues>0] <- 0

bfsets2 <- apply(bfmaintyperdrops, 1, function(x) paste(colnames(bfmaintyperdrops)[x>0], collapse="-"))
bfsets2[flags$dropme | flags$christianstuff | flags$oddmomentdrops | flags$slowdrops] <- "Excluded"
bfsets2[bfsets2==""] <- "Uncategorized"

bfsets3 <- bfsets2
bfsets3[bfsets3 %in% names(table(bfsets2)[table(bfsets2)<10])] <- "Small Categories"

bfstab <- rev(sort(table(bfsets3)))



avlm1 <- with(avlearnmin, data.frame(vn, wordingsimp, respssimp, wordingresponses, flagsets, respflagsets, stringsAsFactors=FALSE))
dupfinder <- with(avlm1, paste(wordingsimp, flagsets, respflagsets))
avlm1b <- with(avlearnmin, data.frame(vn, wordingsimp, respssimp, dupfinder, flagsets, respflagsets, stringsAsFactors=FALSE))
duptable <- as.data.frame(with(avlearnmin, table(dupfinder)))
avlm2pre <- join(avlm1b, duptable)
avlm2 <- avlm2pre[,names(avlm2pre)!="dupfinder"]

avtdup <- duplicated(dupfinder)


# Add In Circumstances

circbind <- data.frame(circcats)
speccircpre <- apply(circbind, 1, function(x) paste(colnames(circbind)[x==TRUE], collapse="-"))
speccirc2 <- gsub("doesntwant-anyreason", "doesntwant", gsub("healthrisk-fetaldefect", "fetaldefect", gsub("womanshealth-healthrisk|womanshealth", "healthrisk", gsub("unmarried-teen", "teen", gsub("circumst-|-circumadditional|circumadditional-", "", speccircpre)))))
circadds <- speccirc2
circadds[!grepl("Conditions", bfsets2) & grepl("Directional", bfsets2)] <- ""
circadds[circadds %in% names(table(circadds)[table(circadds)<4])] <- "Multiple"
circadds[circadds %in% c("Rcircumstances", "circumst")] <- "General"

whenbind <- data.frame(whenvars)
whenpre <- apply(whenbind, 1, function(x) paste(colnames(whenbind)[x==TRUE], collapse="-"))
whencirc2 <- gsub("firsttrimester-secondtrimester-Rmonths", "Rmonths", gsub("firsttrimester-Rmonths", "firsttrimester", gsub("thirdtrimester-partialbirth|weeksmonths-partialbirth", "partialbirth", gsub("trimester-weeksmonths", "trimester", gsub("Rconception-Rmonths|weeksmonths-Rmonths|firsttrimester-secondtrimester-Rmonths", "Rmonths", whenpre)))))
whencirc2[!grepl("Conditions", bfsets2)] <- ""
whencirc2[whencirc2 %in% names(table(whencirc2)[table(whencirc2)<4])] <- "Other"
whencirc2[whencirc2=="Rmonths"] <- "weeksmonths"

actorbind <- data.frame(partisan, whoact)
actorpre <- apply(actorbind, 1, function(x) paste(colnames(actorbind)[x==TRUE], collapse="-"))
actor1 <- gsub("nation-doctor-woman|woman-doctor-woman|doctor-doctor-woman", "doctor-woman", gsub("congress-nation", "congress", gsub("candidates-congress-president", "congress-president", gsub("Rwomandoctor", "doctor-woman", gsub("doctor-woman-Rwomandoctor", "doctor-woman", gsub("candidates-repdem-president", "candidates-president", gsub("Rrepdem", "repdem", gsub("repdem-Rrepdem", "repdem", gsub("president-repcands|president-demcands", "president", gsub("candidates-president", "president", gsub("demcands-repcands", "candidates", gsub("candidates-demcands", "demcands", gsub("candidates-repcands", "repcands", gsub("candidates-Rcandidates", "candidates", actorpre))))))))))))))
actor2 <- gsub("doctor-woman-Rstate", "Rstate", gsub("candidates-nation-repcands", "repcands", gsub("candidates-nation-demcands", "demcands", gsub("candidates-Rdemcands", "demcands", gsub("candidates-Rrepcands", "repcands", gsub("Rcandidates", "candidates", gsub("repdem-Rcandidates|candidates-repdem", "candidates", gsub("candidates-repdem-Rrepcands", "candidates", actor1))))))))
actor2[actor2 %in% names(table(actor2)[table(actor2)<4])] <- "OtherActor"


## Single Issue
sipre <- apply(singissues, 1, function(x) paste(colnames(singissues)[x==TRUE], collapse="-"))

## Identity Groups
idpre <- apply(identgroups, 1, function(x) paste(colnames(identgroups)[x==TRUE], collapse="-"))
idpre[idpre %in% c("prolifechoice-Rchoicelife", "Rchoicelife")] <- "prolifechoice"
idpre[idpre %in% c("prolifechoice-plannedparenthood", "feeltherm-plannedparenthood")] <- "plannedparenthood"
idpre[idpre %in% c("Rreasonable", "Rmainstream")] <- ""



# Add In Measures


measbind <- data.frame(imports, allowprohibregulate, easierharder, legality, morality, directional)

qbind <- measbind[,!grepl("R", colnames(measbind))]
ansbind <- measbind[,grepl("R", colnames(measbind))]


ansmeaspre <- apply(ansbind, 1, function(x) paste(colnames(ansbind)[x==TRUE], collapse="-"))
ansmeas1 <- gsub("Rrestrictions-Ragainst", "Rrestrictions", gsub("Rdifficulty-Rlegalresp", "Rlegalresp", gsub("Rpermitallow-Ragainst", "Rpermitallow", gsub("Ravailable-Rdifficulty|Ravailable", "Rdifficulty", gsub("Rpermitallow-Rlegalresp", "Rlegalresp", gsub("Rpermitallow-Ravailable|Rpermitallow-Rdifficulty|Rpermitallow-Ravailable-Rdifficulty|Rpermitallow-Ragainst|Rpermitallow-Rrestrictions", "Rpermitallow", gsub("Rmoral-Rmurder", "Rmurder", gsub("Rimport-Rpriority", "Rimport", gsub("Rrestrictions-Rlegalresp|Rlegalresp-Ragainst", "Rlegalresp", gsub("Rmoral-Rmoral", "Rmoral", gsub("Rwrong|Rwrong-Racceptable|Rwrong-Ragainst|Rwrong-Rmoral-Racceptable|Rmoral-Racceptable|Rmoral-Ragainst", "Rmoral", gsub("Rwrong-Rmurder-Rfavopp-Ragainst|Rmoral-Rmurder|Rmurder-Ragainst|Rmoral-Rmurder-Ragainst", "Rmurder", gsub("-Rfavopp|-Rnegpos", "", ansmeaspre)))))))))))))
ansmeas1[ansmeas1 %in% names(table(ansmeas1)[table(ansmeas1)<4])] <- "Other"


qmeaspre <- apply(qbind, 1, function(x) paste(colnames(qbind)[x==TRUE], collapse="-"))
qmeas1 <- gsub("import-criticalissue", "import", gsub("permit-legal|beallowed-legal", "legal", gsub("permit-stricter", "permit", qmeaspre)))
qmeas1[qmeas1 %in% names(table(qmeas1)[table(qmeas1)<4])] <- "Other"

measbinds <- gsub("~-", "~", gsub("strong-", "", gsub("Rfavopp|Other", "", paste(qmeas1, ansmeas1, sep="~"))))


newwccats <- as.character(rep(NA, length(qmeas1)))
newwccats[measbinds %in% c("~")] <- ""
newwccats[(grepl("legal", qmeas1) & !(ansmeas1 %in% c("Rmoral", "Rimport", "Roverturn", "Rpriority", "Rmurder", "Rlegalresp-Rmoral", "import-moral~Rimport"))) | grepl("legal", ansmeas1)] <- "Legal"
newwccats[measbinds %in% c("import~", "import~Rimport", "import~Rcare", "~Rimport", "import-legal~Rimport", "import~Other", "import-ban~Rimport", "import-ban~", "import~Rcare-Rmoral", "import-righttochoose~", "import-prohibit~Rimport")] <- "Importance"
newwccats[measbinds %in% c("permit~", "permit~Rpermitallow", "~Rpermitallow", "beallowed~Rpermitallow", "beallowed~", "permit~Rfavopp", "stricter~Rpermitallow", "permit~Rdifficulty", "regulate~Rpermitallow", "beallowed-prohibit~Rpermitallow", "ban~Rpermitallow", "permit~Ragainst")] <- "PermitAllow"
newwccats[measbinds %in% c("moral~", "moral~Rmoral", "~Rmoral", "legal-moral~Rmoral", "import-moral~Rcare-Rmoral", "~Rcare-Rmoral", "prohibit-moral~Rmoral", "prohibit-moral~Rappropriate")] <- "Moral"
newwccats[measbinds %in% c("prohibit~", "prohibit~Rfavopp", "ban~", "legal-moral~Rmoral", "ban~Rfavopp", "strong-prohibit~", "prohibit~Rapprove", "ban~Rapprove", "ban~Ragainst", "prohibit~Rpermitallow", "prohibit~Rtoolittlemuch", "beallowed-prohibit~", "ban~Rrestrictions", "ban~Rpermitallow")] <- "ProhibitBan"
newwccats[measbinds %in% c("~Rdifficulty", "morediff~Rdifficulty", "morediff~Rfavopp", "strong-morediff~Rfavopp", "morediff~", "strong-morediff~", "morediff~Ragainst")] <- "MoreDifficult"
newwccats[measbinds %in% c("regulate~", "~Rrestrictions", "~Rrestrictive")] <- "RestrictRegulate"
newwccats[measbinds %in% c("overturn~Roverturn", "overturn~", "~Roverturn", "legal-overturn~Roverturn", "overturn~Rfavopp")] <- "Overturn"
newwccats[measbinds %in% c("~Rapprove")] <- "Approval"
newwccats[measbinds %in% c("~Rcare")] <- "Care"
newwccats[measbinds %in% c("strong~")] <- "Strength"
newwccats[measbinds %in% c("~Racceptable", "~Rappropriate")] <- "AcceptableAppropriate"
newwccats[measbinds %in% c("righttochoose~", "righttochoose~Rrestrictions", "righttochoose~Ragainst")] <- "RightToChoose"
newwccats[measbinds %in% c("moral~Rmurder", "~Rmurder")] <- "Murder"


bfsets4 <- bfsets2
bfsets4[grepl("Importance-DirectionalTerms", bfsets2) & newwccats %in% c("Importance", "Strength")] <- gsub("Importance-DirectionalTerms", "Importance", bfsets2[grepl("Importance-DirectionalTerms", bfsets2) & newwccats %in% c("Importance", "Strength")])
bfsets4[grepl("Importance-DirectionalTerms", bfsets2) & !(newwccats %in% c("Importance", "Strength"))] <- gsub("Importance-DirectionalTerms", "DirectionalTerms", bfsets2[grepl("Importance-DirectionalTerms", bfsets2) & !(newwccats %in% c("Importance", "Strength"))])

bfsets5 <- bfsets4
bfsets5[bfsets5 %in% names(table(bfsets4)[table(bfsets4)<10])] <- "Small Categories"

majorclass2 <- rep("Neither", length(bfsets4))
majorclass2[grepl("Import", bfsets4)] <- "Importance"
majorclass2[grepl("Direct|Cond|Ident|Public|Notif", bfsets4)] <- "Directional"



nwcc <- paste(bfsets4, newwccats, sep="~")
nwcc[bfsets5=="Excluded"] <- "Excluded"
nwcc[grepl("Conditions", bfsets4) & circadds!=""] <- paste(nwcc[grepl("Conditions", bfsets4) & circadds!=""], circadds[grepl("Conditions", bfsets4) & circadds!=""], sep="~")
nwcc[grepl("Actors|Political", bfsets4) & actor2!=""] <- paste(nwcc[grepl("Actors|Political", bfsets4) & actor2!=""], actor2[grepl("Actors|Political", bfsets4) & actor2!=""], sep="~")
nwcc[grepl("Sing", bfsets4) & sipre!=""] <- paste(nwcc[grepl("Sing", bfsets4) & sipre!=""], sipre[grepl("Sing", bfsets4) & sipre!=""], sep="~")
nwcc[grepl("Ident", bfsets4) & idpre!=""] <- paste(nwcc[grepl("Ident", bfsets4) & idpre!=""], idpre[grepl("Ident", bfsets4) & idpre!=""], sep="~")
nwcc <- gsub("~$", "", gsub("~~", "~", gsub("~NA| |", "", nwcc)))

nwctab <- table(nwcc)
nwcchanger <- names(nwctab)[nwctab<5]

nwcc2 <- nwcc
nwcc2[nwcc %in% nwcchanger] <- sapply(strsplit(nwcc, "~"), function(x) x[1])[nwcc %in% nwcchanger]



avlmfull <- data.frame(avlm2, circumstances=circadds, pregnancyterm=whencirc2, measuretypeQ=qmeas1, measuretypeAns=ansmeas1, fullclassification=nwcc2)
avfull <- data.frame(avlearnmin, bfsets2, circumstances=circadds, pregnancyterm=whencirc2, measuretypeQ=qmeas1, measuretypeAns=ansmeas1, fullclassification=nwcc2)

mintabs <- lapply(names(bfstab), function(x) avfull[bfsets5==x & !avtdup,][order(avlm2[bfsets5==x & !avtdup,"Freq"], decreasing=TRUE),])
names(mintabs) <- gsub("-", "_", names(bfstab))
write.xlsx(mintabs, "AbortionCats/WordingsByMajorTypes.xlsx")

avbuild <- with(avfull, data.frame(UniqueIDlower, bfsets4, bfsets5, majorclass2, circumstances, pregnancyterm, measuretypeQ, measuretypeAns, fullclassification))
avpluspre <- join(av, avbuild)


load("EachFileExports/04-01_MergedDataFiles.rdata")
dfNs <- sapply(dfs, function(x) dim(as.data.frame(x)))[1,]
dfNtm <- data.frame(StudyID=names(dfNs), NewN=dfNs)

avplus <- join(avpluspre, dfNtm)
avplus$N[is.na(avpluspre$N)] <- avplus$NewN[is.na(avpluspre$N)]

save(avplus, file="EachFileExports/07-02_AbortionVariablesReCategorized.rdata")


avmkeep <- avplus[(!(avplus$bfsets5 %in% c("Excluded", "Uncategorized")) | !is.na(avplus$bfsets5)) & (avplus$include!=0 | is.na(avplus$include)) & !grepelf("residents of|residents in", tolower(avplus$sample)) | !(grepelf("likely women|border States|voters in|national adult catholics|female registered voters|illinois|national youth|jewish|ages 18-35|asian|latino|delegates|aged 18-44|who watched|primary|catholic|residing in|county|voters in|hispanic|african", tolower(avplus$sample)) & !grepelf("oversample", tolower(avplus$sample))),]

unqset <- as.character(with(avmkeep, paste(wordingsimp, sep="~")))
unqsetwc <- as.character(with(avmkeep, paste(fullclassification, sep="~")))
unqsetbc <- as.character(with(avmkeep, paste(bfsets5, sep="~")))

unqsorted <- sort(unique(unqset[!is.na(unqset)]))
unqsortedwc <- sort(unique(unqsetwc[!is.na(unqsetwc)]))
unqsortedbc <- sort(unique(unqsetbc[!is.na(unqsetbc)]))

avlister <- with(avmkeep, lapply(unqsorted, function(x) as.data.frame(avmkeep[unqset==x,], stringsAsFactors=FALSE)))
avlisterwc <- with(avmkeep, lapply(unqsortedwc, function(x) as.data.frame(avmkeep[unqsetwc==x,], stringsAsFactors=FALSE)))
avlisterbc <- with(avmkeep, lapply(unqsortedbc, function(x)  as.data.frame(avmkeep[unqsetbc==x,], stringsAsFactors=FALSE)))

ntabs <- function(x, lim=NULL){
    tab <-  rev(sort(table(as.character(x))))
    if(is.null(lim))
        lim <- length(tab)
    tab <- tab[1:lim]
    paste(names(tab), " (", tab, ")", sep="", collapse="; ")
}

qcollapse <- lapply(avlister, function(x) data.frame(wording=as.character(x[1,"wordingsimp"]), responses=gsub("; NA|;NA", "", as.character(paste(names(rev(sort(table(unlist(strsplit(x[,"respssimp"], "; ", fixed=TRUE))))))[1:10], collapse="; "))), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass2"]), subtype=as.character(x[1,"bfsets5"]), specifictype=as.character(x[1,"fullclassification"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE))

wccollapse <- lapply(avlisterwc, function(x) try(data.frame(specifictype=x[1,"fullclassification"], examplewording=gsub("; NA", "", as.character(names(rev(sort(table(as.character(c(x[,"wordingsimp"][x[,"wordingsimp"]!=""], ""))))))[1])), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass2"]), subtype=as.character(x[1,"bfsets5"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE)))

bccollapse <- lapply(avlisterbc, function(x) try(data.frame(subtype=x[1,"bfsets5"], examplewording=gsub("; NA|;NA", "", as.character(names(rev(sort(table(as.character(c(x[,"wordingsimp"][x[,"wordingsimp"]!=""], ""))))))[1])), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass2"]), specifictypes=ntabs(x[,"fullclassification"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE)))

qc <- rbindlist(qcollapse)
wcc <- rbindlist(wccollapse[sapply(wccollapse, class)!="try-error"])
bcc <- rbindlist(bccollapse[sapply(bccollapse, class)!="try-error"])


write.csv(qc[order(paste(qc$type, qc$subtype, qc$specifictype)),], "SummaryInformation/07B-01_ItemBankByWordings.csv")
write.csv(wcc[order(paste(wcc$type, wcc$subtype)),], "SummaryInformation/07B-02_ItemBankByQuestionSubtype.csv")
write.csv(bcc[order(paste(bcc$type)),], "SummaryInformation/07B-03_ItemBankByQuestionType.csv")






