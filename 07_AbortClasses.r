library(weights)
library(foreign)
library(memisc)
library(data.table)
library(quanteda)
library(stringr)

grepelf <- function(pattern, str, fixed=FALSE)
    str_detect(str, pattern, fixed)

load("EachFileExports/06-02_SelectedQuestionInfo.rdata")

abortvars <- qm[qm$abortflag,]

rs <- strsplit(abortvars$respssimp, "; ", fixed=TRUE)
abortvars$rsmp <- abortvars$respssimp
abortvars$rsmp[sapply(rs, length)>20] <- "MULTIPLE"

wordingcombinedabort <- gsub("_", " ", abortvars$wordingresponses)

abortvars <- qm[qm$abortflag,]
abortresps <- strsplit(abortvars$respssimp, "; ", fixed=TRUE)

nropts <- sapply(abortresps, length)

abortin <- (sapply(abortresps, function(x) sum(x=="abortion")>0) | grepelf("on abortion", abortvars$respssimp)) & nropts>10
#abortin[is.na(abortin)] <- FALSE

anyabortin <- grepelf("abortion", abortvars$respssimp)

RespCats <- NULL

RespCats$oppose <- grepelf("oppose|fav|unfavorable", abortvars$respssimp) & nropts<20
RespCats$disagree <- grepelf("disagree", abortvars$respssimp) & !grepelf("candidate|stance", abortvars$respssimp) & nropts<20
RespCats$against <- grepelf("against", abortvars$respssimp) & !grepelf("coup|qualified|common man|gun control|minimum|homosex|affirmative", abortvars$respssimp) & nropts<20
RespCats$legalund <- grepelf("legal", abortvars$respssimp) & !grepelf("climate|gay|covert|education|environ|privacy|he would|hes |his views|same-sex|medicare|security|budget|corruption|reagan|covered|terrorism|minimum|bush|christian", abortvars$respssimp) & nropts<20
RespCats$importance <- grepelf("important", abortvars$respssimp) & nropts<20
RespCats$yesno <- grepelf("yes", abortvars$respssimp) & nropts<20
RespCats$warmcool <- grepelf("warm", abortvars$respssimp) & nropts<20
RespCats$choicelife <- (grepelf("pro-choice", abortvars$respssimp) | (grepelf("choice", abortvars$respssimp) & grepelf("life", abortvars$respssimp))) & !grepelf("talked about issues|trust|gore", abortvars$respssimp) & nropts<20
RespCats$likelihood <- grepelf("likely", abortvars$respssimp) & nropts<20
RespCats$approval <- grepelf("approve", abortvars$respssimp) & !grepelf("not aware|wanted more|illegal|fewer restrictions|wouldnt approve", abortvars$respssimp) & nropts<20
RespCats$closely <- grepelf("closely", abortvars$respssimp) & nropts<20
RespCats$should <- grepelf("should", abortvars$respssimp) & nropts<20
RespCats$permitted <- grepelf("permit", abortvars$respssimp) & nropts<20
RespCats$rightwrong <- grepelf("wrong", abortvars$respssimp) & !grepelf("description", abortvars$respssimp) & nropts<20
RespCats$demsreps <- grepelf("democrat", abortvars$respssimp) & nropts<20
RespCats$effective <- grepelf("effective", abortvars$respssimp) & !grepelf("ineffective programs", abortvars$respssimp) & nropts<20
RespCats$murder <- grepelf("murder", abortvars$respssimp) & !grepelf("his wife|death penalty", abortvars$respssimp) & nropts<20
RespCats$extremists <- grepelf("extremists", abortvars$respssimp) & nropts<20
RespCats$appropriate <- grepelf("appropriate", abortvars$respssimp) & nropts<20
RespCats$satisfied <- grepelf("satisfied", abortvars$respssimp) & !grepelf("sex", abortvars$respssimp) & nropts<20
RespCats$true <- grepelf("true", abortvars$respssimp) & !grepelf("stay true", abortvars$respssimp) & nropts<20
RespCats$majorreason <- grepelf("major reason", abortvars$respssimp) & !grepelf("other reason (vol)", abortvars$respssimp) & nropts<20
RespCats$doubts <- grepelf("doubts", abortvars$respssimp) & !grepelf("leadership", abortvars$respssimp) & nropts<20
RespCats$howwell <- grepelf("very well", abortvars$respssimp) & nropts<20
RespCats$possible <- grepelf("possible", abortvars$respssimp) & nropts<20
RespCats$howmuch <- grepelf("a great deal", abortvars$respssimp) & nropts<20
RespCats$easier <- grepelf("easier|difficult", abortvars$respssimp)  & !grepelf("difficulty|sexual harassment|welfare payments|inequality|lack of|difficulties|prayer", abortvars$respssimp) & nropts<20
RespCats$goodbadthing <- grepelf("good thing", abortvars$respssimp) & nropts<20
RespCats$toomuchlittle <- grepelf("too much", abortvars$respssimp) & !grepelf("too much government|too much to restrict|too much of a politician|government control|spend too much|too much defense|court did too much|pushes religion too much|interferes too much", abortvars$respssimp) & nropts<20
RespCats$candidatenames <- grepelf("clinton|gore|obama|trump|bush|romney|reagan|ford|carter|dukakis|dole|mondale|mccain|santorum|paul|kasich|rubio", abortvars$respssimp) & nropts<30
RespCats$quality <- grepelf("poor|fair", abortvars$respssimp) & grepelf("good", abortvars$respssimp) & !grepelf("unfair|poor attitude", abortvars$respssimp) & nropts<20
RespCats$concerned <- grepelf("very concerned", abortvars$respssimp) & nropts<20
RespCats$reduce <- grepelf("reduce", abortvars$respssimp) & !grepelf("deficit|debt|special interests|paperwork|capital|he would not do enough", abortvars$respssimp) &nropts<20
RespCats$amount <- grepelf("some|a lot", abortvars$respssimp) & grepelf("none|a little", abortvars$respssimp) & !grepelf("none of them", abortvars$respssimp) & nropts<10
RespCats$strong <- grepelf("strong", abortvars$respssimp) & !grepelf("defense|economy|military|stronger|strong Voice", abortvars$respssimp) & nropts<20
RespCats$acceptable <- grepelf("acceptable|moral", abortvars$respssimp) & nropts<20
RespCats$possible <- grepelf("possible", abortvars$respssimp) & nropts<20
RespCats$tooconlib <- grepelf("too conservative|too liberal", abortvars$respssimp) & !grepelf("minorit|gay|supreme court is too conservative", abortvars$respssimp) & nropts<10
RespCats$strict <- grepelf("strict", abortvars$respssimp) & !grepelf("restrict", abortvars$respssimp) & nropts<20
RespCats$relevance <- grepelf("relevan|weight", abortvars$respssimp) & !grepelf("irrelevant answers", abortvars$respssimp) &nropts<20
RespCats$overturn <- grepelf("overturn", abortvars$respssimp) & nropts<20
RespCats$banned <- grepelf("banned", abortvars$respssimp) & nropts<20
RespCats$wouldhappen <- grepelf("would happen", abortvars$respssimp) & nropts<20
RespCats$circumstancesconditions <- grepelf("circumstances|conditions", abortvars$respssimp) & !grepelf("not vote", abortvars$respssimp) & nropts<20
RespCats$safety <- grepelf("safe", abortvars$respssimp) & !grepelf("streets|violence|sexual|public safety|food", abortvars$respssimp) & nropts<20
RespCats$persuasive <- grepelf("persuasive", abortvars$respssimp) & nropts<20

respdf <- as.data.frame(RespCats)

Respuncategorized <- rowSums(respdf)==0
Respmulticategorized <- abortvars$respssimp[rowSums(respdf)>1]




respcat <- rep(NA, dim(respdf)[1])
respcat[respdf$strong] <- "Strong"
respcat[respdf$yesno] <- "Yes No"
respcat[respdf$should] <- "Should or Not"
respcat[respdf$oppose] <- "Favor Oppose"
respcat[respdf$amount] <- "Vague Amounts"
respcat[respdf$importance] <- "Importance"
respcat[respdf$persuasive] <- "Persuasive"
respcat[respdf$safety] <- "Safety"
respcat[respdf$wouldhappen] <- "Would or Would Not Happen"
respcat[respdf$banned] <- "Banned or Not"
respcat[respdf$overturn] <- "Overturn"
respcat[respdf$relevance] <- "Relevance"
respcat[respdf$possible] <- "Possible"
respcat[respdf$strict] <- "How Strict"
respcat[respdf$reduce] <- "Reduce"
respcat[respdf$concerned] <- "How Concerned"
respcat[respdf$quality] <- "Vague Quality"
respcat[respdf$against] <- "For Against"
respcat[respdf$toomuchlittle] <- "Too Much or Little"
respcat[respdf$goodbadthing] <- "Good Bad Thing"
respcat[respdf$easier] <- "Easier or Harder"
respcat[respdf$howmuch] <- "Vague Amount 2"
respcat[respdf$howwell] <- "How Well"
respcat[respdf$doubts] <- "Doubts"
respcat[respdf$majorreason] <- "Major Reason"
respcat[respdf$true] <- "True False"
respcat[respdf$satisfied] <- "Satisfied"
respcat[respdf$appropriate] <- "How Appropriate"
respcat[respdf$extremists] <- "Extreme or Reasonable"
respcat[respdf$effective] <- "How Effective"
respcat[respdf$closely] <- "How Closely"
respcat[respdf$approval] <- "Approval"
respcat[respdf$disagree] <- "Agree Disagree"
respcat[respdf$likelihood] <- "Likelihood"
respcat[respdf$acceptable] <- "Acceptable or Not"
respcat[respdf$permitted] <- "Permitted"
respcat[respdf$legalund] <- "Legal"
respcat[respdf$circumstancesconditions] <- "By Circumstances"
respcat[respdf$rightwrong] <- "Right or Wrong"
respcat[respdf$murder] <- "Murder"
respcat[respdf$choicelife] <- "Pro-Choice or Pro-Life"
respcat[respdf$tooconlib] <- "Too Conservative/Liberal"
respcat[respdf$demsreps] <- "Dems or Resps"
respcat[respdf$candidatenames] <- "Candidate Names"
respcat[respdf$warmcool] <- "Warm or Cool"
respcat[abortin] <- "Abortion Dummy"
respcat[is.na(respcat) & anyabortin] <- "Abortion Dummy" 

respcat <- as.factor(respcat)


#wordingsimpset <- rev(sort(table((abortvars$wordingsimp))))

#Exclude
socialissues <- grepelf("abortion and gay marriage|same sex|same-sex|gay marriage and abortion|civil unions|marriage issues|social issues|gay rights|issues like abortion, immigration|and stem cell|abortion/marriage|banish the ten commandments", wordingcombinedabort) & !grepelf("make it more difficult", wordingcombinedabort)
talk <- grepelf("open talk|talk too|talked with|talking about", wordingcombinedabort)
heardabout <- grepelf("heard about", wordingcombinedabort)
teenpregnancy <- grepelf("teen pregnancy|teenage pregnancy|teenaged pregnancy", wordingcombinedabort)
wouldhappen <- grepelf("would|will", wordingcombinedabort) & grepelf("happen", wordingcombinedabort)
false <- grepelf("false", wordingcombinedabort)
news <- grepelf("follow this news|recent news|following the news|go for news|keep up with the news|news organizations|follow this story|in the news", wordingcombinedabort) & !grepelf("to you personally", wordingcombinedabort)
didyoufollow <- grepelf("did you follow", wordingcombinedabort)
catholic <- grepelf("clergy|catholic|pope", wordingcombinedabort)
happentoknow <- grepelf("happen to know", wordingcombinedabort)
saythatcontributes <- grepelf("would you say thatcontributes", wordingcombinedabort)
fromwhatyou <- grepelf("from what you", wordingcombinedabort)
thirdparty <- grepelf("third-party|third party", wordingcombinedabort)
percent <- grepelf("every 100|percent", wordingcombinedabort)
womensmovement <- grepelf("women movement|women's movement|womens movement", wordingcombinedabort)
homosexuality <- grepelf("homosexuality", wordingcombinedabort)
whichcandidatefavor <- grepelf("which candidate favor", wordingcombinedabort)
partyshould <- grepelf("party should", wordingcombinedabort)
remember <- grepelf("remember", wordingcombinedabort)
campaignissue <- grepelf("campaign issue", wordingcombinedabort)
unplannedpregnancy <- grepelf("unplanned pregnan", wordingcombinedabort)
sexed <- grepelf("sex ed", wordingcombinedabort)
hardabortion <- grepelf("hard abortion", wordingcombinedabort)
candidate <- grepelf("boxer|cuomo|pataki|giuliani|john mccain position|which presidential candidate is", wordingcombinedabort)
afayk <- grepelf("as far as you know", wordingcombinedabort)
apppc <- grepelf("appoint pro-choice", wordingcombinedabort) & !grepelf("import", wordingcombinedabort)
sites <- grepelf("sites", wordingcombinedabort)
guncontrol <- grepelf("where the candidates stand on gun control", wordingcombinedabort)
doyouthink <- grepelf("do you think he is likely|think he would|do you think each of", wordingcombinedabort)
sociallyacceptable <-grepelf("socially acceptable", wordingcombinedabort)
repubplat <- grepelf("republican party platform|republican platform|official platform", wordingcombinedabort) & grepelf("should|what is", wordingcombinedabort)
thinkalito <- grepelf("think", wordingcombinedabort) & grepelf("alito", wordingcombinedabort)
permitexcl <- grepelf("permitted", wordingcombinedabort) & grepelf("party|cand|gore|bush|Kerry|clinton|kerry|dole|reagan|trump|romney|obama|carter|mondale", wordingcombinedabort)
discuss <- grepelf("discuss", wordingcombinedabort) & !grepelf("important|reform|financial|social|in this country|moral", wordingcombinedabort)
movietv <- grepelf("movie|tv|television", wordingcombinedabort)
prmexcl <- grepelf("prm", wordingcombinedabort) & grepelf("cartr|reagn|andsn|kndy|reppty|dempty", wordingcombinedabort)
placement <- grepelf("abortion placement", wordingcombinedabort)
birthcontrol <- grepelf("birth control|family planning", wordingcombinedabort) & !grepelf("protest|clinic", wordingcombinedabort)
clintonabortion <- grepelf("clinton abortion", wordingcombinedabort)
veryclosely <- grepelf("very closely", wordingcombinedabort)
lieberman <- grepelf("lieberman", wordingcombinedabort)
didthey <- grepelf("did the supreme court|did congress|did the senate", wordingcombinedabort)
favorablepalin <- grepelf("favorably toward palin", wordingcombinedabort)
stockmarket <- grepelf("stock market", wordingcombinedabort)
suedoctor <- grepelf("sue a doctor", wordingcombinedabort)
heardincrease <- grepelf("if you heard it would increase", wordingcombinedabort)
perot <- grepelf("perot", wordingcombinedabort)
doleabortion <- grepelf("dole abortion", wordingcombinedabort)
isthereanything <- grepelf("is there anything", wordingcombinedabort)
personallyknow <- grepelf("personally know", wordingcombinedabort)
responsibility <- grepelf("responsibility", wordingcombinedabort)
whichfavors <- grepelf("which", wordingcombinedabort) & grepelf("candidate|party", wordingcombinedabort) & grepelf("favors", wordingcombinedabort)
haveyou <- grepelf("have you yourself", wordingcombinedabort)
genetictest <- grepelf("genetic test", wordingcombinedabort)
ashcroft <- grepelf("ashcroft", wordingcombinedabort)
knowanyone <- grepelf("know anyone", wordingcombinedabort)
willencourage <- grepelf("will encourage", abortvars$wordingsimp)
whatkindofjob <- grepelf("what kind of job", wordingcombinedabort)
doesmccain <- grepelf("does mccain", wordingcombinedabort)
alitoagrees <- grepelf("alito agrees", wordingcombinedabort)
protest <- grepelf("demonstrating|abortion clinic|protester", wordingcombinedabort) & !grepelf("legislation", wordingcombinedabort)
bestknowledge <- grepelf("best of your knowledge", wordingcombinedabort)
foster <- grepelf("foster", wordingcombinedabort)
ifabortionillegal <- grepelf("if abortion", wordingcombinedabort) & grepelf("legal", wordingcombinedabort)
repordemrepeat <- grepelf("(Rep candidate name.*){2}|(Dem candidate name.*){2}", wordingcombinedabort)
lookingback <- grepelf("looking back", wordingcombinedabort)
canda <- grepelf("candidate a", wordingcombinedabort)
womanvicepresident <- grepelf("woman vice president", wordingcombinedabort)
howlikely <- grepelf("how likely is it", wordingcombinedabort)
francis <- grepelf("francis", wordingcombinedabort)
goodthing <- grepelf("good thing", wordingcombinedabort) & !grepelf("restrictions", wordingcombinedabort)
needmoreinfo <- grepelf("need more information", wordingcombinedabort)
abortionwill <- grepelf("abortion will", wordingcombinedabort) & !grepelf("court", wordingcombinedabort)
lifebegin <- grepelf("life begin", wordingcombinedabort) 
religiousleaders <- grepelf("religious leader", wordingcombinedabort)
inflthink <- grepelf("influence", wordingcombinedabort) & grepelf("thinking", wordingcombinedabort)
everhad <- grepelf("ever had", wordingcombinedabort)
activism <- grepelf("activis|donat", wordingcombinedabort)
religiousorg <- grepelf("religious org", wordingcombinedabort)
wouldyousaylegis <- grepelf("would you say the legislation", wordingcombinedabort)
ordgore <- grepelf("ord-gore", wordingcombinedabort)
abortioninformant <- grepelf("abortion informant", wordingcombinedabort)
mostinnc <- grepelf("most in nc", wordingcombinedabort)
interestpoll <- grepelf("interest", wordingcombinedabort) & grepelf("poll|public opinion", wordingcombinedabort)
numberofabortions <- grepelf("number of abortions", wordingcombinedabort)
howdifficult <- grepelf("how difficult", wordingcombinedabort)
physicalforce <- grepelf("physical force", wordingcombinedabort)
idiosyncratic <- grepelf("political goals the religious right might have|what kind of stem cells|problem with women|how many abort|weygand|langevin|which activity on abortion|have you recently seen|too late to prevent pregnancy|girl had been given a drug|highest theyve been|a woman running|once had an abortion|associate it more with the man candidate|for preachers to urge their church", wordingcombinedabort)

exclude <- c(socialissues|talk|heardabout|teenpregnancy|wouldhappen|false|news|didyoufollow|catholic|happentoknow|saythatcontributes|fromwhatyou|thirdparty|percent|womensmovement
             |homosexuality|whichcandidatefavor|partyshould|remember|campaignissue|unplannedpregnancy|sexed|hardabortion|candidate|afayk|apppc|sites|guncontrol|doyouthink|sociallyacceptable|repubplat|thinkalito|permitexcl|discuss|movietv|prmexcl|placement|birthcontrol
             |clintonabortion|veryclosely|lieberman|didthey|favorablepalin|stockmarket|suedoctor|heardincrease|perot|doleabortion|isthereanything|personallyknow|responsibility|whichfavors
             |haveyou|genetictest|ashcroft|knowanyone|willencourage|whatkindofjob|doesmccain|alitoagrees|protest|bestknowledge|foster|ifabortionillegal|repordemrepeat|lookingback|canda
             |womanvicepresident|howlikely|francis|goodthing|needmoreinfo|abortionwill|lifebegin|religiousleaders
             |inflthink|everhad|activism|religiousorg|wouldyousaylegis|ordgore|abortioninformant|mostinnc|interestpoll|numberofabortions|howdifficult|physicalforce|idiosyncratic)


notexcluded <- wordingcombinedabort[!exclude]


# Importance

mostimportantcountry <- grepelf("most important issue|main problem|most important problem|most serious problem", wordingcombinedabort) & grepelf("country|nation|your congressman", wordingcombinedabort)
mostimportantvote <- grepelf("most important|mattered most|matters most", wordingcombinedabort) & grepelf("vot|choice|in dec", wordingcombinedabort) & !grepelf("not one of the|less important|one of most important|handling|why do you disapprove|contribute money or time|take part in a public demonstration|more in favor of it or more opposed to it|mainly positive effect|taught|pass an amendment|included in the health care legislation|oppose his confirmation", wordingcombinedabort)
whichparty <- ((grepelf("which cand|party|candidate|whose position|somewhat less likely", wordingcombinedabort) & grepelf("better|trust|handling|represent|close|prefer", wordingcombinedabort))|grepelf("clinton vs trump|if gore is elected", wordingcombinedabort)) & !grepelf("how much influence|first priority|when choosing", wordingcombinedabort)

importantvote <- (grepelf("importan|centrality|would you ever|if you agreed|differ|share|basis|likely|inclined|never vote|disagrees with|reason to|influence|effct|favorite candidate was", wordingcombinedabort) & grepelf("vote|choice|share|voting|support|cand|handl|vt|were to be chosen|choos", wordingcombinedabort) & !grepelf("senate to know|support the bill|anti-abortion cause|how important to you|why do you disapprove|contribute money or time|take part in a public demonstration|more in favor of it or more opposed to it|mainly positive effect|taught|pass an amendment|included in the health care legislation|somewhat less likely|oppose his confirmation", wordingcombinedabort) & !(mostimportantvote|whichparty)) | grepelf("how important to you is the candidates position on abortion", wordingcombinedabort)
courtimportset <- (grepelf("important", wordingcombinedabort) & grepelf("court", wordingcombinedabort) & !grepelf("vote|news|most important|oppose|direction|compromise|following reasons|important right|long ago", wordingcombinedabort)) | (grepelf("important", wordingcombinedabort) & grepelf("court", wordingcombinedabort) & grepelf("before senators vote", wordingcombinedabort))
importsetcong <- grepelf("important", wordingcombinedabort) & grepelf("issue", wordingcombinedabort) & grepelf("congress", wordingcombinedabort) & !grepelf("better job|court|vote|most|late-term|disapprove|decid|piece of advice|you may support|all of the above|good idea", wordingcombinedabort)


reasonspresidentialchoice <- grepelf("reasons for your choice for president", wordingcombinedabort)


approvecand <- grepelf("favorable|approve|very satisfied", wordingcombinedabort) & grepelf("santorum|bush is handl|bush handle|bush approval", wordingcombinedabort)
choicelife <- grepelf("pro choice|pro-choice|pro-life|pro life", wordingcombinedabort) & !importantvote & !grepelf("thermometer", wordingcombinedabort)
moral <- grepelf("moral|wrong|\\bsin\\b", wordingcombinedabort) & !(mostimportantvote|mostimportantcountry) & !grepelf("main problem", wordingcombinedabort)
legal <- grepelf("legal in all|legal in most|legal always|only legal|legal all|legal in certain|never legal|should abortion be legal|legal net|leg/illeg|outlaw|oppose: making abortions illegal|legal in some states|should be legal on|should only be legal|opposemaking abortions illegal|legalize abortion in all states", wordingcombinedabort) & !grepelf("constitution|santorum|24 weeks|nominee", wordingcombinedabort) & !grepelf("^when the|^when a", wordingcombinedabort)
circumset <- grepelf("circumstances", wordingcombinedabort) & grepelf("wants", wordingcombinedabort) & !grepelf("legal|trimester|first|doctor", wordingcombinedabort)
permit <- grepelf("permi|prm|prevent", wordingcombinedabort) & !grepelf("circumstances|government|govt|suprme|supreme|teenager|partial", wordingcombinedabort) & !(legal|moral)
criticalissue <- grepelf("critical issue", wordingcombinedabort)
partialbirth <- grepelf("partial|late-term|late term|dilation|late abortion|birth abortion|p-b", wordingcombinedabort) & !grepelf("regulate|partially", wordingcombinedabort)
roevwade <- (grepelf("roe|1973", wordingcombinedabort) & !grepelf("each state|likely", wordingcombinedabort))|grepelf("supreme court", wordingcombinedabort) & grepelf("first three", wordingcombinedabort)
conamend <- grepelf("constitutional amendment|amendment to the constitution|abortion amend|amendemnt", wordingcombinedabort) & !grepelf("likely", wordingcombinedabort) & !roevwade

parconsent <- grepelf("consent|consnt|parent's permission", wordingcombinedabort)
notifypar <- grepelf("notif|tell", wordingcombinedabort) & grepelf("parent|teen", wordingcombinedabort) & !parconsent & !grepelf("^when", wordingcombinedabort) & !grepelf("parenthood", wordingcombinedabort)
notifyhus <- grepelf("notif|husband|spouse", wordingcombinedabort) & !notifypar


# Circumstances/Conditions

physhealth <- grepelf("physical health", wordingcombinedabort)
womanslife <- grepelf("life", wordingcombinedabort) & grepelf("endangered|mother", wordingcombinedabort) & !grepelf("moral|partial|rape|ban|not necessary", wordingcombinedabort)
fetaldefect <- grepelf("deform|defect|physically impaired|disabilit|mentally impair", wordingcombinedabort)
mentalhealth <- grepelf("mental", wordingcombinedabort) & grepelf("health", wordingcombinedabort)
healthrisk <- grepelf("health|hlth|abortion woman s health", wordingcombinedabort) & grepelf("risk|endang|impair", wordingcombinedabort) & !(physhealth|mentalhealth|legal|importantvote)
unmarried <- grepelf("unmarried|not married", wordingcombinedabort) & !grepelf("teenager", wordingcombinedabort)
unmarriedteen <- ((grepelf("unmarried|not married", wordingcombinedabort) & grepelf("teen", wordingcombinedabort))|grepelf("high school|under 18", wordingcombinedabort)) & !(parconsent|notifypar)
afford <- grepelf("afford|welfare mother", wordingcombinedabort) & !grepelf("fund|hmo", wordingcombinedabort)
doesntwant <- grepelf("unwanted|do not want|does not want|want no more chld", wordingcombinedabort) & !grepelf("married|closest", wordingcombinedabort)
rapeincest <- grepelf("rape or incest|rape/incest|result of rape|result of incest|through rape|been rape|rape victim", wordingcombinedabort) & !grepelf("life in danger|permitted in all|illegal in all", wordingcombinedabort) & !grepelf("fund|the girl", wordingcombinedabort)

othercircum <- grepelf("under what circumstances|when a pregnancy|be legal if|when the sex|abortn legal if", wordingcombinedabort) & !grepelf("legal under any", wordingcombinedabort) & !(physhealth|womanslife|fetaldefect|mentalhealth|healthrisk|unmarried|unmarriedteen|afford|rapeincest)

womandoctor <- grepelf("woman|women wants", wordingcombinedabort) & grepelf("doctor|physician", wordingcombinedabort) & !grepelf("science", wordingcombinedabort) & !(legal|importantvote)
publicresources <- grepelf("fund|should pay|to pay|subsid|pay for abortions|public h|public f|public e|healthcare bill|health care bill|government pay|federal mon|government should help|government health|financial aid|public spend", wordingcombinedabort) & !grepelf("private|candidate", wordingcombinedabort) 
supcourtreq <- grepelf("publicly state|disclose|divulge|ask roberts|insist roberts|insist he explain|required to state|should have been more forthcoming|nominees should be req|important for the senate to know|president to also consider", wordingcombinedabort)
firsttrimester <- grepelf("first three|first trimester|1st three", wordingcombinedabort) & !grepelf("at what point", wordingcombinedabort) & !(roevwade|legal)
secondtrimester <- grepelf("second three|second trimester|4-6 months", wordingcombinedabort) & !grepelf("at what point", wordingcombinedabort)
thirdtrimester <- grepelf("last three|last trimester|after 6|sixth month|third trimester", wordingcombinedabort)
morediff <- grepelf("more difficult|make it harder|more strict|substantial new limits", wordingcombinedabort) & !grepelf("candidate|nominee", wordingcombinedabort) & !moral
strength <- grepelf("strength|strong|whether your own position", wordingcombinedabort) & !grepelf("favor|defense|strongly support|strongly oppose|strongly agree", wordingcombinedabort) & !(partialbirth|roevwade|importantvote|womandoctor|parconsent|morediff|conamend|notifyhus|notifypar|supcourtreq|publicresources|mostimportantvote|fetaldefect|moral)
impself <- ((grepelf("importan", wordingcombinedabort) & grepelf("issue|r abortion", wordingcombinedabort) & !grepelf("to you in|congress|senate|handl", wordingcombinedabort))|grepelf("importance of abortion to r", wordingcombinedabort)) & !(mostimportantcountry|mostimportantvote|importantvote|conamend|criticalissue|legal|publicresources)
stabilityviews <- grepelf("change|compared|has this made you less in favor|have you become", wordingcombinedabort) & grepelf("abortion", wordingcombinedabort) & !grepelf("nomin", wordingcombinedabort) & !(criticalissue|legal|morediff)
murder <- grepelf("murder", wordingcombinedabort) & grepelf("abortion", wordingcombinedabort) & !grepelf("best course", wordingcombinedabort)
TRAP <- ((grepelf("counsel|law allowing|24 hours|requiring|keep record for publ", wordingcombinedabort))|(grepelf("test|doctor", wordingcombinedabort) & grepelf("survive outside", wordingcombinedabort))) & !(parconsent|notifyhus|notifypar|publicresources)
whenrestrict <- grepelf("at what point", wordingcombinedabort) & grepelf("restrict", wordingcombinedabort)
banabortion <- grepelf("ban", wordingcombinedabort) & grepelf("abortion", wordingcombinedabort) & !(partialbirth|conamend|legal|notifyhus|notifypar|publicresources) & !grepelf("candidate|constitution|gun", wordingcombinedabort)
pills <- grepelf("ru-486|morning after|morning-after|ru486|ru 486|french-made pill|r-u-4-86|now available in france", wordingcombinedabort)
regulate <- grepelf("regulate|government should not interfere", wordingcombinedabort) & !permit
advocateseval <- grepelf("extremist", wordingcombinedabort) & grepelf("reasonable", wordingcombinedabort) & !grepelf("what words or phrases", wordingcombinedabort)

courtnominations <- grepelf("supreme court", wordingcombinedabort) & grepelf("senat", wordingcombinedabort) & !grepelf("elec|heard or read|open-ended", wordingcombinedabort)
fedstatedecision <- grepelf("government", wordingcombinedabort) & grepelf("state", wordingcombinedabort) & grepelf("federal", wordingcombinedabort) & grepelf("control|deci", wordingcombinedabort) & !grepelf("elect|import|court|liberal|econom", wordingcombinedabort)


# Tools for adding additional ones

#rev(sort(table(unlist(tokens_remove(tokens(unlist(wordingcombinedabort[is.na(wccat)]), remove_numbers = TRUE, remove_punct = TRUE), c(stopwords("english"), "abortion", "do", "think", "dont", "know"))))))[1:50]

#addcircs <- grepelf("list of circumstances", wordingcombinedabort[is.na(wccat)])

#rev(sort(table(unlist(tokens_remove(tokens(unique(unlist(wordingcombinedabort[is.na(wccat)][gov])), remove_numbers = TRUE, remove_punct = TRUE), c(stopwords("english"), "abortion", "do", "think", "dont", "know"))))))[1:50]

#gov <- grepelf("government", wordingcombinedabort) & grepelf("state", wordingcombinedabort) & grepelf("federal", wordingcombinedabort) & grepelf("control|deci", wordingcombinedabort) & !grepelf("elect|import|court|liberal|econom", wordingcombinedabort)


#courtnominations <- grepelf("supreme court", wordingcombinedabort) & grepelf("senat", wordingcombinedabort) & !grepelf("elec|heard or read|open-ended", wordingcombinedabort)



#table(wccat[gov])

#abortvars$wordingsimp[gov]

#abortvars$wordingsimp[is.na(wccat)][addcircs]
#wordingcombinedabort[!is.na(wccat) & importsetcong]

#[is.na(wccat)]

#abortvars$wordingsimp[is.na(wccat)][circumset]

#rev(sort(table(unlist(tokens(unlist(abortvars$wordingsimp[is.na(wccat)][circumset]), ngrams=3)))))[1:20]
wcbound <- data.frame(fedstatedecision, advocateseval, regulate, pills, banabortion, whenrestrict, TRAP, murder, stabilityviews, impself, strength, morediff, thirdtrimester, 

wccat <- as.character(rep(NA, length(wordingcombinedabort)))
wccat[fedstatedecision] <- "Federal or State Decision"
wccat[advocateseval] <- "Advocate Evaluations"
wccat[regulate] <- "Regulate"
wccat[pills] <- "Pills"
wccat[banabortion] <- "Ban Abortion"
wccat[whenrestrict] <- "When to Restrict"
wccat[TRAP] <- "TRAP Laws"
wccat[murder] <- "Murder"
wccat[stabilityviews] <- "Stability of Views"
wccat[impself] <- "Importance to Self of Issue"
wccat[strength] <- "Strength of Views"
wccat[morediff] <- "Make Abortion More Difficult to Access"
wccat[thirdtrimester] <- "Legal in Third Trimester"
wccat[secondtrimester] <- "Legal in Second Trimester"
wccat[firsttrimester] <- "Legal in First Trimester"
wccat[supcourtreq|courtimportset] <- "Important to Know Supreme Court Nominee View"
wccat[publicresources] <- "Use Public Resources for Abortion"
wccat[womandoctor] <- "Choice Between Woman and Doctor"
wccat[othercircum] <- "Additional Abortion Circumstances"
wccat[rapeincest] <- "Allow for Rape or Incest"
wccat[doesntwant] <- "Allow if Doesn't Want Baby"
wccat[afford] <- "Allow if Cannot Afford Baby"
wccat[unmarriedteen] <- "Allow if Woman is Unmarried Teen"
wccat[unmarried] <- "Allow if Woman is Unmarried"
wccat[healthrisk] <- "Allow if Puts Woman's Health at Risk"
wccat[mentalhealth] <- "Allow if Puts Woman's Mental Health at Risk"
wccat[fetaldefect] <- "Allow if Fetal Defect Detected"
wccat[womanslife] <- "Allow if Woman's Life Endangered"
wccat[physhealth] <- "Allow if Physical Health Endangered"
wccat[notifyhus] <- "Notify Husband"
wccat[notifypar] <- "Notify Parent if Woman is Minor"
wccat[parconsent] <- "Require Parental Consent if Woman is Minor"
wccat[conamend] <- "Constitutional Amendment to Ban"
wccat[roevwade] <- "Roe vs Wade"
wccat[partialbirth] <- "Partial Birth Abortion"
wccat[criticalissue] <- "Critical Issue for Nation"
wccat[permit] <- "Should Abortion Be Permitted"
wccat[circumset] <- "Permit in All Or Some Circumstances"
wccat[legal] <- "Legal"
wccat[moral] <- "Moral"
wccat[choicelife] <- "Pro-Choice or Pro-Life"
wccat[approvecand] <- "Candidate Approval"
wccat[importsetcong] <- "Importance for Congress"
wccat[importantvote] <- "Importance for Vote"
wccat[whichparty] <- "Preferred Party on Abortion"
wccat[mostimportantvote] <- "Most Important Issue for Voting"
wccat[mostimportantcountry] <- "Most Imporant Issue for Country"
wccat[reasonspresidentialchoice] <- "Reasons for Presidential Choice"
wccat[respcat=="Warm or Cool"] <- "Thermometer Ratings of Groups"
wccat[courtnominations] <- "Senators Should Know/Vote on Justice Abortion View"
wccat[exclude] <- "Excluded"


bigcats <- as.character(rep(NA, length(wordingcombinedabort)))
bigcats[wccat %in% c("Roe vs Wade", "Should Abortion Be Permitted", "Legal", "Choice Between Woman and Doctor", "Permit in All Or Some Circumstances", "Ban Abortion", "Constitutional Amendment to Ban", "Regulate", "When to Restrict")] <- "Regulation"# legal|permit|regulate|banabortion|whenrestrict|conamend|womandoctor|roevwade|circumset
bigcats[wccat %in% c("Partial Birth Abortion", "Legal in First Trimester", "Legal in Second Trimester", "Legal in Third Trimester")] <- "Pregnancy Term"
bigcats[wccat %in% c("Allow if Puts Woman's Health at Risk", "Allow if Fetal Defect Detected", "Allow if Puts Woman's Mental Health at Risk", "Allow if Woman's Life Endangered", "Allow if Physical Health Endangered")] <- "Pregnancy Circumstances"
bigcats[wccat %in% c("Allow if Doesn't Want Baby", "Allow if Cannot Afford Baby", "Allow for Rape or Incest", "Additional Abortion Circumstances", "Allow if Woman is Unmarried", "Allow if Woman is Unmarried Teen")] <- "Maternal Circumstances"#doesntwant|afford|rapeincest|unmarriedteen|unmarried|othercircum
bigcats[wccat %in% c("Make Abortion More Difficult to Access", "Require Parental Consent if Woman is Minor", "Notify Husband", "Notify Parent if Woman is Minor", "Use Public Resources for Abortion", "Pills", "TRAP Laws")] <- "Access"#TRAP|parconsent|notifyhus|notifypar|morediff|publicresources|pills
bigcats[wccat %in% c("Importance for Vote", "Most Imporant Issue for Country", "Importance to Self of Issue", "Strength of Views", "Most Important Issue for Voting", "Critical Issue for Nation", "Important to Know Supreme Court Nominee View", "Reasons for Presidential Choice", "Senators Should Know/Vote on Justice Abortion View", "Importance for Congress")] <- "Importance" #mostimportantcountry|mostimportantvote|importantvote|criticalissue|impself|reasonspresidentialchoice|importsetcong|courtnominations
bigcats[wccat %in% c("Thermometer Ratings of Groups", "Pro-Choice or Pro-Life", "Advocate Evaluations")] <- "Pro-Choice or Pro-Life" #choicelife|advocateseval
bigcats[wccat %in% c("Murder", "Moral")] <- "Morality"#murder|moral
bigcats[exclude] <- NA

majorclass <- rep(NA, length(wordingcombinedabort))
majorclass[grepelf("Import|View|Issue", wccat)] <- "Attitude Strength"
majorclass[grepelf("Allow|Legal|Moral|Require|Roe|Partial|Permit|Pro-Choice|Use Public|Difficult|Murder|Restrict|TRAP|Ban|Regulate|Pills|Notify|Circumstances|Choice Between|Advocate", wccat)] <- "Directional Attitude"
#majorclass[exclude] <- "Excluded"


abortvars$wccat <- as.factor(wccat)
abortvars$bigcats <- as.factor(bigcats)
abortvars$majorclass <- as.factor(majorclass)

avunclassed <- abortvars[is.na(wccat),]

write.csv(rev(sort(table(avunclassed$wordingresponses))), "Troubleshooting/07-01_UncategorizedAbortionQs.csv")

av <- abortvars[,!grepelf("flag", names(abortvars))]

#Import Meta-Info and Merge

load("EachFileExports/02-03_MetaInformation.rdata")

avmeta <-av# merge(av, cm, all.x=TRUE, by="StudyID", sort=FALSE)

avmkeep <- avmeta[(avmeta$wccat!="Excluded" | !is.na(avmeta$wccat)) & (avmeta$include!=0 | is.na(avmeta$include)) & !grepelf("residents of|residents in", tolower(avmeta$sample)) | !(grepelf("likely women|border States|voters in|national adult catholics|female registered voters|illinois|national youth|jewish|ages 18-35|asian|latino|delegates|aged 18-44|who watched|primary|catholic|residing in|county|voters in|hispanic|african", tolower(avmeta$sample)) & !grepelf("oversample", tolower(avmeta$sample))),]

save(av, avmeta, avmkeep, file="EachFileExports/07-01_AbortionVariablesCategorized.rdata")

unqset <- as.character(with(avmkeep, paste(wordingsimp, sep="~")))
unqsetwc <- as.character(with(avmkeep, paste(wccat, sep="~")))
unqsetbc <- as.character(with(avmkeep, paste(bigcats, sep="~")))

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

qcollapse <- lapply(avlister, function(x) data.frame(wording=as.character(x[1,"wordingsimp"]), responses=gsub("; NA", "", as.character(paste(names(rev(sort(table(unlist(strsplit(x[,"respssimp"], "; ", fixed=TRUE))))))[1:10], collapse="; "))), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass"]), subtype=as.character(x[1,"bigcats"]), specifictype=as.character(x[1,"wccat"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE))

wccollapse <- lapply(avlisterwc, function(x) data.frame(specifictype=x[1,"wccat"], examplewording=gsub("; NA", "", as.character(names(rev(sort(table(as.character(x[,"wordingsimp"])))))[1])), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass"]), subtype=as.character(x[1,"bigcats"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE))

bccollapse <- lapply(avlisterbc, function(x) data.frame(subtype=x[1,"bigcats"], examplewording=gsub("; NA", "", as.character(names(rev(sort(table(as.character(x[,"wordingsimp"])))))[1])), timesasked=dim(x)[1], firstasked=min(x[,"sd"], na.rm=TRUE), lastasked=max(x[,"ed"], na.rm=TRUE), type=as.character(x[1,"majorclass"]), specifictypes=ntabs(x[,"wccat"]), totN=sum(as.numeric(x[,"N"]), na.rm=TRUE), surveyorgs=ntabs(x[,"surveyorg"]), surveysponsors=ntabs(x[,"sponsor"]), datatype=ntabs(x[,"datatype"]), stringsAsFactors=FALSE))


qc <- rbindlist(qcollapse)
wcc <- rbindlist(wccollapse)
bcc <- rbindlist(bccollapse)


write.csv(qc[order(paste(qc$type, qc$subtype, qc$specifictype)),], "SummaryInformation/07-01_ItemBankByWordings.csv")
write.csv(wcc[order(paste(wcc$type, wcc$subtype)),], "SummaryInformation/07-02_ItemBankByQuestionSubtype.csv")
write.csv(bcc[order(paste(bcc$type)),], "SummaryInformation/07-03_ItemBankByQuestionType.csv")






