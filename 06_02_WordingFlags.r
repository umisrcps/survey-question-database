library(stringr)

grepelf <- function(pattern, str, fixed=FALSE)
    str_detect(str, pattern, fixed)

load("EachFileExports/05-01_AllQuestionWordingResponses.rdata")
source("99_TranslateResponses.r")

#table(str_detect(md$wordingresponses, "sex|gend|female"))
#table(grepelf("sex|gend|female", md$wordingresponses))

sexflagpre <- (grepelf("sex|gend|female", md$wordingresponses) & !grepelf("repu|dem|peop|shou|think|they|sexu|impor|agre|fav|same-sex|educ|somewhat|grade|sexed|sexage|birth|inter|iwer|ivwr|iwr|homos|marsex|igend|teen|pikup|or more|young|currenthouse|currentsen|cand|mate|acqnt|paid|appr|title nine|believe|mother|between|accept|intgend|wrong|always|support|amer|cong|child|oppo|work|job|trans|care|doc|preg|medi|sex life|enjoy|fear|presid|discrim|partner|amount|buy|hh adult|feel|is name|taxes|length|quota|weight|race|econ|spend|abort|region|zip|iraq|site|adults", md$wordingresponses)) &
    !grepelf("clint|therm|attend|satis|suburb|your age|insur|employ|feminist|parent|health|gender randomization for ll intro|school|influ|others|prestige|market|poor|hunting|marrsex|income|attempts|area code|illegal|division|opinionated|gore|incumb|timing|refuse|prays|dole|porn|bush|view|sex in the city|vote|mccain|incr|having sex|hisp|relig|recoded age|diff|version|msa code|associates|party|violence|lying|how many|order of randomization|belong to|principles|married|do you live in|south|aids|intsex|collapse age|phone|military|repr|hh|rape|offend|retiring|brother|candidate|first|second|third|fourth", md$wordingresponses) &
    !grepelf("female head|vasec|ligat|of head|wife|spouse|relationship|relative|1 adl male|adults", md$wordingresponses) |
    (grepelf("sex|gend|female", md$wordingsimp) & grepelf("obs", md$wordingsimp) & grepelf("inter|iwer|ivwr|iwr", md$wordingsimp))

sexcheck <- respcheck(sexflagpre, "sexfunc")
dropconds <- (grepelf("4|6", sexcheck$unmatched) & !grepelf("female", sexcheck$original)) | sexcheck$translength<2
sexdrops <- sexcheck[dropconds,]

sexflag <- sexflagpre & !(md$respssimp %in% c(sexdrops$original))

write.csv(sexcheck[!dropconds,], "CheckTranslate/SexTrans.csv")
write.csv(rev(sort(table(md$wordingsimp[sexflag]))), "CheckWords/SexWords.csv")
write.csv(sexdrops, "CheckDrops/SexDrops.csv")


raceflagpre <- (((grepelf("white|caucas|anglo", md$wordingresponses) & grepelf("black|african amer|afr amer", md$wordingresponses)) | grepelf("race", md$wordingsimp) | grepelf("race", md$vn)) &
                !grepelf("repub|agree|assemb|repres|democra|state|preference|poor|import|contact|job|relation|justice|confiden|favor|factor|friend|community|school|elect|worr|cities|politic|work|neighbor|want|discrim|marri|attitud|do you think|do you feel|would you say|how many|expens|harder|prejudic|affirmative|employ|likely|easier|lean|treat|approv|trait|dem|party|positiv|impression|relig|ability|welfare|abort|income|phone|economic", md$wordingresponses) &
      !grepelf("suburb|half and half", md$wordingresponses)) &
      !grepelf("income|reason|news|rights|concern|medical|better|satisf|talk|18|countr|contrac|follow|close|govern|presid|senate|house|hse|marry|obama|profil|watch|interviewer|clinton|incr|oppose|prefer|common|dated|cand|sex|horse|year|budget|press|violen|have you|congress|legisl|hiring|hire|address|television|random|form|hurricane|oil|age|spend|angry|weight|lib|strong|strength|aid to|church|tease|too much|difficult|mode|opinion|gender|christ|therm|sample|grace|sen race|proportion|census|trust|often|intell|illness|immigra|date|area code|postadmin|congr race|aff action|gore|timezone|name|wrong|education|ftf|96po|rand|recall", md$wordingresponses) & !grepelf("female|integr", md$respssimp)

raccheck <- respcheck(raceflagpre, "racefunc")
dropconds <- raccheck$translength<2 #| grepelf("female|integr", raccheck$unmatched)
racdrops <- raccheck[dropconds,]

raceflag <- raceflagpre & !(md$respssimp %in% c(racdrops$original))

write.csv(raccheck[!dropconds,], "CheckTranslate/RaceTrans.csv")
write.csv(rev(sort(table(md$wordingsimp[raceflag]))), "CheckWords/RaceWords.csv")
write.csv(racdrops, "CheckDrops/RaceDrops.csv")


hispflagpre <- (grepelf("hisp|latin", md$wordingresponses) & !raceflagpre & !grepelf("born|percent|race of interviewer|issues|party|problem|import|econ|interviewer race|govern|employ|interviewer ethnicity|satisfied|abort|immig|service|prefer|better|life|mother born|discrim|interviewer hisp|father born|issue|cardin|candid|financ|products|hmos|essential|which group|views|legal|immig|presid|telev|portray|relations|information|tensions|public|candi|buy|trust|news|friend|neighbor|pride|celebration|church|clint|reag|bush|obama|trump|educ|therm|popul|rand|polit|gend|school|pope|statements|legis|regul|think|get along|agree|feel close|favor|appro|believe|opini|foreign aid|threat|milit|welfare|how well|prejud|feeling|approve|have in common|how much contact|married|confid|in common|linked|intel|interviewer race|intrace|rate|aid to|angry|hardwork|interviewer ethnicity|close to|cell|as good a chance|congre", md$wordingresponses)) & !grepelf("less|cell|defin|prob|biling", md$respssimp)

hispcheck <- respcheck(hispflagpre, "hispfunc")
dropconds <- hispcheck$translength==0# | grepelf("less|cell|defin|prob|biling", hispcheck$original)
hispdrops <- raccheck[dropconds,]

hispflag <- hispflagpre & !(md$respssimp %in% c(hispdrops$original))

write.csv(rev(sort(table(md$wordingsimp[hispflag]))), "CheckWords/HispWords.csv")
write.csv(hispcheck[!dropconds,], "CheckTranslate/HispTrans.csv")
write.csv(hispdrops, "CheckDrops/HispDrops.csv")

## Note, hispflag needs to be translated after import

nstateflagpre <- ((grepelf("state", md$wordingresponses) | md$wordingsimp=="st" | md$vn=="st") & !grepelf("statem|democ|repub|house|legis|senat|assemb|govern|presid|care|primary|united state|think|know|oppose|iraq|like|feel|satisfied|court|econ|people|world|war|or not|in your|appr|import|vote|true|constit|school|tax|right|state of the union|state of race|abort|local|estate|news|budget|spending|asset|satisfaction|secretary|arab|environ|health|marri|conference|church|legal|liberal|womens|date|ideolog|year|afraid|medica|allow|public|finan|employ|relig|born|christ|foreign|strength|more|party|fund|union|moral|nirvana|watch|spouse|cities|call attempt|education|catholic|europ|order|race|weight|work|sex|age|bible|hse|affect|appoint|future|bush|gore|cheney|lieberman|clinton|kerry|obama|sprint|wireless|father|mother", md$wordingresponses)) |
    (grepelf("florida|fl;|flor", md$respssimp) & grepelf("calif|ca;|california", md$respssimp))# &
   #  !grepelf("statem|democ|repub|house|legis|senat|assemb|govern|presid|care|primary|think|know|oppose|iraq|like|feel|satisfied|court|econ|people|world|war|or not|in your|appr|import|vote|true|constit|school|tax|right|state of the union|state of race|abort|local|estate|news|budget|spending|asset|satisfaction|secretary|arab|environ|health|marri|conference|church|legal|liberal|womens|date|ideolog|year|afraid|medica|allow|public|finan|employ|relig|born|christ|foreign|strength|more|party|fund|union|moral|nirvana|watch|spouse|cities|call attempt|education|catholic|europ|order|race|weight|work|sex|age|bible|hse|affect|appoint|future|bush|gore|cheney|lieberman|clinton|kerry|obama|sprint|wireless", md$wordingresponses)

FIPSflag <- grepelf("fips", md$wordingresponses)

censusregflag <- (grepelf("census reg|region", md$wordingresponses) | (grepelf("south", md$wordingresponses) & grepelf("north east|northeast", md$wordingresponses)))  & !grepelf("democrat|appr|agree|often|bush|health|presiden|oppose|terror|hispanic|asian|money|harvest|church|society|day|republic|phone|vote|liberal|dole|religion|gov|party|violen|nothing|military|crime|gender|cand|obama|think|union|gulf|news|educ|income|perot|clinton|work|child|immigr|econ|race|state|isolat|relig|respondent number|pregn|city|worr|age|school|method|house|few|lean|arm|incn50|finish|market|order|god|import|year|others|age|value|occup|employ|growing|race|event|participate|love|organizat|geographical groups|timing|type of survey|attempt|busy|sample type|father|mother", md$wordingresponses)
  
censusdivflag <- grepelf("census div|division", md$wordingresponses)

locationflag <- nstateflagpre|FIPSflag|censusregflag|censusdivflag

statecheck <- respcheck(locationflag, "stateclean")
dropconds <- statecheck$translength<20 | statecheck$untranslength>3   # | grepelf("less|cell|defin|prob|biling", hispcheck$original)
statedrops <- statecheck[dropconds,]

stateflag <- locationflag & !(md$respssimp %in% c(statedrops$original))

write.csv(rev(sort(table(md$wordingsimp[stateflag]))), "CheckWords/StateWords.csv")
write.csv(statecheck[!dropconds,], "CheckTranslate/StateTrans.csv")
write.csv(statedrops, "CheckDrops/StateDrops.csv")

regioncheck <- respcheck(locationflag, "censregionclean")
dropconds <- regioncheck$translength<4 | regioncheck$untranslength>3   # | grepelf("less|cell|defin|prob|biling", hispcheck$original)
regiondrops <- regioncheck[dropconds,]

regionflag <- locationflag & !(md$respssimp %in% c(regiondrops$original))

write.csv(rev(sort(table(md$wordingsimp[regionflag]))), "CheckWords/regionWords.csv")
write.csv(regioncheck[!dropconds,], "CheckTranslate/regionTrans.csv")
write.csv(regiondrops, "CheckDrops/regionDrops.csv")

divisioncheck <- respcheck(locationflag, "censdivisionclean")
dropconds <- divisioncheck$translength<8 | divisioncheck$untranslength>3   # | grepelf("less|cell|defin|prob|biling", hispcheck$original)
divisiondrops <- divisioncheck[dropconds,]

divisionflag <- locationflag & !(md$respssimp %in% c(divisiondrops$original))

write.csv(rev(sort(table(md$wordingsimp[divisionflag]))), "CheckWords/divisionWords.csv")
write.csv(divisioncheck[!dropconds,], "CheckTranslate/divisionTrans.csv")
write.csv(divisiondrops, "CheckDrops/divisionDrops.csv")

locflag <- locationflag & !((md$respssimp %in% divisiondrops$original) & (md$respssimp %in% regiondrops$original) & (md$respssimp %in% statedrops$original))
locunflag <- locationflag & ((md$respssimp %in% divisiondrops$original) & (md$respssimp %in% regiondrops$original) & (md$respssimp %in% statedrops$original))

write.csv(rev(sort(table(md$wordingsimp[locflag]))), "CheckWords/locationWords.csv")


educationflag <- ((grepelf("educ", md$wordingresponses))|grepelf("grade|level", md$wordingresponses) & grepelf("high|complete", md$wordingresponses) | (grepelf("college", md$respssimp) & grepelf("some|inc", md$respssimp) | grepelf("some col", md$respssimp))) & !grepelf("democrat|republic|house|represent|health|legislat|economy|assembly|state|war|iraq|security|government|california|reduce|david|president|york|texas|robert|sex education|oppose|important|federal|child|united st|tax|agree|federal|country|strongly|deal|improv|relig|church|positive|better|progress|discrim|affirmative|good|you think|likely|sometimes|education or arts|confident|feeling|donate|science education|difficult|standard|constitution|value|problem|proposal|worrie|product|goal", md$wordingresponses) &
    !grepelf("satisfied|race|party|frequent|phone|news|confiden|why you have not|abstinence|responsible|plan|medical|christian|internet|sex|how long|perot|wrs off|dole|rand|approve|politic|deductible|spouse|reduc|pay|crime|reduc|bush|job market|hispanic|interviewer education|state|congress|union|cuts|husband|volunt|view|marital|sports|teacher|at what age|financ|more time|clinton|vote|zip|address|information|trust|cand|econom|women|abortin|educ grp|business|region|satisf|concern|conserv|social class|quality|can usually tell|often|married|weight|do you expect|strong|report card|major reason|employ|what grade would you give|thermom|time zone|age|ethnic|presby|ftf|favor|medicare|times|inc2|marr2|baptist|partner|number", md$wordingresponses)

educcheck <- respcheck(educationflag, "edufunc")
dropconds <- educcheck$translength<4 | educcheck$untranslength>1  # | grepelf("less|cell|defin|prob|biling", hispcheck$original)
educdrops <- educcheck[dropconds,]

educflag <- educationflag & !(md$respssimp %in% c(educdrops$original))

write.csv(rev(sort(table(md$wordingsimp[educflag]))), "CheckWords/EducWords.csv")
write.csv(educcheck[!dropconds,], "CheckTranslate/EducTrans.csv")
write.csv(educdrops, "CheckDrops/EducDrops.csv")

educyrscheck <- respcheck(educationflag, "edufuncyrs")
dropconds <- educyrscheck$translength<14
educyrsdrops <- educyrscheck[dropconds,]

educyrsflag <- educationflag & !(md$respssimp %in% c(educyrsdrops$original, educcheck$original))

write.csv(rev(sort(table(md$wordingsimp[educyrsflag]))), "CheckWords/EducYRSWords.csv")
write.csv(educyrscheck[!dropconds,], "CheckTranslate/EducYRSTrans.csv")
write.csv(educyrsdrops, "CheckDrops/EducYRSDrops.csv")


religdenomflag <- (grepelf("relig", md$wordingresponses) & !grepelf("good|opinion|pres|cand|important|week|unfriendly|times|similar|influe|attitud|agree|accepta|aspect|how well|conversation|school|close|value|america|share|favor|moral|problem|descrim|strong|how many|phone|approp|how often|true|parent|how likely|should|obstacle|interested|spouse|discrim|strength|guidance|only|government|gender|happen to know|do you know|service|freque|violen|do you feel|country|home|pope|month|week|lean|gay|politic|leader|democ|repub|mission|interest|father|mother|active|leave|donate|year|oppose|regular", md$wordingresponses) &
                   !grepelf("how religious|matter|familiar|respect|volunt|positive|pray|strange|party|budget|war|comfortable|likely|mystic|business|vote|public|religious person|conserv|constitution|unfair|mentor|succeed|lifestyle|helpful|spend|order|know about|health|happy|member|were once|do not currently|more|early|television|child|tell|change|like|trait|eternal|abstain|never|march|common|work|law|work|approve|progress|strict|increas|is not a christian|bush|gore|cheney|lieberman|very|conflict|pty|clinton|obama|women|literally|education|has led you|special|is that a christian|religious experience|n of oth|compatible|news|peace|awakening|outside|authorit|income|husband|asian|fips|religious right|conver|abortion|worse|insurance|objection", md$wordingresponses) |
                   (grepelf("christ|cath", md$wordingresponses) & grepelf("jew", md$wordingresponses)) & !grepelf("good|opinion|presi|cand|important|brought|week|unfriendly|times|similar|influe|attitud|agree|accepta|aspect|how well|conversation|school|close|value|america|share|favor|moral|problem|descrim|strong|how many|phone|approp|how often|true|parent|how likely|should|obstacle|interested|spouse|discrim|strength|guidance|only|government|gender|happen to know|do you know|service|freque|violen|do you feel|country|home|pope|month|week|lean|gay|politic|leader|democ|repub|mission|interest|father|mother|active|leave|donate|year|oppose|regular|how religious|matter|familiar|respect|volunt|positive|pray|strange|party|budget|war", md$wordingresponses) &
                   !grepelf("comfortable|likely|mystic|business|vote|public|religious person|conserv|constitution|unfair|mentor|succeed|lifestyle|helpful|spend|order|know about|health|happy|member|were once|do not currently|more|early|television|child|tell|change|like|trait|eternal|abstain|never|march|common|work|law|work|law|work|approve|progress|strict|increas|is not a christian|bush|gore|cheney|lieberman|very|conflict|pty|clinton|obama|women|literally|education|has led you|special|is that a christian|religious experience|n of oth|compatible|news|peace|awakening|outside|authorit|income|husband|asian|fips|religious right|conver|abortion|worse|insurance|objection", md$wordingresponses)) & !grepelf("evangelical|fundamentalist|born-again|born again", md$wordingresponses)

religcheck <- respcheck(religdenomflag, "religfunc")
dropconds <- religcheck$translength<2   # | grepelf("less|cell|defin|prob|biling", hispcheck$original)
religdrops <- religcheck[dropconds,]

religflag <- religdenomflag & !(md$respssimp %in% c(religdrops$original))

write.csv(rev(sort(table(md$wordingsimp[religflag]))), "CheckWords/ReligWords.csv")
write.csv(religcheck[!dropconds,], "CheckTranslate/ReligTrans.csv")
write.csv(religdrops, "CheckDrops/ReligDrops.csv")



religfundflagpre <- (grepelf("fundament|born-again|born again|evangelic|religious right", md$wordingresponses) & !grepelf("democrat|republican|legislat|general|representa|califor|assembl|presid|texas|cornyn|florida|scott|jeff|barbara|robert|bill|mike|richard|cuomo|econo|kashk|dianne|boxer|quinn|carolina|health|tax|black|thermom|govt|worry|married|marry|election|happen to know|control|lean|better|problem|law|friend|constitution|favor|fundamentally|people who are|islamic fundamentalist|fundamental change|fundamental overhaul|vote|respect|gradual development|influence|candidat|opinion|political|therms|easier|abortion|discrim|spouse|strong|fight|respect|governm|rarely|different|sports|conversation|television|fundamentalist muslim|fundamental right|liberty|amendment|likable|how old were you", md$wordingresponses) | grepelf("consider yourself a born-again or evangelical", md$wordingresponses)) & !grepelf("describ|chosen|a lot|moonies|feed|a lot|great deal|not cath", md$respssimp)

religfundcheck <- respcheck(religfundflagpre, "relfundfind")
dropconds <- religfundcheck$translength<1 | religfundcheck$untranslength>4 | grepelf("3|4", religfundcheck$original)#describ|chosen|a lot|moonies|feed|a lot|great deal|not cath
religfunddrops <- religfundcheck[dropconds,]

religfundflag <- religfundflagpre & !(md$respssimp %in% c(religfunddrops$original))

write.csv(rev(sort(table(md$wordingsimp[religfundflag]))), "CheckWords/ReligFundWords.csv")
write.csv(religfundcheck[!dropconds,], "CheckTranslate/ReligFundTrans.csv")
write.csv(religfunddrops, "CheckDrops/ReligFundDrops.csv")



religfreqflagpre <- grepelf("servic|church|worship|temple|mosque|synag", md$wordingresponses) & grepelf("attend|often|freq|week|month|seldom|go to", md$wordingresponses) & !grepelf("democrat|repub|representa|state|assembly|john|jerry|david|cornyn|president|barbara|robert|bill|jeff|new york|cuomo|health|legislat|kashkari|dianne|mother|illinois|problem|not including|important|do you think|do you see|moral|church with r|campaign", md$wordingresponses) &
    !grepelf("clergy|oppose|reason|five years ago|news|product|friend|invite|when you were|volunteer|money|do you pray|medicare|preached|web|secretary|gas|main reason|influence|as a child|tax|society|gay|scripture|leader|teacher|grade|bible|televis|rewarding", md$wordingresponses) &
!grepelf("would you rather|satisfied|church more often|outside of|other than|civic|government service|guidance|business|family|duty|spouse|child|politics|community service|faith|meetings|spanish|latin|particular church|transit|class|party|restaurant|govt|phone|consider yourself|in the past|have you ever|mail|besides attendance|has there ever been|different|meaningful|iraq|do you plan|presence|easier|spirit|job|do you ever|participate in religious services at", md$wordingresponses) & !grepelf("regul|yes|applies|chosen|10 years", md$respssimp)

religfreqcheck <- respcheck(religfreqflagpre, "relfreqfind")
dropconds <- religfreqcheck$translength<3 #grepelf("regul|yes|applies|chosen|10 years", religfreqcheck$original)#religfreqcheck$translength<1 | religfreqcheck$untranslength>4 | grepelf("3|4|describ|chosen|a lot|moonies|feed|a lot|great deal|not cath", religfreqcheck$original)
religfreqdrops <- religfreqcheck[dropconds,]

religfreqflag <- religfreqflagpre & !(md$respssimp %in% c(religfreqdrops$original))

write.csv(rev(sort(table(md$wordingsimp[religfreqflag]))), "CheckWords/ReligfreqWords.csv")
write.csv(religfreqcheck[!dropconds,], "CheckTranslate/ReligfreqTrans.csv")
write.csv(religfreqdrops, "CheckDrops/ReligfreqDrops.csv")


weightflag <- grepelf("weight|wght|wgt|wt", md$wordingresponses) & !grepelf("state|republic|the right|would|want|mike|many|think|feel|jeff|have|barbara|care|sure|able|there|make|please|health|rick|are|enough|all|them|people|work|cuomo|democrat|because|what|gas|job|class|ted|like|see|mother|tom|believe|represent|cost|as a|they|quinn|michael|legislat|dianne|boxer|assembl|like|our|because|refuse|gingrich|school|which|worse|two|growth|former|better|cand|some|reason|listen|watch|transpn|or hasn|one or|on the|of one|party|affirm|postadmin|clinton|dole|church|family|diff|house|senator|resist|timing|gore|gender|respondent number|gun|zip|retired|interviewer initial|order|employ|intell|identify|checkpoint|id number|therm|tax|interviewer|country|date|unique id|age|war|important|pounds|childs|jew|grwth|probe|lbs", md$wordingresponses)

weightflag[md$type=="weight"|md$type=="Weight"] <- TRUE

#partisanflag <- ((grepelf("dem", md$wordingresponses) & grepelf("rep", md$wordingresponses))|grepelf("party", md$wordingresponses)) & !grepelf("favor|approv|trust|better job|positive|oppose", md$wordingsimp)

write.csv(rev(sort(table(md$wordingsimp[weightflag]))), "CheckWords/WeightWords.csv")


# Partisanship Flag

partisanflag <- grepelf("dem", md$wordingresponses) & grepelf("rep", md$wordingresponses) & !grepelf("abort|district|representative|primary|repres|vote|job|happen to know|issue|budget|econ|responsible|state|presid|civil|poor|scandal|blame|senat|blacks|congressman|hearings|work together|convention|farmers|women|wealthy|worker|unemp|retire|business|union|professional|hisp|third major|congress controlled|favor|approv|trust|better job|positive|oppose|like yourself|tea party|considerations|reason|value|reform|will win|ever vote|always|christian|latino|more likely|most important|county|incumb|lobby|congress", md$wordingsimp) &
    !grepelf("special|solve problems|deal of difference|cares more|has more plans|compromise|battle|cand|outcomes|phrase|more conservative|better way|status of seat|country needs|has better ideas about|randomization|were held today|has tried more|out of war|better political leaders|religious|better describes|different ideas about the government|associate more with these terms|phrases|are angry|past ten years|winner|why|whose fault|deficit|more influence|who do you think|contribute|donkey|elephant|liberal|does more|which party will|listens|contacted", md$wordingsimp) &
    !grepelf("called|most members|which party r thinks|which party do you think|contracted|war|describes your impression|most important|your mother|your father|type of|working together|parents|ethics|trump|children|reagan|bush|mondale|ferraro|what have you been happy with|what does it mean to you when someone says they are a democrat|what are those differences|those (important) differences|last five years|race-type|political sign|this year should|biggest problem|congress|the initials|taxes which party|new national party|soc sec|sen ckpt|how long have|house repr|relations which party|obsolete|crime|defense|spending|rand|name not chosen|pre iw session|therm|lik/dis|defse|standing up|protecting|middle class|website|fiscal|iraq|future|in particular that you like|that you dont like|corrupt|stronger political leaders|education|control of congress|didnt vote|did not vote|blame|affect two|majority in congress|incumb", md$wordingresponses) &
    !grepelf("democrats in congress|republicans in congress|recently|how much difference|recall|perot|fundraising|powell|applies more|inflation|ethic|terror|taxes|immig|clergy|endorse|homosex|gay|past political leaders|environ|health|in the news|news story|wasteful|income|energy|agree with|appropriate|conflicts", md$wordingsimp) |
    grepelf("partyid| party id", md$wordingresponses) | grepelf("prty", md$vn)

#partisanselfflag <- ((grepelf("dem", md$wordingresponses) & grepelf("rep", md$wordingresponses))|grepelf("party", md$wordingresponses)) & !grepelf("favor|approv|trust|better job|positive|oppose|like yourself|tea party|considerations|reason|value|reform|will win|ever vote|always|christian|latino|more likely", md$wordingsimp) & grepelf("yourself", md$wordingsimp)

partyflagpre <- partisanflag

partycheck <- respcheck(partyflagpre, "partsorter")
dropconds <- partycheck$translength<2 | partycheck$untranslength>4 | !(grepelf("Democrat", partycheck$translated) & grepelf("Republican", partycheck$translated)) # | grepelf("3|4|describ|chosen|a lot|moonies|feed|a lot|great deal|not cath", religfreqcheck$original)
partycheckdrops <- partycheck[dropconds,]

partyflag <- partyflagpre & !(md$respssimp %in% c(partycheckdrops$original))

write.csv(rev(sort(table(md$wordingsimp[partyflag]))), "CheckWords/PartyWords.csv")
write.csv(partycheck[!dropconds,], "CheckTranslate/PartyTrans.csv")
write.csv(partycheckdrops, "CheckDrops/PartyDrops.csv")

partystrengthpre <- partyflagpre & (md$respssimp %in% partycheckdrops$original)
partystrengthcheck <- respcheck(partystrengthpre, "partystrength")
dropconds <- partystrengthcheck$translength<2 | partystrengthcheck$untranslength>4
partystrengthcheckdrops <- partystrengthcheck[dropconds,]

partystrengthflag <- partystrengthpre & !(md$respssimp %in% c(partystrengthcheckdrops$original))

write.csv(rev(sort(table(md$wordingsimp[partystrengthflag]))), "CheckWords/PartyStrengthWords.csv")
write.csv(partystrengthcheck[!dropconds,], "CheckTranslate/PartyStrengthTrans.csv")
write.csv(partystrengthcheckdrops, "CheckDrops/PartyStrengthDrops.csv")





#censusdivflag2 <- (grepelf("census div", md$wordingresponses))|(grepelf("mountain", md$wordingresponses) & grepelf("pacific", md$wordingresponses))|(grepelf("east north central", md$wordingresponses)) # NOT DONE

presappflag <- grepelf("presi", md$wordingresponses) & grepelf("appr|job", md$wordingresponses) & !grepelf("likely|regardless|difference|better|medicare|fair|vote|debate|which|concern|court|best|representa|law|campaign|leaves office|vice presiden|prepare|senate|health|energy|foreign|welfare|econ|job situation|troop|priorit|haiti|iraq|viet|educat|military|standing|unify|hurrica|securit|prefer|nomin|appropri|middle east|tax|war|often|spend|environ|budget|scandal|domestic|persian|congress|women|crime|situation|cuba|russia|equality|deficit|jobs|cheney|qualif|news|armed|physically|priest|watergate|dole|strike|hillary|cabinet|flood|nuclear|china|europe|drug|defense|transit|tough|relation|crisis|deserve|touch|national|main reason|over his head|employ|terror|power|government|liberal|federal|testif|polic|nader|israel|palestin|continue|gulf|similar|re-elect|talk|immigra|value|most important|already has|afghan|somalia|business|up to the job|trade|strong leadership", md$wordingresponses)

presappcheck <- respcheck(presappflag, "presapp")
dropconds <- presappcheck$translength<2 | presappcheck$untranslength>4
presappcheckdrops <- presappcheck[dropconds,]

presapproveflag <- presappflag & !(md$respssimp %in% c(presappcheckdrops$original))

write.csv(rev(sort(table(md$wordingsimp[presapproveflag]))), "CheckWords/PresAppWords.csv")
write.csv(presappcheck[!dropconds,], "CheckTranslate/PresAppTrans.csv")
write.csv(presappcheckdrops, "CheckDrops/PresAppDrops.csv")


ageflagpre <- (grepelf("age|old|born|birth", md$wordingresponses) | grepelf("18-|between 18 and |18 to ", md$respssimp)) & !grepelf("by|trans|union|show|role|parent|sexism|presid|oral|race|safe|affili|weight|opport|affect|person|name|adult|cand|favor|dif|hurt|change|legal|same|educ|relig|ethn|infl|iwr|sexual|iwer|ivwr|int|child|chld|wif|hus|spo|fips|discrim|agree|partner|abort|liber|homo|belief|socia|occup|income|again|rate|import|approv|percentage|household|obama|think|regist|vote|language|country|state|born in|birthpl|hisp|cong|repr|job|region|inform|class|knowl|fam|citiz|friend|gage|rage|sage|work|housing|us born|house|origin|county|black|nage|conseq|gatekeeper|freq|priority|accept|women|daily|true|menopause|birth control|marri|medical|health", md$respssimp) &
    !grepelf("financ|retire|soldier|satisf|very|believe|per year|someone|america|probably|well|concern|happen|did not|hear|feel|old were you when|insurance|if|type|depends|spend|foreign|gender|marital|govt|opinion|politic|asia|urban|area|problem|dislike|better|environ|gender|wage|ideol|party|welfare|therm|expect|compare|phone|recommend|week|econ|eligi|medicare|business|labor|veteran|drug|terror|impression|supreme", md$respssimp) &
    !grepelf("stamp|grade|repair|office|payment|attn|baptist|when you were|good|govern|sex|have you|trait|respect|hope|right|sex|communit|cell|advantage|immediate|south|population|was it under|employ|survey number|great|inspir|atten|density|racial|men|btr|number of times|born again|born-again|language|income|prices|sample weight|household|hh|average|on a scale from|too old|engage|marriage|hold|compassionate|supporte", md$wordingresponses) &
    !grepelf("speak with|beverage|child|chld|tax|united states|where|knowledge|abort|month|message|shortage|coverage|interviewer age|country|vote|president|expect|possible|born outside|cold war|control|partial|told|wage|born in us|reborn|therm|underage|agenda|cold|usage|image|garbage|educ|product|news|bold|package|teenage|nd person|th person|living with", md$wordingresponses) &
    !grepelf("percentage|in-born|sort of jew|oth person|married|died|bro|kid|manage|provide for|spouse|person #|arrived in us|hobby club|at least 18|how many of|agenci|target|age plan|state|cntry|weight|percent|discrim|person 2|person 3|person 4|person 5|person 6|person 7|encourage|murder|license|damage|age started|friend|miscarriage|mother|sibling|since age|past year|before age|father|since age|sold|age when|age first|courage|son|daught|live birth|old rep", md$wordingresponses) |
    (grepelf("how old are you|age group|age fall between", md$wordingresponses) & !grepelf("language|interviewer age", md$wordingresponses))

agecheck <- respcheck(ageflagpre, "agetrans")

dropconds <- agecheck$translength<4 | agecheck$translength>120 | agecheck$untranslength>4 | grepl("$", agecheck$original, fixed=TRUE) | grepl("%", agecheck$original, fixed=TRUE) | grepelf("thousand", agecheck$original)
agecheckdrops <- agecheck[dropconds,]

ageflag <- ageflagpre & !(md$respssimp %in% c(agecheckdrops$original))

write.csv(rev(sort(table(md$wordingsimp[ageflag]))), "CheckWords/AgeWords.csv")
write.csv(agecheck[!dropconds,], "CheckTranslate/AgeTrans.csv")
write.csv(agecheckdrops, "CheckDrops/AgeDrops.csv")




maritalflagpre <- grepelf("married|marital|divorce|widow", md$wordingresponses) & !grepelf("democrat|house|representa|legisl|repub|general|john|mark|health|should|california|brown|assembl|econ|texas|cornyn|richard|york|bill|florida|robert|war|scott|barbara|jeff|president|mike|dianne|carolina|extramarital|problem|abort|accept|reason|likely|value|government|convinc|social|agree|hear|better|concern|affair|favor|wrong|stor|sex|contracep|recent|church|forever|important|old|gay|why|parent|veteran|member|decision|income|race|good|a sin|view|approve|how|equal|affect|latin|military|comfort|urban|reform|trump|christian|have done|clinton|census|party|justif|relig|hispanic|most|great|debate|employ|you know|willing|very|discrim|would you marry|goal|ther|think|like|your age|elect|before you got|tax", md$wordingresponses)



maritalcheck <- respcheck(maritalflagpre, "maritaltrans")

dropconds <- maritalcheck$translength<1 | maritalcheck$untranslength>4
maritalcheckdrops <- maritalcheck[dropconds,]

maritalflag <- maritalflagpre & !(md$respssimp %in% c(maritalcheckdrops$original))

write.csv(rev(sort(table(md$wordingsimp[maritalflag]))), "CheckWords/MaritalWords.csv")
write.csv(maritalcheck[!dropconds,], "CheckTranslate/MaritalTrans.csv")
write.csv(maritalcheckdrops, "CheckDrops/MaritalDrops.csv")




abortflag <- grepelf("abort|fetus|fetal|pregna|pro choice|pro life|pro-choice|pro-life|life begins|unborn|family planning|pro choice|pro life", md$wordingresponses)
#weightflag <- grepelf("wgt|weight", md$wordingresponses) # STILL NEEDS WORK



droplist <- list(sexdrops, racdrops, hispdrops, statedrops, regiondrops, divisiondrops, educdrops, educyrsdrops, religdrops, religfunddrops, religfreqdrops, partycheckdrops, partystrengthcheckdrops, presappcheckdrops, agecheckdrops, maritalcheckdrops)

preflaglist <- data.frame(sexflagpre, raceflagpre, hispflagpre, locationflag, educationflag, religdenomflag, religfundflagpre, religfreqflagpre, partisanflag, presappflag, ageflagpre, maritalflagpre)

flaglist <- data.frame(sexflag, raceflag, hispflag, stateflag, regionflag, divisionflag, locflag, educflag, educyrsflag, religflag, religfundflag, religfreqflag, weightflag, partyflag, partystrengthflag, presapproveflag, ageflag, maritalflag)


funclist <- list(sexfunc, racefunc, hispfunc, stateclean, censregionclean, censdivisionclean, edufunc, edufuncyrs, religfunc, relfundfind, relfreqfind, partsorter, partystrength, presapp, agetrans, maritaltrans)

preflagged <- rowSums(preflaglist)>0 | abortflag
flagged <- rowSums(flaglist)>0 | abortflag

flagcombo <- preflagged|flagged

class1 <- apply(data.frame(flaglist, abortflag), 1, function(x) gsub("flag", "", names(data.frame(flaglist, abortflag)))[x==TRUE][1])
classmult <- apply(data.frame(flaglist, abortflag), 1, function(x) paste(gsub("flag", "", names(data.frame(flaglist, abortflag)))[x==TRUE], collapse="-"))

save(preflaglist, abortflag, droplist, flaglist, funclist, md, class1, classmult, file="EachFileExports/06-01_Flags.rdata")

kepqfinds <- data.frame(md, flaglist, abortflag, flagcombo, flagged, class1, classmult)

qmp <- kepqfinds[flagcombo|abortflag,]

load("EachFileExports/02-03_MetaInformation.rdata")
qm <- merge(qmp, cm, by="StudyID", all.x=TRUE)

save(qm, kepqfinds, file="EachFileExports/06-02_SelectedQuestionInfo.rdata")



#respsbycode <- lapply(flagset, function(i) rev(sort(table(md$respssimp[i]))))
#wordingsbycode <- lapply(flagset, function(i) rev(sort(table(md$wordingsimp[i]))))
#names(respsbycode) <- names(wordingsbycode) <- colnames(flagset)

#save(wordingsbycode, respsbycode, file="EachFileExports/05-03_UniqueWordingsResponses.rdata")





whatsmissing <- summary(md$type[flagged==FALSE & !is.na(md$type)])
table(flagged, as.character(tolower(md$type)))

missingset <- md$wordingresponses[flagged==FALSE & !is.na(md$type)]




# Not in here for now, but I will try as we continue
#rightdirectionflag <- grepelf("right direction", md$wordingresponses) | (grepelf("satisfied", md$wordingresponses) & grepelf("country|united states", md$wordingresponses)) | (grepelf("how well|satisfied", md$wordingresponses) & grepelf("things are going in the country", md$wordingresponses))
#ideologyflag <- grepelf("liberal", md$wordingresponses) & grepelf("conservative", md$wordingresponses)
#feelingthermsflag <- grepelf("feelings toward", md$wordingresponses)
#congapproveflag <- grepelf("cong", md$wordingresponses) &  grepelf("approve", md$wordingresponses)
#urbanicityflag <- grepelf("urban|city size|suburb", md$wordingresponses)
#econflag <- grepelf("econ", md$wordingresponses) & grepelf("better|condition", md$wordingresponses)
#govsizeflag <- grepelf("small", md$wordingresponses) & grepelf("gov", md$wordingresponses)
#dateflag <- grepelf("date", md$wordingresponses)
#employflag <- grepelf("employ|working", md$wordingresponses) & grepelf("part", md$wordingresponses)
#religimportflag <- grepelf("import", md$wordingresponses) & grepelf("relig", md$wordingresponses)



#religraisedflag <- grepelf("relig", md$wordingresponses) & grepelf("brought|raised", md$wordingresponses)
