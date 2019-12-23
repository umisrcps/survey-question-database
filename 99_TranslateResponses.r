library(stringr)

grepelf <- function(pattern, str, fixed=FALSE)
    str_detect(str, pattern, fixed)

respcheck <- function(flagname, translatefunction){
    eachresp <- strsplit(as.character(names(rev(sort(table(md$respssimp[flagname]))))), "; ")
    n <- rev(sort(table(md$respssimp[flagname])))
    eachresptrans <- lapply(eachresp, function(x) sapply(x, translatefunction))
    transdf <- data.frame(original=sapply(eachresp, function(x) paste(x, collapse="; ")), translated=sapply(eachresptrans, function(x) paste(x[!is.na(x) & x!=""], collapse="; ")), unmatched=sapply(1:length(eachresp), function(x) paste(eachresp[[x]][is.na(eachresptrans[[x]]) | eachresptrans[[x]]==""], collapse="; ")), translength=sapply(eachresptrans, function(x) length(x[!is.na(x) & x!=""])),  untranslength=sapply(eachresptrans, function(x) length(x[is.na(x) | x==""])), n=as.vector(n))
    dduptrans <- transdf[!duplicated(transdf),]
}

sexfunc <- function(x){
    out <- rep(NA, length(x))
    out[grepelf("male|man", x) & !grepelf("female|woman", x)] <- "Male"
    out[x==1|x=="m"|x=="m     "|x==" 1"] <- "Male"
    out[grepelf("female|woman", x)] <- "Female"
    out[x==2|x==0|x=="f"|x=="f     "|x==" 2"] <- "Female"
    out[grepelf("hh|adult|child|age", x)] <- NA
    as.factor(out)
}

racefunc <- function(x){
    white <- grepelf("white|anglo|cauc|american|germa|hermen|engl|jewish|arab|euro|egyp|copt|syri|ital|polish|russi|swed|indoeur|persi|middle east|mediterr|armeni|cajun|celtic|irish|ukran|pakist|greek|greec|irish|norw|portu|ukrain|amaerican|assyerian", x)
    black <- grepelf("black|afric|negro|aa|somali", x)
    hispanic <- grepelf("hisp|latin|span|mex|puerto|cuba|hati|creol|brazi|chili|argen|jamai|domini|guatem|salvad|costa|carrib|honduras|hait|guian|coastri", x) & !grepelf("non|not|~", x)
    other <- grepelf("asi|api|pac|nati|2+|mult|races|other|mixed|pther|mest|brown|indi|ameri/ind|orie|amer/ind|chin|cherok|mix race|birac|bi-rac|hawai|mix|polyn|naveh|navaj|filipino|one of each|idain|vari|samoa|phillip|poly|far east|something else|all of the|amerind|vietnamese|japanese|korean|guamanian|chamorro|yellow|trinidad|bahamanian", x) |grepelf("non|not|~", x)
    rac <- rep(NA, length(x))
    rac[other] <- "other"
    rac[white] <- "white"
    rac[black] <- "black"
    rac[(white & black)|other] <- "other"
    rac[hispanic] <- "hispanic"
    rac[x %in% c("1", " 1", "w")] <- "white"
    rac[x %in% c("2", " 2", "b")] <- "black"
    rac[x %in% c("h")] <- "hispanic"
    rac[x %in% c("o", "a", "u")] <- "other"
    as.factor(rac)
}

hispfunc <- function(x){
    hispanic <- grepelf("mexi|cuba|puert|yes|hisp|si|latin|central|hisp|latin|span|mex|puerto|cuba|hati|creol|brazi|chili|argen|jamai|domini|guatem|salvad|costa|carrib|honduras|hait|guian|coastri|hispanic survey done in spanish", x) & !grepelf("~|not|non", x)
    hispanic[x %in% as.character(c("", -9:999, paste("00", -9:99, sep=""), paste("0", -9:99, sep=""), paste(" ", -9:9, sep=""), "&", "rf")) | (grepelf("dk|na|no ans|ref|don|inap|do not|ny|vietnamese|asian or asian-american (eg asian indian chinese filipino vietnamese or other asian origin groups)|black or african-american (eg negro kenyan nigerian haitian)", x) & !grepelf("nat", x))] <- NA
    hispanic
}

fipslink <- cbind(c(state.abb, "DC"), c(1:2, 4:6, 8:10, 12:13, 15:42, 44:51, 53:56, 11))

stateclean <- function(x){
    y <-   gsub("^.*_or_", "", gsub("_duplicated_", "", gsub("[0-9]", "", gsub(",", "", gsub("*", "", gsub(".", "", gsub(" ", "", gsub(" or ", "_or_", tolower(x)), fixed=TRUE), fixed=TRUE), fixed=TRUE)))))
    for(i in gsub(" ", "", tolower(state.name)))
        y[y==i & !is.na(y)] <- state.abb[gsub(" ", "", tolower(state.name))==i]
    if(!(sum(as.numeric(as.character(x)) %in% c(3, 7, 14, 43, 52))>10))
        for(i in fipslink[,2])
            y[as.numeric(as.character(x))==i & !is.na(x)] <- fipslink[fipslink[,2]==i,1]
    out <- factor(toupper(y), levels=c(state.abb, "DC"))
    out[y %in% c("districtofcolumbia", "washingtondc", "distofcolumbia", "washingtond", "districtofcolumbia/washingtondc", "washdc", "districtofcolu")] <- "DC"
    out[y %in% c("wisconsim")] <- "WI"
    out[y %in% c("lousiana")] <- "LA"
    out[y %in% c("massachuesetts")] <- "MA"
    out[y %in% c("illionois")] <- "IL"
    out[y %in% c("socarolina")] <- "SC"
    out[y %in% c("arisona")] <- "AZ"
    out[y %in% c("tennssee", "tenessee")] <- "TN"
    out[y %in% c("neveda")] <- "NV"
    out[y %in% c("minesota *", "27 minesota *")] <- "MN"
    out[y %in% c("new hampsire")] <- "NH"
    out[y %in% c("missouir")] <- "MO"
    out
    #rev(sort(table(y[is.na(out)])))[1:30] # Use this to check what gets dropped with state clean variables
}

censregionclean <- function(x){
    y <-   gsub("\\(.*\\)", "", gsub("_duplicated_", "", gsub("[0-9]", "", gsub(",", "", gsub("*", "", gsub(".", "", gsub(" ", "", gsub(" or ", "_or_", tolower(x)), fixed=TRUE), fixed=TRUE), fixed=TRUE)))))
    out <- factor(y, levels=c("northeast", "south", "midwest", "west"), labels=c("Northeast", "South", "Midwest", "West"))
    out[y %in% c("northcentral", "northcentral(iliniaksmimn", "midwest:iliniaksmimnmonendohsdwi", "northcentral(illinoisindianaiowakansas", "northcentral(illinoisindianaiowa", "northcentrali", "northcentralil", "northern/central", "northcentral(iliniaksmi", "midwestregion", "mideast")] <- "Midwest"
    out[y %in% c("west(akazcacohiidmtnm", "west:akazcacohiidmtnvnmorutwawy", "west(arizonacaliforniacoloradoo", "west:akazcacohiidmtnvnmoratwawy", "west(arizonacaliforniacoloradooregonutah", "west(akazcacohiidmt", "westregion", "western", "interiorwest", "pacific")] <- "West"
    out[y %in% c("northeast(ctmemanhnjnypa", "northeast(connecticutmassachusettsnew", "northeast(connecticutmassachusetts", "east:ctmemanhnjnyparivt", "northeast(ctmemanhnjny", "northeast(ctmemanhnjnyp", "northeastregion", "east")] <- "Northeast"
    out[y %in% c("southeast", "south:alarflgakylamsncoksctntxva", "south(alabamaarkansasdelawaredistrictof", "south(alardedcflgakyla", "south(alabamaarkansasdelawaredi", "south:alardedcflgakylamdmsncoksctntxvawv", "southern", "south(alardedcflgakylamdmsncoksctntxvawv", "southregion", "southsouth")] <- "South"
    out[x %in% c("northcentral", "northcentral(iliniaksmimn", "midwest:iliniaksmimnmonendohsdwi", "northcentral(illinoisindianaiowakansas", "northcentral(illinoisindianaiowa", "northcentrali", "northcentralil", "northern/central", "northcentral(iliniaksmi", "midwestregion", "mideast", "midwest", "midwest (il in ia ks mi mn mo nd sd oh wi)", "northcentral (il in ia ks mi mn mo nd sd oh wi)")] <- "Midwest"
    out[x %in% c("west(akazcacohiidmtnm", "west:akazcacohiidmtnvnmorutwawy", "west(arizonacaliforniacoloradoo", "west:akazcacohiidmtnvnmoratwawy", "west(arizonacaliforniacoloradooregonutah", "west(akazcacohiidmt", "westregion", "western", "interiorwest", "pacific", "west", "west (az co mt nm ut wy ca id nv or wa ne)")] <- "West"
    out[x %in% c("northeast(ctmemanhnjnypa", "northeast(connecticutmassachusettsnew", "northeast(connecticutmassachusetts", "east:ctmemanhnjnyparivt", "northeast(ctmemanhnjny", "northeast(ctmemanhnjnyp", "northeastregion", "east", "northeast", "east (ct nh me ny ri ma nj pa vt)", "northeast (ct nh me ny ri ma nj pa vt)", "northeast (ct dc de md nh me ny ri ma wv nj pa vt)", "east (ct dc de md nh me ny ri ma wv nj pa vt)")] <- "Northeast"
    out[x %in% c("southeast", "south:alarflgakylamsncoksctntxva", "south(alabamaarkansasdelawaredistrictof", "south(alardedcflgakyla", "south(alabamaarkansasdelawaredi", "south:alardedcflgakylamdmsncoksctntxvawv", "southern", "south(alardedcflgakylamdmsncoksctntxvawv", "southregion", "southsouth", "south", "south (al fl ky ms ok tn va ar ga la nc sc tx dc de md wv)", "south (al fl ky ms ok tn va ar ga la nc sc tx)")] <- "South"
    out[as.numeric(as.character(x))==1] <- "Northeast"
    out[as.numeric(as.character(x))==2] <- "Midwest"
    out[as.numeric(as.character(x))==3] <- "South"
    out[as.numeric(as.character(x))==4] <- "West"
    out
    #rev(sort(table(y[is.na(out)])))[1:30]
}

censdivisionclean <- function(x){
    y <-   gsub("-", "", gsub("\\(.*\\)", "", gsub("_duplicated_", "", gsub("[0-9]", "", gsub(",", "", gsub("*", "", gsub(".", "", gsub(" ", "", gsub(" or ", "_or_", tolower(x)), fixed=TRUE), fixed=TRUE), fixed=TRUE))))))
    out <- factor(y, levels=c("newengland", "middleatlantic", "eastnorthcentral", "westnorthcentral", "southatlantic", "eastsouthcentral", "westsouthcentral", "mountain", "pacific"), labels=c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))
    out[y %in% c("new england", "northeast")] <- "New England"
    out[y %in% c("mid atlantic", "middle atlantic")] <- "Middle Atlantic"
    out[y %in% c("encentral", "eastnorthcentr", "greatlakes", "estnrthcentral", "east north central")] <- "East North Central"
    out[y %in% c("westnorthcentr", "wesynorthcentral", "wstnrthcentral", "west north central")] <- "West North Central"
    out[y %in% c("south atlantic")] <- "South Atlantic"
    out[y %in% c("escentral", "east south central")] <- "East South Central"
    out[y %in% c("southcentral", "wscentral", "west south central")] <- "West South Central"
    out[y %in% c("interiorwest", "moutain", "mountainstates", "mountain", "rocky mountain")] <- "Mountain"
    out[y %in% c("pacificstates", "pacific", "ak/hi")] <- "Pacific"
    out[x %in% c("new england")] <- "New England"
    out[x %in% c("mid atlantic", "middle atlantic", "mid-atlantic", "(2) mid-atlantic", "(2) 2 mid-atlantic", "2 mid-atlantic")] <- "Middle Atlantic"
    out[x %in% c("encentral", "eastnorthcentr", "greatlakes", "estnrthcentral", "east north central")] <- "East North Central"
    out[x %in% c("westnorthcentr", "wesynorthcentral", "wstnrthcentral", "west north central", "w n central", "wn central")] <- "West North Central"
    out[x %in% c("south atlantic", "south altantic")] <- "South Atlantic"
    out[x %in% c("escentral", "east south central")] <- "East South Central"
    out[x %in% c("southcentral", "wscentral", "west south central")] <- "West South Central"
    out[x %in% c("interiorwest", "moutain", "mountainstates", "mountain", "rocky mountain")] <- "Mountain"
    out[x %in% c("pacificstates", "pacific", "ak/hi")] <- "Pacific"
    out[as.numeric(as.character(x))==1] <- "New England"
    out[as.numeric(as.character(x))==2] <- "Middle Atlantic"
    out[as.numeric(as.character(x))==3] <- "East North Central"
    out[as.numeric(as.character(x))==4] <- "West North Central"
    out[as.numeric(as.character(x))==5] <- "South Atlantic"
    out[as.numeric(as.character(x))==6] <- "East South Central"
    out[as.numeric(as.character(x))==7] <- "West South Central"
    out[as.numeric(as.character(x))==8] <- "Mountain"
    out[as.numeric(as.character(x))==9] <- "Pacific"
    out
    #rev(sort(table(y[is.na(out)])))[1:30]
}




edufunc <- function(x){
    out <- rep(NA, length(x))
    lths <- grepl("less|lt|incomplete|no formal", x) & !grepl("coll", x) & !grepl("adult", x)
    hsgrad <- grepl("grad|dipl|complete|credential", x) & grepl("high|hs|h.s.", x) & (!grepelf("grade", x) | grepl("12", x)) & !grepl("less|lt|incomplete", x)
    vocat <- grepl("vocat|techn", x)
    gradenum <- grepl("grade", x) & !grepl("gradu|school grad|hs grad|complete|coll|h.s. d|post|after hs", x)
    somehs <- grepl("some high school|some hs|some school|some h.s.|some grade", x)
    somecol <- grepl("some|of college", x) & !grepl("post|high|hs|h.s.|grad school|some school|some graduate|some grade", x)
    colincomplete <- grepl("coll", x) & grepl("inc", x)
    assoc <- grepl("assoc", x) & !grepl("college graduate", x)
    colgrad <- grepl("grad|degree|college+|graduate school|master|bachelor|doctor|ba level", x) & grepelf("deg|coll|post|study", x) & !grepelf("grade|not", x)
    degrees <- grepl("master|bachelor|doctor", x) & !grepl("no", x)
    addlths <- x %in% c("high school (9-12)", 1, "4. more than 12 years of schooling, no h", "completed 9, 10, 11 grades", "more than 12 years of schooling, no hi", "9-11 gr-no diplo", "9-11 grde,no fur", "*more than 12 years of schooling, no h", "04. more than 12 years of schooling, no hi", "4. more than 12 years of schooling, no", "school up to 5 to 9 years",
                        "9-11 gr+training", "nine years", "11 grades plus non-college training; ", "< high school       ", "primary school not completed (std 7/8)", "nine years completed", "part high school", "secondary school not completed (form 4)", "no high school", "eight years", "9-11 +train", "ten years", "primary scholl completed (std 7/8)", "ten years completed", "eleven years", "eight years completed", "eleven years completed", "9-11 gr+training", "hs 9-11", "10 grages", "finished middle school", "9th - 11th",
                        "completed 8 grades plus non-college tr", "10 grades plus non-college training", "9-11 grde,traini", "seven years completed", "10. ten years completed", "11. eleven years completed", "08. eight years completed", "seven years", "six years completed", "no formal education", "9 grades plus non-college training", "six years", "none/cannot read or write", "attended hs", "school up to 4 years",
                        "09. nine years completed" , "complete primary school", "did not finish high school", "five years completed", "07. seven years completed", "five years", "none, no grades completed", "06. six years completed", "no school", "elementary school_duplicated_7", "literate but no formal schooling", "did not finish middle school", "05. five years completed", "04. four years completed", "02. two years completed", "elementary school_duplicated_2", "elementary school_duplicated_6",
                        "l/hs gra", "0-7th", "8th", "9-11th", "high school inc grades 9-11", "dropout", "grade school graduate", "grade school graduate(8th)", "grade school graduate (8th)", "completed grade school", "grade school education (completed 8th grade)", "completed 8th grade", "4 more than 12 years of schooling no h", "9th", "no schooling completed", "no schooling", "elementary", "non-high school")
    addhsgrad <- x %in% c("completed 12 grades", "tech/trade", 2, "12 yrs completed", "technical/trade/business after high school", "twelve years", "technical/trade/business school after high school", 12, "twelve years completed (or 1 in y3a)", "technical, trade, or business after high school", "12 grades plus non-college training", "h.s. dip +train", "twelve years completed", "04 more than 12 years",
                          "twelve years completed (or 1 in y3e)", "more than 12 years", "technical school/other", "finished high school", "12. twelve years completed (or 1 in y3a)", "high school", "secondly school completed (form 4)", "some tech school", "12. twelve years completed (or 1 in y3e)", "technical school/other                              ", "trade-voc school", "technical, trade, or business", "equivalency <priority over codes 11-43", "tech, grade or business after hs", " technical, trade, or business after high school ", "technical, trade or business after high school ", "tech,voc past hs", "technical, trade or business", "technical, trade, or business school", "technical, trade or business after high school", "high school_duplicated_11", "(3) technical/trade school", "trade/voc-ed school", "trade school", "high school_duplicated_10", "elementary school_duplicated_8", "diploma", "tech", "tech sch", "completed 12th grade", "(2) 2 high school", "hs dip +train", "high school ", "high school", "voc ed")
    addsomecol <- x %in% c("fourteen years", "thirteen years", 3, "fifteen years", "part coll,university", "jr-community col", "13. thirteen years", "junior-community", "15. fifteen years", "2-yr cllg grdt (cmmnty cllg, tc )", "college/polytechnic not complerted (diploma /certificate)", "not completed university degree", "(4) grade 13-17 with no college degree", "i attended college but did not graduate", "attended college but did not graduate", "come col", "(5) 2 or 3 yrs coll/aa-1egree", "2-yr cllg grdt (cmmnty cllg tc )")
    addcollegeplus <- x %in% c("graduate school or more", "grad-bachelors", 4, "17 years or more", "some graduate school", "college graduate (grade 16)        ", "mor th 4 yr col", "post graduate (grade 17 and over)  ", "over 4 yrs coll", "colege, university, grad.", "14. fourteen years", "graduate school", "grad studien", "more than 4 yrs", "graduate/professional school (16 or more)", "7. phd", "some grad school", "phd, litd, scd, dfa, dlit, dph, dphil,", "llb, jd", "(6) some graduate school", "some graduate", "grad \rschool", "jcd, std, thd", "post-graduate not completed", "coll+", "coll/deg",
                               "grad sch", "col grad", "some graduate work", "(5) grade 13-16 with college degree", "(6) grade 17 with college degree", "i have some graduate school courses", "grad/stu", "some graduate work", "some graduate work (completed more than 17 years of education)", "17+ years no postg", "graduate/professional", "(03) phd litd scd dfa dlit dph dphil jsc sjd", "(04) llb jd", "(9) professional (phd jd md)", "i studied at graduate school", "graduate training", "graduate")
    out[lths | somehs | gradenum | addlths] <- "Less Than HS"
    out[hsgrad | vocat | addhsgrad] <- "High School Graduate"
    out[somecol | assoc | colincomplete | addsomecol] <- "Some College"
    out[(colgrad | degrees | addcollegeplus) & !(somecol | assoc | colincomplete | addsomecol)] <- "College Graduate or More"
    out <- ordered(out, c("Less Than HS", "High School Graduate", "Some College", "College Graduate or More"))
    out
}

edufuncyrs <- function(x){
    y <- cut(as.numeric(x), c(-.5, 11.5, 12.5, 15.5, 28), c("Less Than HS", "High School Graduate", "Some College", "College Graduate or More"))
    y
}



religfunc <- function(x){
    protestant <- grepelf("prot|mainline|evang|bapt|method|luth|episc|pente|presby|quak|mennon|advent|unit ch|congregat|nazar|assemblies|four square|foursquare|bapist", x)
    catholic <- grepelf("cath", x)
    christian <- grepelf("chris|church|chrs|orthodox|unitar|univers|othchr|non-denom|jehova|n/c/chr|nondenom|iglesia ni cristo|east/ort|j/witnes|morman|orthodos|greek", x)
    muslim <- grepelf("muslim|islam|moslem", x)
    mormon <- grepelf("mormon|momon|latter day", x)
    jewish <- grepelf("jew", x)
    other <- grepelf("other|budd|hind|deism|oth/rel|druid|o/rel|jain|tribal|bahai|pagan|wicca|african", x)
    athagn <- grepelf("athe|athi|agno|nothing|dont identify|no relig|not relig|no/rel|n/rel|humanist|arent you religious|no specific rel", x)
    rel <- rep(NA, length(x))
    rel[other] <- "other religion"
    rel[christian] <- "christian"
    rel[mormon] <- "mormon"
    rel[protestant] <- "protestant"
    rel[catholic] <- "catholic"
    rel[muslim] <- "muslim"
    rel[jewish] <- "jewish"
    rel[athagn] <- "athiest or agnostic"
    as.factor(rel)
}

relfundfind <- function(x){
    out <- rep(NA, length(x))
    out[grepelf("yes|born|evang|fund|penteco|charism|evng|both", x) & !grepelf("not|nor", x)] <- "Yes"
    out[grepelf("no|main|liberal|moderate|just|progressive|secular", x)] <- "No"
    out[grepelf("know|not sure|no opin|synod|neither|other", x)] <- NA
    out[x==1|x==" 1"|x=="is"] <- "Yes"
    out[grepelf("neither fundamentalist|no neither|jewish", x)] <- "No"
    out[x %in% c(2, " 2", "neither")] <- "No"
    as.factor(out)
}
    
relfreqfind <- function(x){
    out <- rep(NA, length(x))
    out[grepelf("never|hardly ever|seldom|dont go|less often|dont attend|hardly at all|l/onceyr", x) | (grepelf("year", x) & grepelf("less", x))]  <- "Seldom to Never"
    out[grepelf("year|every yr|a yr|sevrl times a yr", x) & !grepelf("less", x)]  <- "Yearly"
    out[grepelf("mont|every now and then|once - twice mth|x/mon|once - twice mth|once - twice mon", x)]  <- "Monthly"
    out[grepelf("week|every wk|almost every wee", x) & !grepelf("more", x)]  <- "Weekly"
    out[x=="two to three times"] <- "Weekly"
    out[grepelf("day|daily|several times a wk|sevrl times a wk", x) | (grepelf("week|often", x) & grepelf("more", x))]  <- "Multiple Times Per Week"
    ordered(out, c("Seldom to Never", "Yearly", "Monthly", "Weekly", "Multiple Times Per Week"))
}


partsorter <- function(x){
    out <- as.character(x)
    dems <- grepelf("dem", x)
    reps <- grepelf("rep", x) & !grepelf("repres", x)
    str <- grepelf("str", x)
    not <- grepelf("not", x)
    ind <- grepelf("ind|no pref|what|else|other|neither|none|the same|both|no party|don't lean|dont lean|non-par|no shw pref|no lean|undesignated|reform|apolit|uncomm|not int|libert|peace|oth|green|refused to lean|constitution|democratic-republican|conserv|working families|does not vote|refused to lean|does not lean|for americans|ticket-splitter|minor party|decline to state", x)
    weak <- grepelf("weak|wk|somewhat", x)
    lean <- grepelf("lean|ln|closer", x)
    strongd <- dems & str & !not & !weak & !reps
    strongr <- reps & str & !not & !weak & !dems
    weakd <- dems & (str & not) | weak & !reps
    weakr <- reps & (str & not) | weak & !dems
    leand <- dems & (lean | ind) & !reps
    leanr <- reps & (lean | ind) & !dems
    equalboth <- grepelf("equal|both|neith|either party", x)
    democrat <- dems & !reps & !str & !weak & !lean & !ind
    republican <- (reps & !dems & !str & !weak & !lean & !ind) | grepelf("registered republican -- go to demographics", x)
    independent <- (ind & !reps & !dems) | (equalboth)# & reps & dems
    out[democrat] <- "Democrat"
    out[republican] <- "Republican"
    out[strongd] <- "Strong Democrat"
    out[strongr] <- "Strong Republican"
    out[weakd] <- "Weak Democrat"
    out[weakr] <- "Weak Republican"
    out[leand] <- "Lean Democrat"
    out[leanr] <- "Lean Republican"
    out[independent] <- "Independent"
    out <- ordered(out, c("Strong Republican", "Republican", "Weak Republican", "Lean Republican", "Independent", "Lean Democrat", "Weak Democrat", "Democrat", "Strong Democrat"))
    out
}

partystrength <- function(x){
    out <- rep(NA, length(x))
    str <- grepelf("str", x)
    not <- grepelf("not", x)
    weak <- grepelf("weak|wk|somewhat", x)
    strongs <- str & !not & !weak
    weaks <- (str & not) | weak
    out[weaks] <- "Weak"
    out[strongs] <- "Strong"
    out
}
    
    
presapp <- function(x){
    out <- rep(NA, length(x))
    disapprove <- grepelf("disap", x)
    approve <- grepelf("appr", x) & !disapprove
    strong <- grepelf("str|a lot|a great deal", x)
    some <- grepelf("some|a little|mild", x)
    not <- grepelf("not", x)
    neither <- grepelf("neither|neutr|mixed|both|nor", x)
    out[disapprove] <- "Disapprove"
    out[approve] <- "Approve"
    out[disapprove & strong & !not] <- "Strongly Disapprove"
    out[approve & strong & !not] <- "Strongly Approve"
    out[(disapprove & strong & not) | (disapprove & some)] <- "Somewhat Disapprove"
    out[(approve & strong & not) | (approve & some)] <- "Somewhat Approve"
    out[neither] <- "Neither Approve Nor Disapprove"
    out <- ordered(out, c("Strongly Disapprove", "Disapprove", "Somewhat Disapprove", "Neither Approve Nor Disapprove", "Somewhat Approve", "Approve", "Strongly Approve"))
    out
}


globalagedistunif <- 18:99
globalagedist <- sapply(1:99, function(x) x=sum(globalagedistunif==x, na.rm=TRUE)/sum(!is.na(globalagedistunif)))
names(globalagedist) <- sort(unique(globalagedistunif))

agetrans <- function(x, agedist=globalagedist){
    out <- rep(NA, length(x))
    x <- gsub("+", "-99", x, fixed=TRUE)
    x <- gsub(" years old|between the ages of |,|ages |age=|  | yrs|yrs|between |age ", "", x)
    x <- gsub(" years", "", x)
    x <- gsub("under the age of ", "0-", x)
    x <- gsub("or were you older than 65?", "65-99", x)
    x <- gsub("(1) ", "", gsub("(2) ", "", gsub("(3) ", "", gsub("(4) ", "", gsub("(5) ", "", gsub("(6) ", "", gsub("(7) ", "", x, fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE)
    x <- gsub("1. ", "", gsub("2. ", "", gsub("3. ", "", gsub("4. ", "", gsub("5. ", "", gsub("6. ", "", gsub("7. ", "", gsub("8. ", "", gsub("9. ", "",x, fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE)
    x <- gsub(" to | - |to ", "-", x)
    x <- gsub("  |^ | $", "", x)
    x[grepelf("over ", x)] <- paste(gsub("over ", "", x[grepelf("over ", x)]), "-99", sep="")
    x[x=='-29\"18-29\"'] <- "18-29"
    x[x=='-49\"30-49\"'] <- "30-49"
    x[x=='-64\"50-64\"'] <- "50-64"
    x[x=='-99\"65-99\"'] <- "65-99"
    x[grepelf("don|ref|no ans|not su|male|no op|dk|respo|not app|omit", x)] <- NA
    x[x=="&"] <- NA
    x <- gsub(" and older| or older| and over| or over| or more|and over| years old or older| or older", "-99", x)
    x <- gsub(" or ov", "-99", x)
    x <- gsub(" & over", "-99", x, fixed=TRUE)
    x <- gsub("undesignated ", "", x)
    x <- gsub("born before ", "", x)
    x <- gsub(" or$|?$", "", x)
    x <- gsub("?", "", x, fixed=TRUE)
    x <- gsub(" (unspec)", "", x, fixed=TRUE)
    x <- gsub(" and ", "-", x)
    x <- as.vector(sapply(x, function(g) strsplit(g, " and older| or older| and over| or over| or more| years")[[1]][1]))
    x <- gsub(" or old", "", x, fixed=TRUE)
    x <- gsub(" ", "", x)
    x <- gsub(" -", "-", x, fixed=TRUE)
    out[try(as.numeric(as.character(x)), silent=TRUE)==x] <- x[try(as.numeric(as.character(x)), silent=TRUE)==x]
    ranges <- strsplit(x, "-")
    out[sapply(ranges, length)==2] <- try(sapply(ranges[sapply(ranges, length)==2], function(r) sample(as.numeric(r[1]):as.numeric(r[2]), 1, prob=agedist[as.numeric(r[1]):as.numeric(r[2])])), silent=TRUE)
    out[out<18] <- NA
    out[out>2050] <- NA
    out[grepelf("Error", out)] <- NA
    out
}
#[try(as.numeric(as.character(x)), silent=TRUE)==x]



maritaltrans <- function(x){
    out <- rep(NA, length(x))
    out[grepelf("widow|unmarried|separ|divorced|never married|sing|seper|unmarr|not married", x) | (grepelf("married", x) & grepelf("not|never", x))] <- "Not Married"
    out[(grepelf("married|partner|cohab|remarr|unm/part", x) & !grepelf("not|un|never", x)) | grepelf("with partner|with a partner", x)] <- "Married"
    out[x %in% c("m", "yes", "(1) yes", 1)] <- "Married"
    out[x %in% c("s", "no", "(5) no", "(2) no", "(0) no", 2:6)] <- "Not Married"
    as.factor(out)
}


