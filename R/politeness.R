
# ######################################################
politeness<-function(texts){
  tallies<-list()
  tpb<-txtProgressBar(0,length(texts))
  for (x in 1:length(texts)){
    textchar<-iconv(texts[x],"latin1", "ASCII", sub="")
    tallies[[x]]<-as.numeric(politeness(textchar))
    setTxtProgressBar(tpb,x)
  }
  return(data.frame(tallies))
}

polite.unit<-function(text, set=c("long","short"), binary=FALSE){
  if(length(text)>1){
    text<-text[1]
    message("Only one text at a time - first text will be used")
  }
  features<-list()
  long.set=("long"%in%set)
  ########################################################
  text<-iconv(text,to="UTF-8",sub=" ")
  l.text<-LIWCwrap(text, dict=polite.dicts)
  c.text<-cleantext(text, stop.words=FALSE)
  c.words<-strsplit(c.text, " ")[[1]]
  if(long.set){
    c.p<-core.parser(text)
    p.words<-tolower(c.p$all.parses)
    p.nonum<-gsub("-[0-99]","",p.words)
    pos.nums<-tolower(c.p$all.pos.nums)
    c.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(p.words)-1)
  }
  ########################################################
  features[["Hedges"]]<-sum(textcounter(hedge.list,c.words,words=T))
  features[["Positive"]]<-sum(textcounter(positive.list,c.words,words=T))
  features[["Negative"]]<-sum(textcounter(negative.list,c.words,words=T))

  features[["ImpersPronoun"]]<-l.text[,"ipron"]
  features[["Swear"]]<-l.text[,"swear"]
  features[["Negate"]]<-l.text[,"negate"]
  features[["FillerPause"]]<-l.text[,"pause"]
  features[["InformalTitle"]]<-l.text[,"intitle"]
  features[["FormalTitle"]]<-l.text[,"title"]

  features[["Subjunctive"]]<-textcounter(c("could you","would you"),c.text)
  features[["Indicative"]]<-textcounter(c("can you","will you"),c.text)
  features[["ByTheWay"]]<-textcounter(c("by the way"),c.text)
  features[["Hello"]]<-sum(textcounter(c("hi","hello","hey"),c.words,words=T))
  features[["Goodbye"]]<-sum(textcounter(c("goodbye", "bye", "see you later"),c.text))
  features[["ForMe"]]<-sum(textcounter(c("for me","for us"),c.text))
  features[["ForYou"]]<-textcounter("for you",c.text)

  features[["Reasoning"]]<-sum(textcounter(c("reason", "why i", "why we", "explain", "you understand","because"),c.text))
  features[["Reassurance"]]<-sum(textcounter(c("'s okay", "n't worry", "no big deal", "not a big deal", "no problem",
                                               "no worries", "'s fine", "you 're good", "is fine", "is okay"),c.text))
  features[["AskAgency"]]<-sum(textcounter(c("do me a favor", "let me", "allow me", "can i", "should i", "may i", "might i", "could i"),c.text))
  features[["GiveAgency"]]<-sum(textcounter(c("let you", "allow you", "you can", "you may", "you could"),c.text))
  features[["GroupIdentity"]]<-sum(textcounter(c("we", "our", "ours", "us", "ourselves"),c.words,words=T))
  features[["questions"]]<-sum(textcounter(c("who","what","where","when","why","how","which"),c.words,words=T))
  #for(q in c("who","what","where","when","why","how","which")) features[[q]]<-sum(q%in%c.words) #getleftpos(p) in (1,2)

  # opening up the conversation/engaging - “Let me know what you think”, “I look forward to your response”, “Please let me know”, etc.
  # Tag Question	Regular expression capturing cases like "..., right?" and "..., don't you?"

  if(!long.set){
    features[["Gratitude"]]<-sum(startsWith(c.words,"thank"))
    features[["Apologies"]]<-sum(textcounter(c("sorry"," woops","oops","whoops"),c.words,words=T))
    features[["InFact"]]<-sum(textcounter(c("really", "actually", "honestly", "surely"),c.words,words=T))
    features[["Please"]]<-sum(grepl("please",p.words,fixed=T))
    features[["FirstPerson"]]<-sum(textcounter(c("i","my","mine","myself"),c.words,words=T))
    features[["SecondPerson"]]<-sum(textcounter(c("you","your","yours","yourself"),c.words,words=T))
  }
  if(long.set){
    features[["Gratitude"]]<-sum(c(startsWith(c.words,"thank"),grepl("(appreciate, i)",p.nonum,fixed=T)))
    features[["Apologies"]]<-(sum(textcounter(c("sorry"," woops","oops","whoops"),c.words,words=T))
                              +sum(textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),p.nonum, words=T)))
    features[["InFact"]]<-(sum(textcounter(c("really", "actually", "honestly", "surely"),c.words,words=T))
                           +sum(textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),p.nonum,words=T)))
    features[["Deference"]]<-sum(textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),c.nums,words=T))
    features[["AdvJust"]]<-sum(grepl("advmod",p.nonum)&grepl("just)",p.nonum,fixed=T))
    features[["BareCommand"]]<-sum(grepl("(1-",pos.nums,fixed=T)&grepl("-VB)",pos.nums,fixed=T)
                                   &(!(textcounter(paste0("-",c("be","do","have","thank","please","hang"),"-"),pos.nums))))

    # Bald Command	 The first word in a sentence is a bare verb with part-of-speech tag VB ("look", "give", "wait" etc.)
    #but is not one of

    features[["ConjStart"]]<-sum(textcounter(paste0(c("so","then","and","but","or"),"-1"),c.nums,words=T))

    features[["PleaseStart"]]<-sum(c.nums=="please-1")
    features[["Please"]]<-sum((!grepl("-1",p.words,fixed=T))&grepl("please",p.words,fixed=T))
    features[["FirstPersonStart"]]<-sum(textcounter(paste0(c("i","my","mine","myself"),"-1"),c.nums,words=T))
    features[["FirstPerson"]]<-sum(textcounter(c("i","my","mine","myself"),c.words,words=T))-features[["FirstPersonStart"]]
    features[["SecondPersonStart"]]<-sum(textcounter(paste0(c("you","your","yours","yourself"),"-1"),c.nums,words=T))
    features[["SecondPerson"]]<-sum(textcounter(c("you","your","yours","yourself"),c.words,words=T))-features[["SecondPersonStart"]]
  }
  if(binary){
    features<-lapply(features, function(x) 1*(x>0))
  }
  return(features)
}

# MARTHA'S IDEAS!
# -compliments for phone or seller? “exact model”, “exact phone”, “just the phone”, “perfect phone”, “definitely interested”, “very interested”, how the seller took great “care”, etc.
# -empathy/constraints: “budget”, “strapped for cash”, “limited”, etc.

# Colloquialism	Regular expression capturing "y'all", "ain't" and words ending in "in'" such as "walkin'", "talkin'", etc., as marked by transcribers
# Safety	Regular expression for all words beginning with the prefix "safe", such as "safe", "safety", "safely"
# Time Minimizing	Regular expression capturing cases like "in a minute" and "let's get this done quick":  ("hurry up?")
#
# Disfluency	Word fragment ("Well I thi-") as indicated by transcribers
# Last Names	Top 5000 most common last names from the 1990 US Census, where first letter is capitalized in transcript
# First Names	Top 1000 most common first names from the 1990 US Census, where first letter is capitalized in transcript
