politeness<-function(text, parser=c("none","spacy"), binary=FALSE, drop.blank=TRUE){
  ########################################################
  text<-iconv(text,to="ASCII",sub=" ")
  sets<-list()
  sets[["liwc"]]<-LIWCwrap(text, dict=polite.dicts)
  sets[["clean"]]<-lapply(text, cleantext, stop.words=FALSE)
  sets[["c.words"]]<-lapply(sets[["clean"]], strsplit, split=" ")
  # if(long.set=="core"){
  #   c.p<-core.parser(text)
  #   p.words<-tolower(c.p$all.parses)
  #   p.nonum<-gsub("-[0-99]","",p.words)
  #   pos.nums<-tolower(c.p$all.pos.nums)
  #   w.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(p.words)-1)
  # } else
  if(long.set=="spacy"){
    s.p<-spacy.parser(text)
    sets[["p.words"]]<-mclapply(s.p$parses,tolower)
    sets[["p.nonum"]]<-mclapply(sets[["p.words"]],gsub, pattern="-[0-9][0-9][0-9]",replacement="")
    sets[["p.nonum"]]<-mclapply(sets[["p.nonum"]],gsub, pattern="-[0-9][0-9]",replacement="")
    sets[["p.nonum"]]<-mclapply(sets[["p.nonum"]],gsub, pattern="-[0-9]",replacement="")
    sets[["pos.nums"]]<-mclapply(s.p$pos.nums,tolower)
    sets[["w.nums"]]<-mclapply(s.p$w.nums,tolower)

  }
  ########################################################
  features[["Hedges"]]<-textcounter(hedge.list,sets[["c.words"]],words=T)
  features[["Positive"]]<-textcounter(positive.list,sets[["c.words"]],words=T)
  features[["Negative"]]<-textcounter(negative.list,sets[["c.words"]],words=T)

  features[["ImpersPronoun"]]<-sets[["liwc"]][,"ipron"]
  features[["Swear"]]<-sets[["liwc"]][,"swear"]
  features[["Negate"]]<-sets[["liwc"]][,"negate"]
  features[["FillerPause"]]<-sets[["liwc"]][,"pause"]
  features[["InformalTitle"]]<-sets[["liwc"]][,"intitle"]
  features[["FormalTitle"]]<-sets[["liwc"]][,"title"]

  features<-list()
  features[["Subjunctive"]]<-textcounter(c("could you","would you"),sets[["clean"]])
  features[["Indicative"]]<-textcounter(c("can you","will you"),sets[["clean"]])
  features[["ByTheWay"]]<-textcounter(c("by the way"),sets[["clean"]])
  features[["LetMeKnow"]]<-textcounter(c("let me know"),sets[["clean"]])
  features[["Goodbye"]]<-textcounter(c("goodbye", "bye", "see you later"),sets[["clean"]])
  features[["ForMe"]]<-textcounter(c("for me","for us"),sets[["clean"]])
  features[["ForYou"]]<-textcounter("for you",sets[["clean"]])
  features[["Reasoning"]]<-textcounter(c("reason", "why i", "why we", "explain", "you understand","because"),sets[["clean"]])
  features[["Reassurance"]]<-textcounter(c("is okay", "not worry", "no big deal", "not a big deal", "no problem",
                                           "no worries", "is fine", "you are good", "is fine", "is okay") ,sets[["clean"]])
  features[["AskAgency"]]<-textcounter(c("do me a favor", "let me", "allow me", "can i", "should i", "may i", "might i", "could i"),sets[["clean"]])
  features[["GiveAgency"]]<-textcounter(c("let you", "allow you", "you can", "you may", "you could"),sets[["clean"]])

  features[["Hello"]]<-textcounter(c("hi","hello","hey"),sets[["c.words"]],words=T)  # "good morning", "good evening", "good afternoon",
  features[["GroupIdentity"]]<-textcounter(c("we", "our", "ours", "us", "ourselves"),sets[["c.words"]],words=T)
  features[["Questions"]]<-textcounter(c("who","what","where","when","why","how","which"),sets[["c.words"]],words=T)
  #for(q in c("who","what","where","when","why","how","which")) features[[q]]<-sum(q%in%c.words) #getleftpos(p) in (1,2)

  # opening up the conversation/engaging - “Let me know what you think”, “I look forward to your response”, “Please let me know”, etc.
  # Tag Question	Regular expression capturing cases like "..., right?" and "..., don't you?"

  #if(long.set=="none"){
  if(long.set!="spacy"){
    features[["Gratitude"]]<-unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))
    features[["Apologies"]]<-textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
    features[["InFact"]]<-textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)
    features[["Please"]]<-grepl("please",sets[["c.words"]],fixed=T)
    features[["FirstPerson"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)
    features[["SecondPerson"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)
  } else {
    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                                unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, i)",x,fixed=T)))))
    features[["Apologies"]]<-(textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
                              +textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),sets[["p.nonum"]], words=T))
    features[["InFact"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)
                           +textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),sets[["p.nonum"]], words=T))
    features[["Deference"]]<-textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),sets[["w.nums"]],words=T)
    features[["AdvJust"]]<-unlist(lapply(sets[["p.nonum"]],function(x) sum(grepl("advmod",unlist(x))&grepl("just)",unlist(x),fixed=T))))
    features[["AdvJust"]]<-unlist(lapply(sets[["p.nonum"]],function(x) sum(grepl("advmod",unlist(x))&grepl("just)",unlist(x),fixed=T))))

    features[["BareCommand"]]<-unlist(lapply(sets[["pos.nums"]],function(x) sum(grepl("(1-",unlist(x),fixed=T)&grepl("-vb)",unlist(x),fixed=T)
                                                                                &(!grepl(paste0("-",c("be","do","have","thank","please","hang","let"),"-"),unlist(x),fixed=T)))))
    features[["ConjStart"]]<-textcounter(paste0(c("so","then","and","but","or"),"-1"),sets[["w.nums"]],words=T)

    features[["PleaseStart"]]<-textcounter("please-1",sets[["w.nums"]],words=T)
    features[["Please"]]<-textcounter("please",sets[["c.words"]],words=T)-features[["PleaseStart"]]

    features[["FirstPersonStart"]]<-textcounter(paste0(c("i","my","mine","myself"),"-1"),sets[["w.nums"]],words=T)
    features[["FirstPerson"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)-features[["FirstPersonStart"]]

    features[["SecondPersonStart"]]<-textcounter(paste0(c("you","your","yours","yourself"),"-1"),sets[["w.nums"]],words=T)
    features[["SecondPerson"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)-features[["SecondPersonStart"]]
  }
  if(binary){
    features<-mclapply(features, function(x) 1*(x>0))
  }
  feature.data<-as.data.frame(features)
  if(drop.blank){
    feature.data<-feature.data[,colMeans(feature.data)!=0]
  }
  return(feature.data)

}
###############################################################
