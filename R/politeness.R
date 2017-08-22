#' Politeness Features
#'
#' @description Detects politeness features in text
#' @param text a character vector of texts.
#' @param parser a character of which parser to use for text. Default is "none" other options are "core" and "spacy".
#' @param binary logical default is FALSE. If TRUE outputed data.frame will only have 0 and 1 as possible values else values will be between 0 and 1.
#' @param drop.blank logical dafault is TRUE should columns with only 0 value be removed from outputed data.frame.
#' @details To use parser = "spacy" requires instalation of
#' @return a data.frame of politeness features. Posible columns are
#' @examples
#'


politeness<-function(text, parser=c("none","spacy"), binary=FALSE, drop.blank=TRUE){
  ########################################################

  text<-iconv(text,to="ASCII",sub=" ")
  sets<-list()
  sets[["liwc"]]<-LIWCwrap(text, dict=polite.dicts)
  sets[["clean"]]<-lapply(text, cleantext, stop.words=FALSE)
  sets[["c.words"]]<-lapply(sets[["clean"]], strsplit, split=" ")
  if(parser=="core"){
    c.p<-core.parser(text)
    sets[["p.words"]]<-mclapply(c.p$parses,tolower)
    sets[["p.nonum"]]<-mclapply(c.p$nonums,tolower)
    sets[["pos.nums"]]<-mclapply(c.p$pos.nums,tolower)
    sets[["w.nums"]]<-mclapply(c.p$w.nums,tolower)
    #   w.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(p.words)-1)
  } else if(parser=="spacy"){
    s.p<-spacy.parser(text)
    sets[["p.words"]]<-mclapply(s.p$parses,tolower)
    sets[["p.nonum"]]<-mclapply(s.p$nonums,tolower)
    sets[["pos.nums"]]<-mclapply(s.p$pos.nums,tolower)
    sets[["w.nums"]]<-mclapply(s.p$w.nums,tolower)

  }
  ########################################################
  features<-list()
  features[["Hedges"]]<-textcounter(hedge.list,sets[["c.words"]],words=T)
  features[["PosEmotion"]]<-textcounter(positive.list,sets[["c.words"]],words=T)
  features[["NegEmotion"]]<-textcounter(negative.list,sets[["c.words"]],words=T)

  features[["Impersonal Pronoun"]]<-sets[["liwc"]][,"ipron"]
  features[["Swear"]]<-sets[["liwc"]][,"swear"]
  features[["Negation"]]<-sets[["liwc"]][,"negate"]
  features[["Filler Pause"]]<-sets[["liwc"]][,"pause"]
  features[["Informal Title"]]<-sets[["liwc"]][,"intitle"]
  features[["Formal Title"]]<-sets[["liwc"]][,"title"]

  # Rename these two!
  features[["Subjunctive"]]<-textcounter(c("could you","would you"),sets[["clean"]])
  features[["Indicative"]]<-textcounter(c("can you","will you"),sets[["clean"]])

  features[["By The Way"]]<-textcounter(c("by the way"),sets[["clean"]])
  features[["Let Me Know"]]<-textcounter(c("let me know"),sets[["clean"]])
  features[["Goodbye"]]<-textcounter(c("goodbye", "bye", "see you later"),sets[["clean"]])
  features[["For Me"]]<-textcounter(c("for me","for us"),sets[["clean"]])
  features[["For You"]]<-textcounter("for you",sets[["clean"]])
  features[["Reasoning"]]<-textcounter(c("reason", "why i", "why we", "explain", "you understand","because"),sets[["clean"]])
  features[["Reassurance"]]<-textcounter(c("is okay", "not worry", "no big deal", "not a big deal", "no problem",
                                           "no worries", "is fine", "you are good", "is fine", "is okay") ,sets[["clean"]])
  features[["Ask Agency"]]<-textcounter(c("do me a favor", "let me", "allow me", "can i", "should i", "may i", "might i", "could i"),sets[["clean"]])
  features[["Give Agency"]]<-textcounter(c("let you", "allow you", "you can", "you may", "you could"),sets[["clean"]])

  features[["Hello"]]<-textcounter(c("hi","hello","hey"),sets[["c.words"]],words=T)  # "good morning", "good evening", "good afternoon",
  features[["Group Identity"]]<-textcounter(c("we", "our", "ours", "us", "ourselves"),sets[["c.words"]],words=T)
  features[["Questions"]]<-textcounter(c("who","what","where","when","why","how","which"),sets[["c.words"]],words=T)
  #for(q in c("who","what","where","when","why","how","which")) features[[q]]<-sum(q%in%c.words) #getleftpos(p) in (1,2)

  # opening up the conversation/engaging - “Let me know what you think”, “I look forward to your response”, “Please let me know”, etc.
  # Tag Question	Regular expression capturing cases like "..., right?" and "..., don't you?"

  #if(parser=="none"){
  if(parser!="spacy"){
    features[["Gratitude"]]<-unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))
    features[["Apology"]]<-textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
    features[["In Fact"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)+
                               textcounter(c("in fact"),sets[["clean"]]))
    features[["Please"]]<-grepl("please",sets[["c.words"]],fixed=T)
    features[["First Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)
    features[["Second Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)
  } else {
    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                                unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, i)",x,fixed=T)))))
    features[["Apology"]]<-(textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
                            +textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),sets[["p.nonum"]], words=T))
    features[["In Fact"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)
                           +textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),sets[["p.nonum"]], words=T))
    features[["Affirmation"]]<-textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),sets[["w.nums"]],words=T)
    features[["Adverb Just"]]<-unlist(lapply(sets[["p.nonum"]] ,function(x) sum(grepl("advmod",unlist(x))&grepl("just)",unlist(x),fixed=T))))

    features[["Bare Command"]]<-unlist(lapply(sets[["pos.nums"]],function(x) sum(grepl("(1-",unlist(x),fixed=T)&grepl("-vb)",unlist(x),fixed=T)
                                                                                &(!grepl(paste0("-",c("be","do","have","thank","please","hang","let"),"-"),unlist(x),fixed=T)))))
    features[["Conjugation Start"]]<-textcounter(paste0(c("so","then","and","but","or"),"-1"),sets[["w.nums"]],words=T)

    features[["Please Start"]]<-textcounter("please-1",sets[["w.nums"]],words=T)
    features[["Please"]]<-textcounter("please",sets[["c.words"]],words=T)-features[["PleaseStart"]]

    features[["First Person Start"]]<-textcounter(paste0(c("i","my","mine","myself"),"-1"),sets[["w.nums"]],words=T)
    features[["First Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)-features[["FirstPersonStart"]]

    features[["Second Person Start"]]<-textcounter(paste0(c("you","your","yours","yourself"),"-1"),sets[["w.nums"]],words=T)
    features[["Second Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)-features[["SecondPersonStart"]]
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
