#' Politeness Features
#'
#' @description Detects politeness features in text
#' @param text character vector of texts.
#' @param parser character Name of dependency parser to use (see details). Without a dependency parser, some features will be approximated, while others cannot be calculated at all.
#' @param binary logical  Return feature counts from each text (default) or a binary indicator for the presence of a feature?
#' @param drop.blank logical Should features that were not found in any text be removed from the data.frame? Default is TRUE
#' @details We currently support SpaCy which must be installed separately and prior to running the detector (see example for implementation).
#' @return a data.frame of politeness features. Posible columns are listed in LINK_TO_TABLE
#' @examples
#'
#' data("phone_offers")
#'
#' politeness(phone_offers$message, parser="none",drop.blank=FALSE)
#'
#' #install.packages("spacyr")
#' #spacyr::spacy_initialize(python_executable = PYTHON_PATH)
#' politeness(phone_offers$message, parser="spacy",drop.blank=FALSE)
#'

politeness<-function(text, parser=c("none","spacy"), binary=FALSE, drop.blank=TRUE){
  ########################################################
  text<-iconv(text,to="ASCII",sub=" ")
  sets<-list()
  sets[["dicts"]]<-dictWrap(text, dict=polite_dicts)
  sets[["clean"]]<-lapply(text, cleantext, stop.words=FALSE)
  sets[["c.words"]]<-lapply(sets[["clean"]], strsplit, split=" ")
  if(parser=="core"){
    c.p<-core.parser(text)
    sets[["p.words"]]<-parallel::mclapply(c.p$parses,tolower)
    sets[["p.nonum"]]<-parallel::mclapply(c.p$nonums,tolower)
    sets[["pos.nums"]]<-parallel::mclapply(c.p$pos.nums,tolower)
    sets[["w.nums"]]<-parallel::mclapply(c.p$w.nums,tolower)
    #   w.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(p.words)-1)
  } else if(parser=="spacy"){
    s.p<-spacyParser(text)
    sets[["p.words"]]<-parallel::mclapply(s.p$parses,tolower)
    sets[["p.nonum"]]<-parallel::mclapply(s.p$nonums,tolower)
    sets[["pos.nums"]]<-parallel::mclapply(s.p$pos.nums,tolower)
    sets[["w.nums"]]<-parallel::mclapply(s.p$w.nums,tolower)

  }
  ########################################################
  features<-list()
  features[["Hedges"]]<-textcounter(hedge_list,sets[["c.words"]],words=T)
  features[["Positive.Emotion"]]<-textcounter(positive_list,sets[["c.words"]],words=T)
  features[["Negative.Emotion"]]<-textcounter(negative_list,sets[["c.words"]],words=T)

  features[["Impersonal.Pronoun"]]<-sets[["dicts"]][,"Pronouns"]
  features[["Swearing"]]<-sets[["dicts"]][,"Swearing"]
  features[["Negation"]]<-sets[["dicts"]][,"Negation"]
  features[["Filler.Pause"]]<-sets[["dicts"]][,"FilledPause"]
  features[["Informal.Title"]]<-sets[["dicts"]][,"InformalTitle"]
  features[["Formal.Title"]]<-sets[["dicts"]][,"FormalTitle"]

  # Rename these two!
  features[["Subjunctive"]]<-textcounter(c("could you","would you"),sets[["clean"]])
  features[["Indicative"]]<-textcounter(c("can you","will you"),sets[["clean"]])

  features[["By.The.Way"]]<-textcounter(c("by the way"),sets[["clean"]])
  features[["Let.Me.Know"]]<-textcounter(c("let me know"),sets[["clean"]])
  features[["Goodbye"]]<-textcounter(c("goodbye", "bye", "see you later"),sets[["clean"]])
  features[["For.Me"]]<-textcounter(c("for me","for us"),sets[["clean"]])
  features[["For.You"]]<-textcounter("for you",sets[["clean"]])
  features[["Reasoning"]]<-textcounter(c("reason", "why i", "why we", "explain", "you understand","because"),sets[["clean"]])
  features[["Reassurance"]]<-textcounter(c("is okay", "not worry", "no big deal", "not a big deal", "no problem",
                                           "no worries", "is fine", "you are good", "is fine", "is okay") ,sets[["clean"]])
  features[["Ask.Agency"]]<-textcounter(c("do me a favor", "let me", "allow me", "can i", "should i", "may i", "might i", "could i"),sets[["clean"]])
  features[["Give.Agency"]]<-textcounter(c("let you", "allow you", "you can", "you may", "you could"),sets[["clean"]])

  features[["Hello"]]<-textcounter(c("hi","hello","hey"),sets[["c.words"]],words=T)  # "good morning", "good evening", "good afternoon",
  features[["Group.Identity"]]<-textcounter(c("we", "our", "ours", "us", "ourselves"),sets[["c.words"]],words=T)
  features[["Questions"]]<-textcounter(c("who","what","where","when","why","how","which"),sets[["c.words"]],words=T)
  #for(q in c("who","what","where","when","why","how","which")) features[[q]]<-sum(q%in%c.words) #getleftpos(p) in (1,2)

  # opening up the conversation/engaging - “Let me know what you think”, “I look forward to your response”, “Please let me know”, etc.
  # Tag Question	Regular expression capturing cases like "..., right?" and "..., don't you?"

  #if(parser=="none"){
  if(parser!="spacy"){
    features[["Gratitude"]]<-unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))
    features[["Apology"]]<-textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
    features[["In.Fact"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)+
                               textcounter(c("in fact"),sets[["clean"]]))
    features[["Please"]]<-1*(grepl("please",sets[["c.words"]],fixed=T))
    features[["First.Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)
    features[["Second.Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)
  } else {
    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                                unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, i)",x,fixed=T)))))
    features[["Apology"]]<-(textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=T)
                            +textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),sets[["p.nonum"]], words=T))
    features[["In.Fact"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=T)
                           +textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),sets[["p.nonum"]], words=T))
    features[["Affirmation"]]<-textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),sets[["w.nums"]],words=T)
    features[["Adverb.Just"]]<-unlist(lapply(sets[["p.nonum"]] ,function(x) sum(grepl("advmod",unlist(x))&grepl("just)",unlist(x),fixed=T))))

    features[["Bare.Command"]]<-unlist(lapply(sets[["pos.nums"]],function(x) sum(grepl("(1-",unlist(x),fixed=T)&grepl("-vb)",unlist(x),fixed=T)
                                                                                &(!grepl(paste0("-",c("be","do","have","thank","please","hang","let"),"-"),unlist(x),fixed=T)))))
    features[["Conjunction.Start"]]<-textcounter(paste0(c("so","then","and","but","or"),"-1"),sets[["w.nums"]],words=T)

    features[["Please.Start"]]<-textcounter("please-1",sets[["w.nums"]],words=T)
    features[["Please"]]<-textcounter("please",sets[["c.words"]],words=T)-features[["Please.Start"]]

    features[["First.Person.Start"]]<-textcounter(paste0(c("i","my","mine","myself"),"-1"),sets[["w.nums"]],words=T)
    features[["First.Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=T)-features[["First.Person.Start"]]

    features[["Second.Person.Start"]]<-textcounter(paste0(c("you","your","yours","yourself"),"-1"),sets[["w.nums"]],words=T)
    features[["Second.Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=T)-features[["Second.Person.Start"]]
  }
  if(binary){
    features<-parallel::mclapply(features, function(x) 1*(x>0))
  }
  feature.data<-as.data.frame(features)
  if(drop.blank){
    feature.data<-feature.data[,colMeans(feature.data)!=0]
  }
  return(feature.data)

}
###############################################################
