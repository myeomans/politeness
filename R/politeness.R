#' Politeness Features
#'
#' @description Detects linguistic markers of politeness in natural language.
#'     This function is the workhorse of the \code{politeness} package, taking an N-length vector of text documents and returning an N-row data.frame of feature counts.
#' @param text character A vector of texts, each of which will be tallied for politeness features.
#' @param parser character Name of dependency parser to use (see details). Without a dependency parser, some features will be approximated, while others cannot be calculated at all.
#' @param metric character What metric to return? Raw feature count totals, Binary presence/absence of features, or feature counts per word  Default is "count".
#' @param drop_blank logical Should features that were not found in any text be removed from the data.frame? Default is TRUE
#' @param num_mc_cores integer Number of cores for parallelization. Default is parallel::detectCores().
#' @param binary logical Deprecated! Do not use (see metric parameter).
#' @details Some politeness features depend on part-of-speech tagged sentences (e.g. "bare commands" are a particular verb class).
#'     To include these features in the analysis, a POS tagger must be initialized beforehand - we currently support SpaCy which must
#'     be installed separately in Python (see example for implementation).
#' @return a data.frame of politeness features, with one row for every item in `text`. Possible politeness features are listed in \code{\link{feature_table}}
#' @references
#' Brown, P., & Levinson, S. C. (1987). Politeness: Some universals in language usage (Vol. 4). Cambridge university press.
#'
#' Danescu-Niculescu-Mizil, C., Sudhof, M., Jurafsky, D., Leskovec, J., & Potts, C. (2013). A computational approach to politeness with application to social factors. arXiv preprint arXiv:1306.6078.
#'
#' Voigt, R., Camp, N. P., Prabhakaran, V., Hamilton, W. L., ... & Eberhardt, J. L. (2017). Language from police body camera footage shows racial disparities in officer respect. Proceedings of the National Academy of Sciences, 201702413.
#'
#' @examples
#'
#' data("phone_offers")
#'
#' politeness(phone_offers$message, parser="none",drop_blank=FALSE)
#'
#' colMeans(politeness(phone_offers$message, parser="none", metric="binary", drop_blank=FALSE))
#' colMeans(politeness(phone_offers$message, parser="none", metric="count", drop_blank=FALSE))
#'
#' dim(politeness(phone_offers$message, parser="none",drop_blank=FALSE))
#' dim(politeness(phone_offers$message, parser="none",drop_blank=TRUE))
#'
#'\dontrun{
#' # Detect multiple cores automatically for parallel processing
#' politeness(phone_offers$message, num_mc_cores=parallel::detectCores())
#'
#' # Connect to SpaCy installation for part-of-speech features
#' install.packages("spacyr")
#' spacyr::spacy_initialize(python_executable = PYTHON_PATH)
#' politeness(phone_offers$message, parser="spacy",drop_blank=FALSE)
#'
#'}
#'
#'
#'
#'
#'@export

politeness<-function(text, parser=c("none","spacy"), metric=c("count","binary","average"), drop_blank=TRUE, num_mc_cores=1, binary=FALSE){
  if (!missing(binary)) {
    warning("argument binary is deprecated; please use metric instead.",
            call. = FALSE)
    metric <- ifelse(binary, "binary", "count")
  }
  #if(binary){ #Handling deprecated input
  #  metric<-"binary"
  #}
  ########################################################

  text<-iconv(text,to="ASCII",sub=" ")
  text[is.na(text) | text==""] <- "   "
  sets<-list()
  sets[["dicts"]]<-dictWrap(text, dict=polite_dicts, num_mc_cores=num_mc_cores)
  sets[["clean"]]<-parallel::mclapply(text, cleantext, stop.words=FALSE,mc.cores=num_mc_cores)
  sets[["c.words"]]<-parallel::mclapply(sets[["clean"]], strsplit, split=" ",mc.cores=num_mc_cores)
  if(parser[1]=="core"){
    # c.p<-core.parser(text)
    # sets[["p.words"]]<-parallel::mclapply(c.p$parses,tolower)
    # sets[["p.nonum"]]<-parallel::mclapply(c.p$nonums,tolower)
    # sets[["pos.nums"]]<-parallel::mclapply(c.p$pos.nums,tolower)
    # sets[["w.nums"]]<-parallel::mclapply(c.p$w.nums,tolower)
    #   w.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=TRUE)[[1]][1])+2, nchar(p.words)-1)
  } else if(parser[1]=="spacy"){
    s.p<-spacyParser(text, num_mc_cores=num_mc_cores)
    sets[["p.words"]]<-parallel::mclapply(s.p$parses,tolower,mc.cores=num_mc_cores)
    sets[["p.nonum"]]<-parallel::mclapply(s.p$nonums,tolower,mc.cores=num_mc_cores)
    sets[["pos.nums"]]<-parallel::mclapply(s.p$pos.nums,tolower,mc.cores=num_mc_cores)
    sets[["w.nums"]]<-parallel::mclapply(s.p$w.nums,tolower,mc.cores=num_mc_cores)
  }
  ########################################################
  features<-list()
  features[["Hedges"]]<-textcounter(hedge_list,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
  features[["Positive.Emotion"]]<-textcounter(positive_list,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
  features[["Negative.Emotion"]]<-textcounter(negative_list,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)

  features[["Impersonal.Pronoun"]]<-sets[["dicts"]][,"Pronouns"]
  features[["Swearing"]]<-sets[["dicts"]][,"Swearing"]
  features[["Negation"]]<-sets[["dicts"]][,"Negation"]
  features[["Filler.Pause"]]<-sets[["dicts"]][,"FilledPause"]
  features[["Informal.Title"]]<-sets[["dicts"]][,"InformalTitle"]
  features[["Formal.Title"]]<-sets[["dicts"]][,"FormalTitle"]

  # Rename these two!
  features[["Subjunctive"]]<-textcounter(c("could you","would you"),sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["Indicative"]]<-textcounter(c("can you","will you"),sets[["clean"]], num_mc_cores=num_mc_cores)

  features[["By.The.Way"]]<-textcounter(c("by the way"),sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["Let.Me.Know"]]<-textcounter(c("let me know"),sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["Goodbye"]]<-textcounter(c("goodbye", "bye", "see you later"),sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["For.Me"]]<-textcounter(c("for me","for us"),sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["For.You"]]<-textcounter("for you",sets[["clean"]], num_mc_cores=num_mc_cores)
  features[["Reasoning"]]<-textcounter(c("reason", "why i", "why we", "explain", "you understand","because"),sets[["clean"]],
                                       num_mc_cores=num_mc_cores)
  features[["Reassurance"]]<-textcounter(c("is okay", "not worry", "no big deal", "not a big deal", "no problem",
                                           "no worries", "is fine", "you are good", "is fine", "is okay") ,sets[["clean"]],
                                         num_mc_cores=num_mc_cores)
  features[["Ask.Agency"]]<-textcounter(c("do me a favor", "let me", "allow me", "can i", "should i",
                                          "may i", "might i", "could i"),sets[["clean"]],
                                        num_mc_cores=num_mc_cores)
  features[["Give.Agency"]]<-textcounter(c("let you", "allow you", "you can", "you may", "you could"),sets[["clean"]],
                                         num_mc_cores=num_mc_cores)

  features[["Hello"]]<-textcounter(c("hi","hello","hey"),sets[["c.words"]],words=TRUE,
                                   num_mc_cores=num_mc_cores)  # "good morning", "good evening", "good afternoon",
  features[["Group.Identity"]]<-textcounter(c("we", "our", "ours", "us", "ourselves"),sets[["c.words"]],words=TRUE,
                                            num_mc_cores=num_mc_cores)

  #if(parser[1]=="none"){
  if(parser[1]!="spacy"){
    cat("Note: Some features cannot be computed without part-of-speech tagging. See ?spacyr::spacyr for details.")
    features[["Questions"]]<-textcounter("?",text, num_mc_cores=num_mc_cores)
    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="grateful"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="gratitude")))))
    features[["Apology"]]<-textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=TRUE,
                                       num_mc_cores=num_mc_cores)
    features[["Actually"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=TRUE,
                                         num_mc_cores=num_mc_cores)+
                               textcounter(c("in fact"),sets[["clean"]],num_mc_cores=num_mc_cores))
    features[["Please"]]<-1*(grepl("please",sets[["c.words"]],fixed=TRUE))
    features[["First.Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=TRUE,
                                            num_mc_cores=num_mc_cores)
    features[["Second.Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=TRUE,
                                             num_mc_cores=num_mc_cores)
  } else {
    q.words<-c("who","what","where","when","why","how","which")
    features[["Questions"]]<-textcounter(c(paste0(q.words,"-1"),paste0(q.words,"-2")),sets[["w.nums"]],words=TRUE,num_mc_cores=num_mc_cores)
    # Tag Questions cases like "right?" and "don't you?", "eh?", "you know?" "what do you think?"
    # Repair Questions	(from SpeedDate)? "pardon?" "sorry?"

    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="grateful"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="gratitude"))))+
                                unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, we)",x,fixed=TRUE))))+
                                unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, i)",x,fixed=TRUE)))))
    features[["Apology"]]<-(textcounter(c("sorry"," woops","oops","whoops"),sets[["c.words"]],words=TRUE,
                                        num_mc_cores=num_mc_cores)
                            +textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),sets[["p.nonum"]], words=TRUE,
                                         num_mc_cores=num_mc_cores))
    features[["Actually"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=TRUE,
                                         num_mc_cores=num_mc_cores)
                             +textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),sets[["p.nonum"]], words=TRUE,
                                          num_mc_cores=num_mc_cores))
    features[["Affirmation"]]<-textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),sets[["w.nums"]],words=TRUE,
                                           num_mc_cores=num_mc_cores)
    features[["Adverb.Just"]]<-unlist(lapply(sets[["p.nonum"]] ,function(x) sum(grepl("advmod",unlist(x))&grepl("just)",unlist(x),fixed=TRUE))))

    features[["Bare.Command"]]<-unlist(lapply(sets[["pos.nums"]],function(x) sum(grepl("(1-",unlist(x),fixed=TRUE)&grepl("-vb)",unlist(x),fixed=TRUE)
                                                                                 &(textcounter(paste0("-",c("be","do","have","thank","please","hang","let"),"-"),
                                                                                               unlist(x),fixed=T)==0))))
    features[["Conjunction.Start"]]<-textcounter(paste0(c("so","then","and","but","or"),"-1"),sets[["w.nums"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)

    features[["Please.Start"]]<-textcounter("please-1",sets[["w.nums"]],words=TRUE,
                                            num_mc_cores=num_mc_cores)
    features[["Please"]]<-textcounter("please",sets[["c.words"]],words=TRUE,num_mc_cores=num_mc_cores)-features[["Please.Start"]]

    features[["First.Person.Start"]]<-textcounter(paste0(c("i","my","mine","myself"),"-1"),sets[["w.nums"]],words=TRUE,
                                                  num_mc_cores=num_mc_cores)
    features[["First.Person"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=TRUE,num_mc_cores=num_mc_cores)-features[["First.Person.Start"]]

    features[["Second.Person.Start"]]<-textcounter(paste0(c("you","your","yours","yourself"),"-1"),sets[["w.nums"]],words=TRUE,
                                                   num_mc_cores=num_mc_cores)
    features[["Second.Person"]]<-textcounter(c("you","your","yours","yourself"),sets[["c.words"]],words=TRUE,
                                             num_mc_cores=num_mc_cores)-features[["Second.Person.Start"]]
  }

  if(metric[1]=="binary"){
    features<-parallel::mclapply(features, function(x) 1*(x>0), mc.cores=num_mc_cores)
  } else if (metric[1]=="average"){
    word_counts <- stringr::str_count(text, "[[:alpha:]]+")
    features<-parallel::mclapply(features, function(x) x/word_counts, mc.cores=num_mc_cores)
  }
  feature.data<-as.data.frame(features)
  if(drop_blank){
    feature.data<-feature.data[,colMeans(feature.data,na.rm=T)!=0, drop=FALSE]
  }
  return(feature.data)
}
###############################################################
