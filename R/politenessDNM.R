#' Politeness Features
#'
#' @description Detects linguistic markers of politeness in natural language.
#'     This function emulates the original features of the Danescu-Niculescu-Mizil Politeness paper. This primarily exists to contrast with the full feature set in the main package, and is not recommended otherwise.
#' @param text character A vector of texts, each of which will be tallied for politeness features.
#' @param uk_english logical Does the text contain any British English spelling? Including variants (e.g. Canadian). Default is FALSE
#' @return a data.frame of politeness features, with one row for every item in `text`. The original names are used where possible.
#' @references
#' Danescu-Niculescu-Mizil, C., Sudhof, M., Jurafsky, D., Leskovec, J., & Potts, C. (2013). A computational approach to politeness with application to social factors. arXiv preprint arXiv:1306.6078.
#'

#' @examples
#'
#'\dontrun{
#' # Connect to SpaCy installation for part-of-speech features
#' install.packages("spacyr")
#' spacyr::spacy_initialize(python_executable = PYTHON_PATH)
#' data("phone_offers")
#'
#' politeness(phone_offers$message)
#'
#'}
#'
#'
#'@export

politenessDNM<-function(text,
                        uk_english=FALSE){
  # settings from the main function hard-coded, to reproduce the original
  parser="spacy"
  metric="count"
  drop_blank=FALSE
  num_mc_cores=1

  text=as.character(unlist(text))
  text[is.na(text)]<-" "
  if(uk_english){
    text<-usWords(text)
  }
  text<-paste(text," ")

  ########################################################
  # Generates broad token lists for feature creation below
  if(length(text)<2000){
    sets<-getTokenSets(text=text,parser=parser[1],num_mc_cores=num_mc_cores)
  } else{

    # Batched loop to minimize memory load on SpaCy for big files
    textList<-split(text, ceiling(seq_along(text)/1000))
    setList<-lapply(1:length(textList),function(x) NA)
    sets<-list()
    tpb<-utils::txtProgressBar(0,length(textList))
    for (x in 1:length(textList)){
      setList[[x]]<-getTokenSets(text=textList[[x]],parser=parser[1],num_mc_cores=num_mc_cores)
      utils::setTxtProgressBar(tpb,x)
    }
    sets[["dicts"]]<-do.call(rbind,lapply(setList,function(x) x[["dicts"]]))
    .names<-names(setList[[1]])[names(setList[[1]])!="dicts"]
    for(n in .names){
      sets[[n]]<-do.call(c,lapply(setList,function(x) x[[n]]))
    }
  }
  ########################################################
  features<-list()

  # Identical
  features[["Counterfactual.Modal"]]<-textcounter(c("could you","would you","might you"),sets[["clean"]], num_mc_cores=num_mc_cores)

  features[["Indicative.Modal"]]<-textcounter(c("can you","will you"),sets[["clean"]], num_mc_cores=num_mc_cores)

  features[["First.Person.Plural"]]<-textcounter(c("we", "our", "ours", "us", "ourselves"),sets[["c.words"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)

  features[["Indirect.Start"]]<-textcounter(c("by the way"),sets[["clean"]], num_mc_cores=num_mc_cores)

  features[["Direct.Start"]]<-textcounter(paste0(c("so","then","and","but","or"),"-1"),sets[["w.nums"]],words=TRUE,
                                          num_mc_cores=num_mc_cores)

  features[["Factuality"]]<-(textcounter(c("really", "actually", "honestly", "surely"),sets[["c.words"]],words=TRUE,
                                         num_mc_cores=num_mc_cores)

                             +textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","pobj(fact, in)","case(fact, in)"),sets[["p.nonum"]], words=TRUE,
                                          num_mc_cores=num_mc_cores))

  # Simpler but otherwise similar
  features[["Apologizing"]]<-(textcounter(c("woops","oops","sorry"),sets[["c.words"]],words=TRUE,
                                          num_mc_cores=num_mc_cores)
                              +textcounter(c("nsubj(apologize, i)","dobj(excuse, me)","dobj(forgive, me)"),sets[["p.nonum"]], words=TRUE,
                                           num_mc_cores=num_mc_cores)
  )
  features[["Affirmation"]]<-textcounter(paste0(c("great","good","nice","good","interesting","cool","excellent","awesome"),"-1"),sets[["w.nums"]],words=TRUE,
                                         num_mc_cores=num_mc_cores)

  features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="thank"))))+
                              unlist(lapply(sets[["p.nonum"]], function(x) sum(grepl("(appreciate, i)",x,fixed=TRUE)))))

  features[["Greeting"]]<-textcounter(c("hi","hello","hey"),sets[["c.words"]],words=TRUE,
                                      num_mc_cores=num_mc_cores)

  features[["Direct.Questions"]]<-textcounter(c(paste0(c("who","what","why","how"),"-1"),
                                                paste0(c("who","what","why","how"),"-2")),
                                              sets[["w.nums"]],words=TRUE,
                                              num_mc_cores=num_mc_cores)

  # Same but no negation scoping
  features[["Positive.Emotion"]]<-textcounter(positive_list,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
  features[["Negative.Emotion"]]<-textcounter(negative_list,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)

  # Bigger list (before subjectivity split)
  old_hedge_list=c("think", "thought", "thinking", "almost",
                   "apparent", "apparently", "appear", "appeared", "appears", "approximately", "around",
                   "assume", "assumed", "certain amount", "certain extent", "certain level", "claim",
                   "claimed", "doubt", "doubtful", "essentially", "estimate",
                   "estimated", "feel", "felt", "frequently", "from our perspective", "generally", "guess",
                   "in general", "in most cases", "in most instances", "in our view", "indicate", "indicated",
                   "largely", "likely", "mainly", "may", "maybe", "might", "mostly", "often", "on the whole",
                   "ought", "perhaps", "plausible", "plausibly", "possible", "possibly", "postulate",
                   "postulated", "presumable", "probable", "probably", "relatively", "roughly", "seems",
                   "should", "sometimes", "somewhat", "suggest", "suggested", "suppose", "suspect", "tend to",
                   "tends to", "typical", "typically", "uncertain", "uncertainly", "unclear", "unclearly",
                   "unlikely", "usually", "broadly", "tended to", "presumably", "suggests",
                   "from this perspective", "from my perspective", "in my view", "in this view", "in our opinion",
                   "in my opinion", "to my knowledge", "fairly", "quite", "rather", "argue", "argues", "argued",
                   "claims", "feels", "indicates", "supposed", "supposes", "suspects", "postulates")

  features[["Hedges"]]<-textcounter(old_hedge_list,sets[["clean"]],
                                    num_mc_cores=num_mc_cores)


  features[["Please.Start"]]<-textcounter(paste0(c("please"),"-1"),sets[["w.nums"]],words=TRUE,
                                          num_mc_cores=num_mc_cores)

  features[["First.Person.Single.Start"]]<-textcounter(paste0(c("i","my","mine","myself"),"-1"),sets[["w.nums"]],words=TRUE,
                                                       num_mc_cores=num_mc_cores)

  features[["Second.Person.Start"]]<-textcounter(paste0(c("you","your","yours","yourself", "yourselves"),"-1"),sets[["w.nums"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)

  features[["Please"]]<-textcounter(c("please"),sets[["c.words"]],words=TRUE,
                                    num_mc_cores=num_mc_cores)-features[["Please.Start"]]
  features[["First.Person.Single"]]<-textcounter(c("i","my","mine","myself"),sets[["c.words"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)-features[["First.Person.Single.Start"]]
  features[["Second.Person"]]<-textcounter(c("you","your","yours","yourself", "yourselves"),sets[["c.words"]],words=TRUE,
                                           num_mc_cores=num_mc_cores)-features[["Second.Person.Start"]]

  feature.data<-as.data.frame(features)
  feature.data[feature.data<0]<-0
  return(feature.data)
}
###############################################################

# ################################################
# Features that are more or less the same (some different names)
#
# #
# # 1. Gratitude
# # 6. Apologizing
# # 2. Deference (affirmation)
# # 3. Greeting ( hello)
# # 5. Factuality
# # 9. Indirect (btw)
# # 11. Direct start (conj start)
# # 12. Counterfactual modal (Could/Would you)
# # 13. Indicative modal (Can/Will you)
# # 15. 1st person plural
#
# "Gratitude"
# "Apology"
# "Affirmation"
# "Hello"
# "Truth.Intensifier"
# "By.The.Way"
# "Conjunction.Start"
# "Could.You"
# "Can.You"
# "First.Person.Plural"
#
# ################################################
# # Similar feature, different definition
#
# # These two did not have negation scoping
# # 4. Positive
# # 5. Negative
# "Positive.Emotion"
# "Negative.Emotion"
#
# # This used to include a lot of subjectivity as well
# # 4. Hedges (old hedges)
# "Hedges"
#
# # This was just one category
# # 10. Direct question
# #why = lambda p: (getleftpos(p) in (1,2) and getleft(p) in ("what","why","who","how")) or (getrightpos(p) in (1,2) and getright(p) in ("what","why","who","how"))
# #why.__name__ = "Direct question"
# "WH.Questions"
# "YesNo.Questions"
#
# # For these, they were originally split into "start" (first token) vs. generic, and we combined them
# # 7. Please
# # 8. Please start
# # 16. 1st person
# # 14. 1st person start
# # 17. 2nd person
# # 18. 2nd person start
# "Please"
# "First.Person.Single"
# "Second.Person"
#
#
# ################################################
# # Not in the original paper at all
# #
# # "Impersonal.Pronoun"
# # "Swearing"
# # "Negation"
# # "Filler.Pause"
# # "Informal.Title"
# # "Formal.Title"
# # "Let.Me.Know"
# # "Goodbye"
# # "For.Me"
# # "For.You"
# # "Reasoning"
# # "Reassurance"
# # "Ask.Agency"
# # "Give.Agency"
# # "Agreement"
# # "Disagreement"
# # "Acknowledgement"
# # "Subjectivity"
# # "Bare.Command"
# # "Adverb.Limiter"
#


