#' Text Counter
#'
#' @description Counts total prevalence of a set of items in each of a set of texts.
#' @param counted character vector of items to search for in the texts.
#' @param texts character vector of to-be-searched text.
#' @param words logical. Default FALSE. Does \code{counted} contain words, or sequences of chracters?
#' @param fixed logical. Default TRUE. Use literal characters instead of regular expressions?
#' @param num_mc_cores integer Number of cores for parallelization. Default is parallel::detectCores().
#' @return numeric vector as long as \code{texts} indicating total frequencies of \code{counted} items.
#' @keywords internal
#'
textcounter<-function (counted, texts, words=FALSE, fixed = TRUE, num_mc_cores = parallel::detectCores()) {

  if(words){
    counts<-unlist(parallel::mclapply(texts,function(x) sum(unlist(x)%in%counted), mc.cores=num_mc_cores))
  }else {
    counts <- rep(0, length(texts))
    for (x in counted) {
      counts <- counts + unlist(sapply(gregexpr(x, texts, fixed = fixed),
                                       function(z) ifelse(z[1] == (-1), 0, length(z))))
    }
  }
  counts[is.na(counts)]<-0
  return(counts)
}

#' Clean Text
#' @description Basic text cleaning
#' @param text character text to be cleaned
#' @param language string. Default "english".
#' @param stop.words logical. Default TRUE
#' @return a character vector
#' @keywords internal
cleantext<-function (text, language = "english", stop.words = TRUE) {
  text <- tolower(text)
  if (language == "english") {
    text <- ctxpand(text)
  }
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("[[:cntrl:]]", " ", text)
  if (length(stop.words) > 1) {
    text <- tm::removeWords(text, stop.words)
  }
  else if (stop.words) {
    text <- tm::removeWords(text, tm::stopwords(language))
  }
  text <- tm::removeNumbers(text)
  text <- tm::stripWhitespace(text)
  text <- paste0(" ",text," ")
  return(as.character(text))
}

#' Contraction Expander
#' @description Expands Contractions
#' @param text a character vector of texts.
#' @return a character vector
#' @keywords internal
ctxpand<-function(text){
  text<-sapply(text, function(x) gsub("let's", "let us", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("i'm", "i am", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("won't", "will not", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("can't", "cannot", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("shan't", "shall not", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("'d", " would", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("'ve", " have", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("'s", " is", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("'ll", " will", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("'re", " are", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("n't", " not", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("u.s.a.", "usa", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("u.s.", "usa", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("e.g.", "eg", x, fixed=TRUE))
  text<-sapply(text, function(x) gsub("i.e.", "ie", x, fixed=TRUE))
  return(text)
}


#' UK to US conversion
#' @description background function to load.
#' @param text character Vector of strings to convert to US spelling.
#' @return character Vector of Americanized strings.
#' @keywords internal
usWords<-function(text){
  toks <- quanteda::tokens(text)
  tokUS<-quanteda::tokens_lookup(toks, politeness::uk2us,
                                 exclusive = FALSE,capkeys = FALSE)
  sentUS<-unlist(lapply(tokUS,paste, collapse=" "),use.names = F)
  return(sentUS)
}

#' Cleaning weird encodings
#' @description Handles curly quotes, umlauts, etc.
#' @param text character Vector of strings to clean.
#' @return character Vector of clean strings.
#' @keywords internal
cleanpunct<-function(text){
  # text<-gsub("‘", "'",text)
  # text<-gsub("’", "'", text)
  # text<-gsub("“", '"', text)
  # text<-gsub("”", '"', text)
  #text<-gsub(",Äô", '\'', text,fixed=T)
  text<-gsub(",", ", ", text,fixed=T)
  text<-gsub(".", ". ", text,fixed=T)
  text<-gsub(". .", "..", text,fixed=T)
  text<-gsub("[\x84\x93\x94]", '"', text)
  text<-gsub("[\u201C\u201D\u201E\u201F\u2033\u2036]", '"', text)
  text<-gsub("[\x82\x91\x92]", "'", text)
  text<-gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", text)
  text<-stringi::stri_trans_general(text, "latin-ascii")
  return(text)
}
