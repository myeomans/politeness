#' Dictionary Wrapper
#'
#' @description background function to load
#' @param text a character vector of texts.
#' @param dict a dictionary class object (see \link[quanteda]{dictionary}) containing dictionaries for six of the politeness features
#' @param binary return the prevalence (percent of words) or the presence (yes/no) of a feature in each text?
#' @param ... arguments passes onto the \code{quanteda:dfm} function
#' @return a matrix with six columns (one for each feature) and a row for every text entered into the function.
#' @keywords internal
#' @importFrom quanteda dfm tokens tokens_lookup quanteda_options

dictWrap<-function (text, dict = NULL, binary = FALSE,  num_mc_cores=parallel::detectCores(), ...) {
  if(is.null(dict)){
    stop("Dictionary Must Be Supplied")
  }
  quanteda_options(threads=num_mc_cores)
  quanteda_options("verbose" = FALSE)
  CTB <- as.matrix(array(0, c(length(text), length(dict))))
  WC <- rep(1,length(text))
  wc1 <- (!is.na(stringr::str_count(text, "[[:alpha:]]+")))
  dic.try <- dfm(tokens_lookup(tokens(text[wc1]), dictionary = dict), ...)
  if(length(dic.try)==0){
    emptyct<-matrix(0,nrow=length(text),ncol=length(dict))
    colnames(emptyct)<-names(dict)
    return(emptyct)
  }else{
    CTD <- as.matrix(dic.try)[, 1:length(dict)]
    if (is.null(nrow(CTD)))
      CTD <- CTD/WC[wc1]
    if (!is.null(nrow(CTD)))
      CTD <- apply(CTD, 2, function(x) x/WC[wc1])
    CTB[wc1, ] <- CTD
    colnames(CTB) <- names(dict)
    if (binary)
      CTB <- 1 * (CTB == 0)
    return(CTB)
  }
}
################################################################

# #################################################################
# # This builds the dicionaries - secret!
# #################################################################
# hedge_list<-readLines("modeldata/hedges.txt")
# positive_list<-readLines("modeldata/positive-words.txt")
# negative_list<-readLines("modeldata/negative-words.txt")
# ################################################################
# load("modeldata/polite_dicts.RDa")
# load("R/sysdata.RDa")
# polite_dicts[["FilledPause"]]<-c("er","sigh","hm*","uh","uh*","um","um*")
# polite_dicts[["InformalTitle"]]<-c("dude*", "bro*", "boss", "bud", "buddy", "champ", "man", "guy*", "guy", "brotha", "sista", "son", "sonny", "chief")
# polite_dicts[["FormalTitle"]]<-c("sir", "ma'am", "maam", "mister", "mr*", "ms*", "madam", "miss", "gentleman", "lady")
#
# ################################################################
# devtools::use_data(hedge_list,
#                    positive_list,
#                    negative_list,
#                    polite_dicts,
#                    receptive_model,
#                    internal = TRUE,
#                    overwrite= TRUE)
