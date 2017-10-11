# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("l_parses","parses",
                         "l_pos_nums","pos.nums",
                         "l_w_nums","w.nums",
                         "doc_id","sentence_id",
                         "token_id","token", "."))


################################################################
# Workflow for SpaCy
################################################################
# spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")
################################################################

#' Spacy Parser
#' @description Return POS tags from natural language.
#' @param txt a character vector of texts.
#' @param num_mc_cores integer Number of cores for parallelization. Default is parallel::detectCores().
#' @return list of compiled POS-tagged items.
#' @keywords internal
#' @import data.table
spacyParser<-function(txt, num_mc_cores=parallel::detectCores()){
  parsedtxt <- spacyr::spacy_parse(txt, dependency=TRUE,lemma=FALSE,pos=TRUE,tag=TRUE,entity=TRUE)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$head_token<-parallel::mclapply(1:nrow(parsedtxt),headTokenGrab, data=parsedtxt, mc.cores=num_mc_cores)
  parsedtxt[parsedtxt$dep_rel=="ROOT",c("dep_rel","head_token","head_token_id")]<-c("root","ROOT",0)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$parses<-paste0(parsedtxt$dep_rel, "(",parsedtxt$head_token,"-",parsedtxt$head_token_id,", ",parsedtxt$token,"-",parsedtxt$token_id,")")
  parsedtxt$w.nums<-paste0(parsedtxt$token,"-",parsedtxt$token_id)

  dt_parsedtxt <- data.table::data.table(parsedtxt)
  all.parses <- dt_parsedtxt[ , .(l_parses = list(parses)), by = "doc_id"][ , l_parses]
  all.pos.nums <- dt_parsedtxt[ , .(l_pos_nums = list(pos.nums)), by = "doc_id"][ , l_pos_nums]
  nonums=parallel::mclapply(all.parses,gsub, pattern="-[0-9][0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9]",replacement="", mc.cores=num_mc_cores)
  w.nums <- dt_parsedtxt[ , .(l_w_nums = list(w.nums)), by = "doc_id"][ , l_w_nums]
  return(list(parses=all.parses,
              pos.nums=all.pos.nums,
              nonums=nonums,
              w.nums=w.nums))
}

#' Head Token Grab
#' @description Compile dependency relation for each row of a SpaCy parse table
#' @param x row in SpaCy parse table
#' @param data data.frame SpaCy parse table
#' @return word tagged as referent from dependencey
#' @keywords internal
#'
headTokenGrab <- function(x, data){
  data <- data.table(data)
  doc_id_local <- data$doc_id[x]
  sentence_id_local <- data$sentence_id[x]
  head_token_id_local <- data$head_token_id[x]
  data <- data[(doc_id==doc_id_local)&
                 (sentence_id==sentence_id_local)&
                 (token_id==head_token_id_local), ]
  return(data[,token])
}
################################################################
# Workflow for coreNLP
################################################################
# options(java.parameters = "-Xmx8g")
# require(rJava)
# require(coreNLP)
# initCoreNLP("/stanford-corenlp/", mem="8g")
################################################################

#' Core Parser
#' @description (deprecated) Part-Of-Speech tagging using Stanford CoreNLP.
#' @param text a character vector of texts.
#' @param num_mc_cores integer Number of cores for parallelization. Default is parallel::detectCores().
#' @return list of
#' @keywords internal
#'

#
# coreParser<-function(text, num_mc_cores=parallel::detectCores()){
#   sentences<-as.list(qdap::sent_detect(text))
#   parses<-list()
#   pos.nums<-list()
#   for (s in 1:length(sentences)){
#     a.s<-coreNLP::annotateString(sentences[[s]])
#     pos.table<-coreNLP::getToken(a.s)[,c("id","token","POS")]
#     dep.table<-coreNLP::getDependency(a.s, type="collapsed")
#     dep.table<-dep.table[dep.table$sentence==1,]
#     dep.char<-c()
#     for (x in 1:nrow(dep.table)){
#       dep.char<-c(dep.char,rowToChar(dep.table[dep.table$depIndex==x,]))
#     }
#     parses[[s]]<-dep.char
#     pos.nums[[s]]<-paste0("(",apply(pos.table,1,paste, collapse="-"),")")
#   }
#   all.parses=do.call(c, parses)
#   all.pos.nums=do.call(c, pos.nums)
#   nonums=parallel::mclapply(all.parses,gsub, pattern="-[0-9][0-9][0-9]",replacement="", mc.cores=num_mc_cores)
#   nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9][0-9]",replacement="", mc.cores=num_mc_cores)
#   nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9]",replacement="", mc.cores=num_mc_cores)
#   w.nums<-substr(all.parses, sapply(all.parses, function(x) gregexpr(",",x,fixed=TRUE)[[1]][1])+2, nchar(all.parses)-1)
#   return(list(parses=all.parses,
#               pos.nums=all.pos.nums,
#               nonums=nonums,
#               w.nums=w.nums))
# }
#
# #' Row To Char
# #' @description constructs POS-tagged words from coreNLP parse table.
# #' @param deps row from coreNLP dependency table
# #' @param data a data.frame
# #' @return a character
# #' @keywords internal
# #'
#
# rowToChar<-function(deps){
#   return(paste0(deps$type,"(",
#                 deps$governor,"-",
#                 deps$governorIdx,", ",
#                 deps$dependent,"-",
#                 deps$depIndex,")"))
# }
