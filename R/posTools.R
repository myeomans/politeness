# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("l_parses","parses",
                         "l_pos_nums","pos.nums",
                         "l_w_nums","w.nums",
                         "doc_id","sentence_id",
                         "token_id","token", ".",
                         "head_token","head_token_id",
                         "dep_rel", "tag"))

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
spacyParser<- function(txt, num_mc_cores=parallel::detectCores()){
  parsedtxt <- spacyr::spacy_parse(txt, dependency=TRUE,lemma=FALSE,pos=TRUE,tag=TRUE,entity=TRUE)
  dt_parsedtxt <- data.table(parsedtxt)
  unique_doc_ids <- dt_parsedtxt[ , unique(doc_id)]
  dt_parsedtxt[ , doc_id := factor(doc_id, levels = unique_doc_ids)]
  dt_head_token <- dt_parsedtxt[ , .(doc_id, sentence_id, token_id,token)]
  setnames(dt_head_token, c("token_id","token"), c("head_token_id","head_token"))
  v_s_keys <-  c("doc_id", "sentence_id", "head_token_id" )
  setkeyv(dt_head_token, v_s_keys)
  setkeyv(dt_parsedtxt, v_s_keys)
  dt_parsedtxt <- dt_head_token[dt_parsedtxt] # left merge on dt_parsedtxt
  dt_parsedtxt[dep_rel=="ROOT" , head_token := "ROOT" ]
  dt_parsedtxt[dep_rel=="ROOT" , head_token_id := 0 ]
  dt_parsedtxt[dep_rel=="ROOT" , dep_rel := "root" ]
  dt_parsedtxt[ , pos.nums := paste0("(",token_id,"-",token,"-",tag,")")]
  dt_parsedtxt[ , parses := paste0(dep_rel, "(",head_token,"-",head_token_id,", ",token,"-",token_id,")")]
  dt_parsedtxt[ , w.nums := paste0(token,"-",token_id)]

  all.parses <- dt_parsedtxt[ , .(l_parses = list(parses)), keyby = "doc_id"][ , l_parses]
  all.pos.nums <- dt_parsedtxt[ , .(l_pos_nums = list(pos.nums)), keyby = "doc_id"][ , l_pos_nums]
  nonums=parallel::mclapply(all.parses,gsub, pattern="-[0-9][0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="-[0-9]",replacement="", mc.cores=num_mc_cores)
  w.nums <- dt_parsedtxt[ , .(l_w_nums = list(w.nums)), keyby = "doc_id"][ , l_w_nums]
  return(list(parses=all.parses,
              pos.nums=all.pos.nums,
              nonums=nonums,
              w.nums=w.nums))
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
