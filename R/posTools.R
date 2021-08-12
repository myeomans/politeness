# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("l_parse_nums","parses",
                         "l_parses","p.nonums",
                         "l_pos_nums","pos.nums",
                         "l_w_nums","w.nums",
                         "rd0","rd1","rd2","rd3","root_dist",
                         "pos.dists","l_pos_dists",
                         "doc_id","sentence_id",
                         "token_id","token", ".",
                         "raw_head_token_id",
                         "head_token","head_token_id",
                         "dep_rel", "tag","question",
                         "anyNeg","parseNeg","negs",
                         "negP1","negP2","negP3","negP4",
                         "negM1","negM2",
                         "parseNeg1","parseNeg2","parseNeg3",
                         "selfies","selfscope",
                         "selfscope1","selfscope2"))

################################################################
# Workflow for SpaCy
################################################################
# spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")
################################################################

#' Spacy Parser
#' @description Return POS tags from natural language.
#' @param txt a character vector of texts.
#' @return list of compiled POS-tagged items.
#' @keywords internal
#' @import data.table
spacyParser<- function(txt){
  parsedtxt <- spacyr::spacy_parse(txt, dependency=TRUE,lemma=FALSE,pos=TRUE,tag=TRUE,entity=FALSE)
  .ds<-paste0(parsedtxt$sentence_id,parsedtxt$doc_id)
  parsedtxt$question<-1*(.ds%in%(.ds[parsedtxt$token=="?"]))
  dt_parsedtxt <- data.table::data.table(parsedtxt)
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
  dt_parsedtxt[,raw_head_token_id:=head_token_id]
  ###### ROOT DISTANCE (for questions)
  dt_parsedtxt[dep_rel=="root" , rd0:=1]

  dt_heads=dt_parsedtxt[rd0==1,c("doc_id","sentence_id","token_id")]
  dt_heads[,rd1:=1]
  setnames(dt_heads, c("token_id"),("head_token_id"))
  dt_parsedtxt <- dt_heads[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[!is.na(rd0),rd1:=1]

  dt_heads=dt_parsedtxt[rd1==1,c("doc_id","sentence_id","token_id")]
  dt_heads[,rd2:=1]
  setnames(dt_heads, c("token_id"),("head_token_id"))
  dt_parsedtxt <- dt_heads[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[!is.na(rd1),rd2:=1]

  dt_heads=dt_parsedtxt[rd2==1,c("doc_id","sentence_id","token_id")]
  dt_heads[,rd3:=1]
  setnames(dt_heads, c("token_id"),("head_token_id"))
  dt_parsedtxt <- dt_heads[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[!is.na(rd2),rd3:=1]

  dt_parsedtxt[,root_dist:=4-sum(rd0,rd1,rd2,rd3,na.rm=T),,by=list(doc_id, sentence_id, token_id)]
  ###### Constuct Tag Sets
  dt_parsedtxt[ , pos.nums := paste0("(",token_id,"-",token,"-",tag,")")]
  dt_parsedtxt[ , parses := paste0(dep_rel, "(",head_token,"-",head_token_id,", ",token,"-",token_id,")")]
  dt_parsedtxt[ , p.nonums := paste0(dep_rel, "(",head_token,", ",token,")")]
  dt_parsedtxt[ , w.nums := paste0(token,"-",token_id)]

  all.parses <- dt_parsedtxt[ , .(l_parse_nums = list(parses)), keyby = "doc_id"][ , l_parse_nums]
  all.pos.nums <- dt_parsedtxt[ , .(l_pos_nums = list(pos.nums)), keyby = "doc_id"][ , l_pos_nums]
  p.nonums <- dt_parsedtxt[ , .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  w.nums <- dt_parsedtxt[ , .(l_w_nums = list(w.nums)), keyby = "doc_id"][ , l_w_nums]

  dt_parsedtxt[ , pos.dists := paste0("(",token_id,"-",token,"-",tag,")","-",root_dist)]

  # Only from questions

  blanks<-unique(dt_parsedtxt, by= "doc_id")
  blanks[,pos.dists:=" "]
  qSet <-dt_parsedtxt[question==1]

  qFull=rbindlist(list(blanks,qSet))

  ques.pos.dists=qFull[, .(l_pos_dists = list(pos.dists)), by=doc_id][ , l_pos_dists]

  #################### Negations!

  setkeyv(dt_parsedtxt, c("doc_id", "sentence_id", "token_id"))


  negations=c(polite_dicts$Negation,"n't","nobody","negate","negative","negation")
  # Expand the negation window
  dt_parsedtxt[,negs:=token%in%negations]
  dt_parsedtxt[,negP1:=shift(negs,1),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP2:=shift(negs,2),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP3:=shift(negs,3),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP4:=shift(negs,4),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negM1:=shift(negs,-1),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negM2:=shift(negs,-2),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,anyNeg:=sum(negP1,negP2,negP3,negP4,negM1,negM2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  blanks<-rbindlist(list(unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id")))
  blanks[,anyNeg:=duplicated(doc_id)]
  blanks[,token:=" "]
  dt_parsedtxt=rbindlist(list(dt_parsedtxt,blanks))

  neg.words=dt_parsedtxt[(anyNeg), .(l_w_nums = list(token)), keyby = "doc_id"][ , l_w_nums]
  unneg.words=dt_parsedtxt[(!anyNeg), .(l_w_nums = list(token)), keyby = "doc_id"][ , l_w_nums]

  dt_parsedtxt=dt_parsedtxt[token!=" "]


  # selfie triplets (for apologies)
  dt_parsedtxt[,selfies:=token%in%c("I","we","We","our","Our","me","Me","us","Us")]
  dt_selfies=dt_parsedtxt[selfies==1,c("doc_id","sentence_id","head_token_id")]
  dt_selfies[,selfscope1:=1]
  dt_parsedtxt <- dt_selfies[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[is.na(selfscope1),selfscope1:=0]

  dt_selfies=dt_parsedtxt[selfscope1==1,c("doc_id","sentence_id","token_id","selfscope1")]
  setnames(dt_selfies, c("token_id","selfscope1"), c("head_token_id","selfscope2"))
  dt_parsedtxt <- dt_selfies[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[is.na(selfscope2),selfscope2:=0]
  dt_parsedtxt[,selfscope:=sum(selfscope1,selfscope2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]


  # adverbial, adpositional and conjunctive clauses shouldn't propogate dependency negations.
  dt_parsedtxt[dep_rel%in%c("advcl","prep"),head_token_id:=0]
  dt_parsedtxt[dep_rel%in%c("conj") & abs(head_token_id-token_id)>3,head_token_id:=0]

  dt_negged<-dt_parsedtxt[dep_rel=="neg",c("doc_id","sentence_id","head_token_id")]
  dt_negged[,parseNeg1:=TRUE]
  dt_parsedtxt <- dt_negged[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")] # left merge on dt_parsedtxt
  setnames(dt_negged, c("head_token_id","parseNeg1"), c("token_id","parseNeg2"))
  dt_parsedtxt <- dt_negged[dt_parsedtxt, on=c("doc_id","sentence_id","token_id")] # left merge on dt_parsedtxt

  dt_parsedtxt[,parseNeg:=sum(parseNeg1,parseNeg2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  dt_superhead=dt_parsedtxt[parseNeg==TRUE, c("doc_id","sentence_id","token_id")]
  setnames(dt_superhead, "token_id","head_token_id")
  dt_superhead[,parseNeg3:=TRUE]
  dt_parsedtxt <- dt_superhead[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")] # left merge on dt_parsedtxt

  dt_parsedtxt[,parseNeg:=sum(parseNeg,parseNeg3,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  # Fill in blanks
  blanks<-rbindlist(list(unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id")))
  blanks[,parseNeg:=duplicated(doc_id)]
  blanks[,p.nonums:=" "]
  blanks[,question:=0]
  blanks[,selfscope:=1]
  dt_parsedtxt=rbindlist(list(dt_parsedtxt,blanks))

  p.negs <- dt_parsedtxt[(parseNeg)&(question==0), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  p.unnegs <- dt_parsedtxt[(!parseNeg)&(question==0), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]

  self.unnegs <- dt_parsedtxt[(!parseNeg)&(question==0)&(selfscope==1), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  return(list(parses=all.parses,
              ques.pos.dists=ques.pos.dists,
              pos.nums=all.pos.nums,
              p.nonums=p.nonums,
              w.nums=w.nums,
              neg.words=neg.words,
              unneg.words=unneg.words,
              p.negs=p.negs,
              p.unnegs=p.unnegs,
              self.unnegs=self.unnegs))
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
