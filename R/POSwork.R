# require(rJava)
# require(coreNLP)
#
# options(java.parameters = "-Xmx1024m")
# initCoreNLP("/stanford-corenlp/", mem="16g")
#
# row.to.char<-function(deps){
#   return(paste0(deps$type,"(",
#                 deps$governor,"-",
#                 deps$governorIdx,", ",
#                 deps$dependent,"-",
#                 deps$depIndex,")"))
# }
#
# core.parser<-function(text){
#   sentences<-as.list(qdap::sent_detect(text))
#   parses<-list()
#   for (s in 1:length(sentences)){
#     a.s<-coreNLP::annotateString(sentences[[s]])
#     dep.table<-coreNLP::getDependency(a.s, type="collapsed")
#     dep.table<-dep.table[dep.table$sentence==1,]
#     dep.char<-c()
#     for (x in 1:nrow(dep.table)){
#       dep.char<-c(dep.char,row.to.char(dep.table[dep.table$depIndex==x,]))
#     }
#     parses[[s]]<-dep.char
#   }
#   all.parses=do.call(c, parses)
#   return(list(sentences=sentences,
#               parses=parses,
#               all.parses=all.parses))
# }
#
# #
# # text1<-"Have you found the answer for your question? that would, in fact, be useful!"
# # core.parser(text1)
# #
# #
#
# #   "Have you found the answer for your question?",
# #   ["csubj(found-3, Have-1)", "dobj(Have-1, you-2)", "root(ROOT-0, found-3)", "det(answer-5, the-4)", "dobj(found-3, answer-5)", "poss(question-8, your-7)", "prep_for(found-3, question-8)"],
# #   "If yes would you please share it?"
# #   ["prep_if(would-3, yes-2)", "root(ROOT-0, would-3)", "nsubj(would-3, you-4)", "ccomp(would-3, please-5)", "nsubj(it-7, share-6)", "xcomp(please-5, it-7)"]
# #   "Sorry :) I dont want to hack the system!!",
# #   ["nsubj(dont-5, I-4)", "xsubj(hack-8, I-4)", "rcmod(-RRB--3, dont-5)", "dep(dont-5, want-6)", "aux(hack-8, to-7)", "xcomp(want-6, hack-8)", "det(!!-11, the-9)", "nn(!!-11, system-10)", "dobj(hack-8, !!-11)"],
# #   ":) is there another way?"
# #   ["cop(there-4, is-3)", "root(ROOT-0, there-4)", "det(way-6, another-5)", "dep(there-4, way-6)"]
# #   "What are you trying to do?",
# #   ["dep(trying-4, What-1)", "aux(trying-4, are-2)", "nsubj(trying-4, you-3)", "xsubj(do-6, you-3)", "root(ROOT-0, trying-4)", "aux(do-6, to-5)", "xcomp(trying-4, do-6)"],
# #   "Why can't you just store the 'Range'?"
# #   ["advmod(ca-2, Why-1)", "advcl(store-6, ca-2)", "neg(ca-2, n't-3)", "nsubj(store-6, you-4)", "advmod(store-6, just-5)", "root(ROOT-0, store-6)", "det(Range-9, the-7)", "dobj(store-6, Range-9)"]
# #   "this was supposed to have been moved to &lt;url&gt; per the cfd.",
# #   ["nsubjpass(supposed-3, this-1)", "xsubj(moved-7, this-1)", "auxpass(supposed-3, was-2)", "root(ROOT-0, supposed-3)", "aux(moved-7, to-4)", "aux(moved-7, have-5)", "auxpass(moved-7, been-6)", "xcomp(supposed-3, moved-7)", "prep_to(moved-7, url-10)", "det(cfd-14, the-13)", "prep_per(url-10, cfd-14)"],
# #   "why wasn't it moved?"
# #   ["advmod(n't-3, why-1)", "cop(n't-3, was-2)", "root(ROOT-0, n't-3)", "nsubj(moved-5, it-4)", "dep(n't-3, moved-5)"]
