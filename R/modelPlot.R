# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("feat","score","freq"))

#' LASSO Coefficient Plot
#' @description Plots feature counts and coefficients from a trained LASSO model
#' @param model1 Trained glmnet model
#' @param model2 Trained glmnet model (optional) If you want the Y axis to reflect a second set of coefficients, instead of feature counts.
#' @param counts Feature counts - either from training data or test data (choose based on application of interest)
#' @param dat logical If TRUE, then function will return a list with the data.frame used for plotting, as well as the plot itself.
#' @import magrittr
#' @return ggplot object. Layers can be added like any ggplot object
#' @description
#' This plots the coefficients from a trained LASSO model.
#'
#'@export

modelPlot<-function(model1,counts,model2=NULL,dat=FALSE){

  plotCoefs<-model1 %>%
    stats::coef(s="lambda.min") %>%
    drop() %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "feat") %>%
    dplyr::rename(score="s1") %>%
    dplyr::filter(score!=0 & feat!="(Intercept)" & !is.na(score))

  if(nrow(plotCoefs)==0){
    stop("Model must have non-zero coefficients")
  }
  plotDat<-plotCoefs %>%
    dplyr::left_join(data.frame(feat=colnames(as.matrix(counts)),
                         freq=colMeans(as.matrix(counts))))  %>%
    dplyr::mutate_at(dplyr::vars(score,freq),~round(.,3))

  if(sum(is.na(plotDat$freq))>0){
    stop("Feature counts must be from same feature set used to train model")
  }

  xrange=range(plotDat$score)*1.2
  yrange=c(.01,.05,.1,.2,.5,1,2,5,10,20,50)
  yrange=yrange[yrange<max(colMeans(counts))]

  thePlot<-plotDat %>%
    ggplot2::ggplot(ggplot2::aes(x=score,y=freq,label=feat,color=score)) +
    ggplot2::scale_color_gradient(low="navyblue",
                                 high="forestgreen")+
    ggplot2::geom_vline(xintercept=0)+
    ggrepel::geom_label_repel(max.overlaps = 15,
                              force_pull=3,
                              force = 10)+
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits=xrange)+
    ggplot2::scale_y_continuous(trans="log2",
                               breaks=yrange)+
    ggplot2::theme_bw() +
    ggplot2::labs(x="Coefficient in Model",y="Uses per Document")+
    ggplot2::theme(legend.position = "none",
                  axis.title=ggplot2::element_text(size=20),
                  axis.text=ggplot2::element_text(size=16))

  if(dat){
    return(list(plot=thePlot,
                data=plotDat))
  } else{
    return(thePlot)
  }
}
