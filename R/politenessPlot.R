
#' Politeness plot
#'
#' @description Plots the prevalence of politeness features in documents, divided by a binary covariate.
#' @param df_polite a data.frame with politeness features calculated from a document set, as output by \code{\link{politeness}}.
#' @param split a vector of covariate values. must have a length equal to the number of documents included in \code{df_polite}. No NA values allowed.
#' @param split_levels character vector of length 2 default NULL. Labels for covariate levels for legend. If NULL, this will be inferred from \code{split}.
#' @param split_name character default NULL. Name of the covariate for legend.
#' @param split_cols character vector of length 2. Name of colors to use.
#' @param top_title character default "". Title of plot.
#' @param drop_blank Features less prevalent than this in the sample value are excluded from the plot. To include all features, set to \code{0}
#' @param middle_out Features less distinctive than this value (measured by p-value of t-test) are excluded. Defaults to 1 (i.e. include all).
#' @details Length of \code{split} must be the same as number of rows of \code{df_polite}. Typically \code{split} should be a two-category variable. However, if a continuous covariate is given, then the top and bottom terciles of that distribution are treated as the two categories (while dropping data from the middle tercile).
#' @return a ggplot of the prevalence of politeness features, conditional on \code{split}. Features are sorted by variance-weighted log odds ratio.
#' @examples
#'
#' data("phone_offers")
#'
#' polite.data<-politeness(phone_offers$message, parser="none", binary=FALSE, drop_blank=FALSE)
#'
#' politeness::politenessPlot(polite.data,
#'                            split=phone_offers$condition,
#'                            split_levels = c("Tough","Warm"),
#'                            split_name = "Condition",
#'                            top_title = "Average Feature Counts")
#'
#'
#' polite.data<-politeness(phone_offers$message, parser="none", binary=TRUE, drop_blank=FALSE)
#'
#' politeness::politenessPlot(polite.data,
#'                            split=phone_offers$condition,
#'                            split_levels = c("Tough","Warm"),
#'                            split_name = "Condition",
#'                            top_title = "Binary Feature Use")
#'
#' @export

politenessPlot<-function(df_polite,
                         split = NULL,
                         split_levels = NULL,
                         split_name = NULL,
                         split_cols = c("firebrick","navy"),
                         top_title = "",
                         drop_blank = 0.05,
                         middle_out = 0.5){

  # confirm that split is the right type
  if(sum(is.na(split))>0){
    stop("split must not have NAs")
  }
  if(is.factor(split)){
    split<-as.character(split)
  }
  # confirm that split only has two values
  if( length(unique(split)) > 2){
    if(is.character(split)){
      stop("split must not have more than two values")
    }
    # if split has more than 2 values transform it into a binary variable by taking the top 33% and top 33%
    # if the cut at 33% and 67% is the same we throw an error
    cuts <- stats::quantile(split, c(1/3, 2/3))
    cut_low <- cuts[1]
    cut_high <- cuts[2]

    if(cut_low == cut_high){
      stop("Cannot convert split into binary variable 33% and 67% percentiles are some value")
    }

    split <- ifelse(split <= cut_low,0,
                    ifelse(split>=cut_high,1, NA_integer_))

    df_polite <- df_polite[!is.na(split),]
    split <- split[!is.na(split)]
    warning("Converting split into binary variable by taking bottom and top 33% of values,
            and removing middle 33% of values in df_polite and split.")

  }
  if( length(split) != nrow(df_polite)){
    stop("split must be same length as document set")
  }

  binary <- setequal(unique(unlist(df_polite)),0:1)

  num_features <- ncol(df_polite)
  l_polite_split <- split(data.frame(df_polite), split)

  if(is.null(split_levels)){
    split_levels <- names(l_polite_split)
  }
  # this makes sure split colors correctly match split levels
  names(split_cols) <- split_levels

  split.data<-data.frame(feature=rep(colnames(df_polite),2),
                         count=c(colMeans(l_polite_split[[1]],na.rm=TRUE),
                                 colMeans(l_polite_split[[2]],na.rm=TRUE)),
                         cond=factor(c(rep(split_levels[1],num_features),
                                       rep(split_levels[2],num_features)), levels = split_levels),
                         se=rep(NA_real_,num_features*2))
  ######################################################
  nonblanks <- colnames(df_polite)[colMeans(df_polite)>=drop_blank]

  split.enough<-names(df_polite)
  if(middle_out<1){
    split.p<-unlist(lapply(names(df_polite), function(x) stats::t.test(l_polite_split[[1]][,x],
                                                                       l_polite_split[[2]][,x])$p.value))
    split.enough<-names(df_polite)[(split.p<middle_out)&(!is.na(split.p))]
  }

  if(sum((split.data$feature%in%nonblanks)&(split.data$feature%in%split.enough))==0){
    stop("All features were excluded. Adjust exclusion settings.")
  }
  split.data<-split.data[(split.data$feature%in%nonblanks)&(split.data$feature%in%split.enough),]
  ######################################################
  if(binary){
    map.type<-"Percentage of Documents Using Feature"
    split.data$se <- sqrt(((split.data$count)*(1-split.data$count))/nrow(df_polite))
    y.breaks <- seq(0,1,.25)
    y.labels <- paste0(seq(0,100,25),"%")
    y.trans <- "identity"
  } else {
    map.type<-"Feature Count per Document"
    split.data$se<-sqrt((split.data$count)/nrow(df_polite))
    tick.set<-c(0.1,0.5,1,2,5,10,20,50,100,200,500,1000)
    y.labels <- y.breaks <- tick.set

    y.trans <- "sqrt"
  }
  ######################################################
  wide<-stats::reshape(split.data, idvar = "feature", timevar = "cond", direction = "wide")
  wide$count.total<-rowMeans(wide[,grepl("count",names(wide))])
  wide$slogodds<-slogodds(wide[,paste0("count.",split_levels[1])],
                          wide[,paste0("count.",split_levels[2])])$slor
  f.order<-unique(wide$feature)[order(wide$slogodds)]
  split.data$feature<-factor(split.data$feature, ordered=TRUE,levels=f.order)
  split.data$cond<-factor(split.data$cond,ordered=TRUE,levels=rev(split_levels))
  split.data$count_minus<-split.data$count-split.data$se
  split.data$count_plus<-split.data$count+split.data$se
  ######################################################
  ggplot2::ggplot(data=split.data,
                  ggplot2::aes_string(x="feature",y="count",fill="cond"),width=2) +
    ggplot2::geom_bar(position=ggplot2::position_dodge(width = 0.8),
                      stat="identity") +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymin="count_minus", ymax="count_plus"), width=0.3,
                           position=ggplot2::position_dodge(width = 0.8)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(name="", breaks=colnames(df_polite),
                              labels=gsub("."," ",colnames(df_polite),fixed=T)) +
    ggplot2::scale_fill_manual(breaks = split_levels,values=split_cols, name=split_name) +
    ggplot2::scale_y_continuous(name=map.type, breaks = y.breaks, labels=y.labels, trans = y.trans) +
    ggplot2::theme_bw(base_size=14) +
    ggplot2::ggtitle(top_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size=20),
                   legend.title = ggplot2::element_text(size=18, face="bold"),
                   legend.text = ggplot2::element_text(size=18),
                   legend.position = "top",
                   legend.background = ggplot2::element_rect(linetype = "solid", color="black"),
                   plot.margin = ggplot2::unit(c(0,25,2,0),"points"),
                   axis.title = ggplot2::element_text(size=16),
                   text=ggplot2::element_text(family="Times"))
}
