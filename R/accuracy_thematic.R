#'@title
#'  Compute thematic measures of accuracy for classification data
#'
#'@description
#'  Compute thematic measures of accuracy for classification data
#'
#'@details
#'
#'  Compute thematic measures of accuracy for classification data
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/10/2021 created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param pred thematic predictions
#'@param obs thematic observations (measured theme such as forest / mixed / nonforest)
#'@param yMat matrix of some response with column headings matching thematic data 
#'@param yPropDir direction to compute proportion (column or row) see apply
#'@param do_balance balance observations by group before computing accuracy
#'@param balance_min what should the minimum number of observations in a group be to balance
#'@param seed when balancing set a random seed for repreducibility

#'
#'@return
#'  a messy set of accuracy objects at this point including a table of overall accuraccies:
#'    overall accuracy
#'    Cohen's Kappa
#'    Median F1 statistic
#'    Matthew's Correlation Coefficient
#'
#'@examples
#' options(scipen = 10e6)
#' N=500
#' seq1 = 1:10
#' seq2 = seq(0,10,.5)
#'
#' #No correlation
#' acc1 = accuracy_thematic(
#'   pred=cut(runif(500,1,10),1:10,include.lowest = T)
#'   ,obs=cut(runif(500,1,10),1:10,include.lowest = T)
#' )
#'
#' #No correlation - 20 classes
#' acc1b = accuracy_thematic(
#'   pred=cut(runif(500,1,10),seq2,include.lowest = T)
#'   ,obs=cut(runif(500,1,10),seq2,include.lowest = T)
#' )
#'
#' #some correlation
#' y_obs = runif(N,1,10)
#' y_pd = y_obs + rnorm(N)
#' too_big_small = y_pd > 10 | y_pd <1
#' y_pd[too_big_small] = sample(1:10,sum(too_big_small),replace=T)
#'
#' acc2 = accuracy_thematic(
#'   pred = cut(y_pd, seq1 , include.lowest = T)
#'   ,obs= cut(y_obs, seq1, include.lowest = T)
#' )
#'
#' #some correlation - 20 bins
#' acc2b = accuracy_thematic(
#'   pred = cut(y_pd, seq2 , include.lowest = T)
#'   ,obs= cut(y_obs, seq2, include.lowest = T)
#' )
#'
#' #strong correlation
#' y_obs = runif(N,1,10)
#' y_pd = y_obs + rnorm(N,0,.5)
#' too_big_small = y_pd > 10 | y_pd <1
#' y_pd[too_big_small] = sample(1:10,sum(too_big_small),replace=T)
#'
#' acc3 = accuracy_thematic(
#'   pred = cut(y_pd, seq1 , include.lowest = T)
#'   ,obs= cut(y_obs, seq1, include.lowest = T)
#' )
#' acc3b = accuracy_thematic(
#'   pred = cut(y_pd, seq2 , include.lowest = T)
#'   ,obs= cut(y_obs, seq2, include.lowest = T)
#' )
#'
#' res_all = data.frame(
#'   bins=c(10,20)
#'   ,cor=sort(rep(c("no","some","strong"),2))
#'   ,plyr::rbind.fill(
#'     list(acc1$res_table
#'          ,acc1b$res_table
#'          ,acc2$res_table
#'          ,acc2b$res_table
#'          ,acc3$res_table
#'          ,acc3b$res_table
#'     )
#'   ))
#'
#' upper.panel<-function(x, y,col,text){
#'   points(x,y, pch=19,col=col)
#'   text(x,y, text)
#'   abline(0,1)
#' }
#' pairs(res_all[,-(1:2)],col=as.factor(res_all[,1])
#'       ,text=apply(res_all[,c(1,2)],1,paste,collapse="")
#'       ,lower.panel = NULL
#'       ,upper.panel = upper.panel
#' )


#'
#'@import MLmetrics mltools greenbrown caret
#'
#'@export
#
#'@seealso \code{\link{mltools::mcc}}\cr \code{\link{greenbrown::AccuracyAssessment}}\cr \code{\link{MLmetrics::F1_Score}}\cr \code{\link{caret::confusionMatrix}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))
#
#various helpers:
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}

accuracy_thematic = function(
                            pred = NA
                           ,obs = NA
                           ,yMat = NA
                           ,yPropDir = c(2,1)
                           ,do_balance = c(F,T)
                           ,balance_min = 75  #ignore balancing requirement for groups with n < 50, e.g.
                           ,seed = 1          #guarantee sample sample when balancing
){

  set.seed(seed)
  #make sure all levels are accounted for
  unq_levels_in = unique(c(as.character(unlist(obs)),as.character(unlist(pred))))

  #make sure everything is a factor
  #if(class(pred) == "factor") pred_in = pred else 
    pred_in = factor(as.character(unlist(pred)), levels = unq_levels_in, labels = unq_levels_in)
  #if(class(obs) == "factor") obs_in = obs else 
    obs_in = factor(as.character(unlist(obs)), levels = unq_levels_in, labels = unq_levels_in)

  #balance sample on observed data - optional
    #some evaluation criteria are affected by balance
  if(do_balance[1]){
    idx_bal = .fn_balance_idx(obs=obs_in,balance_min=balance_min,unq_levels_in)
    obs_in=obs_in[idx_bal]
    pred_in=pred_in[idx_bal]
  }

  #load up results
  res_in0=list()
  res_in0$gb = greenbrown::AccuracyAssessment(table(pred_in,obs_in))
  res_in0$caret = caret::confusionMatrix(pred_in, obs_in, dnn = c("Prediction", "Reference"))[]
  if(is.null(nrow(res_in0$caret$byClass))) res_in0$F1 = res_in0$caret$byClass["F1"]
  else if(nrow(res_in0$caret$byClass)>1) res_in0$F1 = median(res_in0$caret$byClass[,"F1"],na.rm = T)
  else  res_in0$F1 = res_in0$caret$byClass[,"F1"]
  res_in0$mcc = mltools::mcc(preds = pred_in, actuals = obs_in)

  #compute results for response by category
  if(is.matrix(yMat) | is.data.frame(yMat)){

    if(do_balance[1]) yMat_in = yMat[idx_bal,] else yMat_in = yMat
    df_in_yMat = data.frame(pred_in, yMat_in)

    #compute totals and sort columns and rows
    #df_in_yMat$pred_in = as.factor(df_in_yMat$pred_in)
    agg_in = aggregate(.~ pred_in, data = df_in_yMat, FUN = sum,drop=F)
    agg_in = agg_in[order(as.character(agg_in$pred_in)),]
    agg_in = agg_in[,c( names(agg_in)[1],  sort(as.character(names(agg_in)[-1]))) ]
    agg_in[-1,][is.na(agg_in[-1,])]=0
    names(agg_in) = gsub("NA[.]","NA",names(agg_in))
    row.names(agg_in ) = agg_in$pred_in
    
    #compute proportions
    #prop_in = data.frame(agg_in[,1,drop=F],t(apply(agg_in[,-1,drop=F],1,function(x) if(sum(x,na.rm=T) > 0) round(x / sum(x,na.rm=T),3) else x )))
    if(yPropDir[1]==1) prop_in = data.frame(agg_in[,1,drop=F],t(apply(agg_in[,-1,drop=F],1,function(x) if(sum(x,na.rm=T) > 0) round(x / sum(x,na.rm=T),3) else x )))
    if(yPropDir[1]==2) prop_in = data.frame(agg_in[,1,drop=F],(apply(agg_in[,-1,drop=F],2,function(x) if(sum(x,na.rm=T) > 0) round(x / sum(x,na.rm=T),3) else x )))
    prop_in[-1,][is.na(prop_in[-1,])]=0
    row.names(prop_in ) = prop_in$pred_in
    names( prop_in ) = names(agg_in)
    #apply(prop_in[,-1],1,sum) #test column and row aggregates
    #apply(prop_in[,-1],2,sum)
    
    nms_cat = as.character(prop_in[,1])
    nms_cat = nms_cat[nms_cat != "NA"]
    prop_diag = try(diag(as.matrix(prop_in[nms_cat,nms_cat])))
    prop_diag[is.na(prop_diag)] = 0
    #add results 
    res_in0$ySum = agg_in
    res_in0$yProp = prop_in
    res_in0$yProp_diag_smry = try(quantile(prop_diag,c(0,.5,1)))
    names(res_in0$yProp_diag_smry) = paste("diag.p",gsub("[%]","",names(res_in0$yProp_diag_smry)),sep="")
    
  }
  
  #prepare results
  res_in0$res_table = data.frame(
                       overall = res_in0$caret$overall["Accuracy"]
                      ,kappa = res_in0$caret$overall["Kappa"]
                      ,F1 = res_in0$F1
                      ,mcc = res_in0$mcc
  )

    
  #return results
  res_in0
}

#internal function to balance sample
.fn_balance_idx=function(obs,balance_min,unq_levels_in){
    tb_in0 = table(obs)
    min_in = max(balance_min,min(table(obs)))
    f_sample = function(x_in,ni_in,vec_in){
      idx_xin = which(vec_in==x_in)
      if(ni_in < length(idx_xin)) sample(which(vec_in==x_in),ni_in)
      else which(vec_in==x_in)
    }
    idx_bal = unique(unlist(lapply(unq_levels_in, f_sample, min_in, obs)))
    return(idx_bal)
}
#localize accuracy related functions

#create confusion matrix with accuracy in margins
.fn_confMat=function(pred, obs,accuracy=F){}
.fn_f1 <-function(){}
.fn_mcc=function(){}
.fn_cohenK=function(){}

#NOT WORKING
#these functions adapted from MLmetrics
# .fn_confDF=function(pred,obs){
#   transform(
#       as.data.frame(table(pred, obs))
#       , obs = as.character(obs)
#       , pred = as.character(pred)
#       ,Freq = as.integer(Freq)
#     )
# }
# .fn_Precision=function(pred,obs,positive=NA){
#   browser()
#   Confusion_DF <-.fn_confDF(pred,obs)
#   if (is.na(positive)) positive <- as.character(Confusion_DF[1, 1])
#   TP <- as.integer(subset(Confusion_DF, obs == positive & obs == positive)[,"Freq"])
#   FP <- as.integer(sum(subset(Confusion_DF, obs != positive & obs == positive)["Freq"]))
#   Precision <- TP / (TP + FP)
#   return(Precision)
# }
# .fn_Recall=function(pred,obs,positive=NA){
#   Confusion_DF <-.fn_confDF(pred,obs)
#   if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1, 1])
#   TP <- as.integer(subset(Confusion_DF, obs == positive & pred == positive)["Freq"])
#   FN <- as.integer(sum(subset(Confusion_DF, obs == positive & pred != positive)["Freq"]))
#   Recall <- TP/(TP + FN)
#   return(Recall)
# }
# .fn_f1=function(pred,obs,positive=NA){
#   #create long form confusion table
#   Confusion_DF <-.fn_confDF(pred,obs)
#   #assign "positive" value
#   if (is.na(positive)) positive <- as.character(Confusion_DF[1, 1])
#   Precision <- .fn_Precision(obs, pred, positive)
#   Recall <- .fn_Recall(obs, obs, positive)
#   F1_Score <- 2 * (Precision * Recall)/(Precision + Recall)
#   return(F1_Score)
# }
# .fn_f1 <- function(predicted, expected, positive.class="1") {
#   browser()
#   predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
#   expected  <- as.factor(expected)
#   cm = as.matrix(table(expected, predicted))
#
#   precision <- diag(cm) / colSums(cm)
#   recall <- diag(cm) / rowSums(cm)
#   f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
#
#   #Assuming that F1 is zero when it's not possible compute it
#   f1[is.na(f1)] <- 0
#
#   #Binary F1 or Multi-class macro-averaged F1
#   ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
# }


#test metrics for varying levels of correlation
if(F){

  options(scipen = 10e6)
  N=500
  seq1 = 1:10
  seq2 = seq(0,10,.5)

#No correlation
  acc1 = accuracy_thematic(
    pred=cut(runif(500,1,10),1:10,include.lowest = T)
    ,obs=cut(runif(500,1,10),1:10,include.lowest = T)
  )

#No correlation - 20 classes
  acc1b = accuracy_thematic(
    pred=cut(runif(500,1,10),seq2,include.lowest = T)
    ,obs=cut(runif(500,1,10),seq2,include.lowest = T)
  )

#some correlation
  y_obs = runif(N,1,10)
  y_pd = y_obs + rnorm(N)
  too_big_small = y_pd > 10 | y_pd <1
  y_pd[too_big_small] = sample(1:10,sum(too_big_small),replace=T)

  acc2 = accuracy_thematic(
    pred = cut(y_pd, seq1 , include.lowest = T)
    ,obs= cut(y_obs, seq1, include.lowest = T)
  )

#some correlation - 20 bins
  acc2b = accuracy_thematic(
    pred = cut(y_pd, seq2 , include.lowest = T)
    ,obs= cut(y_obs, seq2, include.lowest = T)
  )

#strong correlation
  y_obs = runif(N,1,10)
  y_pd = y_obs + rnorm(N,0,.5)
  too_big_small = y_pd > 10 | y_pd <1
  y_pd[too_big_small] = sample(1:10,sum(too_big_small),replace=T)

  acc3 = accuracy_thematic(
    pred = cut(y_pd, seq1 , include.lowest = T)
    ,obs= cut(y_obs, seq1, include.lowest = T)
  )
  acc3b = accuracy_thematic(
    pred = cut(y_pd, seq2 , include.lowest = T)
    ,obs= cut(y_obs, seq2, include.lowest = T)
  )

  res_all = data.frame(
    bins=c(10,20)
    ,cor=sort(rep(c("no","some","strong"),2))
    ,plyr::rbind.fill(
        list(acc1$res_table
         ,acc1b$res_table
         ,acc2$res_table
         ,acc2b$res_table
         ,acc3$res_table
         ,acc3b$res_table
         )
    ))

  upper.panel<-function(x, y,col,text){
    points(x,y, pch=19,col=col)
    text(x,y, text)
    abline(0,1)
  }
  pairs(res_all[,-(1:2)],col=as.factor(res_all[,1])
        ,text=apply(res_all[,c(1,2)],1,paste,collapse="")
        ,lower.panel = NULL
        ,upper.panel = upper.panel
        )

}
