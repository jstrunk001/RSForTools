% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accuracy_thematic.R
\name{accuracy_thematic}
\alias{accuracy_thematic}
\title{Compute thematic measures of accuracy for classification data}
\usage{
accuracy_thematic(
  pred = NA,
  obs = NA,
  yMat = NA,
  yPropDir = c(2, 1),
  do_balance = c(F, T),
  balance_min = 75,
  seed = 1
)
}
\arguments{
\item{pred}{thematic predictions}

\item{obs}{thematic observations (measured theme such as forest / mixed / nonforest)}

\item{yMat}{matrix of some response with column headings matching thematic data}

\item{yPropDir}{direction to compute proportion (column or row) see apply}

\item{do_balance}{balance observations by group before computing accuracy}

\item{balance_min}{what should the minimum number of observations in a group be to balance}

\item{seed}{when balancing set a random seed for repreducibility}
}
\value{
a messy set of accuracy objects at this point including a table of overall accuraccies:
   overall accuracy
   Cohen's Kappa
   Median F1 statistic
   Matthew's Correlation Coefficient
}
\description{
Compute thematic measures of accuracy for classification data
}
\details{
Compute thematic measures of accuracy for classification data

\cr
Revision History
\tabular{ll}{
1.0 \tab 5/10/2021 created \cr
1.1 \tab date and revisions.. \cr
}
}
\examples{
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
\seealso{
\code{\link{mltools::mcc}}\cr  \code{\link{MLmetrics::F1_Score}}\cr \code{\link{caret::confusionMatrix}}\cr
}
\author{
Jacob Strunk <Jacob.strunk@usda.gov>
}
