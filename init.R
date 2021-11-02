library(shiny)
require(shinyBS)
require(shinydashboard)
require(shinyjs)
require(caret)
require(plyr)
require(dplyr)
require(tidyr)
require(Cairo)
require(raster)
require(gstat)
require(wesanderson)
require(nnet)
require(randomForest)

# car, foreach, methods, plyr, nlme, reshape2, stats, stats4, utils, grDevices


# Not all of these are required but shinyapps.io was crashing and 
# importing one of these solved the issue
require(kernlab)
require(klaR)
require(vcd)
require(e1071)
require(gam)
require(ipred)
require(MASS)
require(ellipse)
require(mda)
require(mgcv)
require(mlbench)
require(party)
require(MLmetrics)
require(Cubist)
require(testthat)


data(meuse)

dmnds <- diamonds#[sample(1:nrow(diamonds),1e3),]

# leaf <- read.csv('/Users/davesteps/Desktop/kaggle_data/leaf/train.csv')

datasets <- list(
  'iris'=iris,
  'cars'=mtcars,
  'meuse'=meuse,
  'diamonds'=data.frame(dmnds),
  'Boston'=Boston
  # 'leaf'=leaf
  # 'midwest'=data.frame(midwest),
  # 'mpg'=data.frame(mpg),
  # 'msleep'=data.frame(msleep),
  # 'txhousing'=data.frame(txhousing)
)

tuneParams <- list(
  'svmLinear'=data.frame(C=c(0.01,0.1,1)),
  'svmRadial'= expand.grid(C = c(0.5,0.6,0.7,0.8),sigma=c(0.1,0.2,0.3,0.4,0.5)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(1,3,5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1))
  # ,'xgboost' = expand.grid(nrounds = 1000,
  #                         eta = c(0.01, 0.001, 0.0001),
  #                         max_depth = c(2, 4, 6, 8, 10),
  #                         gamma = 1,
  #                         colsample_bytree = 0.75,
  #                         min_child_weight = 0,
  #                         subsample = 0.5)
  )


mdls <- list('svmLinear'='svmLinear',
             'svmRadial'='svmRadial',
             'randomForest'='rf',
             'k-NN'='knn',
             'Naive Bayes'='nb'
             # ,'xgboost'='xgboost'
             )
#multinom

mdli <- list(
  'Regression'=c(T,T,T,T,F),
  'Classification'=c(T,T,T,T,T)
) 

# mdli <- list(
#   'Regression'=c(T,T,T,T,F,T),
#   'Classification'=c(T,T,T,T,T,T)
# )  

reg.mdls <- mdls[mdli[['Regression']]]
cls.mdls <- mdls[mdli[['Classification']]]


# 
pal <- c('#b2df8a','#33a02c','#ff7f00','#cab2d6','#b15928',
         '#fdbf6f','#a6cee3','#fb9a99','#1f78b4','#e31a1c')
set.seed(3)
pal <- sample(pal,length(mdls),F)
names(pal) <- mdls

modelCSS <-   function(item,col){
  tags$style(HTML(paste0(".selectize-input [data-value=\"",item,"\"] {background: ",col," !important}")))
}


tableCSS <- function(model,col){
  paste0('if (data[6] == "',model,'")
         $("td", row).css("background", "',col,'");')
}  

label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}



