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
  'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1)),
  'nnet'=expand.grid(size=c(1,3,5),decay=c(0.01,0.1,1)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(1,3,5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1)),
  'glm'=NULL#data.frame()
)


mdls <- list('svmLinear'='svmLinear',
             'svmPoly'='svmPoly',
             'Neural Network'='nnet',
             'randomForest'='rf',
             'k-NN'='knn',
             'Naive Bayes'='nb',
             'GLM'='glm',
             'GAM'='gam')
#multinom

mdli <- list(
  'Regression'=c(T,T,T,T,T,F,T,F),
  'Classification'=c(T,T,T,T,T,T,F,F)
)  

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



