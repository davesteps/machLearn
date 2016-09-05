library(shiny)
require(shinyBS)
require(shinydashboard)
require(shinyjs)
require(caret)
require(dplyr)
require(tidyr)
require(Cairo)
require(raster)
require(gstat)
require(wesanderson)


data(meuse)

dmnds <- diamonds[sample(1:nrow(diamonds),1e3),]

datasets <- list(
  'iris'=iris,
  'cars'=mtcars,
  'meuse'=meuse,
  'diamonds'=data.frame(dmnds),
  'midwest'=data.frame(midwest),
  'mpg'=data.frame(mpg),
  'msleep'=data.frame(msleep),
  'txhousing'=data.frame(txhousing)
)

tuneParams <- list(
  'svmLinear'=data.frame(C=c(0.01,0.1,1)),
  'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1)),
  'nnet'=expand.grid(size=c(1,3,5),decay=c(0.01,0.1,1)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1))
  # 'glm'=data.frame(),
  # 'gam'=data.frame(),
)


models <- data.frame(stringsAsFactors = F,
                     shortName = c('svmLinear','svmPoly','nnet','rf','knn','nb','glm','gam'),
                     longName =  c('svmLinear','svmPoly','Neural Network','randomForest',
                                   'k-Nearest Neighbors','Naive Bayes','GLM',
                                   'GAM'),
                     reg =       c(T,T,T,T,F,F,F,F),
                     cls =       c(T,T,T,T,T,T,F,F)
)

# 
pal <- c('#b2df8a','#33a02c','#ff7f00','#cab2d6','#b15928',
         '#fdbf6f','#a6cee3','#fb9a99','#1f78b4','#e31a1c')
set.seed(3)
pal <- sample(pal,nrow(models),F)
names(pal) <- models$shortName

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




# modelpal <- lapply(1:nrow(models),function(i) modelCSS(models$longName[i],pal[i])) %>% 
# paste(.,collapse = ' ')

# modelpal



reg.mdls <- models$shortName[models$reg]
names(reg.mdls) <- models$longName[models$reg]
cls.mdls <- models$shortName[models$cls]
names(cls.mdls) <- models$longName[models$cls]

# TODO --------------------------------------------------------------------
# (To get it uploaded bare min)

# default state
# github
# test on upload


# phase 2
# match colors w value box
# data description
# transform y-var
# full compliment of algos with fast/slow tuning params
# progress bar
# Draw eye to setup box
# add a few more data sets
# Instructions on training page
# supresswarnings in train
# signifacne testing to choose models in ensemble

# Features/issues:
# class levels not in test/train
# feature selection
# Parital Dependancy plots
# variable importance??
# pre-processing - some algos require
# weights
# tuning metric, unbalanced classes etc
# reasonable values to search over
# how to handle probability outputs
# model specific outputs?
# build tunning params from data

# Server ------------------------------------------------------------------

server <- function(input, output,session) {
  
  # CVtune <- CVtune
  CVtune <- readRDS('initState.Rdata')
  makeReactiveBinding('CVtune')
  
  rawdata <- reactive({
    datasets[[input$dataset]]
  })
  
  observe({
    updateSelectizeInput(session,'yvar',choices=names(rawdata()),selected = names(rawdata())[1])
  })
  
  
  observe({
    nms <- names(rawdata())[names(rawdata())!=input$yvar]
    updateSelectizeInput(session,'xvar',choices=nms,selected = nms)
  })
  
  dataTrain <- NULL
  dataTest <- NULL
  
  # makeReactiveBinding('dataTrain')
  # makeReactiveBinding('dataTest')
  modelType <- 'Regression'

  makeReactiveBinding('modelType')
  
  observeEvent(modelType,{
    
    if(modelType=='Regression'){
      updateSelectizeInput(session,'slt_algo',choices = reg.mdls,selected = reg.mdls)
    } else {
      updateSelectizeInput(session,'slt_algo',choices = cls.mdls,selected = cls.mdls)
      
    }
  })
  
  observe({
    
    yvar <- input$yvar
    xvars <- input$xvar
    testsize <- input$sld_testsplit
    
    if(is.null(yvar)||yvar=='')
      return(NULL)
    
    # extract y and X from raw data
    y <- isolate(rawdata()[,yvar])
    X <-  isolate(rawdata()[,xvars])
    
    # deal with NA values
    yi <- !is.na(y)
    Xi <- complete.cases(X)
    
    df2 <- cbind(y,X)[yi&Xi,]
    
    c <- class(df2$y)
    lvls <- length(unique(df2$y))
    if(lvls<10|(c!='numeric'&c!='integer')){
      modelType <<-'Classification'
      df2$y <- factor(df2$y)
    } else {
      modelType <<-'Regression'
    }
    
    trainIndex <- createDataPartition(df2$y,
                                      p = 1-(testsize/100),
                                      list = FALSE,
                                      times = 1)
    isolate({
      dataTrain <<- df2[ trainIndex,]
      dataTest  <<- df2[-trainIndex,]
    })
  })
  

  
  observeEvent(input$btn_train,{
  
    disable('btn_train')
    on.exit(enable('btn_train'))
    
    mdls <- input$slt_algo
    
    fitControl <- trainControl(method = "cv",savePredictions = T,
                               number = as.integer(input$rdo_CVtype))
    
    
    train2 <- function(method){
      switch(modelType,
             'Regression'=train(y ~ .,
                                data = dataTrain,
                                preProcess = c('scale','center'),
                                method = method,
                                trControl = fitControl,
                                tuneGrid=tuneParams[[method]],
                                linout=T
             ),
             'Classification'=train(y ~ .,
                                    data = dataTrain,
                                    preProcess = c('scale','center'),
                                    method = method,
                                    trControl = fitControl,
                                    tuneGrid=tuneParams[[method]]
             )
      )
      
    }
    
    CVtune <- lapply(mdls,train2)
    names(CVtune) <- mdls
    CVtune<<-CVtune
    # saveRDS(CVtune,'initState.Rdata')
    
  })
  
  
  CVres <- reactive({
    mdls <- isolate(input$slt_algo)
    if(is.null(CVtune)) return(NULL)
    
    fits <- CVtune
    getRes <- function(i){
      name <- mdls[i]
      res <- fits[[i]]$results
      df <- res[(ncol(res)-3):ncol(res)]
      apply(res,1,function(r) paste(r[1:(ncol(res)-4)],collapse = '-')) %>% 
        paste(name,.,sep='-') -> model
      cbind.data.frame(model,df,name,stringsAsFactors =F)
    }
    
    df <- plyr::ldply(1:length(mdls),getRes)
    
    if(isolate(modelType)=='Regression'){
      df$rank <- rank(rank(df$RMSE)+rank(1-df$Rsquared),ties.method = 'first')
    } else {
      df$rank <- rank(rank(1-df$Accuracy)+rank(1-df$Kappa),ties.method = 'first')
    }
    df[2:5] <- round(df[2:5],3)
    df[order(df$rank),]
  })
  
  CVpredObs <- reactive({
    
    mdls <- isolate(input$slt_algo)
    
    fits <- CVtune
    
    getObsPred <- function(i){
      # i <- 2
      bst <- fits[[i]]$bestTune
      preds <- fits[[i]]$pred
      preds$name <- mdls[i]
      preds$model <- paste(bst,collapse = '-') %>% paste(mdls[i],.,sep='-')
      ii <- lapply(1:length(bst),function(i)preds[names(bst)[i]]==bst[i][[1]])
      if(length(bst)>1) data.frame(ii) %>% apply(.,1,all) -> ii else unlist(ii) ->ii
      preds[ii,-which(names(preds)%in%names(bst))]
    }
    
    plyr::ldply(1:length(mdls),getObsPred)
    
    
  })
  
  topModels <- reactive({
    if(is.null(CVres()))
      return()
    CVres() %>% group_by(name) %>% filter(rank==min(rank)) -> df
    # 
    lst <- df$name[order(df$rank)]
    names(lst) <- df$model[order(df$rank)]
    lst
  }) 
  
  observe({
    lst <- topModels()
    updateSelectizeInput(session,'slt_Finalalgo',choices = lst,selected = lst[1])
    
  })
  
  testPreds <- reactive({
    
    tune <- isolate(CVtune)
    if(is.null(tune)) return(NULL)
    
    lapply(CVtune[input$slt_Finalalgo],
           predict.train,isolate(dataTest)) %>% 
      data.frame() -> df
    
    if(isolate(modelType)=='Regression'){
      c <- apply(df[input$slt_Finalalgo],1,mean)
      
      s1 <- 1 - mean((dataTest$y-c)^2)/mean((dataTest$y-mean(dataTest$y))^2)
      s2 <- sqrt(mean((dataTest$y-c)^2))
      
    } else {
      c <- apply(df[input$slt_Finalalgo],1,modal)
      s1 <- sum(c==dataTest$y)/nrow(dataTest)
      s2 <- vcd::Kappa(table(c, dataTest$y))$Unweighted[1]
    }
    list(c=c,s1=s1,s2=s2)
    
  })
  
  
  
  
  
  
  # Outputs ---------------------------------------------------------------------
  
  output$testsetPlot <- renderPlot({
    

    df <- data.frame(obs=dataTest$y,pred=testPreds()$c)
    
    col <- pal[topModels()[[1]]]
    
    if(isolate(modelType)=='Regression'){
      lims <- c(min(df$obs),max(df$obs))
      ggplot(df)+
        geom_abline(alpha=0.5)+
        geom_point(aes(x=obs,y=pred),color=col,size=2)+
        scale_x_continuous(limits = lims)+
        scale_y_continuous(limits = lims)+
        # scale_color_manual(values=pal)+
        coord_equal()+
        # facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
    } else {
      df$pred <- factor(df$pred,levels=levels(df$obs))
      df %>% group_by(pred,obs) %>% 
        summarise(n=n()) %>% 
        ggplot(.)+
        geom_raster(aes(x=obs,y=pred,alpha=n),fill=col)+
        geom_text(aes(x=obs,y=pred,label=n))+
        # scale_fill_manual(values=pal)+
        coord_equal()+
        # facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
      
    }
    
  })
  
  output$testsetS1 <- renderValueBox({
    
    lab <- ifelse(isolate(modelType)=='Regression','Variance explained','Accuracy')
    
    valueBox(paste(round(testPreds()$s1*100,1),'%'),lab,icon = icon('bullseye'))
    
  })
  
  output$testsetS2<- renderValueBox({
    lab <- ifelse(isolate(modelType)=='Regression','RMSE','Kappa')
    valueBox(round(testPreds()$s2,3),subtitle = lab,icon = icon('bullseye'))
  })
  
  
  
  
  output$rawdata <- DT::renderDataTable(rawdata())
  
  output$model_info <- renderDataTable({
    CVres()[c(7,1:6)]
    
  },    options = list(rowCallback = I(
    lapply(1:nrow(models),function(i) tableCSS(models$shortName[i],pal[i])) %>% 
      unlist %>% 
      paste(.,collapse = '') %>% 
      paste('function(row, data) {',.,'}')
  ),
  pageLength = 10,searching = FALSE
  )
  )
  
  
  output$CVplot2 <- renderPlot({
    
    type <- isolate(modelType)
    df <-CVpredObs()
    if(type=='Regression'){
      lims <- c(min(df$obs),max(df$obs))
      ggplot(df)+
        geom_abline(alpha=0.5)+
        geom_point(aes(x=obs,y=pred,col=name))+
        scale_x_continuous(limits = lims)+
        scale_y_continuous(limits = lims)+
        scale_color_manual(values=pal)+
        coord_equal()+
        facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
    } else {
      df %>% group_by(pred,obs,name) %>% 
        summarise(n=n()) %>% 
        ggplot(.)+
        geom_raster(aes(x=obs,y=pred,fill=name,alpha=n))+
        geom_text(aes(x=obs,y=pred,label=n))+
        scale_fill_manual(values=pal)+
        coord_equal()+
        facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
      
    }
  })
  
  output$CVplot1 <- renderPlot({
    resdf <- CVres()
    type <- isolate(modelType)
    
    resdf$model <- factor(resdf$model,levels = rev(resdf$model[resdf$rank]))
    if(type=='Regression'){
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=RMSE-RMSESD,ymax=RMSE+RMSESD),size=1)+
        geom_point(aes(y=RMSE),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p1
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Rsquared-RsquaredSD,ymax=Rsquared+RsquaredSD),size=1)+
        geom_point(aes(y=Rsquared),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p2
    } else {
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Kappa-KappaSD,ymax=Kappa+KappaSD),size=1)+
        geom_point(aes(y=Kappa),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p1
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Accuracy-AccuracySD,ymax=Accuracy+AccuracySD),size=1)+
        geom_point(aes(y=Accuracy),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p2
    }
    
    gridExtra::grid.arrange(p2,p1,ncol=2)
    
  })
  
  output$txt_dataset <- renderPrint(cat('Dataset:',input$dataset))
  output$txt_n <- renderPrint(cat('n obs:',nrow(rawdata())))
  output$txt_Yvar <- renderPrint(cat('Y var:',input$yvar))
  output$txt_testSet <- renderPrint(cat('Test set:',input$sld_testsplit,'%'))
  output$txt_Type <- renderPrint(cat('Model Type:',modelType))
  output$txt_CV <- renderPrint(cat('CV folds:',input$rdo_CVtype))
  output$txt_nModels <- renderPrint(cat('Models trained:',nrow(CVres())))
  output$txt_bestModel <- renderPrint(cat('Best Model:',(CVres()$model[1])))
  output$txt_bestModelStat1 <- renderPrint({
    if(modelType=='Regression'){
      cat('Variance Explained:',(CVres()$Rsquared[1]*100),'%')
    } else {
      cat('Accuracy:',(CVres()$Accuracy[1]))
    }
  })
  output$txt_bestModelStat2 <- renderPrint({
    if(modelType=='Regression'){
      cat('RMSE:',(CVres()$RMSE[1]))
    } else {
      cat('Kappa:',(CVres()$Kappa[1]))
    }
  })
  
  

  
  output$Yplot <- renderPlot({
    
    if(modelType=='Regression'){
      
      
      ggplot(dataTrain,aes(x=y))+
        geom_density(alpha=0.7,adjust=0.5,fill="#5BBCD6")+
        theme_bw()+
        ggtitle('Y Distribution')+
        xlab('')
      
      
      # wes_palettes$Darjeeling
      
    } else {
      pal <- wes_palette('Darjeeling',n = length(unique(dataTrain$y)),type = 'c')
      ggplot(dataTrain,aes(x=y,fill=y))+
        geom_bar(stat='count')+
        scale_fill_manual(values=pal)+
        xlab('')+
        ggtitle('Y Class Frequency')+
        coord_flip()+
        theme(legend.position='none')
    }
    
  })
  
  
}





# UI ----------------------------------------------------------------------

ui <- bootstrapPage(useShinyjs(),
                    # Add custom CSS & Javascript;
                    tagList(tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js"),
                      lapply(1:nrow(models),function(i) modelCSS(models$shortName[i],pal[i]))
                      
                    )),
                    
                    dashboardPage(#skin = 'red',
                      dashboardHeader(title = HTML(paste(icon('cubes'),'machLearn'))
                      ),
                      dashboardSidebar(
                        sidebarMenu(
                          # Setting id makes input$tabs give the tabName of currently-selected tab
                          id = "tabs",
                          menuItem("Step 1: Input Data", tabName = "setup", icon = icon("cog")),
                          menuItem("Step 2: Training & CV",tabName = "model", icon = icon("sitemap"),selected = T),
                          menuItem("Step 3: Model Performance",tabName = "test", icon = icon("sitemap"))
                          # menuItem("Feature selection", icon = icon("align-left", lib = "glyphicon"),
                          # menuSubItem("Boruta", tabName = "boruta"),
                          # menuSubItem("Sequential backward selection",
                          # tabName = "SBS")),
                          # 
                          # menuItem("Feature Importance",tabName = "featSel", icon = icon("sitemap"))
                          # menuItem("Info",tabName = "Info", icon = icon("info"))
                        ),
                        hr(),
                        fluidRow(
                          column(width=1),
                          column(width=10,
                                 h5(textOutput('txt_dataset')),
                                 h5(textOutput('txt_n')),
                                 h5(textOutput('txt_Yvar')),
                                 h5(textOutput('txt_testSet'))
                                 
                                 
                          ),
                          column(width=1)
                          
                          
                          
                        ),
                        absolutePanel(
                          bottom = 10,
                          left = 10,
                          draggable = F,
                          width='100%',
                          height='auto',
                          a(icon('github fa-2x'),href='https://github.com/davesteps/homebrewR',target='_blank')
                        )                  
                      ),
                      dashboardBody(
                        tabItems(
                          tabItem("setup",
                                  box(width = 4,title = 'Input Data',solidHeader = T,status = 'primary',
                                      selectInput('dataset',label = 'Choose Dataset',
                                                  choices = names(datasets),selected='iris'),
                                      # fileInput('fileIn',label = 'Upload Dataset:') %>% disabled(),
                                      
                                      selectizeInput('yvar',label=label.help('Y (Variable to predict):','lbl_yvar'),choices = character(0)),
                                      bsTooltip(id = "lbl_yvar", title = "Variable to predict", 
                                                placement = "right", trigger = "hover"),
                                      selectizeInput('xvar',label=label.help('X (Predict Y as function of):','lbl_xvar'),choices = character(0),multiple = T),
                                      bsTooltip(id = "lbl_xvar", title = "Try and predict Y as function of these variables", 
                                                placement = "right", trigger = "hover"),
                                      sliderInput('sld_testsplit',label = label.help('Test set %','lbl_testsplit'),min = 33,max = 90,step = 1,value = 33),
                                      bsTooltip(id = "lbl_testsplit", title = "% of data to set aside for test data", 
                                                placement = "right", trigger = "hover"),
                                      hr(),
                                      plotOutput('Yplot',height=260)
                                      
                                  ),
                                  box(width=8,
                                      DT::dataTableOutput('rawdata')
                                  )
                          ),
                          tabItem("model",
                                  # bsModal('mdl_tune','Tuning Options',trigger = 'btn_tune',
                                  
                                  # ),
                                  column(width=3,
                                         box(width = 12,title = 'Model Options',solidHeader = T,status = 'primary',
                                             selectInput('slt_algo',label = 'Algorithm:'%>%label.help('lbl_algo'),
                                                         ,choices = reg.mdls,selected = reg.mdls,multiple=T),
                                             selectizeInput('slt_Tune','Parameter Tuning'%>%label.help('lbl_Tune'),
                                                            choices = c('Coarse auto-tune (fast)','Fine auto-tune (slow)','manual')),
                                             # actionButton('btn_tune',label = 'Tuning Options',icon = icon('sliders')
                                             # ),
                                             # p(),
                                             
                                             radioButtons('rdo_CVtype',label = 'Cross-validation folds'%>%label.help('lbl_CV'),
                                                          choices = c('3-fold'=3,'5-fold'=5,'10-fold'=10),inline = T),
                                             
                                             actionButton('btn_train',label = 'Train Models',
                                                          icon = icon('cogs'),#'bullseye','rocket'
                                                          class='btn-danger fa-lg',
                                                          width='100%'),
                                             bsTooltip(id = "lbl_algo", title = "Which algorithms to test", 
                                                       placement = "right", trigger = "hover"),
                                             bsTooltip(id = "lbl_Tune", title = "Type of tuning which is performed to optimize model parameters", 
                                                       placement = "right", trigger = "hover"),
                                             bsTooltip(id = "lbl_CV", title = "Number of splits of training data used to tune parameters", 
                                                       placement = "right", trigger = "hover")
                                             
                                         ),
                                         box(width = 12,title = 'Summary',solidHeader = F,
                                             status = 'primary',
                                             helpText(textOutput('txt_bestModel')),
                                             helpText(textOutput('txt_bestModelStat1')),
                                             helpText(textOutput('txt_bestModelStat2')),
                                             hr(),
                                             helpText(textOutput('txt_Type')),
                                             helpText(textOutput('txt_CV')),
                                             helpText(textOutput('txt_nModels'))
                                             
                                         )
                                  )
                                  ,
                                  tabBox(width = 9,
                                         tabPanel(title = 'CV Model Rank',#icon = icon('sort-amount-asc'),
                                                  h4('Cross-validation results'),
                                                  plotOutput('CVplot1',height=600)
                                         ),
                                         tabPanel(title = 'CV Pred vs Obs',
                                                  h4('Observed vs Predicted (best candidate for algorithm)'),
                                                  plotOutput('CVplot2',height=600)
                                         ),
                                         tabPanel(title = 'CV Stats',
                                                  h4('Performance statiscs from cross-validation'),
                                                  
                                                  dataTableOutput('model_info')
                                         )
                                  )
                          ),
                          tabItem("test",
                                  column(width=3,
                                         box(width = 12,title = 'Test Set Predictions',solidHeader = F,status = 'primary',
                                             # radioButtons('rdo_finalModel','Final model',
                                             #              c('Best Model','Ensemble of top models')),
                                             selectInput('slt_Finalalgo',label = 'Final Model:'%>%label.help('lbl_Finalalgo'),
                                                         choices=models$shortName,multiple=T),
                                             helpText('The best candidate model from cross-validation is used by default. 
                                               Select multiple models to use ensemble predictions of best performing candidates'),
                                             bsTooltip(id = "lbl_Finalalgo", title = "Which algorithms to use to predict test", 
                                                       placement = "right", trigger = "hover")
                                             
                                         ),
                                         valueBoxOutput('testsetS1',width=12),
                                         valueBoxOutput('testsetS2',width=12)
                                  ),
                                  box(width = 6,title = 'Test Set observed vs Predicted',
                                      solidHeader = T,status = 'primary',
                                      plotOutput('testsetPlot')
                                  )
                          )
                        )
                      )
                      
                      
                    ),
                    div(class = "busy", 
                        h4("working..."),
                        h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                    )
)

shinyApp(ui, server)