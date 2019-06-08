#rm(list=ls(all=TRUE))

#### install the necessary libraries ===========================================
list.of.packages <- c("ggplot2", "survival","boot","DT","shiny","shinyjs","cowplot","ggpubr","gridExtra","devtools")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
install.packages(new.packages)
lapply(list.of.packages,library,character.only = TRUE)
devtools::install_github("NaLiuStat/IPDfromKM")
library(IPDfromKM)

#### the server function ========================================================
function(input, output, session) {
  
  ## input date file --------------------------------------- 
  mydata <- eventReactive(input$go,{
    infile <- isolate(input$file1)
    if (is.null(infile)) {return(NULL)}
    read.csv(isolate(input$file1$datapath),header = T,sep = input$sep)
  })
  
  ## input the time intervals -------------------------------
  trisk <- eventReactive(input$go,{
    v1 <- as.numeric(unlist(strsplit(isolate(input$trisk),",")))
    if (is.na(v1)) {return(NULL)}
    else return(v1)
  })
  
  ## input the numbers of patients at risk -------------------
  nrisk <- eventReactive(input$go,{
    v2 <- as.numeric(unlist(strsplit(isolate(input$nrisk),",")))
    if (is.na(v2)) {return(NULL)}
    else return(v2)
  })
  
  ## input the scale of suvival rates --------------------------
  maxy <- eventReactive(input$go,{
    x1 <- as.numeric(isolate(input$maxy))
    return(x1)
  })
  
  ## assign the treatment arm indicators ------------------------
  armind <- eventReactive(input$go,{
    return(as.numeric(isolate(input$armind)))
  })
  
  ## input the initial number of patients ------------------------
  totalpts <- eventReactive(input$go,{
    x2 <- as.numeric(isolate(input$totalpts))
    if (is.na(x2)) {return(NULL)}
    else return(x2)
  })
  
  ## input the total number of events if available ----------------
  totalevent <- eventReactive(input$go,{
    x3 <- as.numeric(isolate(input$totalevent))
    if (is.na(x3)) {return(NULL)}
    else return(x3)
  })
  
  ## preprocess the input data file ---------------------------------
  pre <- eventReactive(input$go,{
    preprocess(dat=mydata(),trisk=trisk(),nrisk=nrisk(),totalpts = totalpts(),maxy=maxy())
  })
  
  ## the reconstructed IPD -------------------------------------------
  mylist <- eventReactive(input$go,{
    getIPD(prep=pre(),armind=armind(),tot.events=totalevent())
  })
  
  ## take the reset buttion reaction for next curve--------------------
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  
  ## output the CSV file -------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = "reconstructIPD.csv",
    content = function(file) {
      write.csv(mylist()[[1]], file, row.names = FALSE)
    }
  )
  
  ## print the CSV file  --------------------------------------------------
  output$table1 <- renderDataTable(round(mylist()[[1]],3))
  
  ## graph plot1: patients number at risk -----------------------------------
  output$plot1 <- renderPlot({
    plotrisk <- plotIPD(est=mylist(),ori_dat=mydata(),maxy=maxy())
    plotrisk
  },height=400)
  
  ## print summary table 1 --------------------------------------------------
  output$summary1 <- DT::renderDataTable(mylist()[[8]], options=list(
    columnDefs = list(list(className = 'dt-center', targets = 2)),
    dom="t",
    lengthMenu = c(20, 10)
  ))
  
  ## graph plot2: survival plots ---------------------------------------------
  output$plot2 <- renderPlot({
    survreport(ipd1=mylist()[[1]],arms=1,interval=6)
  },height=400)
  
  ## print summary table 2 ----------------------------------------------------
  qt1 <- reactive({
    fit1 <- survfit(Surv(time,status)~1,data=mylist()[[1]])
    qt <- data.frame(q = c(.75, .5, .25),
                     qt = quantile(fit1))
    qt <- round(qt,4)
    names(qt) <- c('Survival Rate', 'Time','0.95LCI','0.95UCI')
    return (qt)
  })
  
  output$summary2 <- DT::renderDataTable(round(qt1(), 3),
                                         rownames=FALSE,                                     
                                         options=list(dom="t")
  )
  ## out put the report ------------------------------------------------------
  output$report <- downloadHandler(
    
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('./www/kmreport.Rmd')
      ## temporarily switch to the temp dir, in case you do not have write
      ## permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'kmreport.Rmd', overwrite = TRUE)
      
      ## render report from markdown file
      library(rmarkdown)
      out <- render('kmreport.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  ) 
  
  
}

