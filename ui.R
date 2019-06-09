#rm(list=ls(all=TRUE))
## install the necessary package ==================================================
list.of.packages <- c("ggplot2", "survival","boot","DT","shiny",
                       #"shinyjs",
                      "ggpubr")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#install.packages(list.of.packages)
lapply(list.of.packages,library,character.only = TRUE)

## start the page arrangement =====================================================
fluidPage(
  
  ## title ------------------------------------------------------------------
  div(style="color:darkblue",
      align = "center",
      headerPanel("Reconstruct Individual Patient Data (IPD) From Kaplan-Meier Survival Curve")
  ),
  
  
  #titlePanel("Reconstruct Individual Patient Data (IPD) From Kaplan-Meier Survival Curve"),
  tags$p(tags$h4(("Na Liu, J. Jack Lee"),align="center")),
  tags$p(tags$h5(tags$em("Department of Biostatistics, MD Anderson Cancer Center"),align="center")),
  tags$p(tags$h5(tags$em("Last Updat: 04/12/2019"),align="center")),
  ## sidebar -----------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      div(
        tags$style(type = "text/css",
                   "label { font-size: 12px; }"
        ),
        id = "form",
        # action buttons ----------------------------------
        wellPanel(
          tags$p(tags$h4(tags$strong('Calculating Control',style = "color:darkblue;"))),
          fluidRow(
            actionButton(inputId = "go",label = "Begin Calculation"),
            actionButton(inputId ="resetAll", label = "Reset To Default")
          )),
        # Input the two-column coordinates file ------
        wellPanel(
          tags$p(tags$h4(tags$strong('Choose Data File',style = "color:darkblue;"))),
          fileInput("file1", "Load the coordinates dataset of survival curve extracted by digitize softwares.
                    Accept .csv or .txt files.",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          radioButtons("sep","Separator", inline = T,
                       choices = c(Comma = ",",
                                   Tab = "\t"),
                       selected = ",")
        ),
        # other inputs ---------------------
        wellPanel(
          # reported times at risk --------
          tags$p(tags$h4(tags$strong('Reported Times at Risk',style = "color:darkblue;"))),
          textInput('trisk', 'Separate numbers by commas. 
                    E.g: 0,6,12,18,24. Can be NULL if not available.','NULL'),
          # reported patient number at risk ----
          tags$p(tags$h4(tags$strong('Reported Numbers of Patients at Risk',style = "color:darkblue;"))),
          textInput('nrisk', 'Separate numbers by commas. 
                    E.g: 144,83,45,32,10. Can be NULL if not available. ','NULL'),
          # scale of survival rates -------------
          tags$p(tags$h4(tags$strong('Scale of Survival Rates',style = "color:darkblue;"))),
          selectInput("maxy", "If the survival rates are in the form of percentages, use 100;
                      if in decimals, use 1.", 
                      c("1","100"),selected = "100"
          ),
          # treatment arm indicator ---------------
          tags$p(tags$h4(tags$strong('Treatment Arm Indicator',style = "color:darkblue;"))),
          selectInput("armind", "The treatment arm indicator can be an arbitrary number from 0 to 5.",
                      c("0","1","2","3","4","5"),selected = "0"
          ),
          # initial number of patients -------------
          tags$p(tags$h4(tags$strong('The Initial Number of Patients',style = "color:darkblue;"))),
          textInput('totalpts', 'Can be NULL when the reported times at risk and patients
                    numbers at risk are available. Otherwise, an integer number must be input.',
                    "NULL"
          ),
          # total number of events ------------------
          tags$p(tags$h4(tags$strong('The Initial Number of Patients',style = "color:darkblue;"))),
          textInput('totalevent', 'Can be NULL if not available.','NULL')
          ))),
    ## main panel ----------------------------------------------------------------
    mainPanel(tabsetPanel(
      type = "tabs",
      
      tabPanel("Plot Estimation", value=2, fluidRow(
        plotOutput("plot1"),
        dataTableOutput("summary1")
      )),
      tabPanel("Plot Survival", value=3, fluidRow(
        plotOutput("plot2"),
        dataTableOutput("summary2")
      )),
      tabPanel("Download IPD ",value=4, fluidRow(
        downloadButton("downloadData", "Download IPD as CSV File"),
        dataTableOutput('table1')
      )),
      tabPanel("Download Report",value=5, fluidRow(
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton("report","Download Report")
      )),
      tabPanel("Help",value=1, 
               tags$iframe(style="width: 100%; height: 600px; scrolling=yes",
                           src="KMshinyhelp.pdf")
      )
      
    ))
        )
)





