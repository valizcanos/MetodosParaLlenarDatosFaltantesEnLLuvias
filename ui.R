######LIBRARIES#####
#######################################################################################->
if(require(shiny)==FALSE){install.packages("shiny")} #To design Web App
if(require(shinydashboard)==FALSE){install.packages("shinydashboard")} #To design Web App
if(require(shinythemes)==FALSE){install.packages("shinythemes")} #To design Web App
#if(require(parallel)==FALSE){install.packages("parallel")} #To Parallelism
#if(require(doParallel)==FALSE){install.packages("doParallel")} #To Parallelism
#if(require(doFuture)==FALSE){install.packages("doFuture")} #To Parallelism
#if(require(foreach)==FALSE){install.packages("foreach")} #To Parallelism
#if(require(doSNOW)==FALSE){install.packages("doSNOW")} #To Parallelism
if(require(leaflet)==FALSE){install.packages("leaflet")} #To Interactive maps
if(require(leaflet.extras)==FALSE){install.packages("leaflet.extras")} #To Interactive maps
if(require(htmltools)==FALSE){install.packages("htmltools")} #To Interactive maps
if(require(sp)==FALSE){install.packages("sp")} #To convert coordinates
if(require(raster==FALSE)){install.packages("raster")} #To convert coordinates
if(require(rgdal==FALSE)){install.packages("rgdal")} #To convert coordinates
if(require(lubridate==FALSE)){install.packages("lubridate")} #To insert Date
if(require(dplyr==FALSE)){install.packages("dplyr")} #To subset dataset
if(require(tidyr==FALSE)){install.packages("tidyr")} #To subset dataset
if(require(ggplot2==FALSE)){install.packages("ggplot2")} #To make plots
if(require(plotly==FALSE)){install.packages("plotly")} #To make plots
if(require(kohonen==FALSE)){install.packages("kohonen")} #To make kohonen neural network
if(require(DT==FALSE)){install.packages("DT")} #To interactive tables
if(require(stats==FALSE)){install.packages("stats")} #To descriptive statistics
if(require(devtools==FALSE)){install.packages("devtools")} #To complement some packages
if(require(VIM==FALSE)){install.packages("VIM")} #To K-NN inputation
#if(require(forecast==FALSE)){install.packages("forecast")} #To forecast time series
#if(require(hydroGOF==FALSE)){install.packages("hydroGOF")} #To display errors
if(require(mice==FALSE)){install.packages("mice")} #To impute by deterministic and stochastic regression method
if(require(Amelia==FALSE)){install.packages("Amelia")} #To impute TS

library(shiny)
library(shinydashboard)
library(shinythemes)
#library(parallel)
#library(doParallel)
#library(doFuture)
#library(foreach)
#library(doSnow)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sp)
library(raster)
library(rgdal)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kohonen)
library(DT)
library(stats)
library(devtools)
library(VIM)
#library(forecast)
#library(hydroGOF)
library(mice)
library(Amelia)
#####Main Panel#####
#######################################################################################->
ui <- fluidPage(
  
  titlePanel("QuanTIC Imputation"),
  
  sidebarLayout(
   
    sidebarPanel(
      
      #####Buttons of Panel#####
      fileInput("file1", "Choose CSV File for Variables",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), #Load dataset
      fileInput("file2", "Choose CSV File for Coordinates",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), #Load coordinates
      helpText("Note: Upload .csv files separated by semicolon and comma as decimal separator"),
      helpText("Note: The .csv file for any variable obtained to weather stations must have column names
               'day', 'month' and, 'year' following by the names of weather stations in the other columns. 
               If you want upload monthly dataset you must fill the column 'day' with values 1. If you want upload yearly dataset 
               you must fill the columns 'day' and 'month' with values 1"),
      helpText("Note: The .csv file for coordinates must be the column names 'STATION', 'LON' and, 'LAT' "),
      tags$hr(), #Separate line
      checkboxInput("header", "Header", TRUE), #Headers
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               Tail = "tail",
                               All = "all"),
                   selected = "head"), 
      numericInput('Temp', 'Temp', 1, min = 1, max = 3),#Filer dataset,
      helpText("In option Temp: 1 for daily dataset, 2 for for monthly dataset, 3 for yearly dataset"),
      tags$hr(), #Separate line
      #To Kohonen map
      helpText("Note: The following options are for performing an unsupervised classification of the data by the Self-organizing map of Kohonen (SOM) method"),
      sliderInput("xdim", "Number of Rows for SOM", min = 1, max = 100, value = 20), 
      sliderInput("ydim", "Number of Columns for SOM", min = 1, max = 100, value = 20),
      sliderInput("iterations", "Number of Iterations for SOM", min = 1, max = 500, value = 100),
      sliderInput("maxrate", "Start Learning Rate", min = 0.05, max = 1, value = 0.05),
      sliderInput("minrate", "End Learning Rate", min = 0.001, max = 0.05, value = 0.01),
      tags$hr(), #Separate line
      #To Kmeans cluster
      helpText("Note: The following options are to perform a data classification by the K-Means method"),
      numericInput('xcol', 'X Variable for KMeans',1,min = 1, max = 100),
      numericInput('ycol', 'Y Variable for KMeans',1, min = 1, max = 100),
      numericInput('clusters', 'Cluster count', 2, min = 1, max = 12),
      tags$hr(), #Separate line
      helpText("Note: To estimate the Euclidean distance between the weather stations you must enter the utm zone corresponding to your region"),
      #To estimate the euclidean distance
      numericInput('Zone', 'UTM Zone', 18, min = 0, max = 60),
      tags$hr(), #Separate line
      #To estimate IDW
      helpText("To fill in the missing data by the IDW method you must enter its power. Keep in mind that it may take several minutes"),
      numericInput('pwr', 'IDW_Power', 2, min = 1, max = 5),
      #To estimate IDW
      helpText("To fill in the missing data by the K-NN method, you must enter the number of K-Neighbors. Keep in mind that it may take several minutes."),
      numericInput('kValue', 'K_Value', 34, min = 1, max = 50),
      tags$hr(),
      helpText("NOTE: To imputate the missing data by IDW (IDW_imp), K-NN (KNN_imp), 
               linear regression (DLR_ipm), stochastic linear regression (SLR_imp) and sectional-cross time series (TS_imp) 
               you must create a directory named (without -) -Resultados-. Inside the directory called Resultados, you must create the directories named (without -) -IDW-, 
               -KNN-, -DLR-, -SLR- and -TS-. In each these directories you can see the graphics, the filled data, and the error generate 
               after finished the imputation process")
   ),
   
   #####TabPanel#####
   #######################################################################################->
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset",DT::dataTableOutput("dataset")),
        tabPanel("Coordinates",DT::dataTableOutput("coordinates")),
        tabPanel("Map", leaflet::leafletOutput("myMap")),
        tabPanel("Summary", DT::dataTableOutput("summary")),
        tabPanel("Boxplot 1", 
                 plotOutput("boxplot1.1"),
                 DT::dataTableOutput("boxplot1.2")),
        tabPanel("Missing 1", plotOutput("missing1")),
        tabPanel("Boxplot 2", 
                 plotOutput("boxplot2.1"),
                 DT::dataTableOutput("boxplot2.2")),
        tabPanel("Missing 2", plotOutput("missing2")),
        tabPanel("SOM", plotOutput("kohonen")),
        tabPanel("PCA", plotOutput("pca"),
                 DT::dataTableOutput("PCATable")),
        tabPanel("KMeans", plotOutput("kmeans")),
        tabPanel("Regional",plotly::plotlyOutput("tsregional")),
        tabPanel("Euclidean_Dist.", DT::dataTableOutput("euclideanD")),
        tabPanel("IDW_Imp.",DT::dataTableOutput("IDW")),
        tabPanel("KNN_Imp", DT::dataTableOutput("KNN")),
        tabPanel("DLR_Imp", DT::dataTableOutput("DLR")),
        tabPanel("SLR_Imp", DT::dataTableOutput("SLR")),
        tabPanel("TS_Imp", DT::dataTableOutput("TS"))
      )
    )
   #######################################################################################->    
  )
)

