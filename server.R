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

#####Principal Function#####
#######################################################################################->
server <- function(input, output) { #This function content the variables called input and output 
  
  #####1 Step#####
  #####Upload dataset with a variables of interest#####
  #####################################################################################->
  output$dataset <- DT::renderDataTable({ #Dataset that content the variables as output
    req(input$file1) #Uploaded dataset
    tryCatch(
      {
        df1 <- read.csv2(input$file1$datapath, #The dataset is called df1
                       header = input$header,
                       sep = ";",
                       quote = "") #CSV dataset separated by semicolon 
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df1)) #Shows the first 10 rows
    }
    else if(input$disp == "tail"){
      return(tail(df1)) #Shows the last 10 rows
    }
    else {
      return(df1)
    }
  })
  
  #####2 Step#####
  #####Upload dataset with weather station coordinates#####
  #####################################################################################->
  output$coordinates <- DT::renderDataTable({ #Dataset that content the variables as output
    req(input$file2) #Uploaded coordinates
        df2 <- read.csv(input$file2$datapath, #The dataset is called df1
                       header = input$header,
                       sep = ";",
                       quote = "") #CSV dataset separated by semicolon 

    if(input$disp == "head") {
      return(head(df2)) #Shows the first 10 rows
    }
        else if(input$disp == "tail"){
          return(tail(df2)) #Shows the last 10 rows
        }
    else {
      return(df2)
    }
  })
  
  #####3 Step#####
  #####Interactive map that project the spatial position of weather stations#####
  #####################################################################################->
  my_table <- reactive({
    inFile <- input$file2 #Call the coordinates uploaded in the second step #The object is called inFile
    if (is.null(inFile))
      return(NULL)
    myData = read.csv2(inFile$datapath) #Rename the path of inFile as myData
    df0 = data.frame(myData) #Rename as df0
    df = unique(df0) #Take account the factors of this dataframe
    print(df)
    return(myData)
  })
  
  output$myMap = renderLeaflet({ #Output the object myMap
    if(is.null(my_table())) #If no content data
    {
      return(leaflet()) %>% addProviderTiles(providers$Stamen.Toner)%>%addDrawToolbar() 
    }
    else #The other case
    {
      leaflet(data = my_table()) %>% addProviderTiles(providers$Stamen.Toner) %>% addMarkers(~LON, ~LAT, label = ~STATION) %>%addDrawToolbar()  
    }
  })

  #####4 Step#####
  #####Summary of dataset#####
  #####################################################################################->
  DataSetInput <- reactive({
    inFile2 <- input$file1 #Call the data uploaded in the first step #The object is called inFile2
    if (is.null(inFile2))
      return(NULL)
    myDataSetInput <- read.csv2(inFile2$datapath) #Rename the path of inFile2 as myDataSetInput
    dfDataSetInput = data.frame(myDataSetInput)
    dfDataSetInput= unique(dfDataSetInput)
    return(myDataSetInput)})
  
  output$summary <- DT::renderDataTable({ #Output the summary (Descriptive statistic)
    SUMMARY <- DataSetInput()
    SUMMARY <- SUMMARY[,-c(1:3)] #Extract the columns 1,2 and 3
    SUMMARY <- as.data.frame(summary(SUMMARY))
    colnames(SUMMARY) <- c("Id", "Station", "Summary") #Rename the column names
    print(SUMMARY)
  })
  
  #####5 Step#####
  #####Boxplot by weather stations#####
  #####################################################################################->  
  output$boxplot1.1 <- renderPlot({#To show the distribution of the dataset
    BOXPLOT1 <- DataSetInput() #Create the object BOXPLOT 1 from the function DataSetInput created in the fourth step 
    BOXPLOT1 <- BOXPLOT1[,-c(1:3)] #Extract the columns 1,2 and 3
    n  <- ncol(BOXPLOT1) #Crated the object called n from the number of columns of BOXPLOT1
    par(mar=c(12,3,1,1)) #Dimensions of boxplot
    BOXPLOT1.1 <-boxplot(BOXPLOT1, las=2) #To graphic boxplots
    BOXPLOT1.1
    output$boxplot1.2 <- DT::renderDataTable({
      BOXPLOT1.2 <-boxplot(BOXPLOT1, las=2) #To show the data of the last boxplot
      stats <- as.data.frame(BOXPLOT1.2$stats) #To show the data of the last boxplot
      rownames(stats) <- c("Min", "Q1", "Median", "Q3", "Max") #Rename the row names
      colnames(stats) <- paste(BOXPLOT1.2$names) #Rename the column names
      stats <- t(stats) #Transposed the table
      print(stats)
    })
  })
  
  #####6 Step#####
  #####Barplot by weather stations with outliers#####
  #####################################################################################->
  output$missing1 <- renderPlot({#To show the number of missing values
    BOXPLOT1 <- DataSetInput() #Create the object BOXPLOT 1 from the function DataSetInput created in the fourth step 
    n <- (nrow(BOXPLOT1)-(boxplot(BOXPLOT1[,-c(1:3)], las=2))$n) #Extract the number of values considered for each boxplot
    par(mar=c(12,5,2,1)) #Dimensions of barplot
    bar.1 <- barplot(n, main=paste(nrow(BOXPLOT1),"Values"), ylab="Missing Values", names.arg=paste(colnames(BOXPLOT1[,-c(1:3)])), las=2, col="red") #Graphic the barplot
    #text(bar.1, n*0.75 , paste("     ", n ,sep="") ,cex=0.7, srt=90) #To show the percentage
  })
  
  #####7 Step#####
  #####Boxplot by weather stations without outliers#####
  #####################################################################################->
  remove_outliers <- function(x, na.rm = TRUE, ...) {#To show a new boxplot with less outliers
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...) #To consider the data between 25% and 75% quantiles
    H <- 1.5 * IQR(qnt, na.rm = na.rm) #Range among quantiles
    y <- x
    y[x < (qnt[1] - H)] <- NA #Delete the outliers below of the quantile1 25%
    y[x > (qnt[2] + H)] <- NA #Delete the outliers ubove of the quantile3 75%
    y
  }

  output$boxplot2.1 <-renderPlot({#To show the distribution of the dataset with less outliers
    BOXPLOT2 <- DataSetInput()
    BOXPLOT2 <- remove_outliers(BOXPLOT2[, -c(1:3)])
    par(mar=c(12,3,1,1))
    BOXPLOT2.1 <- boxplot(BOXPLOT2, las=2)
    output$boxplot2.2 <- DT::renderDataTable({
      stats <- as.data.frame(BOXPLOT2.1$stats)
      rownames(stats) <- c("Min", "Q1", "Median", "Q3", "Max")
      colnames(stats) <- paste(BOXPLOT2.1$names)
      stats <- t(stats)
      print(stats)
    })
  })
  
  #####8 Step#####
  #####Barplot by weather stations with outliers#####
  #####################################################################################->
  output$missing2 <- renderPlot({#To show the number of missing values after reduced the outliers
    BOXPLOT2 <- DataSetInput()
    BOXPLOT2 <- remove_outliers(BOXPLOT2[, -c(1:3)])
    n <- (nrow(BOXPLOT2)-(boxplot(BOXPLOT2))$n)
    par(mar=c(12,5,2,1))
    bar.2 <- barplot(n, main=paste(nrow(BOXPLOT2),"Values"), ylab="Missing Values", names.arg=paste(colnames(BOXPLOT2)), las=2, col="red")
    #text(bar.2, n*0.75 , paste("     ", n ,sep="") ,cex=0.7, srt=90) 
  })
  
  #####8 Step#####
  #####Kohonen map#####
  #####################################################################################->
  output$kohonen <- renderPlot({ #Show the Self-Organizing Map (Kohonen Map) that is an unsupervised neural network
    KOHONEN <- DataSetInput() #Rename the data as KOHONEN from the function created in the fouth step
    KOHONEN <- KOHONEN[, -c(1:3)] #Extract the columns 1,2, and 3 that correspond to day, month and year data
    #KOHONEN[!is.na(KOHONEN)] <- 1
    KOHONEN[is.na(KOHONEN)] <- -1 #Replace the missing values with -1
    KOHONEN <- scale(KOHONEN) #Convert each vector of the matrix from subtraction the vector and the division the mean and standard deviation #(x - mean(x)) / sd(x)
    KOHONEN.measures1 <- colnames(KOHONEN) #Create vector from the column names of KOHONEN
    KOHONEN.grid <- somgrid(xdim = input$xdim, ydim=input$ydim, topo="hexagonal") #Give the topology of grids of Kohonen Map
    KOHONEN.som <- som(KOHONEN, grid=KOHONEN.grid, rlen=input$iterations, alpha=c(input$maxrate,input$minrate)) #Model the Kohonen #Include interactive iterations and interactive learn rates
    par(mfrow=c(1,2)) #Divide the two plots in two columns
    plot(KOHONEN.som, type="changes") #Plot the learn rate
    plot(KOHONEN.som, type="dist.neighbours") #Plot the Kohonen Map
  })
  
  #####9 Step#####
  #####Principal component analysis by weather stations#####
  #####################################################################################->
  output$pca <- renderPlot({#To create the principal component analysis
    PCA <- DataSetInput()  #Rename the data as PCA from the function created in the fouth step
    PCA <- PCA[, -c(1:3)] #Extract the columns 1,2, and 3 that correspond to day, month and year data
    #PCA[!is.na(PCA)] <- 1
    PCA[is.na(PCA)] <- -1 #Replace the missing values with -1
    PCA.pca <-prcomp(PCA,center = TRUE, scale. = TRUE)
    print(PCA.pca)
    summary(PCA.pca)
    predict(PCA.pca, 
            newdata=tail(PCA, 2))
    plot(PCA.pca, type = "l", main="Principal Component Analysis", lwd=3, col="red")
    output$PCATable <- DT::renderDataTable({
      PCA.table <- cbind(as.data.frame(PCA.pca$rotation), as.data.frame(PCA.pca$sdev), as.data.frame(PCA.pca$center))
      n <- ncol(PCA.table)
      colnames(PCA.table)[n-1] <- c("StDev")
      colnames(PCA.table)[n] <- c("Center")
      print(PCA.table)
    })

  })
  
  #####9 Step#####
  #####KMeans cluster by weather stations#####
  #####################################################################################->
  output$kmeans<- renderPlot({
    KMEANS <-DataSetInput()
    KMEANS <- KMEANS[,-c(1:3)]
    #KMEANS[!is.na(KMEANS)] <- 1
    KMEANS[is.na(KMEANS)] <- -1
    KMEANS <- KMEANS[, c(input$xcol, input$ycol)]
    CLUSTER <- kmeans(KMEANS, input$clusters)
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    plot(KMEANS, col=CLUSTER$cluster, pch = 20, cex = 3)
    points(CLUSTER$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  #####Plot in time with the mean values of the dataset#####
  #####################################################################################->
  output$tsregional <- renderPlotly({
    REGIONAL <- DataSetInput()
    TEMP = input$Temp
    TemP = c("day","month","year")
    BEGIN.DATES <- paste(REGIONAL[1,3], REGIONAL[1,2], REGIONAL[1,1], sep="/")
    END.DATES <- paste(REGIONAL[nrow(REGIONAL),3],REGIONAL[nrow(REGIONAL),2],REGIONAL[nrow(REGIONAL),1], sep = "/")
    DATES <- seq(as.Date(paste(BEGIN.DATES)), as.Date(paste(END.DATES)), by=paste(TemP[TEMP]))
    DATES <- as.data.frame(DATES)
    REGIONAL <- REGIONAL[, -c(1:3)]
    MEANS <- apply(REGIONAL,1,mean, na.rm=TRUE)
    MEANS <- cbind(DATES,MEANS)
    colnames(MEANS)<- c("Date", "Mean")
    plot_ly(MEANS, x = ~Date, y = ~Mean)
    
  })
  
  #####Euclidean distance between weather stations#####
  #####################################################################################-> 
  output$euclideanD <- DT::renderDataTable({
    COORDINATES <- my_table()
    coordinates(COORDINATES) <- ~ LON + LAT
    proj4string(COORDINATES) <- CRS("+proj=longlat +datum=WGS84")
    Zone <- input$Zone
    COORDINATES <- spTransform(COORDINATES, CRS(paste("+proj=utm", " +zone=", Zone, " +ellps=WGS84", " +datum=WGS84", " +units=m", sep="")))
    #COORDINATES <- spTransform(COORDINATES, CRS("+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    #COORDINATES <- spTransform(COORDINATES, CRS("+init=epsg:3116")) #Magna sirgas origen bogota
    COORDINATES <- as.data.frame(COORDINATES)
    X <- COORDINATES$LON
    Y <- COORDINATES$LAT
    MX1 <- X %*% t(X)
    MX2 <- matrix(rep(t(X),nrow(COORDINATES)), ncol = nrow(COORDINATES), nrow = nrow(COORDINATES))
    MX <- MX1/MX2
    MX <- abs((MX-MX2)**2)
    colnames(MX)<- paste(COORDINATES$STATION)
    rownames(MX)<- paste(COORDINATES$STATION)
    MY1 <- Y %*% t(Y)
    MY2 <- matrix(rep(t(Y), nrow(COORDINATES)), ncol = nrow(COORDINATES), nrow = nrow(COORDINATES))
    MY <- MY1/MY2
    MY <- abs((MY-MY2)**2)
    colnames(MY)<- paste(COORDINATES$STATION)
    rownames(MY)<- paste(COORDINATES$STATION)
    EUCLIDEAND <- round((sqrt(MX+MY)/1000), digits = 0)
    EUCLIDEAND <- as.data.frame(EUCLIDEAND)
    write.csv2(EUCLIDEAND,file = "Euclidean_Distance.csv")
    print(EUCLIDEAND)
  })
    
  #####IDW Interpolation#####
  #####################################################################################->    
  output$IDW <- DT::renderDataTable({

      DS <- DataSetInput() ###############DATASET INPUT
      date <- DS[1:3] ###############EXTRACT DATES IN A NEW OBJECT
      TEMP = input$Temp
      TemP = c("day","month","year")
      BEGGIN <-paste(DS[1,3],DS[1,2],DS[1,1], sep = "/")
      END<-paste(DS[nrow(DS),3], DS[nrow(DS),2], DS[nrow(DS),1], sep="/")
      DATE<- seq(as.Date(paste(BEGGIN)), as.Date(paste(END)), by=paste(TemP[TEMP])) ###############CREATE DATES AS FORMAT .DATE
      DS <- DS[,-c(1:3)] ###############DELETE DATES
      DS <- as.matrix(DS)
      
      IDW <- matrix(nrow = nrow(DS), ncol = ncol(DS)) #############EMPTY IDW MATRIX
      m <- ncol(IDW)
      n <- nrow(IDW)
      power <- input$pwr ###############THE POWER OF THE IDW MODEL
      
      COORDINATES <- my_table() ###############DATASET OF COORDINATES
      coordinates(COORDINATES) <- ~ LON + LAT
      proj4string(COORDINATES) <- CRS("+proj=longlat +datum=WGS84")
      Zone <- input$Zone
      COORDINATES <- spTransform(COORDINATES, CRS(paste("+proj=utm", " +zone=", Zone, " +ellps=WGS84", " +datum=WGS84", " +units=m", sep="")))
      COORDINATES <- as.data.frame(COORDINATES)
      X <- COORDINATES$LON
      Y <- COORDINATES$LAT
      MX1 <- X %*% t(X)
      MX2 <- matrix(rep(t(X),nrow(COORDINATES)), ncol = nrow(COORDINATES), nrow = nrow(COORDINATES))
      MX <- MX1/MX2
      MX <- abs((MX-MX2)**2)
      colnames(MX)<- paste(COORDINATES$STATION)
      rownames(MX)<- paste(COORDINATES$STATION)
      MY1 <- Y %*% t(Y)
      MY2 <- matrix(rep(t(Y), nrow(COORDINATES)), ncol = nrow(COORDINATES), nrow = nrow(COORDINATES))
      MY <- MY1/MY2
      MY <- abs((MY-MY2)**2)
      colnames(MY)<- paste(COORDINATES$STATION)
      rownames(MY)<- paste(COORDINATES$STATION)
      EUCLIDEAND <- round((sqrt(MX+MY)/1000), digits = 0)
      EUCLIDEAND <- as.data.frame(EUCLIDEAND)
      DISTANCE <- as.matrix(EUCLIDEAND)
      DISTANCE <- 1/DISTANCE
      DISTANCE[! is.finite(DISTANCE)]<- 0
      DISTANCE <- DISTANCE**power
      
      for (i in 1:n) {
        for(j in 1:m){
          IDW[i,j] <- sum((DS[i,]*(DISTANCE[,j])), na.rm = TRUE)/sum((DISTANCE[,j]), na.rm = TRUE)
        }
      } ###############RUN IDW MODEL
      
      IDW <- as.data.frame(round(IDW,1)) #ROUND THE DECIMALS
      IDW2 <- as.data.frame(IDW) #NEW DATAFRAME
      DS <- as.data.frame(DS)
      colnames(IDW)<- colnames(DS) #ADD NAMES TO IDW OBJECT
      colnames(IDW2)<- colnames(DS) #ADD NAMES TO IDW2 OBJECT
      #MERGE THE OBSERVE VALUES WITH IMPUTE VALUES
      IDW2[!is.na(DS)] <- DS[!is.na(DS)] 
      ERROR <- (IDW2-IDW)**2
      ERROR <- apply(ERROR, 2, sum)
      ERROR <- ERROR/nrow(IDW) 
      ERROR <- mean(ERROR[1]) #SQUARE MEAN ERROR
      DS <- cbind(date, DS) #ADD DATE
      IDW <- cbind(date, IDW) #ADD DATE
      IDW2 <- data.frame(date, IDW2) #ADD DATE
      write.csv2(IDW2, file = "Resultados/IDW/IDW_imputation.csv") #SAVE IMPUTATION
      write.csv2(ERROR, file = "Resultados/IDW/Error_IDW.csv") #SAVE ERROR
      
      for(j in 4:ncol(IDW2)) {
        ruta <- file.path("Resultados","IDW", paste("IDW_",colnames(IDW2)[j], ".jpeg", sep = ""))
        jpeg(ruta, width=15, height=15, units="in", res=600)
        plot(DATE, IDW2[[j]], main = paste(colnames(IDW2)[j]), type = "l",  xlab = "Time", ylab = "Variable", col="red", lty=1, lwd=1.5, las=2)
        lines(DATE, DS[[j]],  lty=2, lwd=1.5, col="black")
        legend("topright", legend=c("Imputed","Not Imputed"), fill=c("red","black"))
        dev.off()
      }
      #datatable(
      #  IDW2, extensions = 'Buttons', options = list(
      #    dom = 'Bfrtip',
      #    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      #  )
      #)
      print(IDW2)
    })
  #####KNN Interpolation#####
  #####################################################################################->
  output$KNN <- DT::renderDataTable({
    DS <- DataSetInput()###############DATASET INPUT
   
    date <- DS[1:3] ###############EXTRACT DATES IN A NEW OBJECT
    TEMP = input$Temp
    TemP = c("day","month","year")
    BEGGIN <-paste(DS[1,3],DS[1,2],DS[1,1], sep = "/")
    END<-paste(DS[nrow(DS),3], DS[nrow(DS),2], DS[nrow(DS),1], sep="/")
    DATE<- seq(as.Date(paste(BEGGIN)), as.Date(paste(END)), by=paste(TemP[TEMP])) ###############CREATE DATES AS FORMAT .DATE
    
    DS <- DS[,-c(1:3)]
    DS <- as.data.frame(DS)

    K <- input$kValue #NUMBER OF K VALUES

    NCC = boxplot(DS) 
    NCC = NCC$n #NUMBER OF COMPLETE CASES
    NNCC = nrow(DS) - NCC #NUMBER OF NOT COMPLETE CASES

    PNNCC = abs((NNCC/(NNCC+NCC))*100) #PERCENT OF NUMBER NOT COMPLETE CASES

    FILTER = DS[,which(PNNCC <= 10)] #FILTER THE VARIABLE OF THE DATASET THAT HAVE EQUAL OR LESS OF 10% OF MISSING VALUES

    CALIBRATION = FILTER #TO CALIBRATE MODEL
    CALIBRATION[sample(1:nrow(CALIBRATION), round(nrow(CALIBRATION)*0.4), replace= TRUE), sample(1:ncol(CALIBRATION), round(ncol(CALIBRATION)*0.5), replace= FALSE) ] <- NA #Replace the 40% of dataset with NAs
    CALIBRATION = as.data.frame(CALIBRATION)

    VALIDATION = FILTER #TO VALIDATE MODEL
    VALIDATION = as.data.frame(VALIDATION)

    CALIBRATION = kNN(CALIBRATION,  variable = colnames(CALIBRATION),  metric = NULL, k = K, dist_var = colnames(CALIBRATION), weights = NULL) #RUN KNN

    CALIBRATION = CALIBRATION[, -c(((ncol(CALIBRATION)/2)+1):ncol(CALIBRATION))]

    ERROR = (VALIDATION-CALIBRATION)**2
    ERROR = colSums(ERROR, na.rm=TRUE)
    ERROR = ERROR/nrow(CALIBRATION)
    ERROR = round(ERROR,2)
    ERROR = as.data.frame(ERROR)
    ERROR = mean(ERROR$ERROR)
    write.csv2(ERROR, file = "Resultados/KNN/Error_KNN.csv") #SAVE ERROR
    
    KNN <- kNN(DS,  k = K) #RUN KNN WITH ALL DATASET
    KNN <- KNN[,-c(((ncol(KNN)/2)+1):ncol(KNN))]
    KNN <- as.matrix(KNN)
    KNN <- replace(KNN, which(KNN <0), 0)
    KNN <- as.data.frame(KNN)
    KNN <- round(KNN,1)
    DATE <- as.data.frame(DATE)
    date <- as.data.frame(date)
    KNN <- cbind(date, KNN)
    
    DS <- cbind(date, DS)

    write.csv2(KNN, file = "Resultados/KNN/knn_imputation.csv") #SAVE KNN IMPUTATION
    
    for(j in 4:ncol(KNN)) {
        ruta <- file.path("Resultados","KNN", paste("KNN_",colnames(KNN)[j], ".jpeg", sep = ""))
        jpeg(ruta, width=15, height=15, units="in", res=600)
        plot(KNN[[j]], main = paste(colnames(KNN)[j]), type = "l",  xlab = "Time", ylab = "Variable", col="red", lty=1, lwd=1.5, las=2)
        lines(DS[[j]],  lty=2, lwd=1.5, col="black")
        legend("topright", legend=c("Imputed","Not Imputed"), fill=c("red","black"))
        dev.off()
    }
    #datatable(
    #  KNN, extensions = 'Buttons', options = list(
    #    dom = 'Bfrtip',
    #    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #  )
    #)
    print(KNN)

  })

  #####Deterministic Linear Regression#####
  #####################################################################################->
  output$DLR <- DT::renderDataTable({
    DS <- DataSetInput() ###############DATASET INPUT
    date <- DS[1:3] ###############EXTRACT DATES IN A NEW OBJECT
    TEMP = input$Temp
    TemP = c("day","month","year")
    BEGGIN <-paste(DS[1,3],DS[1,2],DS[1,1], sep = "/")
    END<-paste(DS[nrow(DS),3], DS[nrow(DS),2], DS[nrow(DS),1], sep="/")
    DATE<- seq(as.Date(paste(BEGGIN)), as.Date(paste(END)), by=paste(TemP[TEMP])) ###############CREATE DATES AS FORMAT .DATE
    
    DS <- DS[,-c(1:3)]
    DS <- as.data.frame(DS)

    NCC = boxplot(DS) #FROM THIS BOXPLOT ITS EXTRACTS THE NUMBER OF VALUES FOR EACH VARIABLE
    NCC = NCC$n #Number Complete Cases
    NNCC = nrow(DS) - NCC #Number Not Complete Cases
    PNNCC = abs((NNCC/(NNCC+NCC))*100) #Percent of Number Not Complete Cases

    FILTER = DS[,which(PNNCC <= 10)] #Filter the variables of the dataset that have equal or less of 10% of missing values

    CALIBRATION = FILTER #To calibrate dataset
    CALIBRATION[sample(1:nrow(CALIBRATION), round(nrow(CALIBRATION)*0.4), replace= TRUE), sample(1:ncol(CALIBRATION), round(ncol(CALIBRATION)*0.5), replace= FALSE) ] <- NA #Replace the 40% of dataset with NAs
    CALIBRATION = as.data.frame(CALIBRATION)

    VALIDATION = FILTER #To validate dataset
    VALIDATION = as.data.frame(VALIDATION)

    CALIBRATION = mice(CALIBRATION, method = "norm.predict") #Run DLR with the calibration dataset
    CALIBRATION = mice::complete(CALIBRATION)

    ERROR = (VALIDATION-CALIBRATION)**2
    ERROR = colSums(ERROR, na.rm=TRUE)
    ERROR = ERROR/nrow(CALIBRATION)
    ERROR = round(ERROR,2)
    ERROR = as.data.frame(ERROR)
    ERROR = mean(ERROR$ERROR)
    write.csv2(ERROR, file = "Resultados/DLR/Error_DLR.csv") 

    DLR <- mice(DS, method = "norm.predict") #Run DLR with the DataSet
    DLR <- mice::complete(DLR)
    DLR <- round(DLR,1)
    DLR <- as.matrix(DLR)
    DLR <- replace(DLR, which(DLR <0), 0)
    DLR <- as.data.frame(DLR)
    DATE <- as.data.frame(DATE)
    date <- as.data.frame(date)
    DLR <- cbind(date, DLR)
    
    DS <- cbind(date, DS)

    write.csv2(DLR, file = "Resultados/DLR/DLR_imputation.csv")
    
    for(j in 4:ncol(DLR)) {
        ruta <- file.path("Resultados","DLR", paste("DLR_",colnames(DLR)[j], ".jpeg", sep = ""))
        jpeg(ruta, width=15, height=15, units="in", res=600)
        plot(DLR[[j]], main = paste(colnames(DLR)[j]), type = "l",  xlab = "Time", ylab = "Variable", col="red", lty=1, lwd=1.5, las=2)
        lines(DS[[j]],  lty=2, lwd=1.5, col="black")
        legend("topright", legend=c("Imputed","Not Imputed"), fill=c("red","black"))
        dev.off()
    }
    #datatable(
    #  DLR, extensions = 'Buttons', options = list(
    #    dom = 'Bfrtip',
    #    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #  )
    #)
    print(DLR)

  })

  #####Stochastic Linear Regression#####
  #####################################################################################->
  output$SLR <- DT::renderDataTable({
    DS <- DataSetInput() #FROM THIS BOXPLOT ITS EXTRACTS THE NUMBER OF VALUES FOR EACH VARIABLE
    date <- DS[1:3]  ###############EXTRACT DATES IN A NEW OBJECT
    TEMP = input$Temp
    TemP = c("day","month","year")
    BEGGIN <-paste(DS[1,3],DS[1,2],DS[1,1], sep = "/")
    END<-paste(DS[nrow(DS),3], DS[nrow(DS),2], DS[nrow(DS),1], sep="/")
    DATE<- seq(as.Date(paste(BEGGIN)), as.Date(paste(END)), by=paste(TemP[TEMP])) ###############CREATE DATES AS FORMAT .DATE
    
    DS <- DS[,-c(1:3)]
    DS <- as.data.frame(DS)

    NCC = boxplot(DS) #FROM THIS BOXPLOT ITS EXTRACTS THE NUMBER OF VALUES FOR EACH VARIABLE
    NCC = NCC$n #Number Complete Cases
    NNCC = nrow(DS) - NCC #Number Not Complete Cases

    PNNCC = abs((NNCC/(NNCC+NCC))*100) #Percent of Number Not Complete Cases

    FILTER = DS[,which(PNNCC <= 10)] #Filter the variables of the dataset that have equal or less of 10% of missing values

    CALIBRATION = FILTER #To calibrate dataset
    CALIBRATION[sample(1:nrow(CALIBRATION), round(nrow(CALIBRATION)*0.4), replace= TRUE), sample(1:ncol(CALIBRATION), round(ncol(CALIBRATION)*0.5), replace= FALSE) ] <- NA #Replace the 40% of dataset with NAs
    CALIBRATION = as.data.frame(CALIBRATION)

    VALIDATION = FILTER #To validate dataset
    VALIDATION = as.data.frame(VALIDATION)

    CALIBRATION = mice(CALIBRATION, method = "norm.nob") #Run SLR with the calibration dataset
    CALIBRATION = mice::complete(CALIBRATION)

    ERROR = (VALIDATION-CALIBRATION)**2
    ERROR = colSums(ERROR, na.rm=TRUE)
    ERROR = ERROR/nrow(CALIBRATION)
    ERROR = round(ERROR,2)
    ERROR = as.data.frame(ERROR)
    ERROR = mean(ERROR$ERROR)
    write.csv2(ERROR, file = "Resultados/SLR/Error_SLR.csv") 

    SLR <- mice(DS, method = "norm.nob") #Run SLR with the DataSet
    SLR <- mice::complete(SLR)
    SLR <- round(SLR,1)
    SLR <- as.matrix(SLR)
    SLR <- replace(SLR, which(SLR <0), 0)
    SLR <- as.data.frame(SLR)
    DATE <- as.data.frame(DATE)
    date <- as.data.frame(date)
    SLR <- cbind(date, SLR)
    
    DS <- cbind(date, DS)

    write.csv2(SLR, file = "Resultados/SLR/SLR_imputation.csv")
    
    for(j in 4:ncol(SLR)) {
        ruta <- file.path("Resultados","SLR", paste("SLR_",colnames(SLR)[j], ".jpeg", sep = ""))
        jpeg(ruta, width=15, height=15, units="in", res=600)
        plot(SLR[[j]], main = paste(colnames(SLR)[j]), type = "l",  xlab = "Time", ylab = "Variable", col="red", lty=1, lwd=1.5, las=2)
        lines(DS[[j]],  lty=2, lwd=1.5, col="black")
        legend("topright", legend=c("Imputed","Not Imputed"), fill=c("red","black"))
        dev.off()
    }
    #datatable(
    #  SLR, extensions = 'Buttons', options = list(
    #    dom = 'Bfrtip',
    #    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #  )
    #)
    print(SLR)

  })

  #####Time Series Cross Sectional Data#####
  #####################################################################################->
  output$TS <- DT::renderDataTable({
    DS <- DataSetInput() #FROM THIS BOXPLOT ITS EXTRACTS THE NUMBER OF VALUES FOR EACH VARIABLE
    date <- DS[1:3] ###############EXTRACT DATES IN A NEW OBJECT
    TEMP = input$Temp
    TemP = c("day","month","year")
    BEGGIN <-paste(DS[1,3],DS[1,2],DS[1,1], sep = "/")
    END<-paste(DS[nrow(DS),3], DS[nrow(DS),2], DS[nrow(DS),1], sep="/")
    DATE<- seq(as.Date(paste(BEGGIN)), as.Date(paste(END)), by=paste(TemP[TEMP])) ###############CREATE DATES AS FORMAT .DATE
    
    DS <- DS[,-c(1:3)]
    DS <- as.data.frame(DS)

    NCC = boxplot(DS) 
    NCC = NCC$n #Number Complete Cases
    NNCC = nrow(DS) - NCC #Number Not Complete Cases

    PNNCC = abs((NNCC/(NNCC+NCC))*100) #Percent of Number Not Complete Cases

    FILTER = DS[,which(PNNCC <= 10)] #Filter the variables of the dataset that have equal or less of 10% of missing values

    CALIBRATION = FILTER #To calibrate dataset
    CALIBRATION[sample(1:nrow(CALIBRATION), round(nrow(CALIBRATION)*0.4), replace= TRUE), sample(1:ncol(CALIBRATION), round(ncol(CALIBRATION)*0.5), replace= FALSE) ] <- NA #Replace the 40% of dataset with NAs
    CALIBRATION = as.data.frame(CALIBRATION)

    VALIDATION = FILTER #To validate dataset
    VALIDATION = as.data.frame(VALIDATION)

    CALIBRATION = Amelia::amelia(CALIBRATION) #Run TS  Cross Sectional Data
    CALIBRATION = CALIBRATION$imputations$imp5

    ERROR = (VALIDATION-CALIBRATION)**2
    ERROR = colSums(ERROR, na.rm=TRUE)
    ERROR = ERROR/nrow(CALIBRATION)
    ERROR = round(ERROR,2)
    ERROR = as.data.frame(ERROR)
    ERROR = mean(ERROR$ERROR)
    write.csv2(ERROR, file = "Resultados/TS/Error_TS.csv")

    TSCS = Amelia::amelia(DS) 
    TSCS = TSCS$imputations$imp5
    TSCS = as.matrix(TSCS)
    TSCS = replace(TSCS, which(TSCS <0), 0)
    TSCS = as.data.frame(TSCS)
    TSCS <- round(TSCS,1)
    DATE <- as.data.frame(DATE)
    date <- as.data.frame(date)
    TSCS <- cbind(date, TSCS)

    DS <- cbind(date, DS)

    write.csv2(TSCS, file = "Resultados/TS/TS_imputation.csv")

    for(j in 4:ncol(TSCS)) {
        ruta <- file.path("Resultados","TS", paste("TSCS_",colnames(TSCS)[j], ".jpeg", sep = ""))
        jpeg(ruta, width=15, height=15, units="in", res=600)
        plot(TSCS[[j]], main = paste(colnames(TSCS)[j]), type = "l",  xlab = "Time", ylab = "Variable", col="red", lty=1, lwd=1.5, las=2)
        lines(DS[[j]],  lty=2, lwd=1.5, col="black")
        legend("topright", legend=c("Imputed","Not Imputed"), fill=c("red","black"))
        dev.off()
    }
    #datatable(
    #  TSCS, extensions = 'Buttons', options = list(
    #    dom = 'Bfrtip',
    #    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #  )
    #)
    print(TSCS)
  })
  
  #####Final Line#####
  #######################################################################################->
}
#######################################################################################->