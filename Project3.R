library(vistime)
library(devtools)
library(googleVis)
library(manipulate)
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(corrplot)
library(corrr)
library(DT)
library(zoo)
library(readr)
library(shinydashboard)
library(shiny)
library(timevis)
library(tidyverse)
library(lubridate)
library(scales)
library(Cairo)
library(devtools)
library(Rcpp)
library(viridis)
library(shiny)
library(ggplot2)
library(gridExtra)

sStore <- read.csv("SampleSuperstore.csv", TRUE, ",", stringsAsFactors=FALSE)
sStore$orderDate<- as.Date(sStore$orderDate, format='%m/%d/%Y')
sStore$shipDate<- as.Date(sStore$shipDate, format='%m/%d/%Y')

sStore$quarter <- as.yearqtr(as.Date( sStore$orderDate, "%m/%d/%Y"))
sStore$quarter
summary(sStore$quantity)


u <- shinyUI(fluidPage(
  titlePanel(h2("Super Store Shipping Trends", align = "center" ,style = "color:orange")),
  sidebarLayout(position = "right",
                sidebarPanel(h3("Filters", align = "left" ,style = "color:white"),width = 3, 
                             checkboxInput("donum1", "Enable First Graph", value = T),
                             checkboxInput("donum2", "Enable Second Graph", value = T),
                             sliderInput("wt1","Year (2016,2019)",min=1,max=4,value=3),
                             sliderInput("wt2","Quarter(1,4)",min=1,max=4,value=2),
            
                             checkboxGroupInput("shipMode", label = h3("Ship Mode"), 
                                                choices = list("First Class" = 1, "Same Day" = 2, "Second Class" = 3, "Standard Class" = 4),
                                                selected = c(1,2)),
                             checkboxGroupInput("region", label = h3("Region"), 
                                                choices = list("Central" = 1, "East" = 2, "South" = 3, "West" = 4),
                                                selected = c(3,4)),style = "color:orange;background:black"
                ),
                
                mainPanel(h3("Display Graph", align = "left" ,style = "color:white"),
                          fluidRow(column(10,plotOutput(outputId="plotgraph", width="900px",height="400px", ,hover = hoverOpts(id ="plot_hover")))),
                          fluidRow(column(10,plotOutput(outputId="plotgraphmiddle", width="900px",height="400px"))),
                )),style = "background:grey"))

s <- shinyServer(function(input, output) 
{
  #G1
  pt1 <- reactive({
    
    
    if (!input$donum1) return(NULL)
    if (input$wt1 == 1) thisYear <- subset(sStore, orderDate > as.Date("2016-01-01") & orderDate < as.Date("2016-12-30"))
    if (input$wt1 == 2) thisYear <- subset(sStore, orderDate > as.Date("2017-01-01") & orderDate < as.Date("2017-12-30"))
    if (input$wt1 == 3) thisYear <- subset(sStore, orderDate > as.Date("2018-01-01") & orderDate < as.Date("2018-12-30"))
    if (input$wt1 == 4) thisYear <- subset(sStore, orderDate > as.Date("2019-01-01") & orderDate < as.Date("2019-12-30"))

    firstClass      <- 1       %in% input$shipMode
    sameDay      <- 2       %in% input$shipMode
    secondClass      <- 3       %in% input$shipMode
    standardClass      <- 4       %in% input$shipMode
    if(!firstClass)  thisYear <- subset(thisYear, shipMode !="First Class")
    if(!sameDay)  thisYear <- subset(thisYear, shipMode !="Same Day")
    if(!secondClass)  thisYear <- subset(thisYear, shipMode !="Second Class")
    if(!standardClass)  thisYear <- subset(thisYear, shipMode !="Standard Class")
    
    quarterList<-thisYear$quarter
    quarterList <- unique(thisYear$quarter)
    quarterList <- sort(quarterList)
    Central      <- 1       %in% input$region
    East      <- 2       %in% input$region
    South      <- 3       %in% input$region
    West      <- 4       %in% input$region
   
     if(!Central)  thisYear <- subset(thisYear, region !="Central")
    if(!East)  thisYear <- subset(thisYear, region !="East")
    if(!South)  thisYear <- subset(thisYear, region !="South")
    if(!West)  thisYear <- subset(thisYear, region !="West")

    thisYear <- thisYear %>% filter( (quarter == as.yearqtr(quarterList[input$wt2] ) ))
    percentData <- thisYear %>% group_by(quarter) %>% count(shipStatus) %>%
      mutate(ratio=scales::percent(n/sum(n)))
    percentData
    p <- ggplot(thisYear,aes(x=factor(quarter),fill=factor(shipStatus)))+
      geom_bar(position="fill")+
      geom_text(data=percentData, aes(y=n,label=ratio),
                position=position_fill(vjust=0.5)) +
      coord_flip()
  })

  #G2
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    if (input$wt1 == 1) thisYear2 <- subset(sStore, orderDate > as.Date("2016-01-01") & orderDate < as.Date("2016-12-30"))
    if (input$wt1 == 2) thisYear2 <- subset(sStore, orderDate > as.Date("2017-01-01") & orderDate < as.Date("2017-12-30"))
    if (input$wt1 == 3) thisYear2 <- subset(sStore, orderDate > as.Date("2018-01-01") & orderDate < as.Date("2018-12-30"))
    if (input$wt1 == 4) thisYear2 <- subset(sStore, orderDate > as.Date("2019-01-01") & orderDate < as.Date("2019-12-30"))
    
    firstClass      <- 1       %in% input$shipMode
    sameDay      <- 2       %in% input$shipMode
    secondClass      <- 3       %in% input$shipMode
    standardClass      <- 4       %in% input$shipMode
    if(!firstClass)  thisYear2 <- subset(thisYear2, shipMode !="First Class")
    if(!sameDay)  thisYear2 <- subset(thisYear2, shipMode !="Same Day")
    if(!secondClass)  thisYear2 <- subset(thisYear2, shipMode !="Second Class")
    if(!standardClass)  thisYear2 <- subset(thisYear2, shipMode !="Standard Class")
    
    quarterList<-thisYear2$quarter
    quarterList <- unique(thisYear2$quarter)
    quarterList <- sort(quarterList)
   
    Central      <- 1       %in% input$region
    East      <- 2       %in% input$region
    South      <- 3       %in% input$region
    West      <- 4       %in% input$region
    if(!Central)  thisYear2 <- subset(thisYear2, region !="Central")
    if(!East)  thisYear2 <- subset(thisYear2, region !="East")
    if(!South)  thisYear2 <- subset(thisYear2, region !="South")
    if(!West)  thisYear2 <- subset(thisYear2, region !="West")
    
    qtData <- thisYear2 %>% filter( (quarter == as.yearqtr(quarterList[input$wt2] ) ))
    stacks2 <-qtData %>% group_by(orderDate, shipStatus) %>%
      summarise(quantity = sum(numberOfRecords))
    
    ggplot(stacks2, aes(x = orderDate,
                        y = quantity, 
                        fill = shipStatus)) +
      geom_area() +
      labs(title = "",
           subtitle = "",
           caption = "",
           x = "Weekly Order",
           y = "No. Shipments",
           fill = "Ship Status") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(pt1())
    wtlist <- c(input$wt1,input$wt2)
    to_delete <- !sapply(ptlist,is.null)
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=1,nrow=length(ptlist))
  })

  output$plotgraphmiddle = renderPlot({
    ptlist <- list(pt2())
    wtlist <- c(input$wt1,input$wt2)
    
    to_delete <- !sapply(ptlist,is.null)
    
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=1,nrow=length(ptlist))
  })
  
  
})
shinyApp(u,s)
