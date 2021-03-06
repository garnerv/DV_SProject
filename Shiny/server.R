# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  dfct <- eventReactive(c(input$clicks1), 
                        {dfct <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
  })
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  
  
  output$distPlot1 <- renderPlot(height=600, width=800, {
    
    crosstab <- dfct() %>% group_by(PROVIDERSTATE, DRGDEFINITION) %>% summarize(sum_payments = sum(AVERAGETOTALPAYMENTS), sum_charges = sum(AVERAGECOVEREDCHARGES)) %>% mutate(ratio = sum_payments / sum_charges ) %>% mutate(kpi = ifelse(ratio <= KPI_Low_Max_value(), '03 Low', ifelse(ratio <= KPI_Medium_Max_value(), '02 Medium', '01 High')))
    
    
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Medical Data Crosstab: By State in R') +
      labs(x=paste("DRG Medical Disorder"), y=paste("Provider State")) +
      layer(data=crosstab, 
            mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=""), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=round(ratio, 2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, fill=kpi), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.5), 
            position=position_identity()
      )
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  # Begin code for Second Tab, Bar Chart:
  
  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) 
  
  df2 <- df %>% mutate(AVG_DIFFERENCE = (AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS), AVG_DIFF = mean(AVG_DIFFERENCE)) %>% group_by(DRGDEFINITION, AVERAGETOTALPAYMENTS, AVERAGEMEDICAREPAYMENTS, AVG_DIFFERENCE) %>% summarize(AVG_DIFFER = mean(AVG_DIFFERENCE)) 
  
  df3 <- df2 %>% ungroup %>% group_by(DRGDEFINITION) %>% summarise(AVG_DIFF = mean(AVG_DIFFER))
  
  df4 <- eventReactive(input$clicks2, {inner_join(df2, df3, by="DRGDEFINITION") })
  
  output$distPlot2 <- renderPlot(height=600, width=800, {
    plot1 <- ggplot() + 
      #coord_cartesian() + 
      scale_x_discrete() +
      #scale_x_continuous() +
      scale_y_continuous() +
      facet_wrap(~DRGDEFINITION, ncol=1) +
      labs(title='Medical Data \n Procedure Cost Comparison ') +
      labs(x=paste("Average Price"), y=paste("Measure Names")) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVERAGETOTALPAYMENTS"), y=AVERAGETOTALPAYMENTS), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="red"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df4(), 
            mapping=aes(x=paste("AVERAGEMEDICAREPAYMENTS"), y=AVERAGEMEDICAREPAYMENTS), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVG_DIFF"), y=AVG_DIFF), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="green"), 
            position=position_identity()
      ) +
      layer(data=df4(), 
            mapping=aes(x=paste("AVG_DIFF"), y=AVG_DIFF, label=round(AVG_DIFF)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      ) 
    plot1
  })
  
  # Begin code for Third Tab, Scatter Plot:
  dfS <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  dfS1 <- dfS %>% mutate(AVG_DIFFERENCE = AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS, AVG_DIFF = cume_dist(AVG_DIFFERENCE))
  
  dfS2 <- eventReactive(input$clicks3, {dfS1})
  
  output$distPlot3 <- renderPlot(height=600, width=800, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Medical Data \n Percentiles vs Total Discharges') +
      labs(x="Percentile of Average Difference", y=paste("Total Discharges")) +
      layer(data=dfS2(), 
            mapping=aes(x=as.numeric(as.character(AVG_DIFF)), y=as.numeric(as.character(TOTALDISCHARGES))), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    plot3 })
  
})