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

# Begin code for First Tab, CrossTab:
          
      KPI_Low_Max_value <- reactive({input$KPI1})     
      KPI_Medium_Max_value <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
    
      df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
            case
            when kpi < "p1" then \\\'03 Low\\\'
            when kpi < "p2" then \\\'02 Medium\\\'
            else \\\'01 High\\\'
            end kpi
            from (select color, clarity, 
            sum(price) as sum_price, sum(carat) as sum_carat, 
            sum(price) / sum(carat) as kpi
            from diamonds
            group by color, clarity)
            order by clarity;"
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                 MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
      })

      output$distPlot1 <- renderPlot({             
            plot <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_discrete() +
                  scale_y_discrete() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("COLOR"), y=paste("CLARITY")) +
                  layer(data=df1(), 
                        mapping=aes(x=COLOR, y=CLARITY, label=SUM_PRICE), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="text",
                        geom_params=list(colour="black"), 
                        position=position_identity()
                  ) +
                  layer(data=df1(), 
                        mapping=aes(x=COLOR, y=CLARITY, fill=KPI), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="tile",
                        geom_params=list(alpha=rv$alpha), 
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
      
      output$distPlot2 <- renderPlot(height=100, width=200, {
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
      
      output$distPlot3 <- renderPlot(height=100, width=200, {
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
