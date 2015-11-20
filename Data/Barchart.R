
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#df

#summary(df)
#head(df)

df2 <- df %>% mutate(AVG_DIFFERENCE = (AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS), AVG_DIFF = mean(AVG_DIFFERENCE)) %>% group_by(DRGDEFINITION, AVERAGETOTALPAYMENTS, AVERAGEMEDICAREPAYMENTS, AVG_DIFFERENCE) %>% summarize(AVG_DIFFER = mean(AVG_DIFFERENCE)) 

df3 <- df2 %>% ungroup %>% group_by(DRGDEFINITION) %>% summarise(AVG_DIFF = mean(AVG_DIFFER))

df4 <- inner_join(df2, df3, by="DRGDEFINITION")

ggplot() + 
  #coord_cartesian() + 
  scale_x_discrete() +
  #scale_x_continuous() +
  scale_y_continuous() +
  facet_wrap(~DRGDEFINITION, ncol=1) +
  labs(title='Medical Data \n Procedure Cost Comparison ') +
  labs(x=paste("Average Price"), y=paste("Measure Names")) +
  layer(data=df4, 
        mapping=aes(x=paste("AVERAGETOTALPAYMENTS"), y=AVERAGETOTALPAYMENTS), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="red"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=df4, 
        mapping=aes(x=paste("AVERAGEMEDICAREPAYMENTS"), y=AVERAGEMEDICAREPAYMENTS), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) +
  layer(data=df4, 
      mapping=aes(x=paste("AVG_DIFF"), y=AVG_DIFF), 
      stat="identity", 
      stat_params=list(), 
      geom="bar",
      geom_params=list(colour="green"), 
      position=position_identity()
  ) +
  layer(data=df4, 
        mapping=aes(x=paste("AVG_DIFF"), y=AVG_DIFF, label=round(AVG_DIFF)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-0.5), 
        position=position_identity()
  ) 

# The graph I like stops here


