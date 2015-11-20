df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

KPI_Low_Max_value = .2     
KPI_Medium_Max_value = .4


df2 <- df %>% group_by(PROVIDERSTATE, DRGDEFINITION) %>% summarize(sum_payments = sum(AVERAGETOTALPAYMENTS), sum_charges = sum(AVERAGECOVEREDCHARGES)) %>% mutate(ratio = sum_payments / sum_charges ) %>% mutate(kpi = ifelse(ratio <= KPI_Low_Max_value, '03 Low', ifelse(ratio <= KPI_Medium_Max_value, '02 Medium', '01 High')))

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Medical Data Crosstab: By State in R') +
  labs(x=paste("DRG Medical Disorder"), y=paste("Provider State")) +
  layer(data=df2, 
        mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=""), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df2, 
        mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=""), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df2, 
        mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, label=round(ratio, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df2, 
        mapping=aes(x=DRGDEFINITION, y=PROVIDERSTATE, fill=kpi), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.5), 
        position=position_identity()
  ) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, vjust=1))