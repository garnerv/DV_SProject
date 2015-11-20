
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from MEDICALDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gv4353', PASS='orcl_gv4353', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#df

#summary(df)
#head(df)

df2 <- df %>% mutate(AVG_DIFFERENCE = AVERAGETOTALPAYMENTS - AVERAGEMEDICAREPAYMENTS, AVG_DIFF = cume_dist(AVG_DIFFERENCE))

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Medical Data \n Percentiles vs Total Discharges') +
  labs(x="Percentile of Average Difference", y=paste("Total Discharges")) +
  layer(data=df2, 
        mapping=aes(x=as.numeric(as.character(AVG_DIFF)), y=as.numeric(as.character(TOTALDISCHARGES))), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(), 
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
)
