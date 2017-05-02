psych.path = paste(root,"LWLC Data File for Collaboration JCK 032017.xlsx",sep="")
psych.data <- read.xlsx(psych.path,sheetIndex = 1)
library(ggplot2)

con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

q <- (" SELECT mrn, tpt, recorder, response 
from pe where symptom=\"ECOG\"
")

ecog <- dbGetQuery(con,q)
  
qq <- (
  "SELECT mrn,psych_id from master_list;"
)

ml <- dbGetQuery(con,qq)

sfq1 <- 'FACT_fu_T%s_1' ## GF1

sfq2 <- 'FACT_ph_T%s_7' ## GP7 FUCK THIS BULLSHIT



for (tpt in 1:3) {
  sq1 <- sprintf(sfq1, as.character(tpt) )
  sq2 <- sprintf(sfq2, as.character(tpt) )
  
  questions <- psych.data[,c('ID',sq1,sq2)]
  j <- merge(questions,
                             ml, 
                             by.x = 'ID', 
                             by.y = 'psych_id')
  
  names(j)[names(j)==sq1] <- "GF1"  
  names(j)[names(j)==sq2] <- "GP7"

  
  ecog.tpt <- ecog[ecog$tpt==tpt,]
  if (tpt==1) {
  final <- merge(ecog.tpt,j)
  } else {
    final <- rbind(final, 
                             merge(ecog.tpt,j))
  }
}                   

plot(final$response, final$GP7)
plot(final$response, final$GF1)

df <- data.frame( table ( final[,c("response", "GP7")] ) )
df <- data.frame( table ( final[,c("response", "GF1")] ) )


ggplot(df, aes(factor(response), Freq , fill = GF1 )) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  xlab("ECOG Value")


help(geom_bar)
