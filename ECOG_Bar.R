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

final = final[ final$GF1!='DISCONTINUED' , ]
final = droplevels(final,reorder = FALSE)

plot(final$response, final$GP7)
plot(final$response, final$GF1)

dfGP7 <- data.frame( table ( final[,c("response", "GP7")] ) )
dfGF1 <- data.frame( table ( final[,c("response", "GF1")] ) )

write.xlsx(dfGP7, "D:R/Tables/ECOG_Output.xlsx", sheetName="GP7 of BEDDIEBYE" )
write.xlsx(dfGF1, "D:R/Tables/ECOG_Output.xlsx", sheetName="GF1 of WORKING (HAM HATES)", append=TRUE )

ggplot(dfGF1, aes(factor(response), Freq , fill = GF1 )) + 
  geom_bar(stat="identity", position = "dodge", width = 0.6) + 
  xlab("Provider-Documented ECOG Score") +
  ylab("Number of Instances") +
  scale_x_discrete( labels=c(
    "0"="ECOG = 0",
    "1"="ECOG = 1",
    "2"="ECOG = 2",
    "3"="ECOG = 3"
  ) ) +
  theme(axis.text.x = element_text(face="bold", size = 12),
        legend.position=c(0.5,1),
        legend.justification=c(0.5,1),
        axis.text.y = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 14) ) +
  scale_fill_brewer(name=("Patient response to\n\"I am able to work\""),
                      labels=
                      c("\"Not at all\"",
                        "\"A little bit\"",
                        "\"Somewhat\"",
                        "\"Quite a bit\"",
                        "\"Very much\""),
                      palette="Spectral") 

ggplot(dfGP7, aes(factor(response), Freq , fill = GP7 )) + 
  geom_bar(stat="identity", position = "dodge", width = 0.6) + 
  xlab("Provider-Documented ECOG Score") +
  ylab("Number of Instances") +
  scale_x_discrete( labels=c(
    "0"="ECOG = 0",
    "1"="ECOG = 1",
    "2"="ECOG = 2",
    "3"="ECOG = 3"
  ) ) +
  theme(axis.text.x = element_text(face="bold", size = 12),
        legend.position=c(0.5,1),
        legend.justification=c(0.5,1),
        axis.text.y = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 14) ) +
  scale_fill_brewer(name=("Patient response to\n\"I am forced to spend time in bed\""),
                    labels=
                      c("\"Not at all\"",
                        "\"A little bit\"",
                        "\"Somewhat\"",
                        "\"Quite a bit\"",
                        "\"Very much\""),
                    palette="Spectral",
                    direction=-1) 


help(geom_bar)
