### I FEEL SAD AND NERVOUS QUESTIONS

psych.path = paste(root,"LWLC Data File for Collaboration JCK 032017.xlsx",sep="")
psych.data <- read.xlsx(psych.path,sheetIndex = 1)

for (tpt in 1:3) {

con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

qq <- (
  "SELECT mrn,psych_id from master_list;"
)


ros_q <- sprintf( ("
SELECT symptom, tpt, mrn, response=\"Y\" as result
FROM ros 
WHERE symptom in ('Depression','Anxiety')
and tpt = %s;"), tpt)

adverse_q <- sprintf( ("
SELECT symptom, tpt, mrn, result 
from adverse_breakout
WHERE symptom in ('Depression','Anxiety');
and tpt=%s;"), tpt)

ml <- dbGetQuery(con,qq)

sfq1 <- 'FACT_em_T%s_1' ## GE1

sfq2 <- 'FACT_em_T%s_4' ## GE4

sqli <- list()
sqli[["Depression"]]<- sfq1
sqli[["Anxiety"]]<- sfq2

symptom <- "Depression" 
symptom <- "Anxiety" 
# data_q <- adverse_q 

data_q <- ros_q 
query <- sprintf(data_q,symptom)
data <- dbGetQuery(con,query)

for (symptom in c("Anxiety","Depression")) {
  
  sq <- sprintf(sqli[[symptom]], as.character(tpt) )

  questions <- psych.data[,c('ID',sq)]
  j <- merge(questions,
             ml, 
             by.x = 'ID', 
             by.y = 'psych_id')
  
  names(j)[names(j)==sq] <- "TASPSYCH"  

  data.tpt <- data[data$tpt==tpt,]
  data.tpt <- data.tpt[data.tpt$symptom==symptom,]

  final <- merge(j, data.tpt)
  final$symptom <- symptom
  if (symptom=="Anxiety") {
    FFinal <- final
  } else {
    FFinal <- rbind(FFinal, final)
  }
}

#######################
#
#  CHANGE
#  
#  CUTOFFFS
#
# HEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEERE -----------V


FFinal$TASPSYCH <- (as.numeric(as.character(FFinal$TASPSYCH)) > 1) *1
FFinal$result <- NULL
names(FFinal)[names(FFinal)=="TASPSYCH"] <- "result"
FFinal$result[ is.na( FFinal$result ) ] <- 0

ros <- dbGetQuery(con, ros_q)
adverse <- dbGetQuery(con, adverse_q)

df <- merge(FFinal,ros)

fli <- list()

fli[['psych']] <- FFinal
fli[['ros']] <- ros
fli[['adverse']] <- adverse

d <- data.frame()

for ( combo in list(c('psych','ros'),
                c('psych','adverse'),
                c('adverse','ros') ) ) {
  
  source1 <- combo[1]
  source2 <- combo[2]
  l1 <- fli[[source1]]
  l2 <- fli[[source2]]
  
  next_rows <- get_kappa_from_dataframes(l1, l2, source1=source1, source2=source2)
  print(d)
  print(next_rows)
    if (ncol(d) == 0) {
      d<-next_rows
    } else {
      d<-rbind(d,next_rows)
    }
    
  }

app = if(tpt==1) FALSE else TRUE
sName = paste(  "TPT", as.character(tpt)  ,sep =""   )
write.xlsx(d, 'D:/R/Tables/By TPT/GE1_GE4_ACL_MCL_FACT_Comparison.xlsx', append = app, sheetName = sName )

}
