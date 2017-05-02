psych.path = paste(root,"LWLC Data File for Collaboration JCK 032017.xlsx",sep="")
psych.data <- read.xlsx(psych.path,sheetIndex = 1)

con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

# q <- (" SELECT mrn, tpt, recorder, response 
#       from pe where symptom=\"ECOG\"
#       ")
# 
# ecog <- dbGetQuery(con,q)

qq <- (
  "SELECT mrn,psych_id from master_list;"
)


ros_q <- ("
SELECT symptom, tpt, mrn, response=\"Y\" as result
FROM ros 
WHERE symptom in ('Depression','Anxiety');
")

adverse_q <- ("
SELECT symptom, tpt, mrn, result 
from adverse_breakout
WHERE symptom in ('Depression','Anxiety');
")

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
  for (tpt in 1:3) {
    sq <- sprintf(sqli[[symptom]], as.character(tpt) )

    questions <- psych.data[,c('ID',sq)]
    j <- merge(questions,
               ml, 
               by.x = 'ID', 
               by.y = 'psych_id')
    
    names(j)[names(j)==sq] <- "TASPSYCH"  

    data.tpt <- data[data$tpt==tpt,][data$symptom==symptom,]
    
    
    if (tpt==1) {
      final <- merge(j, data.tpt)
    } else {
      final <- rbind(final, merge(j, data.tpt))
    }
  }
  final$symptom <- symptom
  if (symptom=="Anxiety") {
    FFinal <- final
  } else {
    FFinal <- rbind(FFinal, final)
  }
}

FFinal$TASPSYCH <- (as.numeric(as.character(FFinal$TASPSYCH)) > 0) *1
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

VIGGA <- data.frame()

for ( combo in list(c('psych','ros'),
                c('psych','adverse'),
                c('adverse','ros') ) ) {
  
  source1 <- combo[1]
  source2 <- combo[2]
  l1 <- fli[[source1]]
  l2 <- fli[[source2]]
  
  a <- merge( l1,l2,by=c('mrn','symptom','tpt'),suffixes=c('1','2') )
  
  col1 <- 'result1'
  col2 <- 'result2'
  
  for (symptom in c("Anxiety","Depression")) {
    squares <- a[a$symptom==symptom,]
    squares$yy = (squares[col1]==1) & (squares[col2]==1)
    squares$nn = squares[col1]==0 & squares[col2]==0
    squares$yn = squares[col1]==1 & squares[col2]==0
    squares$ny = squares[col1]==0 & squares[col2]==1
    squares <- colSums(squares[c('yy','nn','yn','ny')])
    squares <- data.frame(yy=squares['yy'],
                          nn=squares['nn'],
                          yn=squares['yn'],
                          ny=squares['ny'])
    
    squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$k = (squares$po - squares$pe) / (1 - squares$pe)
    kslice<-squares[c('yy','nn','yn','ny','k')]
    kslice$ds1 = source1
    kslice$ds2 = source2
    kslice$symptom = symptom
    
    if (ncol(VIGGA) == 0) {
      VIGGA<-kslice
    } else {
      VIGGA<-rbind(VIGGA,kslice)
    }
    
  }
}


