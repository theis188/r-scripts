library(rJava)
library(xlsxjars)
library(xlsx)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)
source("D:/R/RScripts/Hamhoon/Functions.r")

stringy <- function(s) {
  s <- as.character(s)
  if (is.na(s) | s=="NULL") {
    return ("NULL")
  }
  return( paste( "\"", s , "\""  ,sep="" ) )
}

psych_stringy <- function(s) {
  if (! s %in% c(0,1,2,3,4) ) {
    return ("NULL")
  }
  return( toString(s)  )
}

### Master List
#
#
#
#

con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

master.path = paste(root,"Master List Extra.xlsx",sep="")
master.list <- read.xlsx(master.path, sheetIndex = 1)

dbExecute(con, "DROP TABLE master_list")
dbExecute(con, "CREATE TABLE master_list
          (mrn integer not null primary key,
          psych_id integer not null,
          t1 date,
          t2 date,
          t3 date)")

cols <- c("Patient.MRN","Pysch.ID","T1","T2","T3")


for (i in 1:length(master.list [,1] ) ) {
  b <- master.list[cols][i,]
  s <- as.character( sapply(b,stringy) )
  insert_str <- paste( s, collapse=",")
  rs <- dbSendStatement(con, sprintf("INSERT INTO master_list VALUES (%s);",insert_str) )
  dbClearResult(rs)
}

rs <- dbGetQuery(con, "SELECT * FROM master_list;")

length(unique(master.list$Patient.MRN))


## Psych Data ###
#
#
#
#


psych.key.path <- "C:/Users/Matthew/Desktop/Correlative Project/PsychEDCEMR/R Data/Psych Symptom Key.xlsx"
psych.keys <- read.xlsx(psych.key.path, sheetIndex = 2)

psych.path = paste(root,"LWLC Data File for Collaboration JCK 032017.xlsx",sep="")
psych.data <- read.xlsx(psych.path,sheetIndex = 1)

dbExecute(con, "DROP TABLE psych")
dbExecute(con, "CREATE TABLE psych
          (psych_id integer not null,
          tpt integer not null,
          symptom text not null,
          qa text not null,
          response integer,
          primary key (psych_id, tpt, qa, symptom) )")

tr <- psych.keys[1:48,6:7]

dbGetQuery(con, "SELECT count(*) FROM psych;")

excuses = c("DISCONTINUED","DID NOT COMPLETE","NOT YET CHECKED")
coll = c()
# coll = c("NOT YET ENTERED","IN PROGRESS","NOT YET CHECKED")

# backwards <- sapply(psych.keys[1:9,6], as.character)

for (srow in 1:length( tr[,2] ) ) {
  tr1 <- tr[srow,]
  pkey <- as.character( tr1[,1] )
  qa <- substr(pkey,1,4)
  symptom <- as.character( tr1[,2] )
  cols <- c("ID",pkey)
  pos = tail(gregexpr('_',pkey)[[1]], n=1)
  tpt = as.integer( substr (pkey,pos-1,pos-1) )
  sl = psych.data[cols]
  for (k in 1:length( sl[,1] ) ) {
    if (sl[k,2] %in% excuses) next
    ins_v <- c(as.character( sl[k,1] ),
               as.character( tpt ),
               symptom,
               qa,
               psych_stringy ( sl[k,2] ) )
    insert_str <- paste( sapply(ins_v , stringy) , collapse="," )
    rs <- dbSendStatement(con, sprintf("INSERT INTO psych VALUES (%s);",insert_str) )
    dbClearResult(rs)
  }
}


### ROS DATA ###
#
#
#
#
#

dbExecute(con, "DROP TABLE ros")
dbExecute(con, "CREATE TABLE ros
          (mrn integer not null,
          tpt integer not null,
          symptom text not null,
          response text,
          primary key (mrn, tpt, symptom) )")


ros.path = paste(root,'ROSandPE Extra.xlsx',sep="")
# ros.data <- read.xlsx(ros.path, sheetIndex=1)

symptoms <- as.character(psych.keys[1:13,2])

excuses <- c("X")

if (sl[k,2] %in% excuses) next

for (tpt in 1:3) {
  ros.data <- read.xlsx(ros.path, sheetIndex=tpt)
  for (symptom in symptoms) {
    sl <- ros.data[ c("Patient.MRN", gsub(" ",".",symptom) ) ]
    for (i in 1: length(  sl[,1] ) ) {
      if (sl[i,2] %in% excuses) next
      ins_v <- c(as.character( sl[i,1] ),
                 as.character( tpt ),
                 symptom,
                 as.character ( sl[i,2] ) )
      ins_s <- as.character( sapply(ins_v, stringy) )
      ins_s <- paste(ins_s,collapse=",")
      rs <- dbSendStatement(con, sprintf("INSERT INTO ros VALUES (%s);",ins_s) )
      dbClearResult(rs)
    }
  }
}

dbGetQuery(con, "SELECT DISTINCT symptom FROM ros;")

### Adverse Events
#
#
#
#

ae.path = paste(root,"Adverse Events Extra.xlsx",sep="")
adverse.events <- read.xlsx(ae.path,sheetIndex = 1)

adverse.events <- collapser(adverse.events,"ROS.Symptom",c("pain"),"Pain")

ae.counts <- table(adverse.events$ROS.Symptom)
ae.counts
hist(ae.counts)

# id integer primary key autoincrement,

dbExecute(con, "DROP TABLE adverse")
dbExecute(con, "CREATE TABLE adverse
          (mrn integer not null,
          symptom text not null,
          start date not null,
          end date,
          grade int)")

getdate <- function (s) {
  if (is.na(s)) { return("NULL") }
  x <- as.character( s )
  if (!grepl ('-',x) ) {x <- as.numeric(x) }
  return ( as.character( as.Date( x, origin = "1899-12-30") ) )
}

for (i in 1:nrow( adverse.events ) ) {
  irow <- adverse.events[i,]
  if ( is.na(irow$ROS.Symptom) ) next
  ins_v <- c(irow$Patient.MRN,
             as.character(irow$ROS.Symptom),
             getdate(irow$Start),
             getdate(irow$Finish),
             irow$Grade)
  ins_s <- as.character( sapply ( ins_v , stringy ) )
  ins_s <- paste(ins_s, collapse=",")
  rs <- dbSendStatement(con, sprintf("INSERT INTO adverse VALUES (%s);",ins_s) )
  dbClearResult(rs)
}

rs <- dbGetQuery(con, "SELECT * FROM adverse limit 30;")
rs

sql_symp <- "SELECT 'Dizzy' symptom UNION SELECT 'Vomit' UNION SELECT 'Constipation' UNION SELECT 'Appetite' UNION SELECT 'Fatigue' UNION SELECT 'Nausea' UNION SELECT 'Pain' UNION SELECT 'Sleep' UNION SELECT 'SOB' UNION SELECT 'Cough' UNION SELECT 'Wt Loss'"
ros_symp <- "'Dizzy','Vomit','Constipation','Appetite','Fatigue','Nausea','Pain','Sleep','SOB','Cough','Wt Loss'"

q <- ("SELECT a.mrn,a.symptom,
      max(
      (a.start < date(m.t1,'-7 days') 
        AND ( (a.end > date(m.t1,'-7 days') )
          OR ( a.end IS NULL ) ) ) 
      ) as t1_ad,
      max(
      (a.start < date(m.t2,'-7 days') 
        AND ( (a.end > date(m.t2,'-7 days') )
          OR ( a.end IS NULL ) ) ) 
      ) as t2_ad,
      max(
      (a.start < date(m.t3,'-7 days') 
        AND ( (a.end > date(m.t3,'-7 days') )
          OR ( a.end IS NULL ) ) ) 
      ) as t3_ad
      FROM adverse a
      JOIN master_list m
      ON a.mrn = m.mrn
      WHERE a.symptom IN (%s)
      GROUP BY a.mrn,a.symptom
      ;")

q <- sprintf(q,ros_symp)

rs <- dbGetQuery(con, q)

qq <- sprintf("CREATE VIEW adcount AS
              %s",q)

dbExecute(con, qq)

qqq <- ("SELECT m.mrn, s.symptom, 1 tpt,
        exists(select 1 from adcount
        where ((adcount.mrn = m.mrn)
        and (s.symptom = adcount.symptom)
        and (adcount.t1_ad = 1))
        ) result
        from master_list m, 
        (%s) s
        WHERE m.t1 is not null
      UNION
        SELECT m.mrn, s.symptom, 2 tpt,
        exists(select 1 from adcount
        where ((adcount.mrn = m.mrn)
        and (s.symptom = adcount.symptom)
        and (adcount.t2_ad = 1))
        ) result
        from master_list m, 
        (%s) s
        WHERE m.t2 is not null
      UNION
        SELECT m.mrn, s.symptom, 3 tpt,
        exists(select 1 from adcount
        where ((adcount.mrn = m.mrn)
        and (s.symptom = adcount.symptom)
        and (adcount.t3_ad = 1))
        ) result
        from master_list m, 
        (%s) s
        WHERE m.t3 is not null
        ")

qqq <- sprintf(qqq,sql_symp,sql_symp,sql_symp)

qqqq <- sprintf("CREATE VIEW adverse_breakout AS
                %s;",qqq)

dbExecute(con, qqqq)

# 
# for (tpt in 1:3) {
#   for (symptom in symptoms) {
#     mrnpsych <- merge(dfList[[tpt]][ c("ID",symptom) ],mrn.id,by.x=c('ID'),by.y=c('Pysch.ID'))
#     tabb <- merge(rosList[[tpt]][ c( 'MRN',symptom ) ],mrnpsych,by.x=c('MRN'),by.y=c('Patient.MRN') )
#     totag <- colSums(data.frame(tabb[,2] == tabb[,4]) == 'TRUE')
#     totdf[symptom][1,] = totdf[symptom][1,] + totag
#     tot <- nrow(tabb)
#     totdf[symptom][2,] = totdf[symptom][2,] + tot
#     posag <- colSums(data.frame(tabb[,2] & tabb[,4]) == 'TRUE')
#     totdf[symptom][3,] = totdf[symptom][3,] + posag
#     totpos <- colSums(data.frame(tabb[,2] | tabb[,4]) == 'TRUE')
#     totdf[symptom][4,] = totdf[symptom][4,] + totpos
#     posagrate <- posag / totpos
#     negag <- colSums(data.frame(tabb[,2]=="FALSE" & tabb[,4]=="FALSE") == 'TRUE')
#     totdf[symptom][5,] = totdf[symptom][5,] + negag
#     totneg <- colSums(data.frame(tabb[,2]=="FALSE" | tabb[,4]=="FALSE") == 'TRUE')
#     totdf[symptom][6,] = totdf[symptom][6,] + totneg
#     negagrate <- negag / totneg
#     patpos <- colSums(mrnpsych[c(symptom)] == 'TRUE')
#     totdf[symptom][7,] = totdf[symptom][7,] + patpos
#     docpos <- colSums(rosList[[tpt]][c(symptom)] == 'TRUE')
#     totdf[symptom][8,] = totdf[symptom][8,] + docpos
#   }
# }
# write.path <- paste(root,'AgreementsNVComb.xlsx',sep="")
# write.xlsx(totdf,write.path)

