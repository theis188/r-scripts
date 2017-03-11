library(rJava)
library(xlsxjars)
library(xlsx)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)
source("D:/R/RScripts/Hamhoon/Functions.r")

stringy <- function(s) {
  if (is.na(s) | s=="NULL") {
    return ("NULL")
  }
  return( paste( "\"", toString(s) , "\""  ,sep="" ) )
}

psych_stringy <- function(s) {
  if (! s %in% c(0,1,2,3,4) ) {
    return ("NULL")
  }
  return( toString(s)  )
}

### Master List

con <- dbConnect(RSQLite::SQLite(), "sqldb.db")

# dbExecute(con, "CREATE TABLE test (a INTEGER)")
# dbListTables(con)
# dbGetQuery(con, "SELECT * FROM USArrests LIMIT 3")

master.path = paste(root,"Master List Extra.xlsx",sep="")
master.list <- read.xlsx(master.path, sheetIndex = 1)

#rs <- dbSendStatement(con, "INSERT INTO test VALUES (:a)", list(a = 1:3))
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
  s <- as.character( lapply(b,stringy) )
  insert_str <- paste( s, collapse=",")
  rs <- dbSendStatement(con, sprintf("INSERT INTO master_list VALUES (%s);",insert_str) )
  dbClearResult(rs)
}

rs <- dbGetQuery(con, "SELECT * FROM master_list;")

length(unique(master.list$Patient.MRN))

# BMS153.patients = master.list[master.list$Study == "BMS CA209-153 NSCLC",]
# bms.mrn = BMS153.patients$Patient.MRN

### Adverse Events

ae.path = paste(root,"Adverse Events Extra.xlsx",sep="")
adverse.events <- read.xlsx(ae.path,sheetIndex = 1)

colnames(adverse.events)
length(unique(adverse.events$Patient.MRN))
levels( adverse.events$ROS.Symptom )

adverse.events <- adverse.events[ sapply ( adverse.events$ROS.Symptom, function(x) { return(!is.na(x)) } ) , ]

adverse.events <- collapser(adverse.events,"ROS.Symptom",c("pain"),"Pain")

ae.counts <- table(adverse.events$ROS.Symptom)
ae.counts
hist(ae.counts)

## Psych Data ###
#
#
#
#

psych.key.path <- "C:/Users/Matthew/Desktop/Correlative Project/PsychEDCEMR/R Data/Psych Symptom Key.xlsx"
psych.keys <- read.xlsx(psych.key.path, sheetIndex = 2)

psych.path = paste(root,"LWLC Data File for Collaboration JCK 113016.xlsx",sep="")
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

excuses = c("DISCONTINUED")
coll = c("NOT YET ENTERED","IN PROGRESS","NOT YET CHECKED")

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

ros.path = paste(root,'ROSandPE Extra.xlsx',sep="")
ros.data <- read.xlsx(ros.path, sheetIndex=1)

symptoms <- as.character(psych.keys[1:13,2])

newDF = data.frame(MRN=ros.data$Patient.MRN )
for (symptom in symptoms) {
  newDF[ toString(symptom) ] <- FALSE
}

rosList = list()
for (tpt in 1:3) {
  assign("temp",newDF)
  rosList[[tpt]] <- temp
}

for (tpt in 1:3) {
  ros.data <- read.xlsx(ros.path, sheetIndex=tpt)
  for (symptom in symptoms) {
    sub <- ros.data[ c("Patient.MRN", gsub(" ",".",symptom) ) ]
    sub <- sub[ !sub[,2] == "X", ]
    sub[,2] <- sub[,2] == "Y"
    rosList[[tpt]] <- rosList[[tpt]][ rosList[[tpt]]$MRN %in% sub[,1] , ]
    rosList[[tpt]][symptom] <- rosList[[tpt]][symptom] | sub[,2]
    
  }
}

# for (i in 1:3) {
#   print (i )
#   print (nrow (rosList[[i]]) )
#   print ( colSums(rosList[[i]][,-1] == "TRUE") )
# }



# num_yes <- lapply(ros.data[1:3], function(x) {
#   x[,-(1:2)]
#   return( rowSums(x == "Y") )
# })

for (tpt in 1:3) {
  print (tpt)
  psychmerge <- merge(dfList[[tpt]]['ID'], 
                  master.list[c('Patient.MRN','Pysch.ID')], 
                  by.x=c('ID') ,
                  by.y = c('Pysch.ID')  )
  ## In Psych, not in master
  print ( dfList[[tpt]][!dfList[[tpt]]$ID %in% psychmerge$ID,]$ID )
  ## In Master, not in psych
  print ( master.list[!master.list$Pysch.ID %in% psychmerge$ID,]$Pysch.ID )
}

for (tpt in 1:3) {
  print (tpt)
  psychmerge <- merge(dfList[[tpt]]['ID'], 
                      master.list[c('Patient.MRN','Pysch.ID')], 
                      by.x=c('ID') ,
                      by.y = c('Pysch.ID')  )
  ## In Psych, not in ROSlist
  print ( psychmerge[!psychmerge$Patient.MRN %in% rosList[[tpt]]$MRN,]$Patient.MRN )
  ## In ROSlist, not in Psych
  print ( rosList[[tpt]] [!rosList[[tpt]]$MRN %in% psychmerge$Patient.MRN,]$MRN )
}

mrn.id <- master.list[c('Pysch.ID','Patient.MRN')]
totdf = data.frame(matrix(0, ncol = 12, nrow = 8))

for (tpt in 1:3) {
  dfList[[tpt]][c("NauseaVomit")] <- as.vector(dfList[[tpt]]$Nausea | dfList[[tpt]]$Vomit)
  dfList[[tpt]] <- dfList[[tpt]][ , !(names (dfList[[tpt]]) %in% c("Vomit","Nausea") ) ]
  rosList[[tpt]][c("NauseaVomit")] = as.vector(rosList[[tpt]]$Nausea | rosList[[tpt]]$Vomit)
  rosList[[tpt]] <- rosList[[tpt]][ , !(names (rosList[[tpt]]) %in% c("Vomit","Nausea") ) ]
}

symptoms <- colnames(dfList[[1]])[2:13]
colnames(totdf) <- symptoms

for (tpt in 1:3) {
  for (symptom in symptoms) {
    mrnpsych <- merge(dfList[[tpt]][ c("ID",symptom) ],mrn.id,by.x=c('ID'),by.y=c('Pysch.ID'))
    tabb <- merge(rosList[[tpt]][ c( 'MRN',symptom ) ],mrnpsych,by.x=c('MRN'),by.y=c('Patient.MRN') )
    totag <- colSums(data.frame(tabb[,2] == tabb[,4]) == 'TRUE')
    totdf[symptom][1,] = totdf[symptom][1,] + totag
    tot <- nrow(tabb)
    totdf[symptom][2,] = totdf[symptom][2,] + tot
    posag <- colSums(data.frame(tabb[,2] & tabb[,4]) == 'TRUE')
    totdf[symptom][3,] = totdf[symptom][3,] + posag
    totpos <- colSums(data.frame(tabb[,2] | tabb[,4]) == 'TRUE')
    totdf[symptom][4,] = totdf[symptom][4,] + totpos
    posagrate <- posag / totpos
    negag <- colSums(data.frame(tabb[,2]=="FALSE" & tabb[,4]=="FALSE") == 'TRUE')
    totdf[symptom][5,] = totdf[symptom][5,] + negag
    totneg <- colSums(data.frame(tabb[,2]=="FALSE" | tabb[,4]=="FALSE") == 'TRUE')
    totdf[symptom][6,] = totdf[symptom][6,] + totneg
    negagrate <- negag / totneg
    patpos <- colSums(mrnpsych[c(symptom)] == 'TRUE')
    totdf[symptom][7,] = totdf[symptom][7,] + patpos
    docpos <- colSums(rosList[[tpt]][c(symptom)] == 'TRUE')
    totdf[symptom][8,] = totdf[symptom][8,] + docpos
  }
}
write.path <- paste(root,'AgreementsNVComb.xlsx',sep="")
write.xlsx(totdf,write.path)

# ROS.counts <- data.frame(num_yes)
# colnames(ROS.counts) <- c("t1","t2","t3")
# ROS.counts$MRN <- ros.data[[1]]$Patient.MRN

# ros.levels <- c("Y","N","U","D","A","R","NP","F","X","S")

####


# ros.data[ rownames(BMS.ros.data), ]  
# BMS.ros.data$Y.Sum <- rowSums(BMS.ros.data == "Y")
# 
# 
# for (tpt in c(1,2,3)){
#   
#   bms.mrn <- bms.mrn[!is.na(bms.mrn)]
#   BMS.ros.data <- ros.data[ros.data$Patient.MRN %in% bms.mrn,]
#   
#   BMS.ros.data[,-(1:2)]
#   rowSums(BMS.ros.data == "Y")
#   
#   cname = paste("tpt", toString(tpt) ,sep="")
#   BMS.tpt.data[,cname] <- rowSums(BMS.ros.data == "Y")
#   
# }


# psych.ids = c(BMS153.patients$Pysch.ID)
# psych.ids <- psych.ids[!is.na(psych.ids)]
# 
# BMS.psych.data <- psych.data[psych.data$ID %in% psych.ids,]

# BMS.tpt.data = data.frame(psych.ids)

# psych.dict <- as.list(c())
# for (i in 1:nrow(psych.keys)) {
#   psych.dict[[ as.character(psych.keys[i,1]) ]] <- as.character(psych.keys[i,2])
# }
# 
# for ( name in colnames(psych.data [,2:76 ] ) ) {
#   print(  psych.data [ c("ID",name) ]  )
#   print( psych.dict[[ name ]] )
# }
#

# for (symptom in symptoms) {
#   keys= psych.keys[,7:8][,1] [ psych.keys[,7:8][,2]==symptom ]
#   # print(symptom)
#   # print(keys)
#   
#   for (key in keys) {
#     pos = tail(gregexpr('_',key)[[1]], n=1)    
#     tpt = as.integer( substr (key,pos-1,pos-1) )
#     # print ( tpt )
#     # print ( key )
#     sub = psych.data[c('ID',toString(key) )]
#     sub <- sub[ !(sub[,2] %in% excuses ), ]
#     if ( grepl("MSAS",toString(key)) ) {
#       sub[ is.na(sub[,2]), 2 ] <- " "
#       sub <- collapser (sub,c(2),coll," ")
#       sub[,2] = sub [,2] %in% c(0,1,2,3,4)
#     } else {
#       sub <- collapser (sub,c(2),coll,0)
#       sub[ is.na(sub[,2]), 2 ] <- 0
#       if (key %in% backwards) {  ## GET LIST OF BACKWARDS QUESTIONS HERE
#         sub[,2] <- sub[,2] %in% c(0,1,2,3)
#       } else {
#         sub[,2] <- sub [,2] %in% c(1,2,3,4)
#       }
#     }
#     dfList[[tpt]] <- dfList[[tpt]][ dfList[[tpt]]$ID %in% sub[,1] , ]
#     dfList[[tpt]][symptom] <- dfList[[tpt]][symptom] | sub[,2]
#     if ( !all ( dfList[[tpt]]$ID == sub[,1] ) ) { 
#       print("Catastrophic Failure") 
#       print(key)
#       print( nrow( sub ) )
#       print( nrow( dfList[[tpt]] ) )
#       print( sub[,1][ !sub[,1] %in% dfList[[tpt]]$ID ] )
#     }
#   }
# }

