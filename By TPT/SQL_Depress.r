con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")
source("D:/R/RScripts/Hamhoon/Good_Functions.r")

for (tpt in 1:3) {

d <- data.frame()

depress_q <- sprintf( ("select m.mrn, d.tpt, d.symptom, 
(CASE when d.symptom=\"Depression\" THEN d.value>16 
ELSE d.value>33 END) result
FROM depress d
JOIN master_list m
ON m.psych_id = d.psych_id
WHERE d.symptom in ('Depression','Anxiety')
AND d.tpt=%s;
"), tpt)

depress <- dbGetQuery(con, depress_q)
  
ros_q <- sprintf( ("select mrn, tpt, symptom,
response='Y' result
from ros
WHERE symptom in ('Depression','Anxiety')
and tpt=%s;"), tpt)
  
ros <- dbGetQuery(con, ros_q)

adverse_q <- sprintf( ("select mrn, tpt, symptom, 
result from adverse_breakout
where symptom in ('Depression','Anxiety')
and tpt = %s;"), tpt)

adverse <- dbGetQuery(con, adverse_q)

qli <- list()
qli[['ros']] <- ros
qli[['depress']] <- depress
qli[['adverse']] <- adverse

for (combo in list(c('ros','depress'),
                   c('ros','adverse'),
                   c('depress','adverse') ) ) {
    source1=combo[1]
    source2=combo[2]
    l1 <- qli[[source1]]
    l2 <- qli[[source2]]
    
    next_rows <- get_kappa_from_dataframes(l1,l2,source1=source1,source2=source2)
    
    if (ncol(d)==0) {
      d <- next_rows
    } else {
      d <- rbind(d,next_rows)
    }
  }

app = if(tpt==1) FALSE else TRUE
sName = paste(  "TPT", as.character(tpt)  ,sep =""   )
write.xlsx( d , "D:/R/Tables/By TPT/depress table.xlsx", append = app, sheetName = sName )

}
