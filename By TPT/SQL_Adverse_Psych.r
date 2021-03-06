# for (i in list( c("MSAS","0","0"),c("FACT","1","3") ) ) { ## LOW CUTOFF

con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")
li = list()

for (tpt in 1:3) {

for (i in list( c("MSAS","2","0"),c("FACT","2","2") ) ) { ## HIGH CUTOFF
  
# for (i in list( c("MSAS","0","0"),c("FACT","1","3") ) ) { ## LOW CUTOFF
  
  qa = i[1]
  lim = i[2]
  flim = i[3]
  
  q <- sprintf("SELECT 
               p.tpt,
               p.psych_id,
               p.symptom,
               CASE WHEN '%s' == 'MSAS'
                THEN CASE
                  WHEN p.response IS NULL THEN 0
                  WHEN p.response >= %s THEN 1
                ELSE 0 
               END 
               ELSE CASE 
                WHEN p.response IS NULL THEN 0
                WHEN p.symptom IN ('Sleep','Appetite') THEN CASE
                  WHEN p.response <= %s THEN 1
                  ELSE 0
                  END
                WHEN p.response >= %s THEN 1
                ELSE 0
                END
               END as patient, 
               a.result adverse
               from psych p
               join master_list m
               on m.psych_id = p.psych_id
               join adverse_breakout a
               on m.mrn = a.mrn
               and p.tpt = a.tpt
               and p.symptom = a.symptom
               where p.qa = '%s' 
               and p.tpt = %s",qa,lim,flim,lim,qa,tpt )
  
  totag_s <- ("SELECT
              sub.symptom,
              sum(sub.adverse==sub.patient)*1.0/count(*) totag%s
              from (%s) sub
              group by sub.symptom;")
  
  posag_s <- ("SELECT
              sub.symptom,
              sum(sub.adverse==sub.patient)*1.0/count(*) posag%s
              from (%s) sub
              where (sub.adverse==1) OR (sub.patient==1)
              group by sub.symptom;")
  
  negag_s <- ("SELECT
              sub.symptom,
              sum(sub.adverse==sub.patient)*1.0/count(*) negag%s
              from (%s) sub
              where (sub.adverse==0) OR (sub.patient==0)
              group by sub.symptom;")
  
  patcount_s <- ("SELECT
                 sub.symptom,
                 sum(sub.patient) patcount%s
                 from (%s) sub
                 group by sub.symptom;")
  
  advcount_s <- ("SELECT
                 sub.symptom,
                 sum(sub.adverse) advcount%s
                 from (%s) sub
                 group by sub.symptom;")
  
  totcount_s <- ("SELECT
                 sub.symptom,
                 count(*) totcount%s
                 from (%s) sub
                 group by sub.symptom;")
  
  squares_s <- ("SELECT
                sub.symptom,
                sum( (sub.patient == 1) and (sub.adverse==1) ) yy,
                sum( (sub.patient == 1) and (sub.adverse==0) ) yn,
                sum( (sub.patient == 0) and (sub.adverse==1) ) ny,
                sum( (sub.patient == 0) and (sub.adverse==0) ) nn
                from (%s) sub
                group by sub.symptom;")
  
  negag_q <- sprintf(negag_s,qa,q)
  posag_q <- sprintf(posag_s,qa,q)
  totag_q <- sprintf(totag_s,qa,q)
  patcount_q <- sprintf(patcount_s,qa,q)
  advcount_q <- sprintf(advcount_s,qa,q)
  totcount_q <- sprintf(totcount_s,qa,q)
  squares_q <-sprintf(squares_s,q)
  
  negag <- dbGetQuery(con, negag_q)
  posag <- dbGetQuery(con, posag_q)
  totag <- dbGetQuery(con, totag_q)
  patcount <- dbGetQuery(con, patcount_q)
  advcount <- dbGetQuery(con, advcount_q)
  totcount <- dbGetQuery(con, totcount_q)
  squares <- dbGetQuery(con, squares_q)
  
  squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
  kstr <- paste("k",qa,sep="")
  squares[c( kstr )] = (squares$po - squares$pe) / (1 - squares$pe)
  kslice <- squares[c("sub.symptom",kstr)]
  
  li[[qa]] <- Reduce(function(...) merge(..., all=TRUE), list(negag, posag, totag, patcount, advcount, totcount, kslice))
  
}

AdPat <- Reduce(function(...) merge(..., all=TRUE), li) 

sName <- paste("TPT" ,as.character(tpt), sep="")

app <- if(tpt==1) FALSE else TRUE

write.xlsx( AdPat , "D:/R/Tables/Redo/By TPT/psych adverse table.xlsx" , 
            sheetName = sName, append = app)

}
