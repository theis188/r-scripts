con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

rs <- dbGetQuery(con, "SELECT distinct symptom FROM psych where qa=\"MSAS\";")

li <- list()

for (i in list( c("MSAS","0","0"),c("FACT","1","2") ) ) { 

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
  (r.response=='Y') physician
from psych p
join master_list m
on m.psych_id = p.psych_id
join ros r
on m.mrn = r.mrn
and p.tpt = r.tpt
and p.symptom = r.symptom
where p.qa = '%s' ",qa,lim,flim,lim,qa )

totag_s <- ("SELECT
  sub.symptom,
  sum(sub.physician==sub.patient)*1.0/count(*) totag%s
from (%s) sub
group by sub.symptom;")

posag_s <- ("SELECT
  sub.symptom,
  sum(sub.physician==sub.patient)*1.0/count(*) posag%s
from (%s) sub
where (sub.physician==1) OR (sub.patient==1)
group by sub.symptom;")

negag_s <- ("SELECT
  sub.symptom,
sum(sub.physician==sub.patient)*1.0/count(*) negag%s
from (%s) sub
where (sub.physician==0) OR (sub.patient==0)
group by sub.symptom;")

patcount_s <- ("SELECT
  sub.symptom,
sum(sub.patient) patcount%s
from (%s) sub
group by sub.symptom;")

phycount_s <- ("SELECT
sub.symptom,
sum(sub.physician) phycount%s
from (%s) sub
group by sub.symptom;")

totcount_s <- ("SELECT
sub.symptom,
count(*) totcount%s
from (%s) sub
group by sub.symptom;")

negag_q <- sprintf(negag_s,qa,q)
posag_q <- sprintf(posag_s,qa,q)
totag_q <- sprintf(totag_s,qa,q)
patcount_q <- sprintf(patcount_s,qa,q)
phycount_q <- sprintf(phycount_s,qa,q)
totcount_q <- sprintf(totcount_s,qa,q)


negag <- dbGetQuery(con, negag_q)
posag <- dbGetQuery(con, posag_q)
totag <- dbGetQuery(con, totag_q)
patcount <- dbGetQuery(con, patcount_q)
phycount <- dbGetQuery(con, phycount_q)
totcount <- dbGetQuery(con, totcount_q)

li[[qa]] <- Reduce(function(...) merge(..., all=TRUE), list(negag, posag, totag, patcount, phycount, totcount))

}

Reduce(function(...) merge(..., all=TRUE), li)

