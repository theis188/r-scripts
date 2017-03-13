con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

rs <- dbGetQuery(con, "SELECT distinct symptom FROM psych where qa=\"MSAS\";")

q <- ("SELECT 
  p.tpt,
  p.psych_id,
  p.symptom,
  CASE
    WHEN p.response IS NULL THEN 0
    WHEN p.response >= 0 THEN 1
    ELSE 0 
  END as patient, 
  (r.response==\"Y\") physician
from psych p
join master_list m
on m.psych_id = p.psych_id
join ros r
on m.mrn = r.mrn
and p.tpt = r.tpt
and p.symptom = r.symptom
where p.qa = \"MSAS\" ")

totag <- ("SELECT
  sub.symptom,
  sum(sub.physician==sub.patient)*1.0/count(*) totag
from (%s) sub
group by sub.symptom;")

posag <- ("SELECT
  sub.symptom,
  sum(sub.physician==sub.patient)*1.0/count(*) posag
from (%s) sub
where (sub.physician==1) OR (sub.patient==1)
group by sub.symptom;")

negag <- ("SELECT
  sub.symptom,
sum(sub.physician==sub.patient)*1.0/count(*) negag
from (%s) sub
where (sub.physician==0) OR (sub.patient==0)
group by sub.symptom;")

fin <- sprintf(negag,q)

rs <- dbGetQuery(con, fin)
                 
# group by symptom;")
rs
