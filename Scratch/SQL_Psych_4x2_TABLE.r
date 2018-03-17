con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")


q<-("SELECT r.response rossi, 
  (CASE WHEN d.symptom=\"Anxiety\" then d.value>33
  when d.symptom=\"Depression\" then d.value>16 end) dep,
  r.symptom as smybdom
  from ros r
  join depress d
  ON r.symptom in ('Anxiety','Depression')
  AND d.symptom = r.symptom
  AND d.tpt = r.tpt
  join master_list m
  on 
  m.psych_id = d.psych_id
  AND m.mrn = r.mrn;
  ")

a<-dbGetQuery(con,q)

table(a)
