con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

rs <- dbGetQuery(con, "SELECT symptom FROM psych where qa=\"MSAS\";")

ros_symp <- "'Dizzy','Vomit','Constipation','Appetite','Fatigue','Nausea','Pain','Sleep','SOB','Cough','Wt Loss'"

# a.mrn, a.symptom, a.start, a.end, m.t1, m.t3
q <- ("SELECT a.symptom, COUNT(*)
FROM adverse a
JOIN master_list m
ON m.mrn = a.mrn
WHERE m.t1 IS NOT NULL
AND m.t2 IS NOT NULL
AND m.t3 IS NOT NULL
AND ((a.end BETWEEN m.t1 AND m.t3) OR 
(a.start BETWEEN m.t1 AND m.t3) OR 
(a.start<m.t1 AND (a.end>m.t1 OR a.end IS NULL) ) )
AND a.symptom IN (%s)
GROUP BY a.symptom
;")

q <- sprintf(q,ros_symp)

rs <- dbGetQuery(con, q)

rs

qq <- ("SELECT distinct a.mrn
FROM adverse a
  JOIN master_list m
  ON m.mrn = a.mrn
  WHERE m.t1 IS NOT NULL
  AND m.t2 IS NOT NULL
  AND m.t3 IS NOT NULL
  AND ((a.end BETWEEN m.t1 AND m.t3) OR 
  (a.start BETWEEN m.t1 AND m.t3) OR 
  (a.start<m.t1 AND (a.end>m.t1 OR a.end IS NULL) ) )
  AND a.symptom IN (%s)
  ")

qq <-sprintf(qq,ros_symp)

qqq <- ("SELECT r.symptom, sum(r.response='Y') count
        FROM ros r
        JOIN (%s) m
        ON r.mrn = m.mrn
        GROUP BY r.symptom
        ;")

qqq <- sprintf(qqq,qq)

rs <- dbGetQuery(con, qqq)

rs




