con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

q <- ("SELECT count(*) FROM labs l
  JOIN master_list m
  ON m.mrn = l.mrn
  WHERE julianday(l.date) BETWEEN
  julianday(m.t1) - 30 AND
    CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
    WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
    ELSE julianday(m.t3) + 30
    END
")

q <- ("SELECT l.mrn, avg(value) avg_alb FROM labs l
  JOIN master_list m
  ON m.mrn = l.mrn
  WHERE julianday(l.date) BETWEEN
  julianday(m.t1) - 30 AND
    CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
    WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
    ELSE julianday(m.t3) + 30
    END
  AND lab=\"Albumin\"
  GROUP BY l.mrn
")

qq <- ("SELECT m.mrn, SUM(p.response is not null) symptom_count
  FROM psych p
  JOIN master_list m
  ON m.psych_id = p.psych_id
  WHERE symptom =\"Wt Loss\"
  AND qa = \"MSAS\"
  group by mrn;
")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
plot(j$avg_alb,j$symptom_count)
cor(j$avg_alb,j$symptom_count)

wsymp = j[j$symptom_count != 0, ]$avg_alb
nsymp = j[j$symptom_count == 0, ]$avg_alb
mean(wsymp)
mean(nsymp)
var(wsymp)
var(nsymp)
boxplot(nsymp,wsymp)
t.test(nsymp,wsymp)
