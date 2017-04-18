con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

##### ros vs Patient Reported Symptom Count MSAS

q <- ("SELECT mrn,avg(response=\"Y\") symptom_count_ros 
       from ros
       group by mrn
       ")
## >= 0 for low cutoff
qq <- ("SELECT m.mrn, avg( ifnull( p.response, -1 ) >=1 ) symptom_count_msas 
       from psych p
       join master_list m
       ON p.qa = \"MSAS\"
       AND p.psych_id = m.psych_id
       group by m.mrn
       ")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
plot(j$symptom_count_msas,j$symptom_count_ros)
cor(j$symptom_count_msas,j$symptom_count_ros)

##### ros vs Patient Reported Symptom Count FACT

q <- ("SELECT mrn,avg(response=\"Y\") symptom_count_ros 
       from ros
       group by mrn
       ")

## >= 2,2 for high cutoff - 3,1 for low
qq <- ("SELECT m.mrn, avg( CASE 
       WHEN p.symptom in (\"Sleep\",\"Appetite\") THEN
       CASE WHEN ifnull(p.response,5) <= 3 THEN 1
       ELSE 0 END
       WHEN ifnull(p.response,-1) >= 1 THEN 1
       ELSE 0
       END) symptom_count_fact 
       from psych p
       join master_list m
       ON p.qa = \"FACT\"
       AND p.psych_id = m.psych_id
       group by m.mrn
       ")


a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
plot(j$symptom_count_fact,j$symptom_count_ros)
cor(j$symptom_count_fact,j$symptom_count_ros)


##### BMI vs PATIENT REPORTED WAIT LOOSS FACT FUCK THIS PLACE

q <- ("SELECT l.mrn, avg(value) avg_bmi FROM vitals l
      JOIN master_list m
      ON m.mrn = l.mrn
      WHERE julianday(l.date) BETWEEN
      julianday(m.t1) - 30 AND
      CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
      WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
      ELSE julianday(m.t3) + 30
      END
      AND vital=\"BMI\"
      GROUP BY l.mrn
      ")

## >= 2,2 for high cutoff - 3,1 for low
qq <- ("SELECT m.mrn, max( CASE 
       WHEN p.symptom in (\"Sleep\",\"Appetite\") THEN
       CASE WHEN ifnull(p.response,5) <= 2 THEN 1
       ELSE 0 END
       WHEN ifnull(p.response,-1) >= 2 THEN 1
       ELSE 0
       END) symptom_count 
       from psych p
       join master_list m
       ON p.qa = \"FACT\"
       AND p.psych_id = m.psych_id
       AND p.symptom == \"Wt Loss\"
       group by m.mrn
       ")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
boxplot( j[ j$symptom_count==1 ,]$avg_bmi, j[ j$symptom_count==0 ,]$avg_bmi )
t.test(j[ j$symptom_count==1 ,]$avg_bmi, j[ j$symptom_count==0 ,]$avg_bmi)

##### BMI vs PATIENT REPORTED WAIT LOOSS MSAS FUCK THIS PLACE

q <- ("SELECT l.mrn, avg(value) avg_bmi FROM vitals l
      JOIN master_list m
      ON m.mrn = l.mrn
      WHERE julianday(l.date) BETWEEN
      julianday(m.t1) - 30 AND
      CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
      WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
      ELSE julianday(m.t3) + 30
      END
      AND vital=\"BMI\"
      GROUP BY l.mrn
      ")

## >= 2,2 for high cutoff - 3,1 for low
qq <- ("SELECT m.mrn, max( ifnull( p.response, -1 ) >=1 ) symptom_count_msas 
       from psych p
       join master_list m
       ON p.qa = \"MSAS\"
       AND p.psych_id = m.psych_id
       AND p.symptom=\"Wt Loss\"
       group by m.mrn
       ")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
boxplot( j[ j$symptom_count_msas==1 ,]$avg_bmi, j[ j$symptom_count_msas==0 ,]$avg_bmi )
t.test(j[ j$symptom_count_msas==1 ,]$avg_bmi, j[ j$symptom_count_msas==0 ,]$avg_bmi)



##### HEMOGOBLIN vs PATIENT REPORTED WAIT LOOSS FACT FUCK THIS PLACE

q <- ("SELECT l.mrn, avg(value) avg_hem FROM labs l
      JOIN master_list m
      ON m.mrn = l.mrn
      WHERE julianday(l.date) BETWEEN
      julianday(m.t1) - 30 AND
      CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
      WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
      ELSE julianday(m.t3) + 30
      END
      AND lab=\"Hemoglobin\"
      GROUP BY l.mrn
      ")

## >= 2,2 for high cutoff - 3,1 for low
qq <- ("SELECT m.mrn, max( CASE 
       WHEN p.symptom in (\"Sleep\",\"Appetite\") THEN
       CASE WHEN ifnull(p.response,5) <= 2 THEN 1
       ELSE 0 END
       WHEN ifnull(p.response,-1) >= 2 THEN 1
       ELSE 0
       END) symptom_count 
       from psych p
       join master_list m
       ON p.qa = \"FACT\"
       AND p.psych_id = m.psych_id
       AND p.symptom == \"Fatigue\"
       group by m.mrn
       ")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
boxplot( j[ j$symptom_count==1 ,]$avg_hem, j[ j$symptom_count==0 ,]$avg_hem )
t.test(j[ j$symptom_count==1 ,]$avg_hem, j[ j$symptom_count==0 ,]$avg_hem)
