con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")


##### BMI vs Physician Reported Symptom Count
symptoms <- c('Dizzy','Vomit','Constipation','Appetite','Fatigue','Nausea','Pain','Sleep','SOB','Cough','Wt Loss','Depression','Anxiety','Diarrhea','Hair Loss','Rash')
q <- ("SELECT l.mrn, avg(value) avg_alb FROM vitals l
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

qq <- ("SELECT mrn,avg(response=\"Y\") symptom_count 
       from ros
       group by mrn
       ")

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
plot(j$avg_alb,j$symptom_count*16)
cor(j$avg_alb,j$symptom_count)

###### BMI LOSS vs. WT LOSS PT REPORT

q1 <- ("SELECT v.mrn, v.value, max(v.date) as date from vitals v
       JOIN master_list m
       ON v.vital=\"BMI\"
       and m.t3 is not null
       and m.mrn = v.mrn
       where julianday( v.date ) between 
       julianday(m.t1) - 30 AND julianday(m.t3) + 30
       AND value is not null
       GROUP BY v.mrn
       ")

q2 <- ("SELECT v.mrn, v.value, min (v.date) as date from vitals v
       JOIN master_list m
       ON v.vital=\"BMI\"
       and m.t3 is not null
       and m.mrn = v.mrn
       where julianday( v.date ) between 
       julianday(m.t1) - 30 AND julianday(m.t3) + 30
       AND value is not null
       GROUP BY v.mrn
       ")

q3 <- ("SELECT q1.mrn, q1.value - q2.value as bmi_difference 
       from (%s) q1
       join (%s) q2
       on q1.mrn = q2.mrn;
       ")

q3 <- sprintf(q3,q1,q2)

a <- dbGetQuery(con,q3)
mean(a$bmi_difference)
hist(a$bmi_difference)
a['lost_weight'] = a$bmi_difference < 0

qq1 <- ("select m.mrn, CASE 
        WHEN p.tpt=1 AND p.response >=1 THEN 1
        WHEN p.tpt=2 AND p.response >=1 THEN 1
        WHEN p.tpt=3 AND p.response >=1 THEN 1
        ELSE 0 END symptom 
        from psych p
        join master_list m
        on p.psych_id = m.psych_id
        where p.qa = \"FACT\"
        and p.symptom = \"Wt Loss\"
        group by m.mrn;
        ")

qq2 <- ("select m.mrn, CASE 
        WHEN p.tpt=1 AND p.response >=0 THEN 1
        WHEN p.tpt=2 AND p.response >=0 THEN 1
        WHEN p.tpt=3 AND p.response >=0 THEN 1
        ELSE 0 END symptom 
        from psych p
        join master_list m
        on p.psych_id = m.psych_id
        where p.qa = \"MSAS\"
        and p.symptom = \"Wt Loss\"
        group by m.mrn;
        ")

aa1 <- dbGetQuery(con,qq1)
aa2 <- dbGetQuery(con,qq2)
aa <- merge( aa1,aa2, by='mrn',suffixes=c('FACT','MSAS') )
aa$symptom = ( (aa$symptomFACT)==1  | (aa$symptomMSAS==1) )
aaa <- dbGetQuery(con,qqq)

j <- merge(aa,a)
f <- j[j$symptomFACT==FALSE,]$bmi_difference
t <- j[j$symptomFACT==TRUE,]$bmi_difference

fx = rnorm(length(f),0,0.02)
tx = rnorm(length(t),1,0.02)
X = c(fx,tx)
Y <- c(f,t)
plot(X,Y,bg='black',pch=21,xlim=c(-0.5,1.5))

boxplot(f,t)
t.test(f,t)

yy<- sum ( (j$lost_weight==TRUE) & (j$symptom==1 ) )
yn<- sum ( (j$lost_weight==TRUE) & (j$symptom==0 ) )
ny<- sum ( (j$lost_weight==FALSE) & (j$symptom==1 ) )
nn<- sum ( (j$lost_weight==FALSE) & (j$symptom==0 ) )

totag <- (yy+nn)/(yy+nn+ny+yn)
posag <- (yy)/(yy+ny+yn)
negag <- (nn)/(nn+yn+ny)

po = (yy + nn) / (yy + yn + nn + ny)
my = (yy + yn) * (yy + ny) / (yy + yn + nn + ny)
mn = (nn + yn) * (nn + ny) / (yy + yn + nn + ny)
pe = (my + mn) / (yy + yn + nn + ny)
sens = yy / (yy+ny)
spec = yy / (yy+yn)
k= (po - pe) / (1 - pe)

jj <- merge(a,aaa)


plot(jj$bmi_difference,jj$symptom)
cor(jj$bmi_difference,jj$symptom)


##### Albumin vs Patient Reported Symptom Count MSAS

q <- ("SELECT l.mrn, avg(value) avg_alb FROM vitals l
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
## >= 0 for low cutoff; >= 1 for high cutoff

qq <- ("SELECT m.mrn, avg( ifnull( p.response, -1 ) >=1 ) symptom_count 
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
plot(j$avg_alb,j$symptom_count*8)
cor(j$avg_alb,j$symptom_count)

##### Albumin vs Patient Reported Symptom Count FACT

q <- ("SELECT l.mrn, avg(value) avg_alb FROM vitals l
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
qq <- ("SELECT m.mrn, avg( CASE 
       WHEN p.symptom in (\"Sleep\",\"Appetite\") THEN
       CASE WHEN ifnull(p.response,5) <= 3 THEN 1
       ELSE 0 END
       WHEN ifnull(p.response,-1) >= 1 THEN 1
       ELSE 0
       END) symptom_count 
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
plot(j$avg_alb,j$symptom_count*8)
cor(j$avg_alb,j$symptom_count)

###### Albumin Vs. Symptom count PE

symptoms <- c('Dizzy','Vomit','Constipation','Appetite','Fatigue','Nausea','Pain','Sleep','SOB','Cough','Wt Loss','Depression','Anxiety','Diarrhea','Hair Loss','Rash')
q <- ("SELECT l.mrn, avg(value) avg_alb FROM vitals l
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

qq <- ("SELECT mrn,avg(response) symptom_count 
       from pe
       where symptom != \"ECOG\"
       group by mrn
       ")
a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
plot(j$avg_alb,j$symptom_count)
cor(j$avg_alb,j$symptom_count)


###### Albumin Vs. ECOG FUCK THIS PLACE

symptoms <- c('Dizzy','Vomit','Constipation','Appetite','Fatigue','Nausea','Pain','Sleep','SOB','Cough','Wt Loss','Depression','Anxiety','Diarrhea','Hair Loss','Rash')
q <- ("SELECT l.mrn, avg(value) avg_alb FROM vitals l
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

qq <- ("SELECT mrn,max(response)>0 symptom_count 
       from pe
       where symptom = \"ECOG\"
       group by mrn
       ")
a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a
aa
j <- merge(a,aa)
boxplot(j[j$symptom_count==0 ,]$avg_alb, j[j$symptom_count==1 ,]$avg_alb)


######## ALBUMIN VS. ADVERSE EVENTS


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

FUCK <-  dbGetQuery(con,"Select Distinct mrn from adverse;")
mrn_s <- paste(FUCK[[1]], collapse = ',')

qq <- sprintf("SELECT mrn,avg(result) symptom_count 
              from adverse_breakout
              WHERE mrn in (%s)
              group by mrn
              ",mrn_s)

a <- dbGetQuery(con, q)
aa <- dbGetQuery(con, qq)
a <- a[a$avg_bmi>25 ,]
aa
j <- merge(a,aa)
plot(j$avg_bmi,j$symptom_count)
cor(j$avg_bmi,j$symptom_count)
