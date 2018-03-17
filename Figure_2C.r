con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")


q1 <- ("SELECT v.mrn, v.value, max(v.date) as date from vitals v
       JOIN master_list m
       ON v.vital=\"Weight\"
       and m.t3 is not null
       and m.mrn = v.mrn
       where julianday( v.date ) < 
       julianday(m.t1) - 45
       AND v.value is not null
       GROUP BY v.mrn
       ")

q2 <- ("SELECT v.mrn, v.value, min (v.date) as date from vitals v
       JOIN master_list m
       ON v.vital=\"Weight\"
       and m.t3 is not null
       and m.mrn = v.mrn
       where julianday( v.date ) >
       julianday(m.t3) + 45
       AND v.value is not null
       GROUP BY v.mrn
       ")

q3 <- ("SELECT q1.mrn, q2.value - q1.value as weight_difference 
       from (%s) q1
       join (%s) q2
       on q1.mrn = q2.mrn;
       ")

q3 <- sprintf(q3,q1,q2)

a <- dbGetQuery(con,q3)
mean(a$weight_difference)
hist(a$weight_difference)
a['lost_weight'] = a$weight_difference < 0

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

j <- merge(aa,a)
f <- j[j$symptomFACT==FALSE,]$weight_difference
t <- j[j$symptomFACT==TRUE,]$weight_difference

fx = rnorm(length(f),0,0.03)
tx = rnorm(length(t),1,0.03)
X = c(fx,tx)
Y <- c(f,t)

boxplot(f,t, boxcol = 'white', outcol='white',
        xlab="", ylab="Weight Change (lbs.)")
points(1+X,Y,bg='black',pch=21,xlim=c(-0.5,1.5))
axis(1, labels = c('Weight Loss\nReported Negative','Weight Loss\nReported Positive') , at=c(1,2),
    padj = 0.5    )
t.test(f,t)
t.test(f,tt)
length(f)
length(t)


