con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

xlims <- list()

xlims[['Albumin']] <- c(3.0,4.5)

xlims[['BMI']] <- c(15,45)

xlims[['Hemoglobin']] <- c(8,16)

q <- list()

q[["ros"]] <- ("SELECT mrn,avg(response=\"Y\")*16 symptom_count 
               from ros
               group by mrn
               ")
## >= 2,2 for high cutoff - 3,1 for low
q[["psychFACT"]] <- ("SELECT m.mrn, avg( CASE 
                 WHEN p.symptom in (\"Sleep\",\"Appetite\") THEN
                 CASE WHEN ifnull(p.response,5) <= 3 THEN 1
                 ELSE 0 END
                 WHEN ifnull(p.response,-1) >= 1 THEN 1
                 ELSE 0
                 END)*8 symptom_count 
                 from psych p
                 join master_list m
                 ON p.qa = \"FACT\"
                 AND p.psych_id = m.psych_id
                 group by m.mrn
                 ")

q[["psychMSAS"]] <- ("SELECT m.mrn, avg( ifnull( p.response, -1 ) >=1 )*8 symptom_count 
                     from psych p
                     join master_list m
                     ON p.qa = \"MSAS\"
                     AND p.psych_id = m.psych_id
                     group by m.mrn
                     ")

q[["adverse"]] <- ("SELECT mrn, avg(result)*16 symptom_count 
                   from adverse_breakout
                   group by mrn")

q[["Albumin"]] <- ("SELECT l.mrn, avg(value) avg_value FROM labs l
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

q[["BMI"]] <- ("SELECT v.mrn, avg(value) avg_value FROM vitals v
              JOIN master_list m
              ON m.mrn = v.mrn
              WHERE julianday(v.date) BETWEEN
              julianday(m.t1) - 30 AND
              CASE WHEN m.t2 IS NULL THEN julianday(m.t1) + 30
              WHEN m.t3 IS NULL THEN julianday(m.t2) + 30
              ELSE julianday(m.t3) + 30
              END
              AND vital=\"BMI\"
              GROUP BY v.mrn
              ")

q[["Hemoglobin"]] <- ("SELECT l.mrn, avg(value) avg_value FROM labs l
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
par(mfrow=c(3,3))
for (vital in c('Albumin','BMI','Hemoglobin')) {
  for (source in c('ros','psychFACT','adverse')) {
    q1 <- q[[vital]]
    q2 <- q[[source]]
    a1 <- dbGetQuery(con, q1)
    a2 <- dbGetQuery(con, q2)
    j <- merge(a1, a2)
    model <- lm(symptom_count ~ avg_value , data = j)
    print(summary(model))
    slope <- summary(model)$coefficients[2,1]
    intercept <- summary(model)$coefficients[1,1]
    l1 <- paste("slope = ", as.character( round( slope , digits = 2 ) ), sep="" )
    l2 <- paste("p = ", as.character( round( summary(model)$coefficients[2,4], digits = 2 ) ), sep="" )
    xlabl <- paste(l1,l2,sep='\n')
    xliml <- xlims[[vital]]
    x1 <- xliml[1]*0.9 + xliml[2]*0.1
    x2 <- xliml[1]*0.1 + xliml[2]*0.9
    y1 <- x1*slope+intercept
    y2 <- x2*slope+intercept
    par(mar=c(4.1,3.1,2.1,1.1) )
    plot(j$avg_value,
         j$symptom_count, 
         pch=21,  
         bg="black",
         xlab=xlabl,
         ylab='',
         ylim=c(0,8),
         xlim= xliml)
    lines(c(x1,x2),c(y1,y2),col="black")
  }
}
