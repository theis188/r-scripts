
dbExecute(con, "Drop table other_patients")

dbExecute(con,"CREATE TABLE other_patients
          (mrn text not null,
          symptom text not null,
          date date not null,
          primary key(mrn,symptom))
          ")



for (symptom in c("Anxiety","Insomnia")) {

  HORSE_SHIT = 
  read.xlsx("C:/Users/Matthew/Desktop/Correlative Project/PsychEDCEMR/R Data/Med prescribing.xlsx",
            sheetName = symptom)
  for (row in 1:nrow(HORSE_SHIT)) {
      mrn <- HORSE_SHIT$MRN[row]
      start <- as.Date(HORSE_SHIT$Start[row])
      rs <- try(dbSendStatement(con,sprintf( ("INSERT INTO other_patients values 
                            (\"%s\",\"%s\",\"%s\");"), mrn , symptom , start ) ) )
      if (class(rs) != 'try-error') {dbClearResult(rs)}
  }

}


ddd <- ("SELECT o.mrn id,o.symptom,m.rx,m.start_date rx_start, o.date symptom_start from other_patients o
        join meds m
        on m.mrn = o.mrn
        where ( (julianday(m.start_date) < julianday(o.date) + 30)
        and ( (m.end_date is null) or ( julianday( m.end_date ) > julianday(o.date) - 30 ) )  )
        ")



ddd <- ("SELECT DISTINCT a.mrn, dep.dep, anx.anx, inso.inso, m.rx, m.start_date
        from adverse_breakout a
        join
        (select mrn, max(result) dep from adverse_breakout where symptom=\"Depression\" group by mrn) dep
        on a.mrn = dep.mrn
        join
        (select mrn, max(result) anx from adverse_breakout where symptom=\"Anxiety\" group by mrn) anx
        on a.mrn = anx.mrn
        join        
        (select mrn, max(result) inso from adverse_breakout where symptom=\"Sleep\" group by mrn) inso
        on a.mrn = inso.mrn
        join meds m
        on m.mrn = a.mrn
        and ( (julianday(m.start_date) < julianday('2015-10-01') + 30)
        and ( (m.end_date is null) or ( julianday( m.end_date ) > julianday('2015-10-01') - 30 ) )  );
        ")

ddddddddd <- ("SELECT DISTINCT a.mrn, dep.dep, anx.anx, inso.inso
        from adverse_breakout a
              join
              (select mrn, max(result) dep from adverse_breakout where symptom=\"Depression\" group by mrn) dep
              on a.mrn = dep.mrn
              join
              (select mrn, max(result) anx from adverse_breakout where symptom=\"Anxiety\" group by mrn) anx
              on a.mrn = anx.mrn
              join        
              (select mrn, max(result) inso from adverse_breakout where symptom=\"Sleep\" group by mrn) inso
              on a.mrn = inso.mrn;
              ")


BLUMBY <- dbGetQuery(con, ddddddddd)
table(BLUMBY$dep)
table(BLUMBY$anx)
table(BLUMBY$inso)
write.xlsx(BLUMBY, "D:/R/Tables/HAM BLUMBY 2.xlsx")






q <- ("
SELECT mrn, tpt, max(response = \"Y\") anx from ros
where symptom=\"Anxiety\"
group by mrn;
")

q <- ("
SELECT mrn, tpt, max(response = \"Y\") anx from ros
where symptom=\"Anxiety\"
group by mrn;
")


ros_df <- dbGetQuery(con, q)

ANX_MEDZ <- ("(\"ALPRAZOLAM\",
\"CHLORDIAZEPOXIDE\",
\"CLONAZEPAM\",
\"DIAZEPAM\",
\"LORAZEPAM\",
\"BUSPIRONE\",
\"ESCITALOPRAM\",
\"FLUOXETINE\",
\"PAROXETINE\",
\"SERTRALINE\",
\"CLOMIPRAMINE\",
\"IMIPRAMINE\",
\"ISOCARBOXAZID\",
\"PHENELZINE\",
\"SELEGILINE\",
\"TRANYLCYPROMINE\",
)")

q <- ("
SELECT distinct mrn,rx from meds
where start_date > date(\"2015-11-01\")
")

qq <- sprintf(q,ANX_MEDZ)

df <- dbGetQuery(con, qq)
df

ros_df$anx_rx <- 0

ros_df$anx_rx <- 1*(ros_df$mrn %in% df$mrn)
