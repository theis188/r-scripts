
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


ddd <- ("SELECT distinct o.mrn id,o.symptom,m.rx,m.start_date rx_start, o.date symptom_start from other_patients o
        join meds m
        on m.mrn = o.mrn
        where ( (julianday(m.start_date) < julianday(o.date) + 30)
        and ( (m.end_date is null) or ( julianday( m.end_date ) > julianday(o.date) - 30 ) )  )
        ")





BLUMBY <- dbGetQuery(con, ddd)

write.xlsx(BLUMBY, "D:/R/Tables/BLUMBY.xlsx")






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
