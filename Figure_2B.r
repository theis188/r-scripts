con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

df <- read.xlsx( "D:/R/Tables/AE vs Rx.xlsx", sheetIndex = 1 )[1:468,1:11][ ,c('mrn','dep','anx','inso') ]

q <- ("SELECT DISTINCT MRN from adverse_breakout;")

all_data <- dbGetQuery(con, q)
all_data$dep <- 0
all_data$anx <- 0
all_data$inso <- 0
  
maxes <- aggregate(df, list(df$mrn) ,max )
all_data[ all_data$mrn %in% maxes[ maxes$dep==1, 'mrn'], 'dep' ] <- 1
all_data[ all_data$mrn %in% maxes[ maxes$inso==1, 'mrn'], 'inso' ] <- 1
all_data[ all_data$mrn %in% maxes[ maxes$anx==1, 'mrn'], 'anx' ] <- 1

inso_q <- ('SELECT m.mrn, max(p.response >= 2) patient_inso 
    from psych p
    join master_list m
    on m.psych_id = p.psych_id
    where symptom = "Sleep"
    group by m.mrn;
')

inso_df <- dbGetQuery(con,inso_q)

dep_q <- ('
    SELECT m.mrn, max(d.value >= 16) patient_dep 
    from depress d
    join master_list m
    on m.psych_id = d.psych_id
    where symptom = "Depression"
    group by m.mrn;          
')

dep_df <- dbGetQuery(con,dep_q)

anx_q <- ('
    SELECT m.mrn, max(d.value >= 33) patient_anx 
          from depress d
          join master_list m
          on m.psych_id = d.psych_id
          where symptom = "Anxiety"
          group by m.mrn;          
          ')

anx_df <- dbGetQuery(con,anx_q)

ret <- Reduce(function(...) merge(... , all.x=TRUE ), list(all_data,inso_df,dep_df,anx_df) )

table ( ret[c('dep','patient_dep')] )
table ( ret[c('anx','patient_anx')] )
table ( ret[c('inso','patient_inso')] )

# dbExecute(con, "DROP TABLE meds")
# dbExecute(con, "CREATE TABLE meds
#           (mrn integer not null primary key,
#           psych_id integer not null,
#           t1 date,
#           t2 date,
#           t3 date)")
# 
# cols <- c("Patient.MRN","Pysch.ID","T1","T2","T3")
# 
# 
# for (i in 1:length(master.list [,1] ) ) {
#   b <- master.list[cols][i,]
#   s <- as.character( sapply(b,stringy) )
#   insert_str <- paste( s, collapse=",")
#   rs <- dbSendStatement(con, sprintf("INSERT INTO master_list VALUES (%s);",insert_str) )
#   dbClearResult(rs)
# }
