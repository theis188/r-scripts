source("D:/R/RScripts/Hamhoon/SQL_Depress_FACT.r")
source("D:/R/RScripts/Hamhoon/Good_Functions.r")

for (tpt in 1:3) {

MFACT <- FFinal ##### I FEEEL SADD NERBISS

OTHER_FUCKING_QUERY <- sprintf( ("SELECT d.symptom, m.mrn, d.tpt,
(CASE WHEN d.symptom=\"Depression\" THEN value>16
ELSE value>33 end) as result
from depress d
join master_list m
on m.psych_id = d.psych_id
where symptom in ('Anxiety','Depression')
and d.tpt = %s;  "), tpt)

 # = data.frame()
OTHER_RESULT <- dbGetQuery(con,OTHER_FUCKING_QUERY)

source1<-"MFACT"
source2<-"ACL-MCL"

VIGGA <-
  get_kappa_from_dataframes(MFACT,OTHER_RESULT,source1 = source1,source2 = source2)

app = if(tpt==1) FALSE else TRUE
sName = paste(  "TPT", as.character(tpt)  ,sep =""   )

write.xlsx( VIGGA , "D:/R/Tables/By TPT/ACLMCL_MFACT_Comparison.xlsx" , append = app, sheetName = sName )

  
}
