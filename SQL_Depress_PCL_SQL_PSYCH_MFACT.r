source("D:/R/RScripts/Hamhoon/SQL_Depress_FACT.r")
source("D:/R/RScripts/Hamhoon/Good_Functions.r")

# source("D:/R/RScripts/Hamhoon/SQL_Depress.r")

MFACT <- FFinal

OTHER_FUCKING_QUERY <- ("SELECT d.symptom, m.mrn, d.tpt,
(CASE WHEN d.symptom=\"Depression\" THEN value>16
ELSE value>33 end) as result
from depress d
join master_list m
on m.psych_id = d.psych_id
where symptom in ('Anxiety','Depression');  ")

 = data.frame()
OTHER_RESULT <- dbGetQuery(con,OTHER_FUCKING_QUERY)

source1<-"MFACT"
source2<-"ACL-MCL"

VIGGA <-
  get_kappa_from_dataframes(MFACT,OTHER_RESULT,source1 = source1,source2 = source2)


  # l1 <- OTHER_RESULT
  # l2 <- PSYCH_FUCKS
  # 
  # a <- merge( l1,l2,by=c('mrn','symptom','tpt'),suffixes=c('1','2') )
  # 
  # source1 <- "ACL-MCL"
  # source2 <- "MFACT"
  # col1 <- 'result1'
  # col2 <- 'result2'
  # 
  # for (symptom in c("Anxiety","Depression")) {
  #   squares <- a[a$symptom==symptom,]
  #   squares$yy = (squares[col1]==1) & (squares[col2]==1)
  #   squares$nn = squares[col1]==0 & squares[col2]==0
  #   squares$yn = squares[col1]==1 & squares[col2]==0
  #   squares$ny = squares[col1]==0 & squares[col2]==1
  #   squares <- colSums(squares[c('yy','nn','yn','ny')])
  #   squares <- data.frame(yy=squares['yy'],
  #                         nn=squares['nn'],
  #                         yn=squares['yn'],
  #                         ny=squares['ny'])
  #   
  #   squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
  #   squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  #   squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  #   squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
  #   squares$k = (squares$po - squares$pe) / (1 - squares$pe)
  #   kslice<-squares[c('yy','nn','yn','ny','k')]
  #   kslice$ds1 = source1
  #   kslice$ds2 = source2
  #   kslice$symptom = symptom
  #   
  #   if (ncol(VIGGA) == 0) {
  #     VIGGA<-kslice
  #   } else {
  #     VIGGA<-rbind(VIGGA,kslice)
  #   }
  #   
  # }

  write.xlsx( VIGGA , "D:/R/Tables/ACLMCL_MFACT_Comparison.xlsx" )
  