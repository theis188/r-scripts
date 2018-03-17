
source("D:/R/RScripts/Hamhoon/SQL_Adverse.r") #AdPhy
source("D:/R/RScripts/Hamhoon/SQL_Pat_Phy.r") #PatPhy
source("D:/R/RScripts/Hamhoon/SQL_Adverse_Psych.r") #AdPat


li=list()
qli=list()
tli=list()

for (i in list( c("MSAS","2","0"),c("FACT","2","2") ) ) { ## HIGH CUTOFF
  
  qa = i[1]
  lim = i[2]
  flim = i[3]

  qli[[qa]] <- sprintf("CASE WHEN '%s' == 'MSAS'
  THEN CASE
  WHEN p%s.response IS NULL THEN 0
  WHEN p%s.response >= %s THEN 1
  ELSE 0 
  END 
  ELSE CASE 
  WHEN p%s.response IS NULL THEN 0
  WHEN p%s.symptom IN ('Sleep','Appetite') THEN CASE
  WHEN p%s.response <= %s THEN 1
  ELSE 0
  END
  WHEN p%s.response >= %s THEN 1
  ELSE 0
  END
  END", qa,qa,qa,lim,qa,qa,qa,flim,qa,lim)
  
  tli[[qa]]<-sprintf("select * from psych where qa = '%s'",qa)
}
  
  qa1 <- 'MSAS'
  qa2 <- 'FACT'
  
  q <- sprintf(
  "SELECT 
   pMSAS.tpt,
   pMSAS.psych_id,
   pMSAS.symptom,
   (%s) patientMSAS,
   (%s) patientFACT
   from (%s) pMSAS
   JOIN (%s) pFACT
   on pMSAS.psych_id = pFACT.psych_id
   and pMSAS.symptom = pFACT.symptom
   and pMSAS.tpt = pFACT.tpt"
   ,qli['MSAS'],qli['FACT'],tli['MSAS'],tli['FACT'] )
 
  # a <- dbGetQuery(con, q)
  
  squares_s <- ("SELECT
                sub.symptom,
              sum( (sub.patientMSAS == 1) and (sub.patientFACT==1) ) yy,
              sum( (sub.patientMSAS == 1) and (sub.patientFACT==0) ) yn,
              sum( (sub.patientMSAS == 0) and (sub.patientFACT==1) ) ny,
              sum( (sub.patientMSAS == 0) and (sub.patientFACT==0) ) nn
              from (%s) sub
              group by sub.symptom;")
  
  squares_q <-sprintf(squares_s,q)
  
  squares <- dbGetQuery(con, squares_q)
  
  squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
  kstr <- paste("k","MSASFACT",sep="")
  squares[c( kstr )] = (squares$po - squares$pe) / (1 - squares$pe)
  MSASFACT <- squares[c("sub.symptom",kstr)]
  


names(AdPhy)[names(AdPhy) == 'k'] <- 'kAdversePhysician'
names(PatPhy)[names(PatPhy) == 'kMSAS'] <- 'kMSASPhysician'
names(PatPhy)[names(PatPhy) == 'kFACT'] <- 'kFACTPhysician'
names(AdPat)[names(AdPat) == 'kMSAS'] <- 'kMSASAdverse'
names(AdPat)[names(AdPat) == 'kFACT'] <- 'kFACTAdverse'

Many.K.Values <- Reduce(function(...) merge(..., all=TRUE), 
                list(
                  AdPhy[,c('sub.symptom','kAdversePhysician')],
                  PatPhy[,c('sub.symptom','kMSASPhysician','kFACTPhysician')],
                  AdPat[,c('sub.symptom','kMSASAdverse','kFACTAdverse')],
                  MSASFACT[,c('sub.symptom','kMSASFACT')]
                ))

write.xlsx(Many.K.Values,'D:/R/Tables/Redo/kappa table.xlsx')

