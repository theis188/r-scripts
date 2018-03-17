con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")
source("D:/R/RScripts/Hamhoon/Good_Functions.r")

d <- data.frame()

depress_q <- ("select m.mrn, d.tpt, d.symptom, 
(CASE when d.symptom=\"Depression\" THEN d.value>16 
ELSE d.value>33 END) result
FROM depress d
JOIN master_list m
ON m.psych_id = d.psych_id
WHERE d.symptom in ('Depression','Anxiety');
")

depress <- dbGetQuery(con, depress_q)
  
ros_q <- ("select mrn, tpt, symptom,
response='Y' result
from ros
WHERE symptom in ('Depression','Anxiety');")
  
ros <- dbGetQuery(con, ros_q)

adverse_q <- ("select mrn, tpt, symptom, 
result from adverse_breakout
where symptom in ('Depression','Anxiety');")

adverse <- dbGetQuery(con, adverse_q)

qli <- list()
qli[['ros']] <- ros
qli[['depress']] <- depress
qli[['adverse']] <- adverse

for (combo in list(c('ros','depress'),
                   c('ros','adverse'),
                   c('depress','adverse') ) ) {
    source1=combo[1]
    source2=combo[2]
    l1 <- qli[[source1]]
    l2 <- qli[[source2]]
    
    next_rows <- get_kappa_from_dataframes(l1,l2,source1=source1,source2=source2)
    
    if (ncol(d)==0) {
      d <- next_rows
    } else {
      d <- rbind(d,next_rows)
    }
  }


write.xlsx( d , "D:/R/Tables/Redo/depress table.xlsx" )



# for (i in list( c('33','Anxiety'), c('16','Depression') ) ) {
#   
#   lim <- i[1]
#   sym <- i[2]
#   q <- ("SELECT m.mrn, d.tpt, r.response=\"Y\" ros, 
#         d.value>=%s depress, a.result adverse
#         from depress d
#         JOIN master_list m
#         ON d.psych_id = m.psych_id
#         JOIN ros r
#         ON r.symptom =\"%s\"
#         AND m.mrn = r.mrn
#         AND r.tpt = d.tpt
#         JOIN adverse_breakout a
#         ON a.symptom =\"%s\"
#         AND a.mrn = m.mrn
#         AND d.tpt = a.tpt
#         WHERE d.symptom=\"%s\"
#   ")
#   q <- sprintf(q,lim,sym,sym,sym)
# 
#   df <- dbGetQuery(con, q)
#   
#   for (combo in list(c('ros','depress'),
#                      c('ros','adverse'),
#                      c('depress','adverse') ) ) {
#     squares <- df[combo]
#     l1 <- combo[1]
#     l2 <- combo[2]
#     squares$yy = squares[l1]==1 & squares[l2]==1
#     squares$nn = squares[l1]==0 & squares[l2]==0
#     squares$yn = squares[l1]==1 & squares[l2]==0
#     squares$ny = squares[l1]==0 & squares[l2]==1
#     
#     squares <- colSums(squares[c('yy','nn','yn','ny')])
#     squares <- data.frame(yy=squares['yy'],
#                           nn=squares['nn'],
#                           yn=squares['yn'],
#                           ny=squares['ny'])
#       
#     squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
#     squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
#     squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
#     squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
#     squares$mc = (squares$yn - squares$ny)^2 / (squares$yn + squares$ny)
#     squares$mcp = 1-pchisq(squares$mc, df=1)
#     squares$k = (squares$po - squares$pe) / (1 - squares$pe)
#     kslice<-squares[c('yy','nn','yn','ny','k','mcp')]
#     kslice$ds1 = l1
#     kslice$ds2 = l2
#     kslice$symptom = sym
#     if (ncol(d) == 0) {
#       d<-kslice
#     } else {
#       d<-rbind(d,kslice)
#     }
#   }
# }






