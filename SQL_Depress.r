con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

d <- data.frame()

for (i in list( c('33','Anxiety'), c('16','Depression') ) ) {
  
  lim <- i[1]
  sym <- i[2]
  q <- ("SELECT m.mrn, d.tpt, r.response=\"Y\" ros, 
        d.value>=%s depress, a.result adverse
        from depress d
        JOIN master_list m
        ON d.psych_id = m.psych_id
        JOIN ros r
        ON r.symptom =\"%s\"
        AND m.mrn = r.mrn
        AND r.tpt = d.tpt
        JOIN adverse_breakout a
        ON a.symptom =\"%s\"
        AND a.mrn = m.mrn
        AND d.tpt = a.tpt
        WHERE d.symptom=\"%s\"
  ")
  q <- sprintf(q,lim,sym,sym,sym)

  df <- dbGetQuery(con, q)
  
  for (combo in list(c('ros','depress'),
                     c('ros','adverse'),
                     c('depress','adverse') ) ) {
    squares <- df[combo]
    l1 <- combo[1]
    l2 <- combo[2]
    squares$yy = squares[l1]==1 & squares[l2]==1
    squares$nn = squares[l1]==0 & squares[l2]==0
    squares$yn = squares[l1]==1 & squares[l2]==0
    squares$ny = squares[l1]==0 & squares[l2]==1
    
    squares <- colSums(squares[c('yy','nn','yn','ny')])
    squares <- data.frame(yy=squares['yy'],
                          nn=squares['nn'],
                          yn=squares['yn'],
                          ny=squares['ny'])
      
    squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$mc = (squares$yn - squares$ny)^2 / (squares$yn + squares$ny)
    squares$mcp = 1-pchisq(squares$mc, df=1)
    squares$k = (squares$po - squares$pe) / (1 - squares$pe)
    kslice<-squares[c('yy','nn','yn','ny','k','mcp')]
    kslice$sub = paste(l1,l2)
    kslice$symptom = sym
    if (ncol(d) == 0) {
      d<-kslice
    } else {
      d<-rbind(d,kslice)
    }
    
  }
}
