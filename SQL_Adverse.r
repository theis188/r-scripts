con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

sub <- sprintf("SELECT 
ae.mrn, ae.symptom, ae.tpt, ae.result adv, (r.response='Y') phys
FROM adverse_breakout ae
JOIN ros r
on ae.mrn = r.mrn
and ae.tpt = r.tpt
and ae.symptom = r.symptom
",qqq)

qqqq <- sprintf("CREATE VIEW adverse_breakout AS
              %s;",qqq)

dbExecute(con, qqqq)

totag_s <- ("SELECT
  sub.symptom,
            sum(sub.phys==sub.adv)*1.0/count(*) totag
            from (%s) sub
            group by sub.symptom;")

posag_s <- ("SELECT
            sub.symptom,
            sum(sub.phys==sub.adv)*1.0/count(*) posag
            from (%s) sub
            where (sub.phys==1) OR (sub.adv==1)
            group by sub.symptom;")

negag_s <- ("SELECT
            sub.symptom,
            sum(sub.phys==sub.adv)*1.0/count(*) negag
            from (%s) sub
            where (sub.phys==0) OR (sub.adv==0)
            group by sub.symptom;")

aecount_s <- ("SELECT
               sub.symptom,
               sum(sub.adv) patcount
               from (%s) sub
               group by sub.symptom;")

phycount_s <- ("SELECT
               sub.symptom,
               sum(sub.phys) phycount
               from (%s) sub
               group by sub.symptom;")

totcount_s <- ("SELECT
               sub.symptom,
               count(*) totcount
               from (%s) sub
               group by sub.symptom;")

squares_s <- ("SELECT
              sub.symptom,
              sum( (sub.adv == 1) and (sub.phys==1) ) yy,
              sum( (sub.adv == 1) and (sub.phys==0) ) yn,
              sum( (sub.adv == 0) and (sub.phys==1) ) ny,
              sum( (sub.adv == 0) and (sub.phys==0) ) nn
              from (%s) sub
              group by sub.symptom;")

negag_q <- sprintf(negag_s,sub)
posag_q <- sprintf(posag_s,sub)
totag_q <- sprintf(totag_s,sub)
aecount_q <- sprintf(aecount_s,sub)
phycount_q <- sprintf(phycount_s,sub)
totcount_q <- sprintf(totcount_s,sub)
squares_q <-sprintf(squares_s,sub)

negag <- dbGetQuery(con, negag_q)
posag <- dbGetQuery(con, posag_q)
totag <- dbGetQuery(con, totag_q)
aecount <- dbGetQuery(con, aecount_q)
phycount <- dbGetQuery(con, phycount_q)
totcount <- dbGetQuery(con, totcount_q)
squares <- dbGetQuery(con, squares_q)

squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
squares$k = (squares$po - squares$pe) / (1 - squares$pe)
kslice = kslice <- squares[c("sub.symptom","k")]

Reduce(function(...) merge(..., all=TRUE), list(negag, posag, totag, aecount, phycount, totcount, kslice))

