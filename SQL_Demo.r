con <- dbConnect(RSQLite::SQLite(), "D:/R/sqldb.db")

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Gender\"
GROUP BY value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- a

q <- (" SELECT value from demo
WHERE characteristic = \"Age\"
;")

a <- dbGetQuery(con, q)
b <- rbind(b, 'median age',median(as.numeric( a$value ) ) )
b <- rbind(b, 'age range',range(as.numeric( a$value )) )


q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Race\"
GROUP BY value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )


q <- (" SELECT value from demo
WHERE characteristic = \"Education\"
AND value != \"0\"
;")

a <- dbGetQuery(con, q)

b <- rbind(b, 'median educ',median(as.numeric( a$value ) ) )
b <- rbind(b, 'educ range',range(as.numeric( a$value )) )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Histology\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Type.of.NSCLC\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Stage.\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Smoking.Status\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

q <- (" SELECT value from demo
WHERE characteristic = \"Pack.years.\"
;")

a <- dbGetQuery(con, q)
b <- rbind(b, 'mean pkyr',mean(as.numeric( a$value ) ) )
b <- rbind(b, 'sd pkyr',sd(as.numeric( a$value )) )
hist( as.numeric( a$value ) )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"Type.of.Treatment\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

q <- (" SELECT value, count(value) count from demo
WHERE characteristic = \"ECOG\"
group by value
;")

a <- dbGetQuery(con, q)
a$pct <- a$`count`/sum( a$`count` )
b <- rbind(b,a )

write.xlsx(b,'D:/R/RScripts/Hamhoon/demo table.xlsx')

q <- ("select count(distinct a.mrn)
from adverse a
join master_list m
on a.mrn = m.mrn
;")
a <- dbGetQuery(con, q)
a
