
source('D:/R/Rscripts/Hamhoon/Good_Functions.r')

df = read.xlsx( 'D:/R/Tables/Redo/patient physician table.xlsx', sheetName = 'Sheet1' )

yy = round( df$patcountMSAS + df$phycountMSAS - (df$totcountMSAS*(1-(df$totagMSAS))) )/2
MSAS = data.frame(yy=yy)
MSAS$symptom = df$sub.symptom
MSAS = MSAS[,c(2,1)]
MSAS$yn = df$patcountMSAS - MSAS$yy
MSAS$ny = df$phycountMSAS - MSAS$yy
MSAS$nn = df$totcountMSAS - (MSAS$yy+MSAS$yn+MSAS$ny)

yy = round( df$patcountFACT + df$phycountFACT - (df$totcountFACT*(1-(df$totagFACT))) )/2
FACT = data.frame(yy=yy)
FACT$symptom = df$sub.symptom
FACT = FACT[,c(2,1)]
FACT$yn = df$patcountFACT - FACT$yy
FACT$ny = df$phycountFACT - FACT$yy
FACT$nn = df$totcountFACT - (FACT$yy+FACT$yn+FACT$ny)

ret<-data.frame()
n_tot<-1000

for (symptom in MSAS$symptom) {
  tmp1 <- MSAS[MSAS$symptom==symptom,]
  tmp2 <- FACT[FACT$symptom==symptom,]
  if ( is.na( tmp1$yy ) || 
       is.na( tmp2$yy ) ) {
    next
  }
  df1 = tmp1[,c('yy','yn','ny','nn')]
  df2 = tmp2[,c('yy','yn','ny','nn')]
  k1 <- get_one_kappa_from_yn_squares(df1)$k
  k2 <- get_one_kappa_from_yn_squares(df2)$k
  k_diffs <- bootstrape_kappus(df1,df2,n_tot = n_tot)
  sorted_ks <- sort(k_diffs)
  fifth <- sorted_ks[round(n_tot*0.05)]
  ninetyfifth <- sorted_ks[round(n_tot*0.95)]
  tmpdf <- data.frame(symptom=symptom,kMSAS=k1,kFACT=k2,MSASmFACT5th=fifth,MSASmFACT95th=ninetyfifth)
  if (nrow(ret)==0) {
    ret<-tmpdf
  }
  else {
    ret <- rbind(ret,tmpdf)
  }
}

write.xlsx(ret,'D:/R/Tables/Redo/patient physician table bootstrappas.xlsx')

##############################################

df1 = read.xlsx( 'D:/R/Tables/Redo/patient physician table.xlsx', sheetName = 'Sheet1' )
df2 = read.xlsx( 'D:/R/Tables/Redo/psych adverse table.xlsx', sheetName = 'Sheet1' )

yy = round( df1$patcountMSAS + df1$phycountMSAS - (df1$totcountMSAS*(1-(df1$totagMSAS))) )/2
phy = data.frame(yy=yy)
phy$symptom = df1$sub.symptom
phy = phy[,c(2,1)]
phy$yn = df1$patcountMSAS - phy$yy
phy$ny = df1$phycountMSAS - phy$yy
phy$nn = df1$totcountMSAS - (phy$yy+phy$yn+phy$ny)

yy = round( df2$patcountMSAS + df2$advcountMSAS - (df2$totcountMSAS*(1-(df2$totagMSAS))) )/2
adv = data.frame(yy=yy)
adv$symptom = df2$sub.symptom
adv = adv[,c(2,1)]
adv$yn = df2$patcountMSAS - adv$yy
adv$ny = df2$advcountMSAS - adv$yy
adv$nn = df2$totcountMSAS - (adv$yy+adv$yn+adv$ny)
ret<-data.frame()
n_tot<-1000

for (symptom in phy$symptom) {
  tmp1 <- phy[phy$symptom==symptom,]
  tmp2 <- adv[adv$symptom==symptom,]
  if ( is.na( tmp1$yy ) || 
       is.na( tmp2$yy ) ) {
    next
  }
  df1 = tmp1[,c('yy','yn','ny','nn')]
  df2 = tmp2[,c('yy','yn','ny','nn')]
  k1 <- get_one_kappa_from_yn_squares(df1)$k
  k2 <- get_one_kappa_from_yn_squares(df2)$k
  k_diffs <- bootstrape_kappus(df1,df2,n_tot = n_tot)
  sorted_ks <- sort(k_diffs)
  fifth <- sorted_ks[round(n_tot*0.05)]
  ninetyfifth <- sorted_ks[round(n_tot*0.95)]
  tmpdf <- data.frame(symptom=symptom,kphy=k1,kadv=k2,MSASmFACT5th=fifth,MSASmFACT95th=ninetyfifth)
  if (nrow(ret)==0) {
    ret<-tmpdf
  }
  else {
    ret <- rbind(ret,tmpdf)
  }
}

write.xlsx(ret,'D:/R/Tables/Redo/adverse physician bootstrappas.xlsx',sheetName = 'MSAS')
df1 = read.xlsx( 'D:/R/Tables/Redo/patient physician table.xlsx', sheetName = 'Sheet1' )
df2 = read.xlsx( 'D:/R/Tables/Redo/psych adverse table.xlsx', sheetName = 'Sheet1' )


yy = round( df1$patcountFACT + df1$phycountFACT - (df1$totcountFACT*(1-(df1$totagFACT))) )/2
phy = data.frame(yy=yy)
phy$symptom = df1$sub.symptom
phy = phy[,c(2,1)]
phy$yn = df1$patcountFACT - phy$yy
phy$ny = df1$phycountFACT - phy$yy
phy$nn = df1$totcountFACT - (phy$yy+phy$yn+phy$ny)

yy = round( df2$patcountFACT + df2$advcountFACT - (df2$totcountFACT*(1-(df2$totagFACT))) )/2
adv = data.frame(yy=yy)
adv$symptom = df2$sub.symptom
adv = adv[,c(2,1)]
adv$yn = df2$patcountFACT - adv$yy
adv$ny = df2$advcountFACT - adv$yy
adv$nn = df2$totcountFACT - (adv$yy+adv$yn+adv$ny)
ret<-data.frame()
n_tot<-1000

for (symptom in phy$symptom) {
  tmp1 <- phy[phy$symptom==symptom,]
  tmp2 <- adv[adv$symptom==symptom,]
  if ( is.na( tmp1$yy ) || 
       is.na( tmp2$yy ) ) {
    next
  }
  df1 = tmp1[,c('yy','yn','ny','nn')]
  df2 = tmp2[,c('yy','yn','ny','nn')]
  k1 <- get_one_kappa_from_yn_squares(df1)$k
  k2 <- get_one_kappa_from_yn_squares(df2)$k
  k_diffs <- bootstrape_kappus(df1,df2,n_tot = n_tot)
  sorted_ks <- sort(k_diffs)
  fifth <- sorted_ks[round(n_tot*0.05)]
  ninetyfifth <- sorted_ks[round(n_tot*0.95)]
  tmpdf <- data.frame(symptom=symptom,kphy=k1,kadv=k2,MSASmFACT5th=fifth,MSASmFACT95th=ninetyfifth)
  if (nrow(ret)==0) {
    ret<-tmpdf
  }
  else {
    ret <- rbind(ret,tmpdf)
  }
}

write.xlsx(ret,'D:/R/Tables/Redo/adverse physician bootstrappas.xlsx',sheetName = 'FACT', append=TRUE)

df = read.xlsx( 'D:/R/Tables/Redo/depress table.xlsx', sheetName = 'Sheet1' )

li = list()
labels = list()
li[[1]]=c('ros','depress')
li[[2]]=c('ros','adverse')
li[[3]]=c('depress','adverse')
labels=list()
labels[['ros']]='EMR'
labels[['depress']]='PCL/CESD'
labels[['adverse']]='AE Log'

ret<-data.frame()
n_tot<-1000

for ( symptom in c('Anxiety','Depression') ) {
  tmpdf = df[ df$symptom==symptom, ]
  for (i in 1:3) {
    j <- i%%3 + 1
    tmp1 = tmpdf[i,]
    tmp2 = tmpdf[j,]
    slice1 <- tmp1[,c('yy','yn','ny','nn')]
    slice2 <- tmp2[,c('yy','yn','ny','nn')]
    k_diffs <- bootstrape_kappus(slice1,slice2,n_tot = n_tot)
    sorted_ks <- sort(k_diffs)
    fifth <- sorted_ks[round(n_tot*0.05)]
    ninetyfifth <- sorted_ks[round(n_tot*0.95)]
    atmpdf <- data.frame(symptom=symptom,
                        k1ds1=labels[[as.character(tmp1$ds1)]],
                        k1ds2=labels[[as.character(tmp1$ds2)]],
                        k2ds1=labels[[as.character(tmp2$ds1)]],
                        k2ds2=labels[[as.character(tmp2$ds2)]],
                        k1=tmp1$k,
                        k2=tmp2$k,
                        k1mk2fifth=fifth,
                        k1mk2ninetyfifth=ninetyfifth)
    if (nrow(ret)==0) {
      ret<-atmpdf
    } else {
      ret <- rbind(ret,atmpdf)
    }
  }
}
write.xlsx(ret,'D:/R/Tables/Redo/depress strappas.xlsx',sheetName = 'Depress', append=FALSE)

