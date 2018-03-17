library(rJava)
library(xlsxjars)
library(xlsx)

excel.data <- read.xlsx("C:/Users/Matthew/Desktop/Correlative Project/PsychEDCEMR/LWLC Data File for Collaboration JCK 113016.xlsx", sheetIndex = 1)

summary(excel.data)



as.double(levels(excel.data[1,3])[1])

as.Date(as.character(19000101), "%Y%m%d") - 2 + as.integer(levels(excel.data[1,5])[as.integer(excel.data[1,5])])

excel.data$FACT_ph_T1_1 + excel.data$FACT_ph_T2_1

ex=excel.data$FACT_ph_T1_1

exx=ex[ex=="NOT YET CHECKED"]
typeof(exx[1])
help(paste)

ex=data.frame(excel.data$FACT_ph_T1_1,excel.data$FACT_ph_T2_1)

labs=c("NOT YET CHECKED","NOT YET ENTERED","DISCONTINUED","IN PROGRESS")

for (row in rev(sequence(NROW(ex))) ){
  for (lab in labs) {
    if ((ex[row,1]==lab) || (ex[row,2]==lab)) {
      ex = ex[-row,]
    }
  }
}

ex$excel.data.FACT_ph_T1_1 <- factor(ex$excel.data.FACT_ph_T1_1)
ex$excel.data.FACT_ph_T2_1 <- factor(ex$excel.data.FACT_ph_T2_1)

ex$T1.num <- as.numeric(as.character(ex$excel.data.FACT_ph_T1_1))
ex$T2.num <- as.numeric(as.character(ex$excel.data.FACT_ph_T2_1))

hist(ex$T1.num)
hist(ex$T2.num)

cor(ex$T1.num,ex$T2.num)



factor(ex$FACT_ph_T1_1)

read.xls("G:/Correlative Project/PsychEDCEMR/Adverse Events.xlsx", sheetIndex = 1)

df<-read.table("C:/Users/Matthew/Desktop/custom.tsv",sep="\t", header=TRUE)

summary(df)

for (j in unique(df$Patient.MRN)) {
  print(c(j, length(df$Patient.MRN[df$Patient.MRN==j] ) ))
}
  

install.packages("rJava")
system("java -version")
