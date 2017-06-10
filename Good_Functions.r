
get_kappa_from_dataframes <- function(l1,l2,source1 = 'source1',source2 = 'source2') {

  ret = data.frame()
    
  a <- merge( l1,l2,by=c('mrn','symptom','tpt'),suffixes=c('1','2') )
  
  # source1 <- "ACL-MCL"
  # source2 <- "MFACT"
  col1 <- 'result1'
  col2 <- 'result2'
  
  symptoms <- unique(a$symptom)
  
  for (symptom in symptoms) {
    squares <- a[a$symptom==symptom,]
    squares$yy = (squares[col1]==1) & (squares[col2]==1)
    squares$nn = squares[col1]==0 & squares[col2]==0
    squares$yn = squares[col1]==1 & squares[col2]==0
    squares$ny = squares[col1]==0 & squares[col2]==1
    squares <- colSums(squares[c('yy','nn','yn','ny')])
    squares <- data.frame(yy=squares['yy'],
                          nn=squares['nn'],
                          yn=squares['yn'],
                          ny=squares['ny'])
    
    squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
    squares$k = (squares$po - squares$pe) / (1 - squares$pe)
    kslice<-squares[c('yy','nn','yn','ny','k')]
    kslice$ds1 = source1
    kslice$ds2 = source2
    kslice$symptom = symptom
    
    if (ncol(ret) == 0) {
      ret<-kslice
    } else {
      ret<-rbind(ret,kslice)
    }
  }
  return (ret) 
}