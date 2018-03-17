
get_one_kappa_from_yn_squares =function(squares) {
  squares$po = ( squares$yy + squares$nn ) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$my = (squares$yy + squares$yn) * (squares$yy + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$mn = (squares$nn + squares$yn) * (squares$nn + squares$ny) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$pe = (squares$my + squares$mn) / (squares$yy + squares$yn + squares$nn + squares$ny)
  squares$k = (squares$po - squares$pe) / (1 - squares$pe)
  kslice<-squares[c('yy','nn','yn','ny','k')]
  return ( kslice )
}

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
    
    kslice = get_one_kappa_from_yn_squares(squares)
    
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

get_yn_vec_to_sample_from = function(df) {
  ret = c()
  for (c in c('yy','nn','ny','yn')) {
    ret = c( ret , rep( c , each = df [ 1,c ] ) )
  }
  return (ret)
}

get_df_from_vec_sample = function(v) {
  ret = data.frame(yy=0, yn=0, ny=0, nn=0)
  ret$yy <- length(which(v=='yy'))
  ret$yn <- length(which(v=='yn'))
  ret$ny <- length(which(v=='ny'))
  ret$nn <- length(which(v=='nn'))
  return(ret)
}

bootstrape_kappus = function(df1,df2,n_tot = 200) {
  k_diffs = c()
  samp1 = get_yn_vec_to_sample_from(df1)
  len1 = length(samp1)
  samp2 = get_yn_vec_to_sample_from(df2)
  len2 = length(samp2)
  for (i in 1:n_tot) {
    tmp1 = sample(samp1, len1, replace=TRUE)
    tmp2 = sample(samp2, len2, replace=TRUE)
    tmpdf1 = get_df_from_vec_sample(tmp1)
    tmpdf2 = get_df_from_vec_sample(tmp2)
    k1 = get_one_kappa_from_yn_squares(tmpdf1)$k
    k2 = get_one_kappa_from_yn_squares(tmpdf2)$k
    k_diffs = c(k_diffs, c(k1-k2) )
  }
  return (k_diffs)
}

root = "C:/Users/Matthew/Desktop/Correlative Project/PsychEDCEMR/R Data/"

collapser <- function (df,coln,levout,levin) {
  df[,coln][df[,coln] %in% levout] <- levin
  df[,coln] <- factor( df[,coln] )
  return( df )
}

# collapes_iter <- (df,l) {
#  for
# }

get_sheet_names <- function(path) {
  dataIn<-loadWorkbook( path )
  sheet <- names( getSheets( dataIn ) )
  return(sheet)
}

get_all_sheets <- function(path) {
  sheets <- get_sheet_names(path)
  dfs = as.list( 1:length(sheets) )
  for (sheet in 1:length(sheets) ) {
    dfs[[sheet]] <- read.xlsx(path, sheetIndex=sheet)
  }
  return(dfs)
}