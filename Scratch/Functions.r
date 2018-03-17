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