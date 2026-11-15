base <- 'group/analysis'
learning <- 'group/R_Learning_Edition'
for (lang in c('English','Chinese')) {
 lines <- readLines(file.path(learning,paste0('Assignment3_Learning_',lang,'.R')),encoding='UTF-8')
 for (f in c(list.files(file.path(base,'R')), 'verify_results.R')) {
  a <- which(lines == paste('# BEGIN ORIGINAL MODULE:',f)); b <- which(lines == paste('# END ORIGINAL MODULE:',f))
  actual <- parse(text=lines[(a+1):(b-1)],keep.source=FALSE)
  original <- readLines(if(f=='verify_results.R') file.path(base,f) else file.path(base,'R',f),encoding='UTF-8')
  if(f=='01_data_eda.R') original[2] <- 'SOURCE <- normalizePath(SOURCE)'
  if(f=='verify_results.R') {
   original[1:5] <- ''
   original <- gsub('file.path(ROOT, "..", "Group_presentation.csv")','SOURCE',original,fixed=TRUE)
  }
  expected <- parse(text=original,keep.source=FALSE)
  stopifnot(identical(actual,expected))
  cat('PASS: unchanged executable module (declared path adaptations only):',lang,f,'\n')
 }
}
