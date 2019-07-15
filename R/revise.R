#' Render a revised .rmd into pdf
#'
#' @param file the path of the .rmd file
#'
#' @return a pdf file in revison mode.
#' @export
#'
#' @examples
#' rosr::create_rmd(template = 'statement_svm', package = 'rosr')
#' rosr::render_revised('manuscript/statement_svm/statement_svm.Rmd')
render_revised <- function(file, clear = TRUE){
  newfile <- gsub('(\\.[^.]+)$', '_revised\\1', file)
  filetxt <- readLines(file, encoding = 'UTF-8')
  filetxt <- gsub('~~~([^~]+)~~~', '\\\\deleted[]{\\1}', filetxt)
  filetxt <- gsub('[\\+]{3}([^+]+)[\\+]{3}', '\\\\added[]{\\1}', filetxt)
  if(sum(grepl('^revise-package: ', filetxt)) == 0){
    loc <- grep('---', filetxt)[2]
    filetxt <- c(filetxt[1:(loc-1)], 'revise-package: changes',  filetxt[loc:length(filetxt)])
  }
  writeLines(filetxt, newfile, useBytes = TRUE)
  rmarkdown::render(newfile)
  if(clear){
    clear_files <- dir(path = dirname(file), basename(gsub('(\\.[^.]+)$', '_revised', file)), full.names = TRUE)
    unlink(clear_files[clear_files != paste0(gsub('(\\.[^.]+)$', '_revised', file), '.pdf')], recursive = TRUE)
  }
}
