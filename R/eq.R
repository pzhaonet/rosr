#' Insert an equation.
#'
#' @param eqs Character. The path to the equation text file, or a data frame of the equations.
#' @param number integer. The number of the equation.
#' @param label Character. The label of the equation.
#' @param style character. The style of the equation.
#' @param skip integer. the number of lines of the data file to skip before beginning to read data.
#' @param if_copy logical. Whether copy the equation into the clipboard.
#'
#' @return A string of the equation.
#' @importFrom clipr clipr_available write_clip
#' @export
#' @examples
#' eq()
eq <- function(eqs = NULL, label = NULL, number = NULL,
               style = c('numbered', 'display', 'inline', 'none'),
               skip = 6, if_copy = TRUE) {
  if(is.null(eqs)) return(message('A source file of the equations must be give.'))
  if(is.null(number) & is.null(label)) return(message('A number or a label of the equation must be given.'))
  style <- match.arg(style)
  latex_label <- ifelse(is.null(label), number, label)
  before <- switch (style,
                    numbered = paste0('\\begin{equation}\n'),
                    display = '$$',
                    inline = '$',
                    none = NULL
  )
  after <- switch (style,
                   numbered = paste0('\n  (\\#eq:', latex_label, ')\n\\end{equation}'),
                   display = '$$',
                   inline = '$',
                   none = NULL
  )

  if(!is.data.frame(eqs)) eqs <- read_eq(eqs, skip)
  eq_txt <- ifelse(is.null(number), eqs$eq[eqs$label == label], eqs$eq[eqs$n == number])
  cat(before, eq_txt, after, sep = '')
  if(if_copy){
    if (clipr::clipr_available()) {
      out_lines <- paste('$$', eq_txt, '$$')
      clipr::write_clip(out_lines)
    } else {
      message('Clipboard is unavailable.')
    }
  }
}

#' Read equations from a file
#'
#' @param eqs character. The path of the equation file.
#' @param skip integer. the number of lines of the data file to skip before beginning to read data.
#'
#' @return a data frame with equations
#' @export
#'
#' @examples
#' eq_file <- file.path(system.file(package = 'rosr'), 'skeleton/equation/rosr-eq.Rmd')
#' eqs <- read_eq(eq_file)
read_eq <- function(eqs, skip = 6){
  eqs <- readLines(eqs, encoding = 'UTF-8')
  eqs <- eqs[-c(1:(skip + 2))]
  eqs <- gsub('[[:space:]]*\\|[[:space:]]*$', '', eqs)
  eqs_split <- strsplit(eqs, '\\|', )
  eqs_df <- data.frame(number = rm_space(sapply(eqs_split, function(x) x[2])), stringsAsFactors = FALSE)
  eqs_df$label <- rm_space(sapply(eqs_split, function(x) x[3]))
  eqs_df$description <- rm_space(sapply(eqs_split, function(x) x[4]))
  eqs_df$eq <- sapply(eqs, function(x) gsub('^.*\\${2}(.+)\\${2}$', '\\1', x))
  # eqs_df$eq <- sapply(eqs, function(x) gsub('.*\\$+([^\\$]+)\\$+.*$', '\\1', x))
  # myenv <- list2env(setNames(as.list(eqs_df), zh))
  return(eqs_df)
  # return(myenv)
}

#' Convert a MathML equation for Microsoft Word
#'
#' @param connection 	a connection object or a character string.
#'
#' @details Copy the MathML codes of an equation into your clipboard, and run eq_word(), then paste from  your clipboard to MS Word.
#' @return a Word equation
#' @export
#'
#' @examples
#' # Copy the MathML codes of an equation into your clipboard.
#' eqw('test')
#' # Now paste to MS Word.
eqw <- function(connection = 'clipboard'){
  if(is.na(connection) | (connection != 'clipboard' & !file.exists(connection))) return(message('connection must be "clipboard" or a valid file path.'))
  mathml <- readLines(connection, encoding = 'UTF-8', warn = FALSE)
  writeLines(c('<?xml version="1.0"?>',
               mathml,
               ''),
             'clipboard',
             useBytes = TRUE)
  message('Now you can paste the equation in MS Word.')
}


#' Insert an image in the 'image/' directory.
#'
#' @importFrom knitr include_graphics
#' @param filename The file name of the image.
#'
#' @return Insert an image.
#' @export
fig <- function(filename) {
  knitr::include_graphics(file.path(getwd(), 'image', filename))
}
