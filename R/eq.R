#' Insert an equation.
#'
#' @param eqs Character. The path to the equation text file, or a data frame of the equations.
#' @param number integer. The number of the equation.
#' @param label Character. The label of the equation.
#' @param style character. The style of the equation.
#' @param skip integer. the number of lines of the data file to skip before beginning to read data.
#'
#' @return A string of the equation.
#' @export
#' @examples
#' \dontrun{
#' eq()
#' }
eq <- function(eqs = NULL, label = NULL, number = NULL,
               style = c('numbered', 'display', 'inline', 'none'),
               skip = 6) {
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
#' \dontrun{
#' eq_file <- paste0(system.file(package = 'rosr'), '/skeleton/equation/rosr-eq.Rmd')
#' eqs <- read_eq(eq_file)
#' }
read_eq <- function(eqs, skip = 6){
  eqs <- read.table(eqs, skip = skip, sep = '|', header = TRUE,
                    stringsAsFactors = FALSE, encoding = 'UTF-8')
  eqs <- eqs[-1, c("number", "label", "description", "eq" )]
  # eqs$eq <- gsub('^[[:space:]]*[\\$]*', '', eqs$eq)
  # eqs$eq <- gsub('[\\$]*[[:space:]]*$', '', eqs$eq)
  eqs$eq <- rm_space(eqs$eq)
  eqs$number <- rm_space(eqs$number)
  eqs$label <- rm_space(eqs$label)
  return(eqs)
}
