#' rosr Rmd template
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Additional arguments to \code{rmarkdown::pdf_document()}.
#'
#' @return An R Markdown output format.
#' @return An R Markdown output format.
#' @export
rmd_template <- function(...,
                         template_name,
                         keep_tex = TRUE,
                         citation_package = "natbib",
                         md_extensions = c(
                           "-autolink_bare_uris", # disables automatic links, needed for plain email in \correspondence
                           "-auto_identifiers"    # disables \hypertarget commands
                         )) {
  template <- system.file("rmarkdown", "templates", template_name, "resources", "template.tex", package = "rosr")
  fmt <- rmarkdown::pdf_document(..., template = template)
  fmt$inherits <- "pdf_document"
  fmt
}

#' SVM statement
#'
#' @param ...
#'
#' @details This was adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
#' @return An R Markdown output format.
#' @export
statement_svm <- function(...){
  rmd_template(template_name = "statement_svm")
}

#' SVM article
#'
#' @param ...
#'
#' @details This was adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
#' @return An R Markdown output format.
#' @export
article_svm <- function(...){
  rmd_template(template_name = "article_svm")
}

#' SVM cv
#'
#' @param ...
#'
#' @details This was adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
#' @return An R Markdown output format.
#' @export
cv_svm <- function(...){
  rmd_template(template_name = "cv_svm")
}

#' SVM syllabus
#'
#' @param ...
#'
#' @details This was adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
#' @return An R Markdown output format.
#' @export
syllabus_svm <- function(...){
  rmd_template(template_name = "syllabus_svm")
}

#' SVM manuscript
#'
#' @param ...
#'
#' @details This was adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
#' @return An R Markdown output format.
#' @export
manuscript_svm <- function(...){
  rmd_template(template_name = "manuscript_svm")
}


#' letter_moderncv
#'
#' @param ...
#'
#' @return An R Markdown output format.
#' @export
letter_moderncv <- function(...){
  rmd_template(template_name = "letter_moderncv")
}

