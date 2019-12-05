#' rosr Rmd template
#'
#' @param template_name the template name.
#' @inheritParams rmarkdown::pdf_document
#' @param
#'   ..., keep_tex, citation_package, md_extensions
#'   Arguments passed to \code{rmarkdown::pdf_document()}.
#'
#' @return An R Markdown output format.
#' @details statement_svm, article_svm, cv_svm, syllabus_svm, manuscript_svm are adapted from \url{https://github.com/svmiller/svm-r-markdown-templates}.
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
  fmt <- rmarkdown::pdf_document(..., template = template, keep_tex = keep_tex)
  fmt$inherits <- "pdf_document"
  fmt
}

#' @rdname rmd_template
#' @export
statement_svm <- function(...){
  rmd_template(template_name = "statement_svm", ...)
}

#' @rdname rmd_template
#' @export
article_svm <- function(...){
  rmd_template(template_name = "article_svm", ...)
}

#' @rdname rmd_template
#' @export
cv_svm <- function(...){
  rmd_template(template_name = "cv_svm", ...)
}

#' @rdname rmd_template
#' @export
syllabus_svm <- function(...){
  rmd_template(template_name = "syllabus_svm", ...)
}

#' @rdname rmd_template
#' @export
manuscript_svm <- function(...){
  rmd_template(template_name = "manuscript_svm", ...)
}

#' @rdname rmd_template
#' @export
letter_moderncv <- function(...){
  rmd_template(template_name = "letter_moderncv", ...)
}

#' @rdname rmd_template
#' @export
beamer_zh <- function(...){
  rmd_template(template_name = "beamer_zh", ...)
}
