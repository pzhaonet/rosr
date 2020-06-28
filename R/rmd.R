#' rosr pdf template
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
pdf_template <- function(...,
                         template_name,
                         keep_tex = TRUE,
                         citation_package = "natbib",
                         md_extensions = c(
                           "-autolink_bare_uris", # disables automatic links, needed for plain email in \correspondence
                           "-auto_identifiers"    # disables \hypertarget commands
                         )) {
  template <- system.file("rmarkdown", "templates", template_name, "resources", "template.tex", package = "rosr")
  if (!rmarkdown::pandoc_available()) {
    return("Pandoc is required.")
  } else {
    fmt <- rmarkdown::pdf_document(..., template = template, keep_tex = keep_tex)
    fmt$inherits <- "pdf_document"
    return(fmt)
  }
}

#' @rdname pdf_template
#' @export
statement_svm <- function(...){
  pdf_template(template_name = "statement_svm", ...)
}

#' @rdname pdf_template
#' @export
article_svm <- function(...){
  pdf_template(template_name = "article_svm", ...)
}

#' @rdname pdf_template
#' @export
cv_svm <- function(...){
  pdf_template(template_name = "cv_svm", ...)
}

#' @rdname pdf_template
#' @export
syllabus_svm <- function(...){
  pdf_template(template_name = "syllabus_svm", ...)
}

#' @rdname pdf_template
#' @export
manuscript_svm <- function(...){
  pdf_template(template_name = "manuscript_svm", ...)
}

#' @rdname pdf_template
#' @export
letter_moderncv <- function(...){
  pdf_template(template_name = "letter_moderncv", ...)
}

#' @rdname pdf_template
#' @export
beamer <- function(...){
  pdf_template(template_name = "beamer", ...)
}
