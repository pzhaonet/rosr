#' Install dependencies
#'
#' @param server character. The server from where to install the dependencies.
#'
#' @return install packages
#' @export
#'
#' @examples
#' \donttest{
#' install_packages()
#' }
install_packages <- function(server = c('cran', 'github')){

  # from CRAN
  if('cran' %in% server){
    pkgs_cran <- c('beginr', 'rmd', 'tufte')
    lapply(pkgs_cran,
           function(i) {
             if(system.file(package = i) == '') {
               install.packages(i)
               message('The package ', i, ' has been installed.')
             }
           }
    )
  }

  # from CRAN
  if('github' %in% server){
    # from github
    pkgs_github <- c('bbucior/drposter')
    lapply(pkgs_github,
           function(x){
             if(system.file(package = strsplit(x, '/')[[1]][2]) == '') {
               devtools::install_github(x)
               message('The package ', x, ' has been installed.')
             }
           }
    )
  }
}

#' Display available sub projects
#'
#' @return character of the project names
#' @export
#'
#' @examples
#' sub_projects()
sub_projects <- function(){
  # don't change the name or sequence.
  c("Rproj", "bib", "data", "image", "R",
    "equation", "rpkg", 'mindmap',
    "manuscript", "poster", "slide",
    "book", "website")
}

#' Display available templates in a list
#'
#' @return a list of templates
#' @export
#'
#' @examples
#' template_ls()
template_ls <- function(){

  # rmarkdown templates
  packages <- c('rticles', 'drposter', 'pagedown', 'xaringan', 'rosr')
  templates_rmarkdown <- sapply(packages,
                                function(x)
                                  list.dirs(
                                    system.file(package = x, 'rmarkdown/templates'),
                                    full.names = FALSE,
                                    recursive = FALSE))

  # bookdownplus templates
  templates_bookdownplus <- list(bookdownplus =
                                   gsub('\\.zip', '',
                                        list.files(pattern = '\\.zip$',
                                                   system.file(package = 'bookdownplus', 'templates'),
                                                   full.names = FALSE)))
  # blogdown templates
  templates_blogdown <- list(blogdown = c('AlexFinn/simple-a', 'devcows/hugo-universal-theme',
                                          'gcushen/hugo-academic', 'jbub/ghostwriter',
                                          'kakawait/hugo-tranquilpeak-theme', 'kishaningithub/hugo-creative-portfolio-theme',
                                          'mattstratton/castanet', 'road2stat/hugo-tanka',
                                          'yihui/hugo-lithium', 'yihui/hugo-xmin'))

  # merge
  templates <- c(templates_rmarkdown,
                 templates_bookdownplus,
                 templates_blogdown)
  return(templates)
}

#' Display available templates in a data frame
#'
#' @return a data frame of templates
#' @export
#'
#' @examples
#' templates()
templates <- function(){
  templates_list <- template_ls()
  package <- rep(names(templates_list), sapply(templates_list, length))
  template <- unlist(template_ls())
  template_df <- data.frame(package = package, templates = template, stringsAsFactors = FALSE, row.names = NULL)
  template_df$sub_project <- template_df$templates
  template_df$sub_project[template_df$package == 'rticles'] <- 'manuscript'
  template_df$sub_project[template_df$package == 'xaringan' | template_df$package == 'rosr'] <- 'slide'
  template_df$sub_project[template_df$package == 'drposter'] <- 'poster'
  template_df$sub_project[template_df$package == 'bookdownplus'] <- 'book'
  template_df$sub_project[grepl('poster', template_df$templates)] <- 'poster'
  template_df$sub_project[template_df$package == 'blogdown'] <- 'website'
  template_df$sub_project[template_df$package == 'pagedown' &
                            template_df$template == 'html-paged'] <- 'book'
  return(template_df)
}

