#' Create a new project or a new sub-project
#'
#' @param project Project name.
#' @param if_render Logical. Whether render the templates.
#' @param dest_dir Character. The destination directory of the new project.
#' @param overwrite Logical. Whether overwrite existing files.
#' @param sub_project Character. The type of the sub-project to create.
#'
#' @return a project folder
#' @export
#'
#' @examples
#' \dontrun{
#' rosr_new()
#' }
rosr_new <- function(project = 'test',
                   if_render = TRUE,
                   dest_dir = NA,
                   overwrite = FALSE,
                   sub_project = get_sub_project()){

  thiswd <- getwd()
  on.exit(setwd(thiswd))

  if(is.na(dest_dir)) dest_dir <- project
  if(dest_dir == '' | dest_dir == '.' | dest_dir == './') {
    dest_dir <- normalizePath(thiswd)
  }
  if(dir.exists(dest_dir) & !overwrite)
    return(message('The destination folder "', dest_dir,
                   '" already exists. Please either set another "dest_dir" so as to create a brand new project, or set "overwrite = TRUE" to overwrite existing files in the "', dest_dir, '" folder.'))

  if(!dir.exists(dest_dir)) dir.create(dest_dir)
  dest_dir <- normalizePath(dest_dir)
  setwd(dest_dir)
  # dest_dir <- getwd()

  #+ ## Rproj ----
  if('Rproj' %in% sub_project){
    rproj <- paste0(system.file(package = 'rosr'), '/skeleton/rproj')
    file.copy(rproj, paste0('-', project, '.Rproj'))
  }

  #+ ## bib ----
  newdir <- 'bib'
  if(newdir %in% sub_project | if_render){
    if(!dir.exists(newdir)) dir.create(newdir)
    bib_template <- paste0(system.file(package = 'rosr'), '/skeleton/bib/rosr.bib')
    file.copy(bib_template, paste0(newdir, '/rosr.bib'))
  }

  #+ ## data ----
  newdir <- 'data'
  if(newdir %in% sub_project | if_render){
    if(!dir.exists(newdir)) dir.create(newdir)
    write.csv(airquality, paste0(newdir, '/rosr.csv'), row.names = FALSE)
  }

  #+ ## image----
  newdir <- 'image'
  if(newdir %in% sub_project | if_render){
    if(!dir.exists(newdir)) dir.create(newdir)
  }

  #+ ## R----
  newdir <- 'R'
  if(newdir %in% sub_project | if_render){
    if(!dir.exists(newdir)) dir.create(newdir)
    r_template <- paste0(system.file(package = 'rosr'), '/skeleton/R/rosr.R')
    file.copy(r_template, paste0(newdir, '/rosr.R'))
  }

  #+ ## equation----
  newdir <- 'equation'
  if(newdir %in% sub_project | if_render){
    if(!dir.exists(newdir)) dir.create(newdir)
    r_template <- paste0(system.file(package = 'rosr'), '/skeleton/equation/rosr-eq.Rmd')
    file.copy(r_template, paste0(newdir, '/rosr-eq.Rmd'))
    if(if_render) rmarkdown::render(paste0(newdir, '/rosr-eq.Rmd'))
  }

  #+ ## rpkg----
  if('rpkg' %in% sub_project){
    mypath <- paste0(system.file(package = 'beginr'), "/zip/rpkg.zip")
    unzip(mypath)

  }

  #+ ## manuscript----
  newdir <- 'manuscript'
  if(newdir %in% sub_project){
    rmd_template ="copernicus_article"
    cp_rmd(rmd_template = rmd_template, if_render = if_render, to = newdir, package = "rticles",
           file_rmd = 'copernicus_article')
    file.remove(paste(newdir, rmd_template, c('sample.bib', 'Nikolaus_Kopernikus.jpg'), sep = '/'))
    preamble_file <- paste(system.file(package = 'rosr'), 'skeleton', newdir, rmd_template, 'preamble.tex', sep = '/')
    file.copy(preamble_file, paste0(newdir, '/copernicus_article'))
  }

  #+ ## poster----
  newdir <- 'poster'
  if(newdir %in% sub_project){
    rmd_template ="drposter"
    cp_rmd(rmd_template = rmd_template, if_render = if_render, to = newdir, package = "drposter", file_rmd = 'skeleton')
  }

  #+ ## slide----
  newdir <- 'slide'
  if(newdir %in% sub_project){
    rmd_template ="xaringan"
    cp_rmd(rmd_template = rmd_template, if_render = if_render, to = newdir, package = "xaringan", file_rmd = 'skeleton')
  }

  #+ ## book ----
  newdir <- 'book'
  if(newdir %in% sub_project){
    if(!dir.exists(newdir)) dir.create(newdir)
    bookdownplus::bookdownplus(template = 'demo', render = FALSE, to = newdir)
    oldbib <- paste0(newdir, '/bib.bib')
    if(file.exists(oldbib)) file.remove(oldbib)
    rmd_template <- 'book'
    my_template = paste(system.file(package = 'rosr'), 'skeleton', rmd_template, sep = '/')
    if(dir.exists(my_template)) file.copy(my_template, '.', recursive = TRUE, overwrite = TRUE)
    if(if_render) {
      bookdownplus::bookdownplus(render = TRUE, to = newdir, new = FALSE)
      setwd(dest_dir)
    }
  }

  #+ ## website----
  newdir <- 'website'
  if(newdir %in% sub_project){
    blogdown::new_site(newdir, serve = FALSE, sample = FALSE)
    rmd_template <- 'website'
    setwd(newdir)
    file.remove(dir('content/post/', full.names = TRUE))
    my_template = paste(system.file(package = 'rosr'), 'skeleton', rmd_template, 'content', sep = '/')
    if(dir.exists(my_template)) file.copy(my_template, '.', recursive = TRUE, overwrite = TRUE)
    dir.create('static/image')
    bib_dir <- 'content/post/bib'
    if(!dir.exists(bib_dir)) dir.create(bib_dir)
    if(dir.exists(my_template)) file.copy(my_template, '.', recursive = TRUE, overwrite = TRUE)
    rproj <- paste0(system.file(package = 'rosr'), '/skeleton/rproj')
    file.copy(rproj, '-blogdown.Rproj')
    if(if_render) {
      blogdown::serve_site()
      setwd(dest_dir)
    }
  }

  #+ ## mindmap----
  newdir <- 'mindmap'
  if(newdir %in% sub_project){
    if(!dir.exists(newdir)) dir.create(newdir)
    tree_widget <- mindr::tree(show_files = TRUE)
    if(if_render) {
      mmfile <- 'rosr.html'
      htmlwidgets::saveWidget(tree_widget, mmfile)
      file.copy(mmfile, paste0(newdir, '/', mmfile))
      file.remove(mmfile)
    }
  }
  message('The rosr project "', project, '" has been generated/updated!')
}

#' Install dependencies
#'
#' @return install packages
#' @export
#'
#' @examples
#' \dontrun{
#' install_packages()
#' }
install_packages <- function(){
  # from CRAN
  pkgs_cran <- c('beginr', 'remotes', 'rmd')
  lapply(pkgs_cran,
         function(i) {
           if(system.file(package = i) == '') {
             install.packages(i)
             message('The package ', i, ' has been installed.')
           }
         }
  )
  # from github
  pkgs_github <- c('bbucior/drposter')
  lapply(pkgs_github,
         function(x){
           if(system.file(package = strsplit(x, '/')[[1]][2]) == '') {
             remotes::install_github(x)
             message('The package ', x, ' has been installed.')
           }
         }
  )
  message('All dependency packages have been installed.')
}

#' Insert an equation.
#'
#' @param eqs Character. The path to the equation text file, or a data frame of the equations.
#' @param number integer. The number of the equation.
#' @param label Character. The label of the equation.
#' @param style character. The style of the equation.
#' @param skip integer. nteger: the number of lines of the data file to skip before beginning to read data.
#'
#' @return A string of the equation.
#' @export
#' @examples eq()
eq <- function(eqs = NULL, label = NULL, number = NULL,
               style = c('numbered', 'display', 'inline', 'none'),
               skip = 6) {
  if(is.null(eqs)) return(message('A source file of the equations must be give.'))
  if(is.null(number) & is.null(label)) return(message('A number of label of the equation must be given.'))
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

  if(!is.data.frame(eqs)) eqs <- read.eq(eqs, skip)
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
#' eq_file <- paste0(system.file(package = 'rosr'), '/skeleton/equation/rosr-eq.Rmd')
#' eqs <- read.eq(eq_file)
read.eq <- function(eqs, skip){
    eqs <- read.table(eqs, skip = skip, sep = '|', header = TRUE,
                      stringsAsFactors = FALSE, encoding = 'UTF-8')
    eqs <- eqs[-1, c("number", "label", "description", "eq" )]
    eqs$eq <- gsub('^[[:space:]]*[\\$]*', '', eqs$eq)
    eqs$eq <- gsub('[\\$]*[[:space:]]*$', '', eqs$eq)
    eqs$number <- rm_space(eqs$number)
    eqs$label <- rm_space(eqs$label)
    return(eqs)
}

#+ # copy rmarkdown template ----
cp_rmd <- function(rmd_template = "copernicus_article", to = newdir, if_render = TRUE, package, file_rmd){
  if(!dir.exists(to)) dir.create(to)
  rmd_dir <- paste0(to, '/', rmd_template)
  rmarkdown::draft(file = rmd_dir, template = rmd_template, package = package, create_dir = TRUE, edit = FALSE)
  path_rmd <- paste0(rmd_dir, '/', rmd_template, '.Rmd')
  my_template = paste(system.file(package = 'rosr'), 'skeleton', to, rmd_template, sep = '/')
  if(dir.exists(my_template)) file.copy(my_template, to, recursive = TRUE, overwrite = TRUE)
  if(if_render) rmarkdown::render(path_rmd, quiet = TRUE, encoding = 'UTF-8') # , intermediates_dir = rmd_dir)
}

#' Display available sub projects
#'
#' @return character of the project names
#' @export
#'
#' @examples
#' get_sub_project()
get_sub_project <- function(){
  c("Rproj", "bib", "data", "image", "R",
    "equation", "rpkg", "manuscript", "poster", "slide",
    "book", "website", 'mindmap')
}

rm_space <- function(string){
  newstring <- gsub('^[[:space:]]*', '', string)
  gsub('[[:space:]]*$', '', newstring)
}
