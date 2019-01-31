#' Create an empty directory
#'
#' @param to the destination directory
#'
#' @return an empty directory
#' @export
#'
#' @examples
#' \dontrun{
#' create_dir()
#' }
create_dir <- function(to = getwd()){
  if(!dir.exists(to)) dir.create(to)
}

#' Create a .Rproj file
#'
#' @param project the name of the .Rproj file
#'
#' @return a .Rproj.file
#' @export
#'
#' @examples
#' \dontrun{
#' create_rproj()
#' }
create_rproj <- function(project){
  rproj <- paste0(system.file(package = 'rosr'), '/skeleton/rproj')
  file.copy(rproj, paste0('-', project, '.Rproj'))
}

#' Create a demo .bib file
#'
#' @param to the destination directory
#'
#' @return a demo .bib file
#' @export
#'
#' @examples
#' \dontrun{
#' create_bib()
#' }
create_bib <- function(to = getwd()){
  create_dir(to)
  bib_template <- paste0(system.file(package = 'rosr'), '/skeleton/bib/rosr.bib')
  file.copy(bib_template, paste0(to, '/rosr.bib'))
}

#' Create a demo data file
#'
#' @param to the destination directory
#'
#' @return a demo data file
#' @export
#'
#' @examples
#' \dontrun{
#' create_data()
#' }
create_data <- function(to = getwd()){
  create_dir(to)
  write.csv(airquality, paste0(to, '/rosr.csv'), row.names = FALSE)
}

#' Create a demo .R script
#'
#' @param to the destination directory
#'
#' @return a demo .R script
#' @export
#'
#' @examples
#' \dontrun{
#' create_r()
#' }
create_r <- function(to = getwd()){
  create_dir(to)
  r_template <- paste0(system.file(package = 'rosr'), '/skeleton/R/rosr.R')
  file.copy(r_template, paste0(to, '/rosr.R'))
}

#' Create a demo equation sheet
#'
#' @param to the destination directory
#' @param if_render Logical. Whether render the templates.
#'
#' @return a demo equation sheet
#' @export
#'
#' @examples
#' \dontrun{
#' create_eq()
#' }
create_eq <- function(to = getwd(), if_render = TRUE){
  create_dir(to)
  r_template <- paste0(system.file(package = 'rosr'), '/skeleton/equation/rosr-eq.Rmd')
  file.copy(r_template, paste0(to, '/rosr-eq.Rmd'))
  if(if_render) rmarkdown::render(paste0(to, '/rosr-eq.Rmd'))
}

#' Create a demo R package
#'
#' @param to the destination directory
#'
#' @return a demo R package
#' @export
#'
#' @examples
#' \dontrun{
#' create_rpkg()
#' }
create_rpkg <- function(to = getwd()){
  mypath <- paste0(system.file(package = 'beginr'), "/zip/rpkg.zip")
  unzip(mypath, exdir = to)
}

#' Create a demo mind map
#'
#' @param to the destination directory
#' @param if_render Logical. Whether render the templates.
#'
#' @return a demo mind map
#' @export
#'
#' @examples
#' \dontrun{
#' create_mindmap()
#' }
create_mindmap <- function(to = getwd(), if_render = FALSE){
  create_dir(to)
  tree_widget <- mindr::tree(show_files = TRUE)
  if(if_render) {
    mmfile <- 'rosr.html'
    htmlwidgets::saveWidget(tree_widget, mmfile)
    file.copy(mmfile, paste0(to, '/', mmfile))
    file.remove(mmfile)
  }
}

#' Create a demo markdown project
#'
#' @param template the name of the r markdown template. It is the folder name in 'package/rmarkdown/templates/'.
#' @param to the desnitation directory
#' @param if_render whether render it.
#' @param package the package name where the template comes from.
#'
#' @return copied file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_rmd()
#' }
create_rmd <- function(to = 'manuscript',
                       template = "copernicus_article",
                       if_render = TRUE,
                       package = 'rticles'
                       ){
  create_dir(to)
  rmd_dir <- paste0(to, '/', template)
  rmarkdown::draft(file = rmd_dir,
                   template = template,
                   package = package,
                   create_dir = TRUE,
                   edit = FALSE)
  if(package == 'rticles2'){
    template_old <- readLines(file.path(rmd_dir, paste0(template, '.Rmd')))
    yaml_old <- get_yaml(template_old)
    bib_loc <- grep('^bibliography.*', yaml_old)
    if(length(bib_loc != 0)) {
      yaml_old[bib_loc] <- 'bibliography: rosr.bib'
    } else {
      yaml_old <- c(yaml_old[1],
                    'bibliography: rosr.bib',
                    yaml_old[2: length(yaml_old)]
                    )
      }
    output_format_loc <- grep('^    base_format.*', yaml_old)
    loc_shift <- length(output_format_loc)
    if(loc_shift != 0) {
      yaml_new <- c(yaml_old[1: (output_format_loc - loc_shift)],
                    '    base_format: bookdown::pdf_document2',
                    '    includes: ',
                    '      in_header: "preamble.tex"',
                    yaml_old[(output_format_loc + 1): length(yaml_old)])
    } else {
      yaml_new <- yaml_old
    }
    body_new <- readLines(file.path(system.file(package = 'rosr'),
                            'skeleton', 'manuscript', 'rosr_rticles.Rmd'))
    file.copy(file.path(system.file(package = 'rosr'),
                        'skeleton', 'manuscript', 'preamble.tex'),
              rmd_dir,
              recursive = TRUE, overwrite = TRUE)
    template_new <- c(yaml_new, body_new)
    rmd_new <- file.path(rmd_dir, 'rosr_rticles.Rmd')
    writeLines(template_new, rmd_new, useBytes = TRUE)
  }

  if(package == 'rticles')
    rmd_new <- copy_rmd(package = package,
                        template = template,
                        sub_project = 'manuscript',
                        rmd_dir = rmd_dir)

  if(package == 'drposter' | package == 'pagedown')
    rmd_new <- copy_rmd(package = package,
                        template = template,
                        sub_project = 'poster',
                        rmd_dir = rmd_dir)

  if(package == 'xaringan' | package == 'rosr')
    rmd_new <- copy_rmd(package = package,
                        template = template,
                        sub_project = 'slide',
                        rmd_dir = rmd_dir)

  if(if_render)
    rmarkdown::render(rmd_new,
                      quiet = TRUE,
                      encoding = 'UTF-8')
}

#' Create a demo bookdown project
#'
#' @param template the name of the r markdown template. It is the folder name in 'package/rmarkdown/templates/'.
#' @param to the desnitation directory.
#' @param package the package name where the template comes from.
#' @param if_render whether render it.
#'
#' @return copied file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_book()
#' }
create_book <- function(to = getwd(),
                        template = "demo",
                        if_render = TRUE,
                        package = 'bookdownplus'){
  oldwd = getwd()
  on.exit(setwd(oldwd))
  if(package == 'bookdownplus'){
    create_dir(to = to)
    bookdir <- file.path(to, template)
    create_dir(to = bookdir)
    bookdownplus::bookdownplus(template = template,
                               render = FALSE,
                               to = bookdir)
    rmd_new <- copy_rmd(package = package,
                        template = template,
                        sub_project = 'book',
                        rmd_dir = bookdir)
    # oldbib <- paste0(bookdir, '/bib/bib.bib')
    # if(file.exists(oldbib)) file.remove(oldbib)
    # my_template <- file.path(system.file(package = 'rosr'),
    #                          'skeleton', 'book')
    # if(dir.exists(my_template))
    #   file.copy(dir(my_template, full.names = TRUE), bookdir, recursive = TRUE, overwrite = TRUE)
    # index_rmd_path <- file.path(bookdir,'index.Rmd')
    # index_rmd <- readLines(index_rmd_path)
    # index_rmd[grep('^bibliography: ', index_rmd)] <- 'bibliography: [bib/rosr.bib]'
    # writeLines(index_rmd, index_rmd_path, useBytes = TRUE)
    if(if_render) {
      bookdownplus::bookdownplus(render = TRUE, to = bookdir, new = FALSE)
    }
  }
  if(package == 'pagedown'){
    create_rmd(to = to,
               template = template,
               if_render = if_render,
               package = 'pagedown')
  }
}

#' Create a demo blogdown website
#'
#' @param theme the name of the r blogdown theme.
#' @param to the desnitation directory
#' @param if_render whether render it.
#' @param oldwd set the working directory aftering rendering the book
#'
#' @return copied file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_website()
#' }
create_website <- function(to = getwd(),
                           theme = "yihui/hugo-lithium",
                           oldwd = getwd()){
  on.exit(setwd(oldwd))
  create_dir(to)
  theme_rename <- gsub('/', '_', theme)
  webdir <- file.path(to, theme_rename)
  create_dir(webdir)
  blogdown::new_site(dir = webdir,
                     serve = FALSE,
                     # sample = FALSE,
                     theme = theme)
  # setwd(webdir)
  my_template = paste(system.file(package = 'rosr'),
                      'skeleton', 'website',
                      theme_rename, 'content', sep = '/')
  if(dir.exists(my_template)){
    file.copy(my_template,  webdir, recursive = TRUE, overwrite = TRUE)
    bib_dir <- 'content/post/bib'
    create_dir(bib_dir)
  }
  rproj <- paste0(system.file(package = 'rosr'), '/skeleton/rproj')
  file.copy(rproj, file.path(webdir, '-blogdown.Rproj'))
  # if(if_render) {
  #   blogdown::serve_site()
  # }
}


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
#' create_rosr()
#' }
create_rosr <- function(project = 'test',
                   if_render = TRUE,
                   dest_dir = NA,
                   overwrite = FALSE,
                   sub_project = sub_projects()){

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

  #+ ## Rproj ----
  if('Rproj' %in% sub_project)
    create_rproj(project = project)

  #+ ## bib ----
  newdir <- 'bib'
  if(newdir %in% sub_project | if_render) create_bib(to = newdir)

  #+ ## data ----
  newdir <- 'data'
  if(newdir %in% sub_project | if_render) create_data(to = newdir)

  #+ ## image----
  newdir <- 'image'
  if(newdir %in% sub_project | if_render) create_dir(to = newdir)

  #+ ## R----
  newdir <- 'R'
  if(newdir %in% sub_project | if_render) create_r(to = newdir)

  #+ ## equation----
  newdir <- 'equation'
  if(newdir %in% sub_project | if_render) create_eq(to = newdir, if_render = if_render)

  #+ ## rpkg----
  if('rpkg' %in% sub_project){
    mypath <- paste0(system.file(package = 'beginr'), "/zip/rpkg.zip")
    unzip(mypath)

  }

  #+ ## manuscript----
  newdir <- 'manuscript'
  if(newdir %in% sub_project){
    template ="copernicus_article"
    create_rmd(template = template,
           if_render = if_render,
           to = newdir,
           package = "rticles")
    file.remove(paste(newdir, template, c('sample.bib', 'Nikolaus_Kopernikus.jpg'), sep = '/'))
    preamble_file <- paste(system.file(package = 'rosr'), 'skeleton', newdir, template, 'preamble.tex', sep = '/')
    file.copy(preamble_file, paste0(newdir, '/copernicus_article'))
  }

  #+ ## poster----
  newdir <- 'poster'
  if(newdir %in% sub_project)
    create_rmd(template = "drposter",
           if_render = if_render,
           to = newdir,
           package = "drposter")

  #+ ## slide----
  newdir <- 'slide'
  if(newdir %in% sub_project)
    create_rmd(template = "xaringan",
           if_render = if_render,
           to = newdir,
           package = "xaringan")

  #+ ## book ----
  newdir <- 'book'
  if(newdir %in% sub_project)
    create_book(to = newdir, #oldwd = dest_dir,
                if_render = if_render)

  #+ ## website----
  newdir <- 'website'
  if(newdir %in% sub_project)
    create_website(to = newdir, oldwd = dest_dir)

  #+ ## mindmap----
  newdir <- 'mindmap'
  if(newdir %in% sub_project) create_mindmap(to = newdir, if_render = if_render)

  message('The rosr project "', project, '" has been generated/updated!')
}
