#' remove the spaces before and after a string.
#'
#' @param string a string.
#'
#' @return a new string.
rm_space <- function(string){
  gsub("^[[:space:]]*|[[:space:]]*$", "", string)
}


#' Get the yaml header of a string vecter from a markdown file
#'
#' @param txt a string vector vecter from a markdown file
#'
#' @return a string vector of the yaml header
get_yaml <- function(txt){
  loc <- grep('^---', txt)
  if(length(loc) == 0) return(NULL)
  txt[loc[1]: loc[2]]
}

#' Copy the existing .Rmd file from the rosr package to the destination directory.
#'
#' @param package Packge name of the source template.
#' @param template Template name of the source template.
#' @param sub_project Sub-project name
#' @param rmd_dir Destination directory
#'
#' @return copied file and the path of the new .Rmd file
copy_rmd <- function(package, template, sub_project, rmd_dir){
  rosr_demo <- file.path(system.file(package = 'rosr'),
                         'skeleton', sub_project, package, template)
  if(dir.exists(rosr_demo))
    file.copy(list.files(rosr_demo, full.names = TRUE),
              rmd_dir,
              recursive = TRUE, overwrite = TRUE)
  rmd_new <- file.path(rmd_dir, paste0(template, '.Rmd'))
  return(rmd_new)
}
