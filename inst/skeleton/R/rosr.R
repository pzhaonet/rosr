#' ---
#' title: A demo R script
#' author: Peng Zhao (<pzhao@pzhao.net>)
#' date: 2019-01-13
#' # History:
#' # 2019-01-11 created.
#' ---
#'

#= # Introduction ----
#' This file shows you how to write an R script in a special way.
#' You do not have to follow the style of this file, but structured comments are always recommended.
#' This file can be converted directly into an R markdown file and a mind map.
#' The complete rules can be found online ([in English](http://www.pzhao.org/slides/mindr/#25), and [in Chinese](http://www.pzhao.org/zh/post/r-with-markdown2/))
#'
#= # Rmd Options ----
#' First we set up some options, which are performd in the Rmd file produced by the last line of this file.
# knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
# knitr::opts_knit$set(root.dir = '../')

#= # Read data ----
#' If the data files are in the 'data/' folder, then they can be read like this.

aq <- read.csv('data/rosr.csv')

#= # Calculation ----
#' Here we do a simple calculation,
#' and save the result into a variable,
#' which can be called in your manuscripts.
temperature <- mean(aq$Temp)

#= # Plot ----
#' Now we plot a figure, and save it as a file, which can be called in your manuscript.
#+ plot, eval = FALSE
png('image/rosr_R.png')
beginr::plotpairs(aq)
dev.off()

#= # Why do this ----
#' Why do we write an r script like this? Because it can be converted to other formats easily.
#'
#= ## Create a mindmap ----
#' Now we created a mindmap of this script.
#+ mindmap, eval = FALSE
# mindr::mm('R/rosr.R', widget_name = 'mindmap/rosr_R.html')

#= ## Create an Rmd file ----
#' Now we convert this script into an Rmd file, which you can later open and knit into a document.
#+ rmd, eval = FALSE
mindr::mm('R/rosr.R', 'manuscript/rosr_R.Rmd')
