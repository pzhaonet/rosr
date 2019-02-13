#' A shiny app for creating a rosr project
#'
#' @return a shinyapp which can be displayed in a web browser.
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#' create_ui()
#' }
create_ui <- function(){
  return(shiny::shinyApp(ui = rosr_ui, server = rosr_server))
}

#' UI for the Shiny app create_ui
#'
#' @return A UI function
rosr_ui <- function(){
  sidebarLayout(
    mainPanel(
      fluidPage(
        wellPanel(
          fluidRow(
            column(2, checkboxGroupInput('demo', 'demo',
                                         choices = sub_projects()[1:8],
                                         selected = sub_projects()[1:8]),
                   actionLink("demo_all","Select All")),
            column(2, checkboxGroupInput('manuscript', 'manuscript',
                                         choices = templates()$templates[templates()$sub_project == 'manuscript'],
                                         selected = 'copernicus_article'),
                   actionLink("manuscript_all","Select All")),
            column(2, checkboxGroupInput('poster', 'poster',
                                         choices = templates()$templates[templates()$sub_project == 'poster'],
                                         selected = 'drposter'),
                   actionLink("poster_all","Select All")),
            column(2, checkboxGroupInput('slide', 'slide',
                                         choices = templates()$templates[templates()$sub_project == 'slide'],
                                         selected = 'xaringan'),
                   actionLink("slide_all","Select All")),
            column(2, checkboxGroupInput('book', 'book',
                                         choices = templates()$templates[templates()$sub_project == 'book'],
                                         selected = 'demo'),
                   actionLink("book_all","Select All")),
            column(2, checkboxGroupInput('website', 'website',
                                         choices = templates()$templates[templates()$sub_project == 'website'],
                                         selected = 'yihui/hugo-lithium'),
                   actionLink("website_all","Select All"),
                   br(),
                   br(),
                   textInput('text_website', label = 'Or another template:', value = '', placeholder = 'chipsenkbeil/grid-side'))
          ),
          fluidRow(
            column(12, align="right",
                   actionButton('select_all', 'Select all'),
                   actionButton('select_suggested', 'Default')
            )
          )
        )
      )
    ),
    sidebarPanel(
      wellPanel(
        textInput('proj', 'Project', 'new_project'),
        textInput('path', 'Path', file.path(getwd(), 'new_project')),
        radioButtons('if_render', 'Render', choices = c('Yep', 'Nope'), selected = 'Nope')
      ),
      fluidRow(
        column(12, align="right",
               actionButton('create', 'Create!')
        )
      )
    ),
    position = 'left'
  )
}

#' Server for the Shiny app create_ui
#'
#' @param input The input of the server.
#' @param output The output of the server.
#' @param session The session of the server.
rosr_server <- function(input, output, session) {
  template_df <- templates()
  install_packages()
  sub_project <- sub_projects()[9:13]
  sub_project_label <- sub_project
  template_selected <- c('copernicus_article', 'drposter', 'xaringan', 'demo', 'yihui/hugo-lithium')

  observeEvent(input$create, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating...", detail = 'demo...', value = 0)
    pr <- sum(sapply(c('demo', sub_project), function(x) length(input[[x]])))

    # create demos
    create_rosr(project = input$proj, dest_dir = input$path,
                if_render = input$if_render == 'Yep',
                sub_project = input$demo, overwrite = TRUE)

    # create manuscript, slide, poster
    for (j in 2:4){
      if(length(input[[sub_project[j - 1]]] != 0))
        for(i in input[[sub_project[j - 1]]]){
          progress$inc(1/pr, detail = paste0(sub_project[j - 1], ' ', i, '...'))
          create_rmd(to = file.path(input$path, sub_project[j - 1]),
                     template = i,
                     package = template_df$package[template_df$templates == i],
                     if_render =  input$if_render == 'Yep')
        }
    }

    # create book
    if(length(input$book != 0))
      for(i in input$book) {
        progress$inc(1/pr, detail = paste0('book ', i, '...'))
        create_book(to = file.path(input$path, 'book'),
                    template = i,
                    package = template_df$package[template_df$templates == i],
                    if_render =  input$if_render == 'Yep')
      }

    # create website
    if(length(input$website != 0))
      for(i in input$website){
        progress$inc(1/pr, detail = paste0('website ', i, '...'))
        create_website(to = file.path(input$path, 'website'),
                       theme = i)
      }
    if(input$text_website != ''){
      progress$inc(1/pr, detail = paste0('website ', input$text_website, '...'))
      create_website(to = file.path(input$path, 'website'),
                     theme = input$text_website)
    }

    progress$inc(1/pr, detail = 'Done!')
  }
  )

  # function as a shor version of updateCheckboxGroupInput()
  select_all <- function(sub_project = 'manuscript', all = FALSE){
    sub_project_lab <- sub_project
    if(sub_project == 'demo'){
      mychoices <- sub_projects()[1:8]
    } else {
      mychoices <- template_of_sub_project(sub_project = sub_project)
    }
    if(all) {
      myselected <- mychoices
    } else{
      myselected <- NULL
    }
      return(
      updateCheckboxGroupInput(session, sub_project, sub_project_lab,
                               choices = mychoices,
                               selected = myselected)
    )
  }

  # function controlling the update rule of the check boxes
  observe_select_all <- function(sub_project = 'manuscript'){
    input_id <- paste0(sub_project, '_all')
    if(input[[input_id]] == 0) return(NULL)
    if(input[[input_id]]%%2 == 0)  return(select_all(sub_project = sub_project, all = TRUE))
    return(select_all(sub_project = sub_project, all = FALSE))
  }

  # update the check boxex
  observe({observe_select_all(sub_project = 'demo')})
  observe({observe_select_all(sub_project = 'manuscript')})
  observe({observe_select_all(sub_project = 'slide')})
  observe({observe_select_all(sub_project = 'poster')})
  observe({observe_select_all(sub_project = 'book')})
  observe({observe_select_all(sub_project = 'website')})
  observe({
    if(input$select_all == 0) return(NULL)
    else if (input$select_all%%2 == 0) {
      lapply(c('demo', sub_project), function(x) select_all(x, all = FALSE))
    } else {
      lapply(c('demo', sub_project), function(x) select_all(x, all = TRUE))
    }
  })
  observe({
    if(input$select_suggested == 0) return(NULL)
      select_all('demo', all = TRUE)
      for(k in 1:5)
        updateCheckboxGroupInput(session, sub_project[k], sub_project_label[k],
                               choices = template_of_sub_project(sub_project = sub_project[k]),
                               selected = template_selected[k])
  #   }
  })
}

#' Get the template name of the given sub-project name
#'
#' @param sub_project the name of the sub-project
#'
#' @return the name of the template
template_of_sub_project <- function(sub_project){
  templates()$templates[templates()$sub_project == sub_project]
}
