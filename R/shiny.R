#' A shiny app for creating a rosr project
#'
#' @return a shinyapp which can be displayed in a web browser.
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
            column(2, checkboxGroupInput('demo', 'Demo',
                                         choices = sub_projects()[1:8],
                                         selected = sub_projects()[1:8]),
                   actionLink("demo_all","Select All")),
            column(2, checkboxGroupInput('manuscript', 'Manuscript Templates',
                                         choices = templates()$templates[templates()$sub_project == 'manuscript'],
                                         selected = 'copernicus_article'),
                   actionLink("manuscript_all","Select All")),
            column(2, checkboxGroupInput('poster', 'Poster Templates',
                                         choices = templates()$templates[templates()$sub_project == 'poster'],
                                         selected = 'drposter'),
                   actionLink("poster_all","Select All")),
            column(2, checkboxGroupInput('slide', 'Slides Templates',
                                         choices = templates()$templates[templates()$sub_project == 'slide'],
                                         selected = 'xaringan'),
                   actionLink("slide_all","Select All")),
            column(2, checkboxGroupInput('book', 'Book Templates',
                                         choices = templates()$templates[templates()$sub_project == 'book'],
                                         selected = 'demo'),
                   actionLink("book_all","Select All")),
            column(2, checkboxGroupInput('website', 'Website Templates',
                                         choices = templates()$templates[templates()$sub_project == 'website'],
                                         selected = 'yihui/hugo-lithium'),
                   actionLink("website_all","Select All"),
                   br(),
                   br(),
                   textInput('text-website', label = 'Or another template:', value = ''))
          ),
          fluidRow(
            column(12, align="right",
            actionButton('select_all', 'Select all'),
            actionButton('select_suggested', 'Select suggested')
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
  observeEvent(input$create, {
    create_rosr(project = input$proj, dest_dir = input$path,
                if_render = input$if_render == 'Yep',
                sub_project = input$demo, overwrite = TRUE)
    if(length(input$manuscript != 0))
      for(i in input$manuscript)
        create_rmd(to = file.path(input$path, 'manuscript'),
                   template = i,
                   package = template_df$package[template_df$templates == i],
                   if_render =  input$if_render == 'Yep')
    if(length(input$poster != 0))
      for(i in input$poster)
        create_rmd(to = file.path(input$path, 'poster'),
                   template = i,
                   package = template_df$package[template_df$templates == i],
                   if_render =  input$if_render == 'Yep')
    if(length(input$slide != 0))
      for(i in input$slide)
        create_rmd(to = file.path(input$path, 'slide'),
                   template = i,
                   package = template_df$package[template_df$templates == i],
                   if_render =  input$if_render == 'Yep')
    if(length(input$book != 0))
      for(i in input$book)
        create_book(to = file.path(input$path, 'book'),
                    template = i,
                    package = template_df$package[template_df$templates == i],
                    if_render =  input$if_render == 'Yep')
    if(length(input$website != 0))
      for(i in input$website)
        create_website(to = file.path(input$path, 'website'),
                       theme = i)
    if(input$text-website != '')
      create_website(to = file.path(input$path, 'website'),
                     theme = input$text-website,
                     if_render =  input$if_render == 'Yep')
  }
  )

  observe({
    if(input$demo_all == 0) return(NULL)
    else if (input$demo_all%%2 == 0) {
      updateCheckboxGroupInput(session,'demo', 'Demo', choices = sub_projects()[1:8], selected = sub_projects()[1:8])
    } else {
      updateCheckboxGroupInput(session,'demo', 'Demo', choices = sub_projects()[1:8])
    }
  })

  observe({
    if(input$manuscript_all == 0) return(NULL)
    else if (input$manuscript_all%%2 == 0) {
      updateCheckboxGroupInput(session,'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'],
                               selected = templates()$templates[templates()$sub_project == 'manuscript'])
    } else {
      updateCheckboxGroupInput(session, 'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'])
    }
  })

  observe({
    if(input$slide_all == 0) return(NULL)
    else if (input$slide_all%%2 == 0) {
      updateCheckboxGroupInput(session,'slide', 'Slide Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'],
                               selected = templates()$templates[templates()$sub_project == 'slide'])
    } else {
      updateCheckboxGroupInput(session, 'slide', 'Slide Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'])
    }
  })

  observe({
    if(input$poster_all == 0) return(NULL)
    else if (input$poster_all%%2 == 0) {
      updateCheckboxGroupInput(session,'poster', 'Poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'],
                               selected = templates()$templates[templates()$sub_project == 'poster'])
    } else {
      updateCheckboxGroupInput(session, 'poster', 'poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'])
    }
  })

  observe({
    if(input$book_all == 0) return(NULL)
    else if (input$book_all%%2 == 0) {
      updateCheckboxGroupInput(session,'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'],
                               selected = templates()$templates[templates()$sub_project == 'book'])
    } else {
      updateCheckboxGroupInput(session, 'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'])
    }
  })

  observe({
    if(input$website_all == 0) return(NULL)
    else if (input$website_all%%2 == 0) {
      updateCheckboxGroupInput(session,'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'],
                               selected = templates()$templates[templates()$sub_project == 'website'])
    } else {
      updateCheckboxGroupInput(session, 'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'])
    }
  })

  observe({
    if(input$select_all == 0) return(NULL)
    else if (input$select_all%%2 == 0) {
      updateCheckboxGroupInput(session,'demo', 'Demo', choices = sub_projects()[1:8])
      updateCheckboxGroupInput(session,'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'])
      updateCheckboxGroupInput(session,'slide', 'Slide Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'])
      updateCheckboxGroupInput(session,'poster', 'Poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'])
      updateCheckboxGroupInput(session,'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'])
      updateCheckboxGroupInput(session, 'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'])
    } else {
      updateCheckboxGroupInput(session,'demo', 'Demo', choices = sub_projects()[1:8], selected = sub_projects()[1:8])
      updateCheckboxGroupInput(session,'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'],
                               selected = templates()$templates[templates()$sub_project == 'manuscript'])
      updateCheckboxGroupInput(session,'slide', 'Slide Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'],
                               selected = templates()$templates[templates()$sub_project == 'slide'])
      updateCheckboxGroupInput(session,'poster', 'Poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'],
                               selected = templates()$templates[templates()$sub_project == 'poster'])
      updateCheckboxGroupInput(session,'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'],
                               selected = templates()$templates[templates()$sub_project == 'book'])
      updateCheckboxGroupInput(session,'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'],
                               selected = templates()$templates[templates()$sub_project == 'website'])
    }
  })

  observe({
    if(input$select_suggested == 0) return(NULL)
    else if (input$select_suggested%%2 == 0) {
      updateCheckboxGroupInput(session,'demo', 'Demo', choices = sub_projects()[1:8])
      updateCheckboxGroupInput(session,'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'])
      updateCheckboxGroupInput(session,'slide', 'Slide Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'])
      updateCheckboxGroupInput(session,'poster', 'Poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'])
      updateCheckboxGroupInput(session,'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'])
      updateCheckboxGroupInput(session, 'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'])
    } else {
      updateCheckboxGroupInput(session,'demo', 'Demo',
                               choices = sub_projects()[1:8],
                               selected = sub_projects()[1:8])
      updateCheckboxGroupInput(session,'manuscript', 'Manuscript Templates',
                               choices = templates()$templates[templates()$sub_project == 'manuscript'],
                               selected = 'copernicus_article')
      updateCheckboxGroupInput(session,'poster', 'Poster Templates',
                               choices = templates()$templates[templates()$sub_project == 'poster'],
                               selected = 'drposter')
      updateCheckboxGroupInput(session,'slide', 'Slides Templates',
                               choices = templates()$templates[templates()$sub_project == 'slide'],
                               selected = 'xaringan')
      updateCheckboxGroupInput(session,'book', 'Book Templates',
                               choices = templates()$templates[templates()$sub_project == 'book'],
                               selected = 'demo')
      updateCheckboxGroupInput(session,'website', 'Website Templates',
                               choices = templates()$templates[templates()$sub_project == 'website'],
                               selected = 'yihui/hugo-lithium')
    }
  })
}
