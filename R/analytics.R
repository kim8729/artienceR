#' @title Launch an RStudio addin which allows to schedule an Rscript interactively.
#' @description Launch an RStudio addin which allows to schedule an Rscript interactively.
#'
#' @return the return of \code{\link[shiny]{runGadget}}
#' @export
#' @examples
#' \dontrun{
#' cron_rstudioaddin()
#' }
#'
#'
#'

runanalytics = function(){
library(googleAnalyticsR)
library(dplyr)


# requireNamespace("cronR")
requireNamespace("shiny")
requireNamespace("miniUI")
requireNamespace("shinyFiles")
requireNamespace("digest")

check <- NULL

ui <- miniUI::miniPage(
  # Shiny fileinput resethandler
  # shiny::tags$script('
  #                    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
  #                    var id = "#" + x + "_progress";
  #                    var idBar = id + " .bar";
  #                    $(id).css("visibility", "hidden");
  #                    $(idBar).css("width", "0%");
  #                    });
  #                    '),

  miniUI::gadgetTitleBar("Easy use Google Analytics API"),

  miniUI::miniTabstripPanel(
    miniUI::miniTabPanel(title = 'Create AOuth 2.0', icon = shiny::icon("cloud-upload"),
                         miniUI::miniContentPanel(
                           #shiny::uiOutput('fileSelect'),
                           shiny::h4("Create OAuth2 Files"),
                           shiny::textInput('clientid',label="Google API Client ID",value="697648298070-sbrcvan1t6lhe326k7hf68pp2mhhuf9n.apps.googleusercontent.com"),
                           shiny::textInput('clientpw',label="Google API Client PW",value="CpB_dD2wU9iuGI-2VCMunVz9"),
                           shiny::actionButton('create', "Create OAuth2", icon = shiny::icon("play-circle")),
                           shiny::br(),
                           shiny::br()
                         )),
    miniUI::miniTabPanel(title = 'Data Export', icon = shiny::icon("table"),
                         miniUI::miniContentPanel(
                           shiny::fillRow(flex = c(3, 3),
                                          shiny::column(6,
                                                        shiny::h4("Edit Data"),
                                                        shiny::dateRangeInput("date","Select Date Range",start=Sys.Date()-30,end = Sys.Date()),
                                                        shiny::br(),
                                                        shiny::selectInput("property",label = "Select Property & View",choices = c("defaults"),multiple = T ),
                                                        shiny::selectInput("dimension",label = "Select Dimension",choices = c("defaults"),multiple = T ),
                                                        shiny::selectInput("metric",label = "Select Metric",choices = c("defaults"),multiple = T ),
                                                        shiny::uiOutput("getFiles"),
                                                        shiny::actionButton('showjob', "Show job", icon = shiny::icon("clock-o")),
                                                        shiny::actionButton('deletejob', "Delete job", icon = shiny::icon("remove"))
                                          ),
                                          shiny::column(6
                                          ))
                         ),
                         miniUI::miniButtonBlock(border = "bottom",
                                                 shiny::actionButton('deletecrontab', "Completely clear current crontab schedule", icon = shiny::icon("delete"))
                         )
    )
  )
)

# Server code for the gadget.
server <- function(input, output, session) {
  shiny::observeEvent(input$create, {
    options(googleAuthR.client_id = input$clientid)
    options(googleAuthR.client_secret = input$clientpw)
    ga_auth()

    account = ga_account_list() %>% mutate(totalname = paste0(webPropertyId," | ",viewName)) %>% select(webPropertyId,viewName,totalname,viewId)
    updateSelectInput(session,inputId = "property",choices = unique(account$totalname))
    meta = google_analytics_meta()
    dimension = meta %>% filter(grepl("DIMENSION",meta$type)) %>% select(name)
    metric = meta %>% filter(grepl("METRIC",meta$type)) %>% select(name)
    updateSelectInput(session,inputId = "dimension",choices = unique(dimension))
    updateSelectInput(session,inputId = "metric",choices = unique(metric))
    })



}

# Use a modal dialog as a viewr.
viewer <- shiny::dialogViewer("Cron job scheduler", width = 700, height = 800)
#viewer <- shiny::paneViewer()
shiny::runGadget(ui, server, viewer = viewer)}
