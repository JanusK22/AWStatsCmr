library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(ggthemr)
library(plotly)
library(fresh)
library(shinycssloaders)
library(shinyalert)
library(reactable)


# Theme

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Box config
box_config <- tibble::tribble(
  ~background, ~labelStatus,
  "red", "warning",
  "purple", "success",
  "green", "primary",
  "yellow", "danger",
  "fuchsia", "info"
)

box_factory <- function(background, labelStatus) {
  box(
    title = "Cyberpunk Box",
    collapsible = TRUE,
    background = background,
    height = "200px",
    label = boxLabel(1, labelStatus)
  )
}

boxes <- purrr::pmap(box_config, box_factory)


dashboardPage(
  
  skin = "midnight",
  controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
  dashboardHeader(
    title = "Cameroon Alphabetisation/Work",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      style = "position: fixed; overflow: visible; width:350px",
      id = "sidebarid",
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Datas", tabName = "datas", icon = icon("table")),
      menuItem("Charts",
        tabName = "Charts", icon = icon("chart-bar"),
        menuSubItem("Employement Rate", tabName = "Line", icon = icon("chart-line")),
        menuSubItem("Primary School rate", tabName = "Bar", icon = icon("chart-bar")),
        menuSubItem("Alphabetization", tabName = "Regions", icon = icon("globe-africa"))
      ),
      menuItem("Code",
        tabName = "code", icon = icon("code"),
        menuSubItem("Script.R", tabName = "global", icon = icon("angle-right")),
        menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
        menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
      ),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),


  # Dashboard Body

  ############################ Dashboard tabs #################################
  dashboardBody(
    tabItems(

      ############################ Intro#################################
      tabItem(
        tabName = "intro",useShinyalert(),
        tags$iframe(
          src = "./home.html",style = "overflow-y:hidden",style = "overflow-x:hidden",
          width = "100%", height = "900px",style="width : 1500px",seamless="seamless",
          frameborder = 5, scrolling = "no"
        )
      ),
      
      ############################ Table#################################
      tabItem(
        tabName = "datas",
        fluidRow(
        box(
          width = 12, background = "black", title = "Datas...", status = "purple", solidHeader = TRUE,
          reactableOutput("table")
        )
        ))
      ,

      ############################ Plot Bar#################################
      # first tab content #Bar
      tabItem(
        tabName = "Bar",
        fluidRow(
          box(
            width = 3, background = "black", title = "Description", status = "navy", solidHeader = TRUE,
            "In this second Plot we will plott a visuals statistics informations about the Primary School Rate in Cameroon in a Bar Plot"
          ),
          box(
            width = 3, background = "black", title = "Subrgroup", status = "purple", solidHeader = TRUE,
            checkboxGroupButtons(
              inputId = "checkbar", label = "", choices = c("Féminin", "Masculin", "Total" ,"Pauvre","Non pauvre" ,"Rural" ,"Féminin Pauvre","Féminin Non pauvre","Masculin Non pauvre" , "Masculin Pauvre"), status = "danger",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              ), selected = "Total"
            )
          ),
          box(
            solidHeader = TRUE,
            width = 10,
            title = "Plot", status = "purple",
            dropdownButton(awesomeRadio("bartheme", "Theme", c("dust", "flat", "flat dark", "camouflage", "chalk", "copper", "fresh", "light", "sea")),
              circle = TRUE, status = "primary", icon = icon("gear"), width = "10px", size = "sm",
              tooltip = tooltipOptions(title = "Choose a theme")
            ), column(
              6,
              withSpinner(plotlyOutput(outputId = "bar", width = "800px", height = "600px"),
              type  = 6, size = 0.5 , color = "#5D3FD3")
            )
          )
        )
      ),

      ############################ Plot Line#################################
      # second tab content #LINE
      tabItem(
        tabName = "Line",
        fluidRow(
          box(
            solidHeader = TRUE, width = 3, background = "black", title = "Description", status = "navy",
            "In this second Plot we will plott a visuals statistics informations about the Employement Rate in Cameroon in a Line Plot"
          ),
          box(
            solidHeader = TRUE, width = 3, background = "black", title = "Subrgroup", status = "purple",
            checkboxGroupButtons(
              inputId = "checkline", label = "", choices = c("Féminin", "Masculin", "Total", "Rural", "Urbain"), status = "primary",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              ), selected = "Total"
            )
          ),
          column(
            10, box(
              solidHeader = TRUE, background = "black", title = "Plot", status = "purple", width = 10,
              withSpinner(
              plotlyOutput(outputId = "line", width = "900px", height = "600px"),
              type  = 6, size = 0.5 , color = "#5D3FD3")
            )
          )
        )
      ),

      ############################ Plot Region#################################
      tabItem(
        tabName = "Regions",
        fluidRow(
          box(
            solidHeader = TRUE,
            width = 3, background = "black", title = "Description", status = "navy",
            "In this first Plot we will plott a visuals statistics informations about the alphabetisazion Rate in Cameroon in a map"
          ), box(solidHeader = TRUE, width = 3, background = "black", title = "Group 1", status = "info", pickerInput(
            "variable", "Subgroup:",
            c("Féminin", "Masculin", "Total" ,"Pauvre","Non pauvre","Féminin Pauvre","Féminin Non pauvre","Masculin Non pauvre" , "Masculin Pauvre"),
            options = list(
              `live-search` = TRUE, style = "btn-primary"
            )
          )),
          box(solidHeader = TRUE, width = 2, background = "black", title = "Compare", status = "navy", switchInput(
            inputId = "compareregion",
            value = FALSE, onStatus = "danger", onLabel = "Yes", offLabel = "No",
            offStatus = "info"
          )),
          box(solidHeader = TRUE, width = 3, background = "black", title = "Group 2", status = "danger", pickerInput(
            "variable2", "Subgroup:",
            c("Féminin", "Masculin", "Total" ,"Pauvre","Non pauvre" ,"Féminin Pauvre","Féminin Non pauvre","Masculin Non pauvre" , "Masculin Pauvre"),
            options = list(
              `live-search` = TRUE, style = "btn-danger"
            )
          ))
        ),
        fluidRow(box(
          solidHeader = TRUE,
          width = 10,
          title = "Plot", status = "purple",
          dropdownButton(awesomeRadio("radiotheme", "Theme", status = "info", c("dust", "flat", "flat dark", "camouflage", "chalk", "copper", "fresh", "light", "sea")),
            circle = TRUE, status = "primary", icon = icon("gear"), width = "10px", size = "sm",
            tooltip = tooltipOptions(title = "Choose a theme")
          ), column(
            6,withSpinner(
            plotOutput(outputId = "plotregion", width = "500px", height = "500px"),
            type  = 6, size = 0.5 , color = "#5D3FD3"),
          ),
          column(6,withSpinner( plotOutput(outputId = "plotregion2", click = "infoplotregion2", width = "500px", height = "500px"),
                 type  = 6, size = 0.5 , color = "#5D3FD3"))
        ))
      )

      ############################ Download code#################################
      ,
      tabItem(
        tabName = "global",
        fluidRow(
          box(
            width = NULL, status = "purple", solidHeader = TRUE, title = "script.R",
            downloadButton("downloadData1", "Download"),
            br(), br(),
            pre(textscript)
          )
        )
      ),
      tabItem(
        tabName = "ui",
        fluidRow(
          box(
            width = NULL, status = "purple", solidHeader = TRUE, title = "ui.R",
            downloadButton("downloadData2", "Download"),
            br(), br(),
            pre(textui)
          )
        )
      ),
      tabItem(
        tabName = "server",
        fluidRow(
          box(
            width = NULL, status = "purple", solidHeader = TRUE, title = "server.R",
            downloadButton("downloadData3", "Download"),
            br(), br(),
            pre(textserver)
          )
        )
      ),
      ############################ Source code#################################
      tabItem(
        tabName = "about",
        fluidPage(
          box(
            width = NULL, status = "purple", solidHeader = TRUE, title = "About the App...",
          tags$iframe(
            src = "about.html",
            width = "100%", height = "800px",
            frameborder = 0, scrolling = "auto"
          ))
        )
      )
    )
  ),

  ################################## Footer#############################################
  footer = dashboardFooter(
    left = "Janus Kuete",
    right = "DataViz, 2021"
  )
)
