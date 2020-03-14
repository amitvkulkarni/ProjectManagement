library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(shinythemes)
library(rhandsontable)
library(bsplus)
library(htmltools)
library(giphyr)
library(rpivotTable)
library(plotly)
library(shinyalert)
library(shinyalert)
library(shinyjs)
library(supercaliheatmapwidget)


dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdownmenu"),
                  title = "Project Management",dropdownMenuOutput("msgOutput")
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id= "tabs", width = 350,
                menuItem("Home", icon = icon("home"), tabName = "home"),
                menuItem("Projects & Tasks", icon = icon("tasks"), tabName = "projects_tasks"),
                #menuItem("Team", icon = icon("users"), tabName = "team"),
                #menuItem("Timesheets", icon = icon("clock"), tabName = "timesheets"),
                menuItem("Do it Yourself", icon = icon("hand-pointer"), tabName = "doitYourself"),
                menuItem("Help", icon = icon("hands-helping"), tabName = "help"),
                menuItem("Generate Report", tabName = "sectors", icon = icon("download"),
                         radioButtons('format', 'Document format', c('PDF', 'Word'),inline = FALSE, selected = 1),
                         downloadButton("report", "Download Report", class = "butt"),
                         tags$head(tags$style(".butt{color: blue !important;}"))))
    
  ),
  dashboardBody(
    useShinyalert(), 
    useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
    

    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
    
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(90%);
             left: calc(0.5%);
             }
             "
        )
      )
    ),
    
    fluidPage(
      tabItems(
        tabItem(tabName = "home",
                
                
                #tags$img(src = "home.jpg", width = "1600", height = "350"),
                
                fluidRow(
                  
                  valueBoxOutput("Completed", width = 3),
                  valueBoxOutput("WIP", width = 3),
                  valueBoxOutput("Delayed", width = 3),
                  valueBoxOutput("OnHold", width = 3)
                ),
                
                fluidRow(
                  
                  box(plotlyOutput("Project_category"), width = 4,solidHeader = TRUE, status = "primary", title = "Research Areas", collapsible = TRUE),
                  box(plotlyOutput("Project_status"), width = 8,solidHeader = TRUE, status = "primary", title = "Project Status", collapsible = TRUE),
                  box(plotlyOutput("Complexity"), width = 4,solidHeader = TRUE, status = "primary", title = "Project Complexity", collapsible = TRUE),
                  box(plotlyOutput("Audits"), width = 4,solidHeader = TRUE, status = "primary", title = "Audit Status", collapsible = TRUE),
                  box(plotlyOutput("Proj_task_cnt"), width = 4,solidHeader = TRUE, status = "primary", title = "Project & Tasks", collapsible = TRUE),
                  box(DTOutput("upcoming_deadlines"), width = 6,solidHeader = TRUE, status = "primary", title = "Upcoming Deadlines", collapsible = TRUE),
                  box(DTOutput("overdue_tasks"), width = 6,solidHeader = TRUE, status = "primary", title = "Overdue Tasks", collapsible = TRUE),
                  box(DTOutput("Overview"), width = 12,solidHeader = TRUE, status = "primary", title = "Projects Overview", collapsible = TRUE)
                  
                  
                )
                
                
                
        ),
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
        tabItem(tabName = "projects_tasks",
                box(tabBox(id = "tabset_projects",width = 12,
                           tabPanel("Projects", icon = icon("tasks"),
                                    
                                    box(withSpinner(rHandsontableOutput("Projects")), width = 12),
                                    actionButton("saveBtnProjects", "Save Projects", icon = icon("save")),
                                    actionButton("BtnResetProjects", "Reset Filters", icon = icon("eraser"))),
                           
                           tabPanel("Project_schedule", icon = icon("tasks"),
                                    selectInput("selectProject", label = h5("Select Project"),choices = sort(unique(raw_data_projects$PROJECT.NAME), decreasing = F), 
                                                selected = 1, width = "15%"),
                                    plotlyOutput("schedule")
                                    
                                    ),
                           tabPanel("Project_schedule_new", icon = icon("tasks"),
                                    selectInput("selectProject_new", label = h5("Select Project"),choices = sort(unique(raw_data_projects$PROJECT.NAME), decreasing = F), 
                                                selected = 1, width = "15%"),  
                                    timevisOutput("schedule_new"),
                                    
                                    box(withSpinner(rHandsontableOutput("schedule_table")), width = 12)
                                    
                                    
                                    )
                           ), 
                    title = "Project Details",collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary"),
                
                
                
                box(tabBox(id = "tabset_tasks",width = 12,
                           
                           tabPanel("Tasks",icon = icon("tasks"),
                                    
                                    box(withSpinner(rHandsontableOutput("Tasks")), width = 12),
                                    actionButton("saveBtnTasks", "Save Tasks", icon = icon("save")),
                                    actionButton("BtnResetTasks", "Reset Filters", icon = icon("eraser")),
                                    #verbatimTextOutput("selected")
                                    ),
                           
                           tabPanel("Metrics", icon = icon("signal"),
                                    box(withSpinner(plotlyOutput("Tasks_Leads")), width = 5))
                                    #actionButton("down","Download Plot", icon = icon("download")))
                           
                           # tabPanel("Do It Youself", icon = icon("hand-pointer"),
                           #          rpivotTableOutput("pivot"))
                           ), collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary", title = "Tasks Details")),
   

  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
        # tabItem(tabName = "timesheets",
        # 
        #         selectInput("selectAnalyst", label = h3(""),choices = list("Vinayaka.Shivayogi.Javalli", "Mahesh.Kumar.Natikar"), selected = 1),
        #         box(tabBox(id = "tabset_timesheet",width = 12,
        #                tabPanel("Time", icon = icon("user-clock"),
        #                     supercaliheatmapwidgetOutput("timesheet"))
        #         ),title = "Resource Availability", width = 12, solidHeader = TRUE, status = "primary")
        # ),
        # 
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  tabItem(tabName = "help",
          box(tags$img(src = "Projects.gif", height="100%", width="100%")),
          box(tags$img(src = "Tasks.gif",height="100%", width="100%")),
          box(tags$img(src = "DIY.gif",height="100%", width="100%")),
          box(tags$img(src = "Download.gif",height="100%", width="100%"))
        ),
          
          
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "doitYourself",
                
                # box(h2("You can explore the projects and Tasks data further by using the pivot like feature below and guess what, this works exactly same as Microsoft Excel !!!"), 
                #     width = 12, height = 5, status = "primary"),br(), br(),br(), br(),br(), br(),
              fluidRow(  
                #tags$img(src = "DIY1.jpg", width = "1600", height = "350"),
                selectInput("selectData", label = h3(""),choices = list("Projects", "Tasks"), selected = 1),
                box(rpivotTableOutput("pivot"), width = "100%", height = "100%")
                  
              )  
                
         )
        
      )
    )
  )
)
