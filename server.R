shinyServer(function(input, output, session){ 
  
  observe({
    if (is.null(input$format) || input$format == "") {
      shinyjs::disable("report")
    } else {
      shinyjs::enable("report")
    }
  })
  
  # observe({
  #   shinyjs::toggleState("report", !is.null(input$format) && input$format != "")
  # })
  
  dt_tasks <- reactive({ raw_data_tasks })
  dt_projects <- reactive({ raw_data_projects })
  
    
  vals <- reactiveValues()
  

  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$Tasks <- renderRHandsontable({
    
  ##------Based on the project slectio in the project table, the tasks table will be filtered for selected project------------------------------------------   
          
      if (is.null(input$Projects$changes$changes[[1]][[4]])) {
              rhandsontable(dt_tasks(),readOnly = FALSE, search = TRUE, selectCallback = TRUE) %>% 
              hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
              hot_table(highlightRow = TRUE, highlightCol = TRUE) %>% 
              hot_col("STATUS",renderer = text_renderer_tasks, type = "autocomplete") %>% 
              hot_rows(fixedRowsTop = 1)
          }
          else{
              rhandsontable(dt_tasks_filtered(),readOnly = FALSE, search = TRUE, selectCallback = TRUE) %>% 
              hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
              hot_table(highlightRow = TRUE, highlightCol = TRUE) %>% 
              hot_col("STATUS",renderer = text_renderer_tasks, type = "autocomplete") %>% 
              hot_rows(fixedRowsTop = 1)
            
      
          }
    
      })
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  ## Filter the data in the Tasks table based on the selection made on the projects table
  
          output$selected <- renderPrint({ 
            #input$Projects$changes$changes[[1]][[4]]
            input$Projects_select$select$r
            
          })
          
          
          
          dt_tasks_filtered <- reactive({
            dt_tasks() %>%
              #filter(dt_tasks()$PROJECT.NAME == input$Projects$changes$changes[[1]][[4]])
              filter(dt_tasks()$PROJECT.NAME == as.character(dt_projects()$PROJECT.NAME[input$Projects_select$select$r]))
          })
  
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  output$Projects <- renderRHandsontable({
    
    rhandsontable(dt_projects(), readOnly = FALSE, search = TRUE, selectCallback = TRUE ) %>% 
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE ) %>%
      hot_table(highlightRow = TRUE, highlightCol = TRUE) %>% 
      hot_col("PROJECT.STATUS", renderer = text_renderer, type = "autocomplete") %>% 
      hot_rows(fixedRowsTop = 1)
      
      
      
  })
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
 
  output$pivot <- renderRpivotTable({
    
      if (input$selectData == "Projects") {
            rpivotTable(dt_projects())
        }
    else{
            rpivotTable(dt_tasks())
        }
      
  })
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  # on click of button the file will be saved to the working directory
  observeEvent(input$saveBtnProjects, 
               #write.csv(hot_to_r(input$Projects), file = "./Data/project_tracker.csv",row.names = FALSE)
               saveRDS(hot_to_r(input$Projects),"Projects.rds")
               )
  
  
  observeEvent(input$saveBtnTasks, 
               #write.csv(hot_to_r(input$Tasks), file = "./Data/task_tracker.csv",row.names = FALSE)
               saveRDS(hot_to_r(input$Projects),"Projects.rds")
               )
  
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  ## Projects & Tasks --> Metrics --> plots the status of all the tasks for each of the leads
  output$Tasks_Leads <- renderPlotly({
    
    gg_plot1 <- ggplot(dt_tasks()) +
      aes(x = STATUS, fill = STATUS) +
      geom_bar() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Tasks Status by Leads") +
      theme_bw() +
      facet_wrap(vars(LEAD), scales = "free")
    
      # Assign the ggplot1 to vals$ggplot1 and print it so that during download, we can retrive the same
      vals$gg_plot1 <- gg_plot1
      print(gg_plot1)
    
  })
  

  
  ##-----------------------All the notifications are grouped here-------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$BtnResetTasks,{
    #shinyalert("Reset Successful!", "All fields have been reset", type = "info")
    shinyjs:: js$refresh()
    showNotification("Tasks Reset completed !", type = "message")
  })
  
  observeEvent(input$BtnResetProjects,{
    #shinyalert("Reset Successful!", "All fields have been reset", type = "info")
    shinyjs:: js$refresh()
    showNotification("Projects Reset completed !", type = "message")
  })
  
  observeEvent(input$saveBtnProjects,{
      showNotification("Projects data saved successfully !", type = "message")
  })
  
  observeEvent(input$saveBtnTasks,{
    showNotification("Tasks data saved successfully !", type = "message")
  })
  #------------------------------------------------------------------------------------------------------------------------------------------------------
 
  
  
  ##----------------Timesheet tab------------------------------------------------------------------------------------------------------------------------
  
  # set.seed(2020-01-01)
  # calendar_data <- data.frame(
  #   day = seq(as.Date("2020-01-02"), as.Date("2020-12-31"), "1 day"),
  #   val = sample(100, 365, replace=TRUE)
  # )



        observeEvent(input$selectAnalyst,{
      
          output$timesheet <- renderSupercaliheatmapwidget({
      
                dt_time_tmp <- dt_time() %>%
                                  filter(Name == input$selectAnalyst)
      
                supercal(dt_time_tmp, datetime_col = day, value_col = val, height="100%", range = 1)
      
                })
      
       })
  

  
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  ##                                    Section for all the javascripts functions
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  # Custom renderer function to highlight the entire text in the column with specified color
  color_renderer <- "
  
      function(instance, td) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.background = 'AQUA';
        
      }
    "
  
  
  # Custom renderer function to highlight the entire text in the column with specified color
  # Text Renderer
  
  text_renderer <-  "
          function(instance, td, row, col, prop, value, cellProperties) {
          
              if (instance.getData()[row][8] === 'Completed') {
                    td.style.background = 'AQUA';
                  } 
              else if(instance.getData()[row][8] === 'Delayed') {
                    td.style.background = 'RED';
              }
              else{
                  td.style.background = 'YELLOW';
              };
              Handsontable.renderers.TextRenderer.apply(this, arguments);
          }"
  
  text_renderer_tasks <-  "
          function(instance, td, row, col, prop, value, cellProperties) {
          
              if (instance.getData()[row][11] === 'Completed') {
                    td.style.background = 'AQUA';
                  } 
              else if(instance.getData()[row][11] === 'Delayed') {
                    td.style.background = 'RED';
              }
              else{
                  td.style.background = 'YELLOW';
              };
              Handsontable.renderers.TextRenderer.apply(this, arguments);
          }"
  

 
  ##---------------------------------------------------------------------------------------------------------------------------------------------------
  ##-----------------------------------------HOME page METRICS-----------------------------------------------------------------------
  ##------------------------- code to populate the valueboxes on the home page------------------------------------------------------------------------
  

  output$Completed <- renderValueBox({
    
    completed_projects <- dt_projects() %>%
                            filter(dt_projects()$PROJECT.STATUS == "Completed")
    
    valueBox(value = tags$p(NROW(completed_projects), style = "font-size: 150%;"), 
              "Projects Completed ", icon = icon("stack-overflow"), color = "green")
  })
  
  
  output$WIP <- renderValueBox({
    
    WIP_projects <- dt_projects() %>%
      filter(dt_projects()$PROJECT.STATUS == "In Progress")
    
    valueBox(value = tags$p(NROW(WIP_projects), style = "font-size: 150%;"),  
             "Projects In Progress ", icon = icon("spinner"), color = "yellow")
  })
  
  
  output$Delayed <- renderValueBox({
    
    Delayed_projects <- dt_projects() %>%
      filter(dt_projects()$PROJECT.STATUS == "Delayed")
    
    valueBox(value = tags$p(NROW(Delayed_projects), style = "font-size: 150%;"),  
             "Projects Delayed ", icon = icon("exclamation-triangle"), color = "teal")
  })
  
  output$OnHold <- renderValueBox({
    
    OnHold_projects <- dt_projects() %>%
      filter(dt_projects()$PROJECT.STATUS == "On Hold")
    
    valueBox(value = tags$p(NROW(OnHold_projects), style = "font-size: 150%;"),   
             "Projects On Hold", icon = icon("fire"), color = "red")
  })
  

  ##-----------------------Project Categories----------------------------------------------------------------------------------------------------------------------------
  
  
  ##---------------Home page / project share of research area pie chart------------------------------------------
    output$Project_category <- renderPlotly({
      
          df_ProjectType <- dt_projects() %>% 
            group_by(PROJECT.TYPE) %>% 
            summarise("#Projects" = n())
          
          fig <- plot_ly(type='pie', labels=df_ProjectType$PROJECT.TYPE, values=df_ProjectType$`#Projects`, 
                 textinfo='label+percent',insidetextorientation='radial')
          
          fig <- fig %>% layout(legend = list(orientation = 'h'))
          
          
    })
  
  
  ##---------------Home page / Project status chart--------------------------------------------------------------
    output$Project_status <- renderPlotly({
      
      ggplot(dt_projects()) +
        aes(x = PROJECT.TYPE, fill = PROJECT.STATUS) +
        geom_bar(position = "dodge") +
        scale_fill_hue() +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none") +
        facet_wrap(vars(PROJECT.STATUS))
        
    })
    
    ##---------------Home page / project complexity donut chart-------------------------------------------------  
    output$Complexity <- renderPlotly({
      
          p1 <- dt_projects() %>%
            group_by(COMPLEXITY) %>%
            summarise("Projects" = n())
          
          fig <- p1 %>% plot_ly(labels = ~COMPLEXITY, values = ~Projects)
          fig <- fig %>% add_pie(hole = 0.6)
          fig <- fig %>% layout(showlegend = T,xaxis = list(showgrid = T),yaxis = list(showgrid = T))
          fig  

    })
  
  ##---------------Home page / Overview of projects------------------------------------------------- 
  output$Overview <- renderDT({
    
    df_tmp_projects <-dt_projects()
    
    datatable(df_tmp_projects[,-1],options = list(pageLength = 10))
      
      
  })
  
  
   
    
  ##---------------Home page / Upcoming deadlines-------------------------------------------------  
  
  output$upcoming_deadlines <- renderDT({
          
    
    
    df_tmp_upcoming <- dt_tasks()
  
    df_tmp_upcoming$END.DATE <- dmy(df_tmp_upcoming$END.DATE)
    df_tmp_upcoming$START.DATE <- dmy(df_tmp_upcoming$START.DATE)
      
    df_tmp_upcoming <- df_tmp_upcoming%>% 
                          filter((as.Date(END.DATE) > today()) & (STATUS != "Completed"))
    
    
    datatable(df_tmp_upcoming[,c("PROJECT.NAME", "TASK.NAME", "START.DATE","END.DATE", "STATUS")],options = list(pageLength = 5))
    
  })

  
  ##---------------Home page / Overdue Tasks-------------------------------------------------  
  
  output$overdue_tasks <- renderDT({
        
    df_tmp_overdue <- dt_tasks()
    df_tmp_overdue$END.DATE <- dmy(df_tmp_overdue$END.DATE)
    df_tmp_overdue$START.DATE <- dmy(df_tmp_overdue$START.DATE)
    
    df_tmp_overdue <- df_tmp_overdue %>% 
                        filter(as.Date(END.DATE) < today() & STATUS != "Completed")
      
    
    datatable(df_tmp_overdue[,c("PROJECT.NAME", "TASK.NAME", "START.DATE","END.DATE", "STATUS")  ],options = list(pageLength = 5))
              
  })
  
  ##---------------Home page / Audits Tasks-------------------------------------------------  
    
    output$Audits <- renderPlotly({

      p1 <- dt_projects() %>%
        group_by(AUDIT) %>%
        summarise("count" = n())

      fig <- p1 %>% plot_ly(labels = ~AUDIT, values = ~count)
      fig <- fig %>% add_pie(hole = 0.6)
      fig <- fig %>% layout(showlegend = T,xaxis = list(showgrid = T),yaxis = list(showgrid = T))
      fig


    })

  ##---------------Home page / projects & Tasks count-------------------------------------------------  
  
# 
  output$Proj_task_cnt <- renderPlotly({


    df_proj_task <- dt_tasks() %>%
      group_by(PROJECT.NAME, TASK.NAME) %>%
      summarise("cnt" = n()) %>%
      arrange(desc(cnt))


    ggplot(df_proj_task) +
      aes(x = PROJECT.NAME) +
      geom_bar(fill = "#6baed6") +
      labs(y = "Number of Tasks") +
      coord_flip() +
      theme_bw()


  })
  

  ##---------------------------------------------------------------------------------------------------------------------------------------------------
  ##-----------------------------------------Projects page Gantt----------------------------------------------------------------------------------------
  
  
    output$schedule <- renderPlotly({
        
            df <- dt_tasks() %>% 
                      filter( PROJECT.NAME == input$selectProject) %>% 
                      select("TASK.NAME","ANALYST", "START.DATE", "DAYS")
            
            
            
            # Convert to dates
            df$START.DATE <- dmy(df$START.DATE)
            #df$END.DATE <- dmy(df$END.DATE)
            
            df$START.DATE <- as.Date(df$START.DATE, format = "%m/%d/%Y")
            #df$END.DATE <- as.Date(df$END.DATE, format = "%m/%d/%Y")
            
            
            # Sample client name
            client = "Sample Client"
            
            # Choose colors based on number of resources
            cols <- RColorBrewer::brewer.pal(length(unique(df$ANALYST)), name = "Set3")
            df$color <- factor(df$ANALYST, labels = cols)
            
            # Initialize empty plot 
            fig <- plot_ly()
            
            # Each task is a separate trace
            # Each trace is essentially a thick line plot
            # x-axis ticks are dates and handled automatically
            
            for(i in 1:(nrow(df))){
              fig <- add_trace(fig,
                               x = c(df$START.DATE[i], df$START.DATE[i] + (df$DAYS[i])),  # x0, x1
                               y = c(i,i),  # y0, y1
                               mode = "lines",
                               line = list(color = df$color[i], width = 20),
                               showlegend = F,
                               hoverinfo = "text",
                               
                               # Create custom hover text
                               
                               text = paste("Task: ", df$TASK.NAME[i], "<br>",
                                            "Duration: ", df$DAYS[i], "days<br>",
                                            "Resource: ", df$ANALYST[i]),
                               
                               evaluate = T  # needed to avoid lazy loading
              )
            }

            fig
  
      })
  
  
       observeEvent(input$selectProject_new,{

           output$schedule_new <- renderTimevis({
                     df <- dt_tasks() %>%
                       filter( PROJECT.NAME == input$selectProject_new) %>%
                       select("TASK.NAME","ANALYST", "START.DATE", "END.DATE")
                     
                     df$START.DATE <- dmy(df$START.DATE)
                     df$END.DATE <- dmy(df$END.DATE)
                     
                     colnames(df)[3] <- "start"
                     colnames(df)[4] <- "end" 
                
                 timevis(df)
                  
               })
           
           output$schedule_table <- renderRHandsontable({
              
             df1 <- dt_tasks() %>% 
               filter(PROJECT.NAME == input$selectProject_new)
             
             rhandsontable(df1, readOnly = TRUE)
               
           })
           
           
     })
       
       
       # ##-------------------------Dynamic report generation-------------------------------------------
   
       output$report = downloadHandler(
         filename<- function(){
           paste("Summary",Sys.Date(),switch(
             input$format, PDF = '.pdf', Word = '.docx'
           ),sep = "")
         },
         
         
         content = function(file) {
           if (input$format=="PDF"){
             #### Progressing indicator
             withProgress(message = 'Download in progress',
                          detail = 'This may take a while...', value = 0, {
                            for (i in 1:15) {
                              incProgress(1/15)
                              Sys.sleep(0.01)
                            }
                            
                            ## End of progression
                            src <- normalizePath('report.Rmd')
                            
                            # temporarily switch to the temp dir, in case you do not have write
                            # permission to the current working directory
                            owd <- setwd(tempdir())
                            on.exit(setwd(owd))
                            file.copy(src, 'report.Rmd', overwrite = TRUE)

                            library(rmarkdown)
                            out <- render('report.Rmd', pdf_document())
                            file.rename(out, file)
                            
                          })
             ### below is the end of pdf content
           }else{
             withProgress(message = 'Download in progress',
                          detail = 'This may take a while...', value = 0, {
                            for (i in 1:15) {
                              incProgress(1/15)
                              Sys.sleep(0.01)
                            }
                            
                            ## End of progression
                            src <- normalizePath('report.Rmd')
                            
                            # temporarily switch to the temp dir, in case you do not have write
                            # permission to the current working directory
                            owd <- setwd(tempdir())
                            on.exit(setwd(owd))
                            file.copy(src, 'report.Rmd', overwrite = TRUE)
                            
                            library(rmarkdown)
                            out <- render('report.Rmd', word_document())
                            file.rename(out, file)
                          })
           }
           
         })
  
  
      
        
})















  

















