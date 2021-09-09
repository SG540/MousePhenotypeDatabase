
####Version 0.1 by Giovanni Sala (Fujita Health University, Liverpool University)####
##7 Sept 2021




####Libraries####

list.of.packages <- c('RMySQL', 'openxlsx', 'odbc', 'dplyr', 'shiny',
                      'nlme', 'stringr', 'tidyverse', 'gtools','plyr',
                      'ggpubr','rstatix')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(RMySQL)
library(openxlsx)
library(odbc)
library(dplyr)
library(nlme)
library(stringr)
library(tidyverse)
library(gtools)
library(plyr)
library(shiny)
library(ggpubr)
library(rstatix)

#library('shinyWidgets')
#shinyWidgetsGallery()
#for more ideas

####Server####


server <- function(input, output, session) {

  #connection ----
  
  connection <- reactive({dbConnect(odbc::odbc(), 
                  .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                  Server="204.2.195.88", Database="mouse_phenotype_database",
                  UID = "guest_mpd_app",
                  PWD="fuffu_2021_!?_sambapati", Port=17703)})
  
  #joining tasks' tables  ----
  
  datasetInput <- reactive({
    
    mouse_info <- DBI::dbReadTable(connection(), "mouse_info") %>%
      dplyr::select(MouseID,Genotype,Gender,GroupID) %>%
      dplyr::filter(!Genotype=="X")

    if (input$task %in% c(
      "bm_t01",
      "crsoc_t01",
      "ep_t01",
      "ghns_t01",
      "hp_t01",
      "ld_t01",
      "ppi_t01",
      "tm_t01")) {
      
      tb <- DBI::dbReadTable(connection(), input$task)
      dplyr::right_join(tb,mouse_info,by="MouseID") %>%
        filter(GroupID==input$group)
      
    } else if (input$task %in% c(
      "fz_day1",
      "fz_day2",
      "fz_day3")) {
      
      tb <- DBI::dbReadTable(connection(), paste0(input$task))
      tb_sum <- DBI::dbReadTable(connection(), paste0(input$task,"_sum"))
      dplyr::right_join(tb,mouse_info,by="MouseID") %>%
        left_join(., tb_sum, by="MouseID") %>%
        filter(GroupID==input$group)
    
    } else if (input$task=="ps_t02") {
      
      tb <- DBI::dbReadTable(connection(), paste0(input$task))
      tb_sum <- DBI::dbReadTable(connection(), paste0("ps_sum_t02"))
      dplyr::right_join(tb,mouse_info,by="MouseID") %>%
        left_join(., tb_sum, by="MouseID") %>%
        filter(GroupID==input$group)
      
    } else if (input$task %in% c("si","hc")) {
      
      tb <- DBI::dbReadTable(connection(), paste0(input$task, "_t01"))
      tb_sum <- DBI::dbReadTable(connection(), paste0(input$task, "_sum_t01"))
      dplyr::right_join(tb,tb_sum,by=paste0(toupper(input$task),"_TrialID")) %>%
        left_join(., mouse_info, by="MouseID") %>%
        filter(GroupID==input$group)
      
    } else {
      
      tb <- DBI::dbReadTable(connection(), paste0(input$task,"_t01"))
      tb_sum <- DBI::dbReadTable(connection(), paste0(input$task,"_sum_t01"))
      dplyr::right_join(tb,mouse_info,by="MouseID") %>%
        left_join(., tb_sum, by="MouseID") %>%
        filter(GroupID==input$group) 
      
    }
    
  })
  
  

  
  
  
  #outputs ----
  
  #tables
  output$table <- renderDataTable({
    datasetInput()
    
  })
  
  #strain table
  output$table_strain <- renderDataTable({
    DBI::dbReadTable(connection(), "strain_info")
  })
  
  ##plot ----

  output$plot <- renderPlot({
    
    
    
    #create a set of if else statements
    
    if (input$task == "ghns_t01"){
      
      data <- datasetInput()
      
      List_var <- paste0("GHNS_", c("RectalTemperature","BodyWeight",
                                    "WireHang","GripStrength")) ##to be modified 
      List_label <- c("Body temperature (C)","Body weight (g)",
                      "Wire Hang latency (s)","Grip strength (N)")
      
      nc <- 2
      nr <- 2
      
    } else if (input$task == "ep_t01") { 
      
      data <- datasetInput()
      
      List_var <- paste0("EP_", c("TotalDistance","PercentageOpenArmEntries",
                                  "OpenArmStayRatio","TotalEntries"))  
      List_label <- c("Distance traveled (cm)","Entries into open arms (%)",
                      "Time on open arms (ratio)","Number of entries")
      
      nc <- 2
      nr <- 2
      
    } else if (input$task == "ld_t01") {
      
      data <- datasetInput()
      
      List_var <- paste0("LD_", c("DarkDistance","LightDistance",
                                  "NumberOfTransitions","LightTime"))  
      List_label <- c("Distance traveled Dark (cm)","Distance traveled Light (cm)",
                      "Number of transitions","Stay time in light (s)")
      
      nc <- 2
      nr <- 2  
      
    } else if (input$task == "hp_t01") {
      
      data <- datasetInput()
      
      List_var <- paste0("HP_", c("Latency"))  
      List_label <- c("Latency (s)")
      
      nc <- 1
      nr <- 1 
      
    } else if (input$task == "si") {
      
      data <- datasetInput() %>%
        dplyr::select(SI_TrialID, SI_AverageTotalDistance, SI_NumTotalContacts,
                      SI_TotalDurActiveContact_sec, SI_TotalDurContact_sec,
                      Genotype, Gender, GroupID) %>%
        dplyr::distinct()
      
      List_var <- paste0("SI_", c("AverageTotalDistance", "NumTotalContacts",
                                  "TotalDurActiveContact_sec","TotalDurContact_sec"))  
      List_label <- c("Distance traveled (cm)","Number of contacts",
                     "Active contact (s)","Total duration of contacts (s)")
      
      nc <- 2
      nr <- 2 
      
    }
    
    myplots <- vector('list', length(List_var))
    
    
    for (i in seq_along(List_var)) {
      
      stat.test <- data %>% #obtain p-values
        t_test(as.formula(paste0(List_var[i], " ~ Genotype"))) %>%
        add_xy_position(x = "Genotype")
      
      labels <- NULL
      labels=c()
      for (n in sort(unique(data$Genotype))){
        labels = c(labels, paste0(n, " (n = ", table(data$Genotype)[n], ")"))
      }
      
      
      #message(i)
      myplots[[i]] <- local({
        i <- i
        
        
        Plot <- 
          ggboxplot(data, x = "Genotype", y = List_var[i],
                    fill = "Genotype", alpha=0.5)+ 
          #stat_compare_means(label.y = I(max(data[,List_var[i]]) + 
           #                                I(12*sd(data[,List_var[i]])/sqrt(length(data[,1]))
            #                               )))+
          stat_pvalue_manual(stat.test, label = "p", tip.length = 0.01, vjust = 0.5) +
          #geom_jitter(aes(x=Genotype, y=data[,List_var[i]]), col = "black", width = .2, alpha=.6) +
          #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
          scale_fill_viridis_d(label=labels) +
          
          ylab(List_label[i]) +
          #theme_bw() +
          theme_classic() + 
          theme(text = element_text(size = 15), axis.text.y = element_text(size = 15),
                legend.text=element_text(size = 15#,face="bold"
                ),
                axis.title.x = element_blank(), axis.title.y = element_text(size = 15,
                                          margin = margin(t = 0, r = 15, b = 0, l = 0)),
                axis.text.x = element_blank(), legend.text.align = 0,
                legend.title = element_blank(), axis.line = element_line(),
                panel.border = element_blank()) +
          
          theme(axis.line = element_line(size = 1)) +
          theme(axis.ticks = element_line(size = 1), 
                axis.ticks.length = unit(.3, "cm")) +
          
          theme(plot.margin = unit(c(.8,.8,.8,.8), "cm"))
        

      })
      
      
    }
    
    ggarrange(plotlist = myplots,
              #labels = c("A", "B", "C", "D"), font.label = list(size = 20), 
              ncol = nc, nrow = nr, #if you find a way to generalize this, that would be very nice
              common.legend = TRUE, legend = "bottom")
    
  }
  )
  
  
  # Downloadable xlsx of selected dataset ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(datasetInput(), file, rowNames = FALSE)
    }
  )
  
  output$downloadStrains <- downloadHandler(
    filename = paste("Strains", ".xlsx", sep = ""),
    content = function(file) {
      write.xlsx(DBI::dbReadTable(connection(), "strain_info"),
                 file, rowNames = FALSE)
    }
  )
  
}

####ui####

ui <- fluidPage(
  titlePanel("Mouse Phenotype Database"),
  
  

    #consider put one download button in each panel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", HTML("Get well soon, Nozomu!")),
        tabPanel("Strain Summary",downloadLink("downloadStrains", "Download"),
          dataTableOutput("table_strain")),
        
        tabPanel("Tables and Plots",
           
                 sidebarLayout(
                  sidebarPanel(      
          selectInput("group", "Enter group", choices = unique(
                   
          sort(DBI::dbReadTable(dbConnect(odbc::odbc(), 
                .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                Server="204.2.195.88", Database="mouse_phenotype_database",
                UID = "guest_mpd_app",
                PWD="fuffu_2021_!?_sambapati", Port=17703), "mouse_info")[,"GroupID"])

                      )),
                 
          selectInput("task", "Enter task", choices = c(
                   
                   "Barnes Maze"="bm_t01",
                   "Barnes Maze Probe"="bm_probe",
                   "Crawley Social Interaction"="crsoc_t01",
                   "Elevated Plus Maze"="ep_t01",
                   "Fear Conditioning Day 1"="fz_day1",
                   "Fear Conditioning Day 2"="fz_day2",
                   "Fear Conditioning Day 3"="fz_day3",
                   "General Assessment"="ghns_t01",
                   "Home Cage Social Interaction"="hc",
                   "Hot Plate"="hp_t01",
                   "Light/Dark Transition"="ld_t01",
                   "Open Field"="of",
                   "Porsolt Swim"="ps",
                   "Porsolt Swim Day 2"="ps_t02", #GS: not the best of the designs
                   "Pre-Pulse Inhibition"="ppi_t01",
                   "Rotarod"="rr",
                   "Social Interaction"="si",
                   "Tail Suspension"="ts",
                   "T-Maze"="tm_t01"
                   
                 )),
                
          selectInput("tableorplot", "Enter Table or Plot", choices = c("Table","Plot")),
          
          downloadLink("downloadData", "Download")),
                      
          mainPanel(
            conditionalPanel(condition="input.tableorplot == 'Table'",
                             dataTableOutput("table")),
            conditionalPanel(condition="input.tableorplot == 'Plot'",
                             plotOutput("plot", width = "700px", height = "700px"))
            )
                 
                 )),
        tabPanel("About", HTML("
        
        <p>This app provides all results for the publication:</p>

<p>If you refer to this app, please cite this publication.</p>
<ul>
							<li>This is version 0.1 of this.</li>
							<li>The red dot at the bottom shows the true effect size. Blue dots show the naive random-effects estimate, and PET and PEESE estimates, if selected.</li>
							</ul>
							"))
      )))
  

####Run####

shinyApp(ui = ui, server = server)






