####Libraries####
rm(list = ls(all.names = TRUE))
list.of.packages <- c('RMySQL', 'openxlsx', 'odbc', 'dplyr', 'shiny',
                      'nlme', 'stringr', 'tidyverse', 'gtools','plyr',
                      'ggpubr','rstatix', 'rsconnect','shinyBS','ggplot2',
                      'dbplyr','DBI','slider')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(openxlsx)
library(tidyverse)
library(gtools)

wd <- "~/Dropbox/研究室/投稿論文/2021/Giovanni_MouseDatabase/MousePhenotypeDatabase/Tables"
setwd(wd)

group_list <- read.xlsx("mouse_info.xlsx") %>% #DBI::dbReadTable(connection(), "mouse_info")
  dplyr::select(MouseID,Genotype,Gender,GroupID) %>%
  dplyr::pull(GroupID) %>%
  unique()

mouse_info_all <- read.xlsx("mouse_info.xlsx") %>% #DBI::dbReadTable(connection(), "mouse_info")
  dplyr::select(MouseID,Genotype,Gender,GroupID) %>%
  dplyr::filter(!Genotype=="X")


#### set some lists and variables ####

task_list <- c(
  
  "Barnes Maze Probe"="bm_probe",
  "Barnes Maze"="bm_t01",
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
  "Porsolt Swim Day 2"="ps_t02",
  "Pre-Pulse Inhibition"="ppi_t01",
  "Rotarod"="rr",
  "Social Interaction"="si",
  "T-Maze"="tm_t01",
  "Tail Suspension"="ts"
  
)

task_sum_list <- c(
  
  "bm_probe_sum_t01",
  "bm_t01",
  "crsoc_t01",
  "ep_t01",
  "fz_day1_sum",
  "fz_day2_sum",
  "fz_day3_sum",
  "ghns_t01",
  "hc_sum_t01",
  "hp_t01",
  "ld_t01",
  "of_sum_t01",
  "ps_sum_t01",
  "ps_sum_t02",
  "ppi_t01",
  "rr_sum_t01",
  "si_sum_t01",
  "tm_t01",
  "ts_sum_t01")

# task name
keys <- list("ghns_t01", 
             "ld_t01",
             "ep_t01",
             "hp_t01",
             "si",
             "crsoc_t01",
             "of", 
             "rr", 
             "ps",
             "ps_t02",
             "ppi_t01",
             "ts",
             "tm_t01",
             "bm_t01",
             "bm_probe",
             "fz_day1",
             "fz_day2",
             "fz_day3",
             "hc")

# task name2
val_task <- list("GHNS",
                 "LD",
                 "EP",
                 "HP",
                 "SI",
                 "CrSoc",
                 "OF", 
                 "RR", 
                 "PS_Day1",
                 "PS_Day2",
                 "PPI",
                 "TS",
                 "TM",
                 "BM",
                 "BM_probe",
                 "FZ_Day1",
                 "FZ_Day2",
                 "FZ_Day3",
                 "HC")

# column names to plot
val_varname <- list(c("RectalTemperature","BodyWeight","WireHang","GripStrength"), #GHNS
                    c("DarkDistance","LightDistance","NumberOfTransitions","LightTime"), #LD
                    c("TotalDistance","PercentageOpenArmEntries","OpenArmStayRatio","TotalEntries"), #EP
                    c("Latency"), #HP
                    c("AverageTotalDistance", "NumTotalContacts","TotalDurActiveContact_sec","TotalDurContact_sec"), #SI
                    c("STofCage","TotalDistance","STofCage","TotalDistance"), #CrSoc
                    c("Distance","CentTime","VerAct","StCounts"), #OF
                    c("Latency"), #RR
                    c("Immobility","Distance"), #PS_Day1
                    c("Immobility","Distance"), #PS_Day2
                    c("ASR_value","PPI_value"), #PPI
                    c("Immobility","Distance"), #TS
                    c("TotalDistance","Error","Latency","PercentageOfCorrectResponses"), #TM
                    c("ErrorsTo1st","LatencyTo1st","DistanceTo1st","OmissionErrors"), #BM
                    c("Around_hole_Time"), #BM_probe
                    c("Immobility","Distance","DV"), #FZ_Day1
                    c("Immobility","Immobility","Distance","Distance"), #FZ_Day2
                    c("Immobility","Immobility","Distance","Distance"), #FZ_Day3
                    c("AverageDistribution","TotalArea")) #HCSI

# label names in plot
val_ylab <- list(c("Body temperature (C)","Body weight (g)","Wire hang latency (sec)","Grip strength (N)"), #GHNS
                 c("Distance traveled Dark (cm)","Distance traveled Light (cm)","Number of transitions","Stay time in light (sec)"), #LD
                 c("Distance traveled (cm)","Entries into open arms (%)","Time on open arms (ratio)","Number of entries"), #EP
                 c("Latency (sec)"), #HP
                 c("Distance traveled (cm)","Number of contacts","Active contact (sec)","Total duration of contacts (sec)"), #SI
                 c("Time spent around cage (sec)\n in sociability test","Distance travelled (cm)\n in sociability test","Time spent around cage (sec)\n in social novelty presence test","Distance travelled (cm)\n in social novelty presence test"), #CrSoc
                 c("Distance traveled (cm)","Time spent in center (sec)","Vertical activity","Number of stereotypic counts"), #OF
                 c("Latency (sec)"), #RR
                 c("Immobility (%)","Distance traveled (cm)"), #PS_Day1
                 c("Immobility (%)","Distance traveled (cm)"), #PS_Day2
                 c("Startle amplitude","Prepulse inhibition"), #PPI
                 c("Immobility (%)","Distance traveled (cm)"), #TS
                 c("Distance traveled (cm)","Number of errors","Latency (sec)","Correct responses (%)"), #TM
                 c("Number of errors to target hole","Latency to target hole (sec)","Distance to target hole (cm)","Number of omission errors"), #BM
                 c("Time spent around each hole (sec)"), #BM_probe
                 c("Freezing (%)","Distance travelled (cm)","Distance travelled (cm)"), #FZ_Day1
                 c("Freezing (%)","Freezing (%)","Distance travelled (cm)","Distance travelled (cm)"), #FZ_Day2
                 c("Freezing (%)","Freezing (%)","Distance travelled (cm)","Distance travelled (cm)"), #FZ_Day3
                 c("Mean number of particles","Activity level (arbitrary unit)")) #HCSI

# number of columns/rows in plot
val_numcolrow <- list(c(2,2), #GHNS
                      c(2,2), #LD
                      c(2,2), #EP
                      c(1,1), #HP
                      c(2,2), #SI
                      c(2,2), #CrSoc
                      c(2,2), #OF
                      c(1,1), #RR
                      c(2,1), #PS_Day1
                      c(2,1), #PS_Day2
                      c(1,2), #PPI
                      c(2,1), #TS
                      c(2,2), #TM
                      c(2,2), #BM
                      c(1,1), #BM_probe
                      c(2,2), #FZ_Day1
                      c(2,2), #FZ_Day2
                      c(2,2), #FZ_Day3
                      c(1,2)) #HCSI

# x axis to plot
val_xaxis <- list("Genotype", #GHNS
                  "Genotype", #LD
                  "Genotype", #EP
                  "Genotype", #HP
                  "Genotype", #SI
                  c("CrSoc_Place","Genotype","CrSoc_Place","Genotype"), #CSI
                  "OF_Time", #OF
                  "RR_Time", #RR
                  "PS_Day1_Time", #PS_Day1
                  "PS_Day2_Time", #PS_Day2
                  c("PPI_ASR","PPI_PPI"), #PPI
                  "TS_Time", #TS
                  "TM_Time", #TM
                  "BM_Time", #BM
                  "BM_probe_Angle", #BM_Probe
                  "FZ_Day1_Time", #FZ_Day1
                  "FZ_Day2_Time", #FZ_Day2
                  "FZ_Day3_Time", #FZ_Day3
                  "HC_Time") #HCSI

# x label to plot
val_xlab <- list(NA, #GHNS
                 NA, #LD
                 NA, #EP
                 NA, #HP
                 NA, #SI
                 NA, #CSI
                 "Time (min)", #OF
                 "Trials", #RR
                 "Time (min)", #PS_Day1
                 "Time (min)", #PS_Day2
                 c("Sound level (dB)","Prepulse sound level (dB)"), #PPI
                 "Time (min)", #TS
                 "Sessions", #TM
                 "Trials", #BM
                 "Distance (angle) from target", #BM_probe
                 c("Time (min)","Time (min)","Time (sec)"), #FZ_Day1
                 "Time (min)", #FZ_Day2
                 "Time (min)", #FZ_Day3
                 "Time") #HCSI

# set hash tables
hash_task <- list2env(setNames(val_task, keys), hash = TRUE) # List_var
hash_varname <- list2env(setNames(val_varname, keys), hash = TRUE) # List_label
hash_ylab <- list2env(setNames(val_ylab, keys), hash = TRUE) # nc
hash_numcolrow <- list2env(setNames(val_numcolrow, keys), hash = TRUE) # nr
hash_xaxis <- list2env(setNames(val_xaxis, keys), hash = TRUE) # x axis
hash_xlab <- list2env(setNames(val_xlab, keys), hash = TRUE) # x label

for (i in 1:length(task_list)) {
  
  if (task_list[i] %in% c(
    "bm_t01",
    "crsoc_t01",
    "ep_t01",
    "ghns_t01",
    "hp_t01",
    "ld_t01",
    "ppi_t01",
    "tm_t01")) {
    
    tmp <- dplyr::inner_join(read.xlsx(paste0(task_list[i],".xlsx")), #DBI::dbReadTable(connection, input$task),
                             mouse_info_all, by="MouseID")
    assign(paste0("task_", task_list[i]), tmp)
    
  } else if (task_list[i] %in% c(
    "fz_day1",
    "fz_day2",
    "fz_day3")) {
    
    tmp <- dplyr::inner_join(read.xlsx(paste0(task_list[i],".xlsx")), #DBI::dbReadTable(connection, paste0(input$task))
                             mouse_info_all, by="MouseID") %>% 
      dplyr::inner_join(., read.xlsx(paste0(task_list[i],"_sum.xlsx")), by="MouseID") #DBI::dbReadTable(connection, paste0(input$task,"_sum"))
    assign(paste0("task_", task_list[i]), tmp)
      
    
  } else if (task_list[i]=="ps_t02") {
    
    tmp <- dplyr::inner_join(read.xlsx(paste0(task_list[i],".xlsx")), #DBI::dbReadTable(connection, paste0(input$task)),
                             mouse_info_all, by="MouseID") %>%
      dplyr::inner_join(., read.xlsx("ps_sum_t02.xlsx"), by="MouseID") #DBI::dbReadTable(connection, paste0("ps_sum_t02"))
    assign(paste0("task_", task_list[i]), tmp)
    
  } else if (task_list[i] %in% c("si","hc")) {
    
    tmp <- dplyr::inner_join(read.xlsx(paste0(task_list[i],"_t01.xlsx")), #DBI::dbReadTable(connection, paste0(input$task, "_t01")),
                      read.xlsx(paste0(task_list[i],"_sum_t01.xlsx")), #DBI::dbReadTable(connection, paste0(input$task, "_sum_t01")),
                      by=paste0(toupper(task_list[i]),"_TrialID")) %>%
      dplyr::inner_join(., mouse_info_all, by="MouseID")
    assign(paste0("task_", task_list[i]), tmp)
    
  } else {
    
    tmp <- dplyr::inner_join(read.xlsx(paste0(task_list[i],"_t01.xlsx")),#DBI::dbReadTable(connection, paste0(input$task,"_t01")),
                      mouse_info_all, by="MouseID")
    assign(paste0("task_", task_list[i]), tmp)
    
  }
  
  tmp <- dplyr::inner_join(read.xlsx(paste0(task_sum_list[i],".xlsx")), 
                           mouse_info_all, by="MouseID")
  assign(paste0("task_sum_", task_list[i]), tmp)
  
}


#### define mkPlot function####
mkplot <- function(datasetInput, group, task){
  print(group)
  print(task)
  
  # set variables
  prefix <- unlist(mget(task, envir = hash_task))
  List_var <- paste0(prefix, "_", unlist(mget(task, envir = hash_varname))) %>% as.character()
  List_label <- unlist(mget(task, envir = hash_ylab)) %>% as.character()
  nc <- unlist(mget(task, envir = hash_numcolrow))[1] %>% as.integer()
  nr <- unlist(mget(task, envir = hash_numcolrow))[2] %>% as.integer()
  xaxis <- unlist(mget(task, envir = hash_xaxis)) %>% as.character()
  xlab <- unlist(mget(task, envir = hash_xlab)) %>% as.character()

    tb <- datasetInput %>%
    dplyr::mutate_if(is.integer,as.numeric)
  
  # filtering if needed
  if(task == "tm_t01"){
    tb <- dplyr::filter(tb, TM_Time <= 5) %>%
      dplyr::mutate(TM_PercentageOfCorrectResponses = TM_NumberOfCorrectResponses / TM_NumberOfFreeChoices * 100)
  } else if (task == "of"){
    tb <- tb %>%
      dplyr::mutate(OF_Distance = slide_vec(.x = OF_Distance, .f = mean, .before = 4),
                    OF_CentTime = slide_vec(.x = OF_CentTime, .f = mean, .before = 4),
                    OF_VerAct = slide_vec(.x = OF_VerAct, .f = mean, .before = 4),
                    OF_StCounts = slide_vec(.x = OF_StCounts, .f = mean, .before = 4)) %>%
      filter(OF_Time %% 5 == 0)
  } else if (task == "bm_t01"){
    tb <- dplyr::filter(tb, BM_Time <= 15)
  } else if (task == "bm_probe"){
    tb <- dplyr::filter(tb, str_detect(BM_probe_SubTrialNumber, "PT"))# %>%
    # dplyr::mutate_if(is.integer,as.numeric)#not sure if correct
  } else if (task == "ppi_t01"){
    tb <- pivot_longer(tb, contains("ASR"), names_to = "ASR", values_to = "PPI_ASR_value", names_prefix = "PPI_ASR") %>%
      pivot_longer(contains("PPI_PPI"), names_to = "PPI", values_to = "PPI_PPI_value", names_prefix = "PPI_PPI") %>%
      dplyr::mutate(PPI = str_replace(PPI, "_", "-")) %>%
      rename_at(vars(c(ASR,PPI)), ~ paste0("PPI_",c("ASR","PPI")))
  } else if (str_detect(task, "fz")){ 
    if (task == "fz_day1"){
      tb <- tb %>%
        dplyr::mutate(FZ_Day1_Experiment = NA) #%>%
      #dplyr::mutate_if(is.integer,as.numeric)
      
      tb2 <- dplyr::filter(tb, FZ_Day1_Type=="Distance_Shock") %>%
        dplyr::filter(!FZ_Day1_Time %in% c(24,48) & !is.na(FZ_Day1_DV)) %>% # I think that this part should be treated when making database
        dplyr::mutate(FZ_Day1_Time = case_when(FZ_Day1_Time>24 & FZ_Day1_Time<48 ~ FZ_Day1_Time - 1, 
                                               FZ_Day1_Time>48 ~ FZ_Day1_Time - 2,
                                               TRUE ~ FZ_Day1_Time), 
                      FZ_Day1_Shock = case_when((FZ_Day1_Time-1) %/% 23 == 0 ~ "Shock 1",
                                                (FZ_Day1_Time-1) %/% 23 == 1 ~ "Shock 2",
                                                (FZ_Day1_Time-1) %/% 23 == 2 ~ "Shock 3",
                                                TRUE ~ FZ_Day1_Type)) %>%
        dplyr::mutate(FZ_Day1_Time = case_when((FZ_Day1_Time-1) %/% 23 == 0 ~ as.numeric(as.numeric(FZ_Day1_Time)/4+0.25),
                                               (FZ_Day1_Time-1) %/% 23 == 1 ~ as.numeric(as.numeric(FZ_Day1_Time-23)/4+0.25),
                                               (FZ_Day1_Time-1) %/% 23 == 2 ~ as.numeric(as.numeric(FZ_Day1_Time-46)/4+0.25),
                                               TRUE ~ FZ_Day1_Time))
    }
    
    tb1 <- dplyr::select(tb, c("MouseID","Genotype",paste0(prefix,c("_Type","_Experiment","_Time","_DV")))) %>%
      dplyr::filter(get(paste0(prefix,"_Type")) != "Distance_Shock") %>% 
      dplyr::filter(get(paste0(prefix,"_Time")) <= 8) %>%
      # group_by(MouseID) %>%
      # do(head(., 1)) %>% #remove the second data point
      pivot_wider(names_from = contains(paste0(prefix,"_Type")), 
                  values_from = contains(paste0(prefix,"_DV")),
                  names_prefix = paste0(prefix,"_"))
  } else if (task == "crsoc_t01"){
    tb <- tb %>%
      dplyr::filter(CrSoc_SubID != "00") %>%
      dplyr::rename("CrSoc_NEinCenterArea" = CrSoc_NEinCenter) %>%
      pivot_longer(contains("NEin") & contains("Area") , names_to = "Area", values_to = "CrSoc_NEinArea", names_prefix = "CrSoc_NEin") %>%
      pivot_longer(contains("NEin") & contains("Cage") , names_to = "Cage", values_to = "CrSoc_NEinCage", names_prefix = "CrSoc_NEin") %>%
      pivot_longer(contains("STof") & contains("Area") , names_to = "Area2", values_to = "CrSoc_STofArea", names_prefix = "CrSoc_STof") %>%
      pivot_longer(contains("STof") & contains("Cage") , names_to = "Cage2", values_to = "CrSoc_STofCage", names_prefix = "CrSoc_STof") %>%
      pivot_longer(starts_with("Area") | starts_with("Cage") , names_to = "CrSoc_Type", values_to = "CrSoc_Place") %>%
      dplyr::mutate(CrSoc_Place = case_when(str_detect(CrSoc_Place, "Left") ~ "Left", #"Empty side",
                                            str_detect(CrSoc_Place, "Right") ~ "Right", #"Stranger side",
                                            str_detect(CrSoc_Place, "Center") ~ "Center"),
                    CrSoc_Type = case_when(CrSoc_Type == "Area" ~ "CrSoc_NEinArea",
                                           CrSoc_Type == "Area2" ~ "CrSoc_STofArea",
                                           CrSoc_Type == "Cage" ~ "CrSoc_NEinCage",
                                           CrSoc_Type == "Cage2" ~ "CrSoc_STofCage")) %>%
      dplyr::mutate(CrSoc_Experiment = if_else(str_detect(CrSoc_SubID, "1"), "Sociability test", "Preference test"),
                    CrSoc_Place = case_when(CrSoc_SubID == "L1" & CrSoc_Place == "Left" ~ "Stranger 1 side",
                                            CrSoc_SubID == "L1" & CrSoc_Place == "Right" ~ "Empty side",
                                            CrSoc_SubID == "R1" & CrSoc_Place == "Left" ~ "Empty side",
                                            CrSoc_SubID == "R1" & CrSoc_Place == "Right" ~ "Stranger 1 side",
                                            CrSoc_SubID == "L2" & CrSoc_Place == "Left" ~ "Stranger 1 side",
                                            CrSoc_SubID == "L2" & CrSoc_Place == "Right" ~ "Stranger 2 side",
                                            CrSoc_SubID == "R2" & CrSoc_Place == "Left" ~ "Stranger 2 side",
                                            CrSoc_SubID == "R2" & CrSoc_Place == "Right" ~ "Stranger 1 side",
                                            TRUE ~ CrSoc_Place),) %>%
      # CrSoc_Place_Soc = ifelse(CrSoc_Experiment == "Sociability test", CrSoc_Place, NA),
      # CrSoc_Place_Pref = ifelse(CrSoc_Experiment == "Preference test", CrSoc_Place, NA)) %>% 
      dplyr::filter(CrSoc_Type == "CrSoc_STofCage") %>%
      dplyr::select(!c("CrSoc_NEinArea","CrSoc_STofArea","CrSoc_NEinCage")) %>%
      distinct()
  }
  
  if (task %in% keys) {
    myplots <- vector('list', length(List_var))
    
    for (i in seq_along(List_var)) {
      labels <- NULL
      labels = c()
      tb_tmp <- tb %>%
        group_by(MouseID) %>%
        do(head(., 1))
      
      for (n in unique(tb_tmp$Genotype)){
        labels = sort(c(labels, paste0(n, " (n = ", table(tb_tmp$Genotype)[n], ")")))
      }
      
      tb <- tb %>% 
        # filter(!is.na(get(List_var[i]))) %>%
        transform(Genotype = factor(Genotype, levels=sort(unique(tb$Genotype))))
      ncomb <- length(unique(tb_tmp$Genotype)) * (length(unique(tb_tmp$Genotype))-1) / 2
      #message(i)
      myplots[[i]] <- local({
        i <- i
        
        if (xaxis[1] == "Genotype") {
          stat.test <- tb %>% #obtain p-values
            t_test(as.formula(paste0(List_var[i], " ~ Genotype"))) %>%
            add_xy_position(x = "Genotype")
          
          Plot <- 
            ggboxplot(tb, x = "Genotype", y = List_var[i],
                      fill = "Genotype", alpha=0.5)+ 
            # stat_compare_means(label.y = I(max(tb[,List_var[i]]) + 
            #                                  I(12*sd(tb[,List_var[i]])/sqrt(length(tb[,1]))
            #                                  )))+
            stat_pvalue_manual(stat.test, label = "p", tip.length = 0.01, vjust=-0.5) +
            geom_jitter(aes(x=Genotype, y=tb[,List_var[i]]), col = "black", height = 0, width = .2, alpha=.6) +
            scale_fill_viridis_d(label=labels) +
            coord_cartesian(ylim = c(NA, max(tb[,List_var[i]])*1.1)) +
            ylab(List_label[i]) +
            #theme_bw() +
            theme_classic() + 
            theme(text = element_text(size = 15),
                  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                  axis.title.x = element_blank(), 
                  axis.text.y = element_text(size = 15),
                  axis.text.x = element_blank(), 
                  axis.line = element_line(size = 1),
                  axis.ticks = element_line(size = 1), 
                  axis.ticks.length = unit(.3, "cm"),
                  legend.title = element_blank(), 
                  legend.text=element_text(size = 15), #,face="bold"
                  legend.text.align = 0,
                  panel.border = element_blank(),
                  plot.margin = unit(c(.8,.8,.8,.8), "cm"))
        } else if (task == "bm_probe") {
          Plot <- ggbarplot(tb, x=xaxis, y=List_var[i], fill="Genotype", add = "mean_se", 
                            facet.by="BM_probe_SubTrialNumber", position = position_dodge(0.9), alpha=0.7) +
            scale_fill_viridis_d(label=labels) +
            scale_y_continuous(expand = c(0, 0)) + 
            xlab(xlab) +
            ylab(List_label[i]) +
            theme_classic() + 
            theme(text = element_text(size = 15),
                  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                  axis.title.x = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12, angle = 45, vjust=1, hjust=1),
                  axis.line = element_line(size = 1),
                  axis.ticks = element_line(size = 1),
                  axis.ticks.length = unit(.3, "cm"),
                  legend.text.align = 0,
                  legend.title = element_blank(), 
                  legend.text=element_text(size = 15), #,face="bold"
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  plot.margin = unit(c(.8,.8,.8,.8), "cm")) 
        } else if (task == "ppi_t01") {
          tb <- tb %>%
            dplyr::select(-contains(xaxis[3-i])) %>%
            distinct()
          
          stat.test <- tb %>% #obtain p-values
            group_by(get(xaxis[i])) %>%
            t_test(as.formula(paste0(List_var[i], " ~ Genotype"))) %>%
            add_xy_position(x = "Genotype") #%>%
          colnames(stat.test)[1] <- xaxis[i]
          stat.test$y.position <- c(rep(seq(max(tb[,List_var[i]])*0.9, max(tb[,List_var[i]])*1.1, by=max(tb[,List_var[i]])/20)[1:ncomb],length(unique(tb[,xaxis[i]]))))
          
          Plot <- 
            ggbarplot(tb, x="Genotype", y=List_var[i], fill="Genotype", add = "mean_se", 
                      facet.by = xaxis[i], alpha=0.5) +
            geom_jitter(aes(x=Genotype, y=get(List_var[i])), col = "black", height = 0, width = .2, alpha=.6) +
            stat_pvalue_manual(stat.test, label = "p", tip.length = 0.01, vjust=-0.5) +
            scale_fill_viridis_d(label=labels) +
            scale_y_continuous(expand = c(0, 0)) +
            coord_cartesian(ylim = c(0, max(tb[,List_var[i]]*1.1))) +
            xlab(xlab[i]) +
            ylab(List_label[i]) +
            facet_wrap(as.formula(paste0("~",xaxis[i])), strip.position = "bottom", nrow=1) +
            theme_classic() +
            theme(text = element_text(size = 15),
                  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                  axis.title.x = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_blank(),
                  axis.line = element_line(size = 1),
                  axis.ticks.y = element_line(size = 1),
                  axis.ticks.x = element_blank(),
                  axis.ticks.length = unit(.3, "cm"),
                  legend.text.align = 0,
                  legend.title = element_blank(),
                  legend.text=element_text(size = 15), #,face="bold"
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  strip.text = element_text(size=12, vjust = 2),
                  plot.margin = unit(c(.8,.8,.8,.8), "cm"))
        } else if (str_detect(task, "fz")) { #fz
          tb1 <- tb1 %>% transform(Genotype = factor(Genotype, levels=sort(unique(tb$Genotype))))
          
          if (task == "fz_day1") { #day1
            if (i < 3) { #freezing/distance
              # stat.test <- tb1 %>%
              #   group_by(MouseID,Genotype) %>%
              #   summarize(value = mean(get(List_var[i]))) %>%
              #   ungroup() %>%
              #   anova_test(value ~ Genotype)
              Plot <- 
                ggline(tb1, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", alpha=0.7) +
                annotate("rect", xmin = 2, xmax = 2.5, ymin = -Inf, ymax = Inf,  fill = "lightgrey", alpha=.6) +
                annotate("rect", xmin = 4, xmax = 4.5, ymin = -Inf, ymax = Inf,  fill = "lightgrey", alpha=.6) +
                annotate("rect", xmin = 6, xmax = 6.5, ymin = -Inf, ymax = Inf,  fill = "lightgrey", alpha=.6) +
                stat_summary(aes(group=Genotype, color=Genotype), fun=mean, geom="line", size=1) + 
                stat_compare_means(aes(group = Genotype), label = "p.signif") +
                # annotate("text", x=1,y=60,label=paste(italic(P), "=", stat.test$p)) +
                # stat_compare_means(aes(group = Genotype), label = "p.signif") +
                scale_color_viridis_d(label=labels) +
                xlab(xlab[i]) +
                ylab(List_label[i]) +
                #theme_bw() +
                theme_classic() + 
                theme(text = element_text(size = 15),
                      axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                      axis.title.x = element_text(size = 15),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      axis.line = element_line(size = 1),
                      axis.ticks = element_line(size = 1),
                      axis.ticks.length = unit(.3, "cm"),
                      legend.text.align = 0,
                      legend.title = element_blank(), 
                      legend.text=element_text(size = 15), #,face="bold"
                      panel.border = element_blank(),
                      plot.margin = unit(c(.8,.8,.8,.8), "cm"))
            } else { #shock_distance
              tb2 <- tb2 %>% transform(Genotype = factor(Genotype, levels=sort(unique(tb$Genotype))))
              
              Plot <- ggline(tb2, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", 
                             facet.by = "FZ_Day1_Shock", alpha=0.7) +
                annotate("rect", xmin = 7, xmax = 15, ymin = -Inf, ymax = Inf,  fill = "#474a4d", alpha=.6) +
                stat_summary(aes(group=Genotype, color=Genotype), fun=mean, geom="line", size=1) + 
                # annotate("text", x=12, y=18, label=paste("P =", stat.test$p)) +
                scale_color_viridis_d(label=labels) +
                scale_x_discrete(breaks=seq(0, 6, by=1)) + 
                xlab(xlab[i]) +
                ylab(List_label[i]) +
                #theme_bw() +
                theme_classic() + 
                theme(text = element_text(size = 15),
                      axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                      axis.title.x = element_text(size = 15),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      axis.line = element_line(size = 1),
                      axis.ticks = element_line(size = 1),
                      axis.ticks.length = unit(.3, "cm"),
                      legend.text.align = 0,
                      legend.title = element_blank(), 
                      legend.text=element_text(size = 15), #,face="bold"
                      panel.border = element_blank(),
                      strip.background = element_blank(),
                      plot.margin = unit(c(.8,.8,.8,.8), "cm"))
            }
          } else { #day2 & 3
            if(i==1 | i==3){#context
              tb1 <- tb1 %>%
                dplyr::filter(get(paste0(prefix,"_Experiment")) == "Context")
              
              Plot <- 
                ggline(tb1, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", alpha=0.7) +
                stat_summary(aes(group=Genotype, color=Genotype), fun=mean, geom="line", size=1) + 
                stat_compare_means(aes(group = Genotype), label = "p.signif") +
                # annotate("text", x=12, y=18, label=paste("P =", stat.test$p)) +
                scale_color_viridis_d(label=labels) +
                scale_x_discrete(breaks=seq(0, 5, by=1)) + 
                xlab(xlab) +
                ylab(List_label[i]) +
                #theme_bw() +
                theme_classic() + 
                theme(text = element_text(size = 15),
                      axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                      axis.title.x = element_text(size = 15),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      axis.line = element_line(size = 1),
                      axis.ticks = element_line(size = 1),
                      axis.ticks.length = unit(.3, "cm"),
                      legend.text.align = 0,
                      legend.title = element_blank(), 
                      legend.text=element_text(size = 15), #,face="bold"
                      panel.border = element_blank(),
                      strip.background = element_blank(),
                      plot.margin = unit(c(.8,.8,.8,.8), "cm"))
            } else { #cued
              tb1 <- tb1 %>%
                dplyr::filter(get(paste0(prefix,"_Experiment")) == "Cued")
              
              Plot <- 
                ggline(tb1, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", alpha=0.7) +
                annotate("rect", xmin = 3, xmax = 6, ymin = -Inf, ymax = Inf,  fill = "lightgrey", alpha=0.6) +
                stat_summary(aes(group=Genotype, color=Genotype), fun=mean, geom="line", size=1) + 
                stat_compare_means(aes(group = Genotype), label = "p.signif") +
                # annotate("text", x=12, y=18, label=paste("P =", stat.test$p)) +
                scale_color_viridis_d(label=labels) +
                scale_x_discrete(breaks=seq(0, 6, by=1)) + 
                xlab(xlab) +
                ylab(List_label[i]) +
                #theme_bw() +
                theme_classic() + 
                theme(text = element_text(size = 15),
                      axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                      axis.title.x = element_text(size = 15),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      axis.line = element_line(size = 1),
                      axis.ticks = element_line(size = 1),
                      axis.ticks.length = unit(.3, "cm"),
                      legend.text.align = 0,
                      legend.title = element_blank(), 
                      legend.text=element_text(size = 15), #,face="bold"
                      panel.border = element_blank(),
                      strip.background = element_blank(),
                      plot.margin = unit(c(.8,.8,.8,.8), "cm"))
            }
            
          }
          
        } else if (task == "hc") {
          stat.test <- tb %>%
            group_by_("MouseID","Genotype") %>%
            summarize_(.dots = setNames(paste0('mean(', List_var[i], ')'), List_var[i])) %>%
            ungroup() %>%
            t_test(as.formula(paste0(List_var[i], "~Genotype")))
          stat.test$x.position <- max(tb[,xaxis],na.rm=TRUE)*0.8
          stat.test$y.position <- max(tb[,List_var[i]],na.rm=TRUE)
          
          xminli <- c(0, seq((max(tb$HC_Time, na.rm = T)-1) %/% 24)) * 24 + 1
          xmaxli <- c(0, seq((max(tb$HC_Time, na.rm = T)-1) %/% 24)) * 24 + 12
          Plot <- 
            ggline(tb, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", alpha=0.7) +
            annotate("rect", xmin = xminli, xmax = xmaxli, ymin = -Inf, ymax = Inf,  fill = "lightgrey", alpha=.6) +
            annotate("text", label=paste(expression("Repeated measures ANOVA, \nP ="), stat.test$p), x =  stat.test$x.position, y = stat.test$y.position, size=4) +
            stat_summary(aes(group=Genotype, color=Genotype), fun=mean, geom="line", size=1) + 
            # annotate("text", x=1,y=60,label=paste(italic(P), "=", stat.test$p)) +
            # stat_compare_means(aes(group = Genotype), label = "p.signif") +
            scale_color_viridis_d(label=labels) +
            xlab(xlab) +
            ylab(List_label[i]) +
            #theme_bw() +
            theme_classic() + 
            theme(text = element_text(size = 15),
                  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                  axis.title.x = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_blank(),
                  axis.line = element_line(size = 1),
                  axis.ticks.y = element_line(size = 1),
                  axis.ticks.x = element_blank(),
                  axis.ticks.length.y = unit(.3, "cm"),
                  legend.text.align = 0,
                  legend.title = element_blank(), 
                  legend.text=element_text(size = 15), #,face="bold"
                  panel.border = element_blank(),
                  plot.margin = unit(c(.8,.8,.8,.8), "cm"))
        } else if (task == "crsoc_t01") {
          if(i < 3){
            tb1 <- dplyr::select(tb, c("MouseID","Genotype",paste0(prefix,c("_SubID","_Type","_Experiment")),xaxis[i],List_var[i])) %>%
              dplyr::filter(CrSoc_Experiment == "Sociability test")
          } else{
            tb1 <- dplyr::select(tb, c("MouseID","Genotype",paste0(prefix,c("_SubID","_Type","_Experiment")),xaxis[i],List_var[i])) %>%
              dplyr::filter(CrSoc_Experiment == "Preference test")
          }
          if(xaxis[i] == "Genotype"){
            tb1 <- dplyr::select(tb1, !c(paste0(prefix,c("_Type","_Experiment")))) %>%
              distinct()
            stat.test <- tb1 %>%
              t_test(as.formula(paste0(List_var[i], "~", xaxis[i])))
            stat.test$y.position <- c(seq(max(tb1[,List_var[i]])*0.9, max(tb1[,List_var[i]])*1.1, by=max(tb1[,List_var[i]])/20)[1:1])
            
            Plot <- 
              ggbarplot(tb1, x="Genotype", y=List_var[i], fill=xaxis[i], alpha=0.8, add = "mean_se") +
              geom_jitter(aes(x=get(xaxis[i]), y=get(List_var[i])), col = "black", height = 0, width = .2, alpha=.6) +
              stat_pvalue_manual(stat.test, label = "p", tip.length = 0.01, vjust=-0.5) +
              scale_fill_viridis_d(label=labels) +
              scale_y_continuous(expand = c(0, 0)) +
              coord_cartesian(ylim = c(0, max(tb1[,List_var[i]]*1.1))) +
              ylab(List_label[i]) +
              #theme_bw() +
              theme_classic() + 
              theme(text = element_text(size = 15),
                    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                    axis.title.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_blank(),
                    axis.line = element_line(size = 1),
                    axis.ticks.y = element_line(size = 1),
                    axis.ticks.x = element_blank(),
                    axis.ticks.length.y = unit(.3, "cm"),
                    legend.text.align = 0,
                    legend.title = element_blank(), 
                    legend.text=element_text(size = 15), #,face="bold"
                    panel.border = element_blank(),
                    plot.margin = unit(c(.8,.8,.8,.8), "cm"))
          } else {
            # tb1 <- tb1 %>% dplyr::mutate(!!xaxis[i] := factor(get(xaxis[i]), levels=sort(unique(tb1[[xaxis[i]]]))))
            tb1 <- transform(tb1, CrSoc_Place = factor(CrSoc_Place, levels = sort(unique(tb1[[xaxis[i]]]))))
            
            stat.test <- tb1 %>%
              group_by(Genotype) %>%
              t_test(as.formula(paste0(List_var[i], "~", xaxis[i])), paired = T)
            stat.test$y.position <- c(rep(seq(max(tb1[,List_var[i]])*0.9, max(tb1[,List_var[i]])*1.1, by=max(tb1[,List_var[i]])/20)[1:1],length(unique(tb1[,xaxis[i]]))))
            
            Plot <- 
              ggbarplot(tb1, x=xaxis[i], y=List_var[i], fill="Genotype", add = "mean_se", alpha=xaxis[i], facet.by = "Genotype") +
              geom_jitter(aes(x=get(xaxis[i]), y=get(List_var[i])), col = "black", height = 0, width = 0, alpha=.6) +
              geom_line(aes(x=get(xaxis[i]), y=get(List_var[i]), group=MouseID), size=.5, alpha=.5) +
              stat_pvalue_manual(stat.test, label = "p", tip.length = 0.01, vjust=-0.5) +
              scale_fill_viridis_d(labels=labels) +
              scale_alpha_manual(values=c(0.3,0.9), labels=sort(unique(tb1[[xaxis[i]]]))) +
              scale_y_continuous(expand = c(0, 0)) +
              coord_cartesian(ylim = c(0, max(tb1[,List_var[i]])*1.1)) +
              ylab(List_label[i]) +
              # facet_wrap(~Genotype, strip.position = "bottom", nrow=1) +
              #theme_bw() +
              theme_classic() + 
              theme(text = element_text(size = 15),
                    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                    axis.title.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12, angle = 45, vjust=1, hjust=1),
                    axis.line = element_line(size = 1),
                    axis.ticks.y = element_line(size = 1),
                    axis.ticks.x = element_blank(),
                    axis.ticks.length.y = unit(.3, "cm"),
                    legend.text.align = 0,
                    legend.title = element_blank(), 
                    legend.text=element_text(size = 15), #,face="bold"
                    strip.background = element_blank(),
                    # strip.placement = "outside",
                    strip.text = element_blank(),#element_text(size=12, vjust = 2),
                    panel.border = element_blank(),
                    plot.margin = unit(c(.8,.8,.8,.8), "cm")) +
              guides(alpha = "none")
          }
        } else {
          
          stat.test <- tb %>%
            anova_test(dv=List_var[i], wid="MouseID", within=xaxis, between="Genotype")
          # group_by_("MouseID","Genotype") %>%
          # summarize_(.dots = setNames(paste0('mean(', List_var[i], ')'), List_var[i])) %>%
          # ungroup() %>%
          # t_test(as.formula(paste0(List_var[i], "~Genotype")))
          if(class(stat.test)[2] == "list"){
            stat.test <- stat.test$ANOVA
          }
          stat.test <- head(stat.test, n=1)
          stat.test$x.position <- max(tb[,xaxis],na.rm=TRUE)*0.7
          stat.test$y.position <- max(tb[,List_var[i]],na.rm=TRUE)*0.85
          
          if (task == "of"){
            Plot <- 
              ggline(tb, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", numeric.x.axis = TRUE) +
              annotate("text", label=paste(expression("Repeated measures ANOVA, \nP ="), stat.test$p), x =  stat.test$x.position, y = stat.test$y.position, size=4) +
              scale_color_viridis_d(label=labels) +
              scale_x_continuous(limits = c(NA,max(tb[,xaxis],na.rm=TRUE)),
                                 breaks = seq(max(tb[,xaxis],na.rm=TRUE) %% 2, max(tb[,xaxis],na.rm=TRUE), by = as.integer(max(tb[,xaxis],na.rm=TRUE)/6))) + #24->6
              xlab(xlab) +
              ylab(List_label[i]) +
              theme_classic() + 
              theme(text = element_text(size = 15),
                    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                    axis.title.x = element_text(size = 15),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12),#, angle = 45, vjust=1, hjust=1),
                    axis.line = element_line(size = 1),
                    axis.ticks = element_line(size = 1),
                    axis.ticks.length = unit(.3, "cm"),
                    legend.text.align = 0,
                    legend.title = element_blank(), 
                    legend.text=element_text(size = 15), #,face="bold"
                    panel.border = element_blank(),
                    plot.margin = unit(c(.8,.8,.8,.8), "cm"))
          } else{
            Plot <- 
              ggline(tb, x=xaxis, y=List_var[i], color="Genotype", add = "mean_se", numeric.x.axis = TRUE) +
              annotate("text", label=paste(expression("Repeated measures ANOVA, \nP ="), stat.test$p), x =  stat.test$x.position, y = stat.test$y.position, size=4) +
              scale_color_viridis_d(label=labels) +
              scale_x_continuous(limits = c(NA,max(tb[,xaxis],na.rm=TRUE)),
                                 breaks = seq(max(tb[,xaxis],na.rm=TRUE) %% 2, max(tb[,xaxis],na.rm=TRUE), by = as.integer(max(tb[,xaxis],na.rm=TRUE)/5))) + #6->5
              xlab(xlab) +
              ylab(List_label[i]) +
              theme_classic() + 
              theme(text = element_text(size = 15),
                    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0)),
                    axis.title.x = element_text(size = 15),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12),
                    axis.line = element_line(size = 1),
                    axis.ticks = element_line(size = 1),
                    axis.ticks.length = unit(.3, "cm"),
                    legend.text.align = 0,
                    legend.title = element_blank(), 
                    legend.text=element_text(size = 15), #,face="bold"
                    panel.border = element_blank(),
                    plot.margin = unit(c(.8,.8,.8,.8), "cm"))
          }
        }
      })
    }
  }
  
  plot <- ggarrange(plotlist = myplots,
            ncol = nc, nrow = nr, 
            common.legend = TRUE, legend = "bottom")
  ggsave(paste0("./", group, "/", group, "_", task, ".pdf"), plot, w=10, h=10)
  
}


for (group in group_list){
  # group <- "Aldh_1st"
  mouse_info <- mouse_info_all %>%
    dplyr::filter(GroupID==group)
  num_rows <- NULL
  for (task in task_list) {
    num <- dplyr::inner_join(get(paste0("task_sum_", task)), mouse_info, by="MouseID") %>% nrow()
    num_rows <- c(num_rows, num)
  }
  task_1group_list <- task_list[which(num_rows != 0)]
  for (task in task_1group_list){
    datasetInput <-  dplyr::inner_join(get(paste0("task_", task)), mouse_info)
    dir.create(group, recursive = TRUE)
    mkplot(datasetInput, group, task)
  }
}
