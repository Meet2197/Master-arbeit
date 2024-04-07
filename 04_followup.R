library(tidyverse)
library(lubridate)
library(janitor)
library(DT)
theme_set(theme_light())

#data_path <- "C:\\Downloads\\txDevelop"
#result_path <- "C:\\Downloads\\d"
#this_file <- "C:\\IDE\\workspace_bitcare\\txdash\\runAll.R"

dataFile <- paste0(result_path, "/DZIFTx.RData");
if (!file.exists(dataFile)){
  print(paste0(dataFile," File does not exist"))
} else {
  load(dataFile)
}

#generating
fudaten_liste <- df_clean %>% 
  filter(grepl("General|Recipient", file)) %>% 
  filter(Status > 0) %>% 
  filter(grepl("TxDate|DateFU", Feld)) %>% 
  select(1, 7, 8, 9, 10) %>% 
  mutate(key = case_when(grepl("TxDate", Feld) ~ "TxDate",
                         grepl("DateFU", Feld) ~ "DateFU")) %>% 
  select(-Feld) %>% 
  group_split(key)

fu_exists <- NULL
report_tab_fu <- NULL
if (!is.null(fudaten_liste) && length(fudaten_liste) > 1) {
  df_fu <- fudaten_liste[[2]] %>% 
    mutate(value = dmy(value)) %>% 
    unique %>% 
    spread(key, value)  %>% 
    left_join(
      fudaten_liste[[1]] %>%
        mutate(value = dmy(value)) %>% 
        group_by(PatientID) %>% 
        arrange(PatientID, value) %>% 
        mutate(visit = c(1:n())) %>% 
        spread(key, value) %>% 
        rename(FuDate = DateFU) %>% ungroup()
    ) %>% 
    mutate(export_date = ymd(Sys.Date()),
           TxPeriod=export_date-TxDate,
           FuPeriod=export_date-FuDate,
           Fu_3m=interval(TxDate %m+% months(3)-days(28), TxDate %m+% months(3)+days(28)), # when should the FU be?
           Fu_6m=interval(TxDate %m+% months(6)-days(28), TxDate %m+% months(6)+days(28)),
           Fu_9m=interval(TxDate %m+% months(9)-days(28), TxDate %m+% months(9)+days(28)),
           Fu_12m=interval(TxDate %m+% months(12)-days(28), TxDate %m+% months(12)+days(28)),
           checked_3m=FuDate %within% Fu_3m, # was the FU in this interval?
           checked_6m=FuDate %within% Fu_6m,
           checked_9m=FuDate %within% Fu_9m,
           checked_12m=FuDate %within% Fu_12m,
           mandatory_3m=Fu_3m %within% interval(TxDate,TxDate+TxPeriod), # should there be a FU in this interval?
           mandatory_6m=Fu_6m %within% interval(TxDate,TxDate+TxPeriod),
           mandatory_9m=Fu_9m %within% interval(TxDate,TxDate+TxPeriod),
           mandatory_12m=Fu_12m %within% interval(TxDate,TxDate+TxPeriod),
           eval_3m=ifelse(checked_3m==T & mandatory_3m==T, "ok",
                          ifelse(checked_3m==F & mandatory_3m==F, "ok",
                                 ifelse(checked_3m==T & mandatory_3m==F, "not needed","not done"))),
           eval_6m=ifelse(checked_6m==T & mandatory_6m==T, "ok",
                          ifelse(checked_6m==F & mandatory_6m==F, "ok",
                                 ifelse(checked_6m==T & mandatory_6m==F, "not needed","not done"))),
           eval_9m=ifelse(checked_9m==T & mandatory_9m==T, "ok",
                          ifelse(checked_9m==F & mandatory_9m==F, "ok",
                                 ifelse(checked_9m==T & mandatory_9m==F, "not needed","not done"))),
           eval_12m=ifelse(checked_12m==T & mandatory_12m==T, "ok",
                           ifelse(checked_12m==F & mandatory_12m==F, "ok",
                                  ifelse(checked_12m==T & mandatory_12m==F, "not needed","not done")))) 


  tab_fu <- df_fu %>%
    select(PatientID, organ, center, TxDate, FuDate, contains("check"), contains("mandatory")) %>%
    filter(!(is.na(TxDate) | is.na(FuDate))) %>%
    select(-TxDate, -FuDate) %>%
    group_by(PatientID, organ, center) %>%
    summarise(IST_month_3   = 1*any(checked_3m),
              SOLL_month_3  = 1*any(mandatory_3m),
              IST_month_6   = 1*any(checked_6m),
              SOLL_month_6  = 1*any(mandatory_6m),
              IST_month_9   = 1*any(checked_9m),
              SOLL_month_9  = 1*any(mandatory_9m),
              IST_month_12  = 1*any(checked_12m),
              SOLL_month_12 = 1*any(mandatory_12m))


  report_tab_fu <- tab_fu %>%
    group_by(center, organ) %>%
    summarise(IST_month_3          = sum(IST_month_3),
              SOLL_month_3         = sum(SOLL_month_3),
              Performance_month_3  = round(100*IST_month_3/SOLL_month_3, digits=2),
              IST_month_6          = sum(IST_month_6),
              SOLL_month_6         = sum(SOLL_month_6),
              Performance_month_6  = round(100*IST_month_6/SOLL_month_6, digits=2),
              IST_month_9          = sum(IST_month_9),
              SOLL_month_9         = sum(SOLL_month_9),
              Performance_month_9  = round(100*IST_month_9/SOLL_month_9, digits=2),
              IST_month_12         = sum(IST_month_12),
              SOLL_month_12        = sum(SOLL_month_12),
              Performance_month_12 = round(100*IST_month_12/SOLL_month_12, digits=2)) %>%
    ungroup()

  #### which graph and tab has to be rendered
  fu_exists <- report_tab_fu %>%
    count(organ, center) %>% 
    count(organ) %>% 
    as.data.frame()
  
  #### Figure Preparation            
  fig_fu <- report_tab_fu %>%
    gather(contains("IST"), contains("SOLL"), key = "time", value = "Patients") %>%
    select(-contains("Perf")) %>%
    separate(time, c("ind", "times", "month")) %>% 
    select(-contains("time"))
  fig_fu$month <- sapply(fig_fu$month, as.numeric)
  fig_fu <- fig_fu %>% select(center, organ, Indikator = ind, 
                              Monat = month, Patienten = Patients)


  graph <- function(Organ){
    g <- fig_fu %>% 
      filter(organ == Organ) %>% 
      mutate(Indikator = fct_relevel(Indikator, "SOLL", "IST")) %>% 
      ggplot(aes(Monat, Patienten, fill = Indikator)) +
      geom_area(position = "identity", alpha = 0.8) +
      facet_wrap(~center, nrow = 1) +
      scale_color_manual(values = c("#d6604d", "#4393c3"))+
      scale_fill_manual(values = c("#d6604d", "#4393c3")) +
      scale_x_continuous(breaks = c(3, 6, 9, 12)) +
      theme(strip.text.x = element_text(size = 12, face = "bold"), 
            strip.background = element_rect(fill="#0a2d6e"),
            axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 14)) +
      labs(title = paste0("Organ: ", Organ))
    return(g)
  }
}
  
  graphs <- NULL
  if (!is.null(fu_exists) && nrow(fu_exists) > 0) {
    for (i in 1:nrow(fu_exists)) {
      graphs[[i]] <- graph(fu_exists[i, 1])
    }
  }


### SAVE
file <- paste0(result_path, "/followup.RData")
save(graphs, report_tab_fu, file = file)

# delete local variables
rm(file)
rm(df_clean)
rm(df_fu)
rm(fig_fu)
rm(fu_exists)
rm(personenvariablen)
rm(fudaten_liste)
rm(graphs)
rm(report_tab_fu)
rm(tab_fu)
rm(graph)
