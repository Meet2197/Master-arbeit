library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
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

# function to read a CSV file (using fread, as it is a more robust csv parser)
# usage:  readCSV(filename, TRUE)
readCSV <- function(filename, header, stringsAsFactors = TRUE, na.strings = "NA") {
  content <- fread(
    file = filename, 
    head = header, 
    sep = ";", 
    encoding = "UTF-8", 
    integer64 = "character", 
    colClasses = c("character"), 
    na.strings = na.strings)
  return(as_tibble(content))
}

# Generating
biosample_summary <- function (file, path) {
  
  patient_list <- df_clean %>% 
    select(PatientID, center, organ) %>% 
    clean_names() %>% 
    distinct()
  
  files1 <- list.files(pattern = file, path = path, recursive = TRUE)
  data1 <- lapply(files1, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
                    mutate(file = x) %>% 
                    filter(Status >= 0))
  
  files1e <- gsub(".csv", "(E).csv", files1)
  data1e <- lapply(files1e, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
                     mutate(file = x) %>% 
                     filter(Status >= 0))
  
  files1m <- gsub(".csv", "(M).csv", files1)
  data1m <- lapply(files1m, function(x) readCSV(paste0(path, "/", x), TRUE) %>% 
                     select(Feld, Pflichtfeld))
  
  
  files1c <- gsub(".csv", "(C).csv", files1)
  data1c <- lapply(files1c, function(x) readCSV(paste0(path, "/", x), TRUE))
  
  if (length(data1) > 0) { 
    df <- bind_rows(data1) %>%
      clean_names() %>% distinct() %>% 
      select(patient_id, dokument, container, erfassungsdatum, status, 
             contains("id_"), probentyp = contains("probenart")) %>% 
      left_join(patient_list) %>% 
      left_join(
        data1c %>% 
          bind_rows() %>% distinct() %>% 
          filter(grepl("Probenart", Feld)) %>% 
          select(probentyp = Code, Probenart = Bedeutung)
      ) %>% 
      distinct()
    
    df_probenart <- df %>% 
      filter(status == 2) %>% 
      count(center, organ, Probenart) %>% 
      mutate(Probenart = case_when(is.na(Probenart) ~ "keine Angabe",
                                   TRUE ~ Probenart)) %>% 
      na.omit() %>% 
      rename(Zentrum = center, Organ = organ) %>%
      gather(key, value, -Probenart, -n) %>%
      group_by(Probenart, value, key) %>% 
      summarise(n = sum(n)) 
    
    titel <- NULL
    if (grepl("Blood", file)) {
      titel <- "Blut"
    } else {
      titel <- "Bio"
    }
    
    heatmap_probe <- df_probenart %>% 
      ggplot(aes(value, Probenart, fill = n, label = n)) +
      geom_tile() +
      facet_wrap(~key, scales = "free_x") +
      geom_label(color = "black", fill = "white") +
      scale_fill_viridis_c(guide = "none") +
      labs(x = "", 
           y = "",
           title = paste0("Absolute Verteilung der ", titel, "proben")) +
      theme(strip.background = element_rect(fill = "#001540"),
            strip.text = element_text(colour = 'white', face = "bold"),
            legend.position = "bottom")
  
    return(list(df_probenart, heatmap_probe))  
  } else {
    return(NULL)
  }
}

files <- c(
  "BloodSample.csv",
  "OtherSample.csv"
)

df <- NULL
for (i in seq_along(files)) {
  df[[i]] <- biosample_summary(files[i], data_path)
}

### SAVE
file <- paste0(result_path, "/biosamples.RData")
save(df, file = file)

# delete local variables
rm(i)
rm(file)
rm(df)
rm(files)
rm(biosample_summary)
rm(df_clean)
rm(personenvariablen)
rm(readCSV)