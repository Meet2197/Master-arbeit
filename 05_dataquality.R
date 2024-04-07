library(tidyverse)
library(lubridate)
library(janitor)
library(DT)
theme_set(theme_light())

# Setting Source path  --------------
source.path <- "../TxDash_exports/"
path_index <- dir(path = source.path, recursive = F, pattern = "DZIFTx_*") %>% 
  sort(decreasing = TRUE)
recent.export <- path_index[1]
load(paste0(source.path, recent.export))

record_overall_dc <- df.all %>% 
  filter(Status == 2) %>% 
  filter(Pflichtfeld == "Ja") %>% 
  select(file, center, organ, entry_value) %>% 
  mutate(test = case_when(entry_value != 3 ~ "OK")) %>% 
  count(center, organ, test, file) %>% 
  spread(test, n, fill = 0) %>% 
  rename(MISS = 5) %>% 
  mutate(missing_perc = MISS/(OK+MISS)*100) %>% 
  mutate(file = gsub("\\_Reci", "Reci", file),
         file = gsub("\\_", " ", file),
         file = case_when(grepl("AntiInf", file) ~ "Antinfectiouse prophylaxis",
                          grepl("Bacteri", file) ~ "Bacterial complications",
                          grepl("Parasi", file) ~ "Parasite complications",
                          TRUE ~ file))

record_overall_dc %>% 
  ggplot(aes(center, organ, fill = missing_perc, label = paste0(round(missing_perc, 0), "%"))) +
  geom_tile() +
  facet_wrap(~file, ncol = 2) +
  scale_fill_gradient2(low = "#5e4fa2", mid = "#ffffbf", high = "#9e0142", midpoint = 5) +
  labs(fill = "Fehlende Werte in %",
       x = "",
       y = "") +
  theme(strip.background =element_rect(fill = "#0a2d6e"),
        strip.text = element_text(face = "bold")) +
  ggrepel::geom_text_repel(box.padding = 0, point.padding = 0)


record_overall_dc %>% 
  select(1, 2, 3, 6) %>% 
  spread(center, missing_perc) %>% 
  ggr
  gather(organ, missing_perc, -center, -file) %>% 
  ggplot(aes(center, missing_perc, color = file, group = file)) +
  geom_line() +
  geom_point() +
  facet_wrap(~organ) +
  coord_polar()
  


df.all %>% 
  filter(Status == 2) %>% 
  filter(Pflichtfeld == "Ja") %>% 
  select(file, center, organ, Feld, entry_value) %>% 
  mutate(test = case_when(entry_value != 3 ~ "OK")) %>% 
  count(center, organ, test, Feld, file) %>% 
  spread(test, n, fill = 0) %>% 
  rename(MISS = 5, OK = 6) %>% 
  mutate(missing_perc = MISS/(OK+MISS)*100) %>% 
  mutate(file = gsub("\\_Reci", "Reci", file),
         file = gsub("\\_", " ", file),
         file = case_when(grepl("AntiInf", file) ~ "Antinfectiouse prophylaxis",
                          grepl("Bacteri", file) ~ "Bacterial complications",
                          grepl("Parasi", file) ~ "Parasite complications",
                          TRUE ~ file))
