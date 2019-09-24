### Setup

library(tidyverse)
library(rvest)

### Import data

citylab <- read_html("https://www.citylab.com/life/2019/02/global-megaregions-economic-powerhouse-megalopolis/583729/")

regions_2015 <- citylab %>%
  html_nodes(".tg") %>% 
  html_table(header = TRUE)
regions_2015 <- regions_2015[[1]]

CPES <- read_html("https://academic.oup.com/view-large/5748053")

regions_2000 <- CPES %>% 
  html_nodes("div.table-wrap:nth-child(2) > div:nth-child(2) > table:nth-child(1)") %>% 
  html_table(header = TRUE)

regions_2000 <- regions_2000[[1]]

### Join data

names(regions_2015)[3:4] <- c("Population (millions, 2015)", "GDP (billions, 2015)")

names(regions_2000)[c(1:2, 4)] <- c("Mega-Region", "Population (millions, 2000)", 
                                    "GDP (billions, 2000)")

regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Rom-Mil-Tur"] <- "Rome-Mil-Tur"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Char-lanta"] <- "Char-Lanta"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "So-Cal"] <- "SoCal"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Barce-Lyon"] <- "Barcelona-Lyon"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Nor-Cal"] <- "NorCal"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Hong-Zen"] <- "Hong-Shen"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Vienna-pest"] <- "Vienna-Budapest"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Rio-Paulo"] <- "São Paolo"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Tel Aviv-Amman-Beirut"] <- "Cairo-Aviv"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Delhi–Lahore"] <- "Delhi-Lahore"
regions_2000$`Mega-Region`[regions_2000$`Mega-Region` == "Singapore"] <- "Singa-Lumpur"

regions_2000 %>% filter(`Mega-Region` == "Am-Brus-Twerp" | `Mega-Region` == "Frank-Gart" | 
                          `Mega-Region` == "Paris") %>% 
  mutate(`Patents (2001)` = as.numeric(`Patents (2001)`)) -> Par_Am_Mun
Par_Am_Mun[, c(1, 3, 5, 7, 9)] <- NA
c("Par-Am-Mun", colSums(Par_Am_Mun[, -1])) %>% 
  rbind(., filter(regions_2000, `Mega-Region` != "Am-Brus-Twerp" & `Mega-Region` != "Frank-Gart" & 
                    `Mega-Region` != "Paris")) -> regions_2000

regions_2000 %>% filter(`Mega-Region` == "Dal-Austin" | `Mega-Region` == "Hou-Orleans") %>% 
  mutate(`Population (millions, 2000)` = as.numeric(`Population (millions, 2000)`), 
         `GDP (billions, 2000)` = as.numeric(`GDP (billions, 2000)`), 
         `Authors (2001)` = as.numeric(`Authors (2001)`), 
         `Patents (2001)` = as.numeric(`Patents (2001)`)) -> Texas
Texas[, c(1, 3, 5, 7, 9)] <- NA
c("Texas Triangle", colSums(Texas[, -1])) %>% 
  rbind(., filter(regions_2000, `Mega-Region` != "Dal-Austin" & `Mega-Region` != "Hou-Orleans")) -> regions_2000


regions <- full_join(regions_2015, regions_2000, by = "Mega-Region")


### Data manipulation

regions <- regions %>% mutate(`GDP (billions, 2015)` = substring(`GDP (billions, 2015)`, 2)) %>% 
  mutate(`GDP (billions, 2015)` = gsub(",", "", `GDP (billions, 2015)`))

sapply(regions[, c(3:12)], as.numeric) -> regions[, c(3:12)]

### Calculate GDP per capita

regions <- regions %>% mutate(`GDP per capita 2015` = `GDP (billions, 2015)`/(`Population (millions, 2015)`/1000)) %>% 
  mutate(`GDP per capita 2000` = `GDP (billions, 2000)`/(`Population (millions, 2000)`/1000))

regions$`GDP per capita 2015`[is.na(regions$`GDP per capita 2015`)] <- regions$`GDP per capita 2000`[is.na(regions$`GDP per capita 2015`)]
