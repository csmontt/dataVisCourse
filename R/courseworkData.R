# Data for coursework-----------------------------------------------------------

# Load packages ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "haven", "tidyverse", "forcats", "RColorBrewer", 
               "ggmap", "jsonlite")

# load data-----------------

data <- read_dta("./data/complete_app_pooleylong.dta")

data_selection <- data %>% select(namenumber,
                             yearbi,
                             codefa,
                             hiscam_f,
                             occ_ba, 
                             placework,
                             urban,
                             migra,
                             occ:agesmov,
                             educa,
                             yearbe,
                             wexp,
                             unem,
                             mas,
                             children
                             )

write.csv(data_selection, "./data/careers.csv")

careers <- read_csv("./data/careers.csv")

# Geocode placeofwork-----------------------------------------------------------
#geocode_API_KEY <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

# register geocode api key
#register_google(key = geocode_API_KEY)

careers$to_geocode <- paste0(careers$placework, ", uk")
unique_dirs <- unique(careers$to_geocode)

# geocoede dirs
#dirs_geo <- geocode(unique_dirs, output = "more")
#write_csv(dirs_geo, "dirs_geo.csv")

# load geocoded dirs
dirs <- read_csv("./data/dirs_geo.csv")

dirs_df <- as_tibble(cbind(unique_dirs, dirs))

dirs_country <- dirs_df %>% group_by(country) %>% 
        summarize(total = n()) %>% 
        arrange(desc(total))

dirs_df <- dirs_df %>% filter(country == "United Kingdom") %>% 
        dplyr::select(unique_dirs, lon, lat)

dirs_df$unique_dirs <- as.character(dirs_df$unique_dirs)

# merge geocoded dirs with original dataset-------------------------------------
careers <- careers %>% 
        left_join(dirs_df, by = c("to_geocode" = "unique_dirs")) %>%
        dplyr::select(namenumber:nyearocc, agesmov:children, lon,lat)


# create twenty years intervals
careers <- careers %>%
  mutate(twenty_years=case_when(
    yearocc %in% 1791:1810 ~ "1791", # see graph 1791:1804 the decline is steeper
    yearocc %in% 1811:1830 ~ "1805",
    yearocc %in% 1831:1850 ~ "1831",
    yearocc %in% 1851:1870 ~ "1851",
    yearocc %in% 1871:1890 ~ "1871",
    yearocc %in% 1891:1910 ~ "1891",
    yearocc %in% 1911:1930 ~ "1911",
    yearocc %in% 1931:1955 ~ "1931"
  )) %>% na.omit()

source("./R/labelOcc.R")

# filter out domestic service and army
careers <- careers %>% filter(occ >= 1 & occ <= 9)
careers$occ <- as.factor(careers$occ)

# filter out domestic service and army
careers <- careers %>% filter(codefa >= 1 & codefa <= 9)
careers$codefa <- as.factor(careers$codefa)

#recode occupation observation
careers <- labelOcc(careers, "occ", "occ")

#recode occupation father
careers <- labelOcc(careers, "codefa", "rcodefa")


# create age intervals------------------------------------------------------
# too few individuals with observations after 60 years old, filter them out
# so we don´t have skew results
careers <- careers %>% filter(agesmov < 60) %>% 
            mutate(age_int = case_when(agesmov < 20 ~ "14 - 20",
                                      (agesmov >= 20 & agesmov < 30)  ~ "20 - 29",
                                      (agesmov >= 30 & agesmov < 40)  ~ "30 - 39",
                                      (agesmov >= 40 & agesmov < 50)  ~ "40 - 49",
                                      (agesmov >= 50 & agesmov < 60)  ~ "50 - 59",
                                      (agesmov >= 60 ~ ">= 60")))


# create cohort variable
careers <- careers %>% 
        mutate(cohort = ifelse(yearbi >= 1780 & yearbi <= 1804, "1780-1804", 
                               ifelse(yearbi >= 1805 & yearbi <= 1829, "1805-1829",
                                      ifelse(yearbi >= 1830 & yearbi <= 1854, "1830-1854",
                                             ifelse(yearbi >= 1855 & yearbi <= 1880, "1855-1880", NA)))))


# Calculate average hiscma per age group by person
careers <- careers %>% dplyr::group_by(namenumber, age_int) %>% 
                        mutate(hiscam_age = mean(hiscam, na.rm = TRUE))

careers$placework <- gsub("\"keighley", "keighley", careers$placework)

careers$occ <- as.character(careers$occ)


write_csv(careers, "./data/courseworkData.csv")


