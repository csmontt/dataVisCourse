########################################################################
## Project: Data Visualization Coursework
## Script purpose: Create data for each graphic in coursework
## Date: 03-04-2019
## Author: Cristóbal Montt
########################################################################

## Do https://resources.github.com/whitepapers/github-and-rstudio/

# Install and load require packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "readr", "ggplot2", "haven", "forcats", "RColorBrewer")

# avoid character variables read as factors
options(stringsAsFactors = FALSE)
source("labelOcc.R")

# Read original data from Montt, C., & Maas, I. (2015). The openness of 
# Britain during industrialisation. Determinants of career success 
# of British men born between 1780 and 1880. Research in Social 
# Stratification and Mobility, 42, 123-135.

data <- read_dta("./data/complete_app_pooleylong.dta")

data_selection <- data %>% select(namenumber,
                             yearbi,
                             codefa,
                             hiscam_f,
                             occ_ba, # occ before or after (0,1) revise
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

# write.csv(data_selection, "careers.csv")

# --------------------------------------------------------------------------
# data transformation for "changes of occupational structure over time"
#---------------------------------------------------------------------------

data_occs <- data_selection %>% dplyr::select(codefa, educa, occ, 
                                              hiscam, yearocc, yearbi)

data_occs <- data_occs %>%
  mutate(twenty_years=case_when(
    yearocc %in% 1791:1804 ~ "1791", # see graph 1791:1804 instead of 1791:1810 the decline is steeper
    yearocc %in% 1805:1830 ~ "1805",
    yearocc %in% 1831:1850 ~ "1831",
    yearocc %in% 1851:1870 ~ "1851",
    yearocc %in% 1871:1890 ~ "1871",
    yearocc %in% 1891:1910 ~ "1891",
    yearocc %in% 1911:1930 ~ "1911",
    yearocc %in% 1931:1955 ~ "1931"
  )) %>% na.omit()

data_occs <- data_occs %>%
  mutate(ten_years=case_when(
    yearocc %in% 1791:1800 ~ "1791", # see graph 1791:1804 instead of 1791:1810 the decline is steeper
    yearocc %in% 1801:1810 ~ "1801",
    yearocc %in% 1811:1820 ~ "1811",
    yearocc %in% 1821:1830 ~ "1821",
    yearocc %in% 1831:1840 ~ "1831",
    yearocc %in% 1841:1850 ~ "1841",
    yearocc %in% 1851:1860 ~ "1851",
    yearocc %in% 1861:1870 ~ "1861",
    yearocc %in% 1871:1880 ~ "1871",
    yearocc %in% 1881:1890 ~ "1881",
    yearocc %in% 1891:1900 ~ "1891",
    yearocc %in% 1901:1910 ~ "1901",
    yearocc %in% 1911:1920 ~ "1911",
    yearocc %in% 1921:1930 ~ "1921",
    yearocc %in% 1931:1940 ~ "1931" 
# didnt used 10 years or less cause there is no data for every occupation 
# for evey year, which messes up the visualization. Besides I'm interested
# in genereal trends, not the specific variations between years
  )) %>% na.omit()

# filter out domestic service and army
data_occs <- data_occs %>% filter(occ >= 1 & occ <= 9)

# transform occ to factor so we can recode it
data_occs$occ <- as.factor(data_occs$occ)

#recode occupation sons
data_occs <- labelOcc(data_occs, "occ", "occ")


data_occs <- data_occs %>% dplyr::select(twenty_years, occ) %>% 
                   group_by(twenty_years) %>%
                     mutate(total_obs = n()) 

data_occs <- data_occs %>% group_by(twenty_years, occ) %>% 
                mutate(n_occs = n()) %>% distinct() %>% 
                 arrange(twenty_years, occ) 

data_occs <- data_occs %>% group_by(twenty_years, occ) %>% 
        mutate(prop = (n_occs/total_obs))


# checking calculation is correct should give 1 for each time period
data_occs %>% group_by(twenty_years) %>% summarize(tot = sum(prop))

data_occs$twenty_years <- as.numeric(data_occs$twenty_years)

data_occs$occ <- as.factor(data_occs$occ) 

#data_occs$prop <- round(data_occs$prop, 2)

write_csv(data_occs, "./data/occupations_by_year.csv")


# Making the same graph with ggplot2
myColors <- c(brewer.pal(9,"Blues"), brewer.pal(6,"Reds"))[c(4:9,13:15)]
names(myColors) <- levels(data_occs$occ)
colScale <- scale_fill_manual(name = "occ", values = myColors)

p1 <- ggplot(data_occs, aes(x=twenty_years, y=prop, fill=occ)) + 
    geom_area(alpha=0.6 , size=1, colour="black") 
p_scale <- p1 + colScale
p_scale


# --------------------------------------------------------------------------
# Data Transformations for "changes in occupational attainment over cohorts"
#---------------------------------------------------------------------------

# a lo mejor hay muy pocos weones con observaciones después de los 60
# agrandar ese tramo
careers_cohorts <- data_selection %>% filter(agesmov < 60) %>% 
            mutate(age_int = case_when(agesmov < 20 ~ "< 20",
                                      (agesmov >= 20 & agesmov < 30)  ~ "20 - 29",
                                      (agesmov >= 30 & agesmov < 40)  ~ "30 - 39",
                                      (agesmov >= 40 & agesmov < 50)  ~ "40 - 49",
                                      (agesmov >= 50 & agesmov < 60)  ~ "50 - 59"))
                                      #  (agesmov >= 60 ~ ">= 60")))

                                    

careers_cohorts <- careers_cohorts %>% filter(codefa >= 1 & codefa <= 9)


careers_cohorts$codefa <- as.factor(careers_cohorts$codefa)

#recode occupation father
careers_cohorts <- labelOcc(careers_cohorts, "codefa", "rcodefa")


# Don´t know why case_when didnt work, had to use contidional mutating instead
careers_cohorts <- careers_cohorts %>% 
        mutate(cohort = ifelse(yearbi >= 1780 & yearbi <= 1804, "1780-1804", 
                               ifelse(yearbi >= 1805 & yearbi <= 1829, "1805-1829",
                                      ifelse(yearbi >= 1830 & yearbi <= 1854, "1830-1854",
                                             ifelse(yearbi >= 1855 & yearbi <= 1880, "1855-1880", NA)))))


careers_cohorts$age_int <- as.numeric(as.factor(careers_cohorts$age_int))

# Calculate average hiscma per age group by person
careers_cohorts <- careers_cohorts %>% dplyr::group_by(namenumber, age_int) %>% 
                        mutate(hiscam_age = mean(hiscam, na.rm = TRUE))


careers_cohorts <- careers_cohorts %>% 
                        dplyr::select(namenumber, codefa, rcodefa, 
                                      age_int, cohort, hiscam_age)

careers_cohorts_av <- careers_cohorts %>% group_by(cohort, age_int) %>% 
        summarize(avHcoho = mean(hiscam_age, na.rm = TRUE))

write_csv(careers_cohorts_av, "./data/careers_cohorts.csv")

# same graph in ggplot2
# change in cohorts
ggplot(careers_cohorts_av, aes(x = age_int, y = avHcoho, 
                    group = factor(cohort),
                    color = factor(cohort))) + 
        stat_smooth(se= FALSE) 


# --------------------------------------------------------------------------
# data transformation for "changes of effect of father's ocupation overs 
# son's career status"
#---------------------------------------------------------------------------

careers_cohof <- careers_cohorts %>% group_by(cohort, rcodefa, age_int) %>% 
        summarize(avHcohoF = mean(hiscam_age, na.rm = TRUE))

ggplot(careers_cohof, aes(x = age_int, y = avHcohoF, 
                    group = factor(rcodefa),
                    color = factor(rcodefa))) + 
        stat_smooth(se= FALSE) + 
        facet_grid(cohort ~ rcodefa)

ggplot(careers_cohof, aes(x = age_int, y = avHcohoF, 
                    group = factor(rcodefa),
                    color = factor(rcodefa))) + 
        stat_smooth(se= FALSE) + 
        facet_grid(rcodefa ~ cohort)

ggplot(careers_cohof, aes(x = age_int, y = avHcohoF, 
                    group = factor(cohort),
                    color = factor(cohort))) + 
        stat_smooth(se= FALSE) + 
        facet_wrap(~rcodefa)

write_csv(careers_cohof, "./data/careers_cohorts_father.csv")


