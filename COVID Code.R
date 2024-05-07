#Analysis code for the article: 
#Population-level effect of COVID-19 vaccination coverage on transmission and mortality during Omicron dominance: a global time series analysis
#Authors: Stephen Chukwuma Ogbodo, Joseph Junior Damba, Omotayo Olaoye, and Divine-Favour Chichenim Ofili.
#Happy coding! :)



# SECTION 1: CLEAN DATA AND PLOT FIGURES 1 AND 2 ---------------------------------------------

#Download dataset from OWID COVID-19 database: https://github.com/owid/covid-19-data/tree/master/public/data

#Import dataset into R environment
library(readr)
dat <- read_csv("data/owid-covid-data.csv")

#Load useful packages for data cleaning, plotting and modelling
library(tidyverse)
library(descr)
library(zoo)
library(patchwork)
library(modelsummary)
library(gtsummary)

#Select relevant variables:
dat<- 
  owid_covid_data |>
  dplyr::select(location, continent, date,
         new_cases, new_cases_per_million, new_cases_smoothed_per_million,
         new_deaths, new_deaths_per_million, new_deaths_smoothed_per_million,
         new_tests_per_thousand,
         people_fully_vaccinated_per_hundred,
         population, population_density, median_age, aged_65_older, 
         gdp_per_capita, extreme_poverty, 
         life_expectancy, reproduction_rate, human_development_index,
         cardiovasc_death_rate, diabetes_prevalence, hospital_beds_per_thousand, 
         handwashing_facilities, stringency_index,
         female_smokers, male_smokers)|>
  rename(Continent = continent, Country = location)

#Remove rows that do not represent countries
dat<- dat|>
  filter(Country != "Africa" & Country != "Asia" & Country != "Europe" & Country != "European Union"
         & Country != "High income" & Country != "Low income" & Country != "Lower middle income" 
         & Country != "North America" & Country != "South America" & Country != "Oceania" & 
           Country != "Upper middle income" & Country != "World")

#Need to summarize the variables of interest from daily measures.
#From the "date" variable, create "day", "month" and "year" variables for each row to enable adding up events across time as needed.
dat<-
  dat|>
  separate_wider_delim(cols = date, 
                       delim = '-',
                       names = c("year", "month", "day"))
#Also create variable "yearmon", a unique timestamp for each month between Jan 2020 and Dec 2023.
dat$yearmon <- paste(dat$year, dat$month, sep="-")
dat$yearmon<- as.character(dat$yearmon)
dat$yearmon <- as.yearmon(dat$yearmon)

#Create monthly dataset for plots
dat1 <- dat %>%
  group_by(Continent,Country, year, yearmon, month) %>%
  summarise(
    new_monthly_cases = sum(new_cases_per_million, na.rm = T), #new monthly cases per million
    new_monthly_cases_smoothed = sum(new_cases_smoothed_per_million, na.rm = T), #new monthly cases per million
    fully_vac = max(people_fully_vaccinated_per_hundred, na.rm = T), #new monthly cases per million
    new_monthly_deaths = sum(new_deaths_smoothed_per_million, na.rm = T)) #new monthly cases per million

#Replace generated infinite values with "NA" 
dat1[sapply(dat1, is.infinite)] <- NA

#Figure 1: Global monthly COVID-19 cases and deaths (per million population) over four years
owid_covid_data %>%
  select(location, date,
         new_cases_per_million, new_cases_smoothed_per_million,
         new_deaths_per_million, new_deaths_smoothed_per_million)%>%
  filter(location == "World")%>%
  separate_wider_delim(cols = date, 
                       delim = '-',
                       names = c("year", "month", "day"))%>%
  mutate(yearmon = as.yearmon(paste(year, month, sep="-"))) %>%
  group_by(yearmon) %>%
  summarise(cases = sum(new_cases_smoothed_per_million, na.rm = T),
            deaths = sum(new_deaths_smoothed_per_million, na.rm = T)) %>%
  ggplot(., aes(x = yearmon, y = cases, color = "Cases")) +
  geom_smooth(span = 0.15, se = FALSE, size = 0.8) +
  geom_smooth(aes(x = yearmon, y = deaths * 50, color = "Deaths"), span = 0.15, se = FALSE, size = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_yearmon(breaks = dat1$yearmon) +
  labs(x = "Time",
       y = "Global average COVID-19 cases (per million) and deaths (per 50 million)",
       color = "Outcomes") +
  scale_color_manual(values = c("Cases" = "blue", "Deaths" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  guides(x = guide_axis(angle = 90)) +
  theme_light(base_family = "IBM Plex Sans") +  
  theme(plot.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10))) +
  geom_rect(aes(xmin = as.numeric(as.yearmon("Jan 2022")), xmax = as.numeric(as.yearmon("May 2023")),
                ymin = -Inf, ymax = Inf), color = "transparent", alpha = 0.006) + 
  geom_text(aes(x = as.numeric(as.yearmon("Sep 2022")), y = 8300, label = "Study period\n (Omicron dominance)"), vjust = -1, hjust = 0.5, size = 5, color = "darkgreen", show.legend = FALSE)
ggsave("fig1.jpg", units="in", width=10, height=7, dpi=1200)


#Figure 2: Map of full vaccination coverage across the world in Jan 2022
#Useful Libraries
library(sf)        #managing map datasets
library(scales)    #scales for plotting
library(ggthemes)  #themes for plotting
dat1b <- dat %>%
  filter(yearmon == "Jan 2022") %>%
  group_by(Continent,Country) %>%
  summarise(fully_vac = max(people_fully_vaccinated_per_hundred, na.rm = T))
dat1b[sapply(dat1b, is.infinite)] <- NA #replace infinite values with NA

#Join vaccination coverage dataset (dat1b) to "map_data", which contains coordinates for plotting global maps (not provided). 

#First rename countries in map dataset to align with COVID dataset
map_data_cleaned <- map_data %>%
  rename(country_ = "country") %>%
  mutate(country = case_when(country_ == "Cabo Verde" ~ "Cape Verde", country_ == "Congo, Rep." ~ "Congo", country_ == "Congo, Dem. Rep." ~ "Democratic Republic of Congo",
                             country_ == "Egypt, Arab Rep." ~ "Egypt", country_ == "Gambia, The" ~ "Gambia", country_ == "Brunei Darussalam" ~ "Brunei",
                             country_ == "Hong Kong SAR, China" ~ "Hong Kong", country_ == "Lao PDR" ~ "Laos", country_ == "Macao SAR, China" ~ "Macao",
                             country_ == "Iran, Islamic Rep." ~ "Iran", country_ == "Kyrgyz Republic" ~ "Kyrgyzstan", country_ == "Syrian Arab Republic" ~ "Syria",
                             country_ == "West Bank and Gaza" ~ "Palestine", country_ == "Korea, Dem. People's Rep." ~ "North Korea", country_ == "Korea, Rep." ~ "South Korea",
                             country_ == "Timor-Leste" ~ "Timor", country_ == "Turkiye" ~ "Turkey", country_ == "Viet Nam" ~ "Vietnam",
                             country_ == "Yemen, Rep." ~ "Yemen", country_ == "Russian Federation" ~ "Russia", country_ == "Slovak Republic" ~ "Slovakia",
                             country_ == "Bahamas, The" ~ "Bahamas", country_ == "Virgin Islands (U.S.)" ~ "British Virgin Islands", country_ == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                             country_ == "St. Lucia" ~ "Saint Lucia", country_ == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                             country_ == "Venezuela, RB" ~ "Venezuela", .default = country_))
map <- dat1b %>%
  rename(country = "Country") %>%
  left_join(map_data_cleaned, by = "country") %>% 
  distinct(country, .keep_all = TRUE)

#Convert into a spatial file for mapping with the sf package
map <- st_as_sf(map)

#Plot vaccination coverage across the world
map1<-
  map %>% 
  ggplot() + 
  geom_sf(colour = "white",
          linewidth = 0.1,
          # Filling in data as a function of TFR (percentiles):
          mapping = aes(fill = ntile(fully_vac, 100))) +
  # Creating our own gradient scale:
  scale_fill_gradient2(low = muted("lightyellow"), 
                       high = muted("green")) +
  coord_sf(crs = st_crs("ESRI:53030")) +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "",
       fill = "Full vaccination \ncoverage (%) \n ") +
  theme(plot.title = element_text(face = "bold", size = 30), legend.position = "bottom",
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_bw()
map1
ggsave("fig2.jpg", units="in", width=10, height=7, dpi=1800)



# SECTION 2: CLEAN DATA FOR DESCRIPTIVE TABLE AND REGRESSION ANALYSIS ---------------------

#Select useful variables:
dat<- 
  owid_covid_data |>
  select(location, continent, date,
         new_cases,
         new_deaths,
         new_cases_per_million, 
         new_deaths_per_million, 
         total_tests_per_thousand,
         people_fully_vaccinated_per_hundred,
         population, population_density, median_age, aged_65_older, 
         gdp_per_capita, life_expectancy, human_development_index,
         cardiovasc_death_rate, diabetes_prevalence, hospital_beds_per_thousand, 
         stringency_index)|>
  rename(Continent = continent) |>
  group_by(location)|>
  #Delete countries without ANY data for each variable
  filter(any(!is.na(people_fully_vaccinated_per_hundred)) & any(!is.na(new_cases)) & any(!is.na(new_deaths)) & any(!is.na(stringency_index))
         & any(!is.na(population)) & any(!is.na(population_density)) & any(!is.na(median_age)) & any(!is.na(aged_65_older))
         & any(!is.na(gdp_per_capita)) & any(!is.na(life_expectancy)) & any(!is.na(cardiovasc_death_rate)) & any(!is.na(diabetes_prevalence))
         & any(!is.na(hospital_beds_per_thousand)))

#Need to convert a number of variables from daily measures into biyearly measures
#First, separate out the month of each row to enable adding up events across each month
dat<-
  dat|>
  separate_wider_delim(cols = date, 
                       delim = '-',
                       names = c("year", "month", "day"))
dat$yearmon <- paste(dat$year, dat$month, sep="-")
dat$yearmon <- as.factor(dat$yearmon)
dat1 <- dat %>%
  group_by(Continent,location, year, yearmon, month) %>%
  summarise(
    cases_per = sum(new_cases_per_million, na.rm = T),
    deaths_per = sum(new_deaths_per_million, na.rm = T),
    cases = sum(new_cases, na.rm = T),
    deaths = sum(new_deaths, na.rm = T),
    fully_vac = max(people_fully_vaccinated_per_hundred, na.rm = T), #new monthly cases per million
    avg_pop = mean(population, na.rm = T),
    strin = mean(stringency_index),
    pop = mean(population, na.rm = T),
    pop_den = mean(population_density, na.rm = T),
    med_age = mean(median_age, na.rm = T),
    aged_65 = mean(aged_65_older, na.rm = T),
    life = mean(life_expectancy, na.rm = T),
    gdp = mean(gdp_per_capita, na.rm = T),
    hdi = mean(human_development_index, na.rm = T),
    card = mean(cardiovasc_death_rate, na.rm = T),
    diab = mean(diabetes_prevalence, na.rm = T),
    beds = mean(hospital_beds_per_thousand, na.rm = T))

dat1[sapply(dat1, is.infinite)] <- NA #replace infinite values with NA

#Remove rows without countries
dat1<- dat1|>
  filter(location != "Africa" & location != "Asia" & location != "Europe" & location != "European Union"
         & location != "High income" & location != "Low income" & location != "Lower middle income" 
         & location != "North America" & location != "South America" & location != "Oceania" & 
           location != "Upper middle income" & location != "World")

#Select months in study period: Jan 2022 to May 2023
dat2<- dat1|>
  filter(yearmon %in% c("2022-01", "2022-02", "2022-03", "2022-04", 
                        "2022-05", "2022-06", "2022-07", "2022-08", 
                        "2022-09", "2022-10", "2022-11", "2022-12", 
                        "2023-01", "2023-02", "2023-03", "2023-04")) %>%
  mutate(time = case_when(yearmon == "2022-01" ~ 1, yearmon == "2022-02" ~ 2, yearmon == "2022-03" ~ 3, 
                          yearmon == "2022-04" ~ 4, yearmon == "2022-05" ~ 5, yearmon == "2022-06" ~ 6, 
                          yearmon == "2022-07" ~ 7, yearmon == "2022-08" ~ 8, yearmon == "2022-09" ~ 9, 
                          yearmon == "2022-10" ~ 10, yearmon == "2022-11" ~ 11, yearmon == "2022-12" ~ 12, 
                          yearmon == "2023-01" ~ 13, yearmon == "2023-02" ~ 14, yearmon == "2023-03" ~ 15, 
                          yearmon == "2023-04" ~ 16))

#Linear interpolation for missing data in Vaccination Coverage and Stringency Index 
library(zoo)
dat3 <- dat2 %>%
  group_by(Continent, location) %>%
  mutate(new_vacc = na.approx(fully_vac, na.rm = F)) |> #Fill in NA by linear interpolation
  fill(new_vacc, .direction = "down")|> #Replace trailing NA's with the preceding value
  mutate(stringg = case_when(time==16 ~ 0, .default = strin)) |> #Assign zero stringency to the last month
  mutate(string = na.approx(stringg, na.rm = F))|> #Fill in NA by linear interpolation
  select(-c(fully_vac, strin, stringg)) |>
  ungroup() 

#Create categories of Vaccination Coverage
dat3<-
  dat3 %>%
  mutate(vac = case_when(new_vacc >= 80 ~ "e. 80% or more", new_vacc < 80 & new_vacc >= 70 ~ "d. 70 to 79%", new_vacc < 70 & new_vacc >= 60 ~ "c. 60 to 69%", new_vacc < 60 & new_vacc >= 50 ~ "b. 50 to 59%", new_vacc < 50 ~ "a. <50%", .default = NA))


#Include Testing Data (Total tests per thousand in each country in 2021)
test <- dat %>%
  filter(year == 2021) %>%
  group_by(Continent,location, year) %>%
  summarise(
    tests = max(total_tests_per_thousand, na.rm = T)) |>
  filter(location != "Africa" & location != "Asia" & location != "Europe" & location != "European Union"
         & location != "High income" & location != "Low income" & location != "Lower middle income" 
         & location != "North America" & location != "South America" & location != "Oceania" & 
           location != "Upper middle income" & location != "World") |>
  ungroup() |>
  select(-c(year, Continent))

test[sapply(test, is.infinite)] <- NA #replace infinite values with NA

#Attach to dat3
dat4 <- dat3 %>%
  full_join(test, by = "location")

#Retain complete cases
dat4 <- dat4 %>%
  filter(complete.cases(.))
unique(dat4$location)



# SECTION 3: DESCRIPTIVE STATS ------------------------------------------------------------

library(gtsummary)

dat5<- dat4%>%
  select(-c(yearmon, location, year, month, cases, deaths, avg_pop))

#OVERALL
table1 <- tbl_summary(dat5, missing="no", digits=all_continuous() ~ 2,
                      percent = "column",
                      label = list(pop~"Population",
                                   cases_per~"Cases per million", 
                                   deaths_per~"Deaths per million")) %>%
  add_n() %>% # add column with total number of non-missing observations
  bold_labels()
as_flex_table(table1)

#BY LEVELS OF VACCINATION
table2 <- tbl_summary(dat5, by=vac, missing="no", digits=all_continuous() ~ 0,
                      percent = "row",
                      label = list(pop~"Population",
                                   cases_per~"Cases per million", 
                                   deaths_per~"Deaths per million")) %>%
  add_n() %>% # add column with total number of non-missing observations
  bold_labels() %>%
  add_p()
as_flex_table(table2)
chisq.test(dat4$Continent, dat4$vac)

#Population count
pop<-dat4%>%
  filter(time == 10)%>%
  select(location, pop)
sum(pop$pop)



# SECTION 4: REGRESSION MODELS ------------------------------------------------------------

#One month lag for exposure and stringency index
dat5 <- dat4%>%
  group_by(Continent, location) %>%
  mutate(vac_l = dplyr::lag(vac, n=1, order_by = time, default = NA), #categorical
         vacc_l = dplyr::lag(new_vacc, n=1, order_by = time, default = NA), #continuous
         string_l = dplyr::lag(string, n=1, order_by = time, default = NA)) #stringency
#In later sensitivity tests, we vary lag to 0 and 2 months.

library(glmmTMB) #For random effect negative binomial models
library(broom)   #Tidying for mixed effect models
library(MASS)    #Create fixed effect negative binomial models
library(sandwich)#Compute robust errors for fixed effect models
library(lmtest)  #Produce confidence intervals for fixed effect models with robust errors (coeftest())

#Random Effects Negative Binomial Models (Errors are clustered at the country level)

#OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod1a <- glmmTMB(cases  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1a, exponentiate = T, estimate_fun = function(x) style_number(x, digits = 3))
?tbl_regression

#Categorical Exposure Variable
mod1b <- glmmTMB(cases  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time),
                           data = dat5, 
                           family = nbinom2)
tbl_regression(mod1b, exponentiate = T, estimate_fun = function(x) style_number(x, digits = 3))

#OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod1c <- glmmTMB(deaths  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1c, exponentiate = T, estimate_fun = function(x) style_number(x, digits = 3))

#Categorical Exposure Variable
mod1d <- glmmTMB(deaths  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1d, exponentiate = T, estimate_fun = function(x) style_number(x, digits = 3))

#MODEL DOT-AND-WHISKER PLOT
mod_labels <- c("vac_le. 80% or more" = "80% or more",
                "vac_ld. 70 to 79%" = "70 to 79%",
                "vac_lc. 60 to 69%" = "60 to 69%",
                "vac_lb. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod1b,
                    "New Deaths" = mod1d)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))

models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                          size = 1,
                          linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage") +
  labs(color = "Model Outcome") + theme_bw()+
  theme(
    axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
    legend.text=element_text(size=16))
ggsave("fig3.jpg", units="in", width=10, height=7, dpi=1200)



# SECTION 5: SENSITIVITY ANALYSES ----------------------------------------------------

#1. Fixed Effects Negative Binomial Models (Estimates are not produced for time-invariant covariates because they are all captured in the dummies for country) 

##OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod2a <- glm.nb(cases  ~ vacc_l + offset(log(pop)) + location + string_l + month, dat5)
#Cluster-robust SE
robust_se <- sandwich::vcovHC(mod2a, cluster = "location")
mod2a_robust<-coeftest(mod2a, vcov = robust_se)
tbl_regression(mod2a_robust, exponentiate = T)
exp(-0.02)

#Categorical Exposure Variable
mod2b <- MASS::glm.nb(cases  ~ vac_l + offset(log(pop)) + location + string_l + month, dat5)
#Cluster-robust SE
robust_se <- sandwich::vcovHC(mod2b, cluster = "location")
mod2b_robust<-coeftest(mod2b, vcov = robust_se)
tbl_regression(mod2b_robust, exponentiate = T)
exp(c(-2.88,	-4.11, -1.64))

##OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod2c <- glm.nb(deaths  ~ vacc_l + offset(log(pop)) + location + string_l + month, dat5)
#Cluster-robust SE
robust_se <- sandwich::vcovHC(mod2c, cluster = "location")
mod2c_robust<-coeftest(mod2c, vcov = robust_se)
tbl_regression(mod2c_robust, exponentiate = T)
exp(c(-0.09,	-0.12, -0.06))

#Categorical Exposure Variable
mod2d <- MASS::glm.nb(deaths  ~ vac_l + offset(log(pop)) + location + string_l + month, dat5)
#Cluster-robust SE
robust_se <- sandwich::vcovHC(mod2d, cluster = "location")
mod2d_robust<-coeftest(mod2d, vcov = robust_se)
tbl_regression(mod2d_robust, exponentiate = T)
exp(c(-2.30,	-3.17, -1.42))

#Model Plot
mod_labels <- c("vac_le. 80% or more" = "80% or more",
                "vac_ld. 70 to 79%" = "70 to 79%",
                "vac_lc. 60 to 69%" = "60 to 69%",
                "vac_lb. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod2b_robust,
               "New Deaths" = mod2d_robust)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))
models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                     size = 1,
                     linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage")+
  My_Theme
ggsave("fig6.jpg", units="in", width=10, height=7, dpi=300)



#2. Missing stringency data: imputing zero for all months in 2023
library(tidyverse)
dat3 <- dat2 %>%
  group_by(Continent, location) %>%
  mutate(new_vacc = na.approx(fully_vac, na.rm = F)) |> #Fill in NA by linear interpolation
  fill(new_vacc, .direction = "down")|> #Replace trailing NA's with the preceding value
  mutate(string = case_when(time %in% c(13, 14, 15, 16) ~ 0, .default = strin)) |> #Assign zero stringency to the last four months (Jan to Apr)
  select(-c(fully_vac, strin)) |>
  ungroup() 

#Create categories of Vaccination Coverage
dat3<-
  dat3 %>%
  mutate(vac = case_when(new_vacc >= 80 ~ "e. 80% or more", new_vacc < 80 & new_vacc >= 70 ~ "d. 70 to 79%", new_vacc < 70 & new_vacc >= 60 ~ "c. 60 to 69%", new_vacc < 60 & new_vacc >= 50 ~ "b. 50 to 59%", new_vacc < 50 ~ "a. <50%", .default = NA))

#Include testing data
dat4 <- dat3 %>%
  full_join(test, by = "location")

#Retain complete cases
dat4 <- dat4 %>%
  filter(complete.cases(.))
unique(dat4$location)

#One month lag
dat5 <- dat4%>%
  group_by(Continent, location) %>%
  mutate(vac_l = dplyr::lag(vac, n=1, order_by = time, default = NA), #categorical
         vacc_l = dplyr::lag(new_vacc, n=1, order_by = time, default = NA), #continuous
         string_l = dplyr::lag(string, n=1, order_by = time, default = NA)) #stringency

#Random Effects Negative Binomial Models

##OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod1a <- glmmTMB(cases  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1a, exponentiate = T)

#Categorical Exposure Variable
mod1b <- glmmTMB(cases  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1b, exponentiate = T)

##OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod1c <- glmmTMB(deaths  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1c, exponentiate = T)

#Categorical Exposure Variable
mod1d <- glmmTMB(deaths  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1d, exponentiate = T)

#Model Plot
mod_labels <- c("vac_le. 80% or more" = "80% or more",
                "vac_ld. 70 to 79%" = "70 to 79%",
                "vac_lc. 60 to 69%" = "60 to 69%",
                "vac_lb. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod1b,
               "New Deaths" = mod1d)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))
models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                     size = 1,
                     linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage")+
  My_Theme
ggsave("fig7.jpg", units="in", width=10, height=7, dpi=300)



#3. No Lags for Exposure and Stringency

#First step: re-create the original dat5 with the linearly interpolated stringency index. Then proceed to modelling

#Random Effects Negative Binomial Models

##OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod1a <- glmmTMB(cases  ~ new_vacc + offset(log(pop)) + pop_den + card + diab + med_age + string + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1a, exponentiate = T)

#Categorical Exposure Variable
mod1b <- glmmTMB(cases  ~ vac + offset(log(pop)) + pop_den + card + diab + med_age + string + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1b, exponentiate = T)

##OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod1c <- glmmTMB(deaths  ~ new_vacc + offset(log(pop)) + pop_den + card + diab + med_age + string + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1c, exponentiate = T)

#Categorical Exposure Variable
mod1d <- glmmTMB(deaths  ~ vac + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1d, exponentiate = T)

#Model Plot
mod_labels <- c("vace. 80% or more" = "80% or more",
                "vacd. 70 to 79%" = "70 to 79%",
                "vacc. 60 to 69%" = "60 to 69%",
                "vacb. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod1b,
               "New Deaths" = mod1d)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))
models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                     size = 1,
                     linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage")+
  My_Theme
ggsave("fig8.jpg", units="in", width=10, height=7, dpi=300)



#4. Two-month Lag for vaccination and stringency

dat5 <- dat4%>%
  group_by(Continent, location) %>%
  mutate(vac_l2 = dplyr::lag(vac, n=2, order_by = time, default = NA), #categorical
         vacc_l2 = dplyr::lag(new_vacc, n=2, order_by = time, default = NA), #continuous
         string_l2 = dplyr::lag(string, n=2, order_by = time, default = NA)) #stringency

#Random Effects Negative Binomial Models

##OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod1a <- glmmTMB(cases  ~ vacc_l2 + offset(log(pop)) + pop_den + card + diab + med_age + string_l2 + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1a, exponentiate = T)

#Categorical Exposure Variable
mod1b <- glmmTMB(cases  ~ vac_l2 + offset(log(pop)) + pop_den + card + diab + med_age + string_l2 + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1b, exponentiate = T)

##OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod1c <- glmmTMB(deaths  ~ vacc_l2 + offset(log(pop)) + pop_den + card + diab + med_age + string_l2 + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time),
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1c, exponentiate = T)

#Categorical Exposure Variable
mod1d <- glmmTMB(deaths  ~ vac_l2 + offset(log(pop)) + pop_den + card + diab + med_age + string_l2 + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1d, exponentiate = T)

#Model Plot
mod_labels <- c("vac_l2e. 80% or more" = "80% or more",
                "vac_l2d. 70 to 79%" = "70 to 79%",
                "vac_l2c. 60 to 69%" = "60 to 69%",
                "vac_l2b. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod1b,
               "New Deaths" = mod1d)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))
models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                     size = 1,
                     linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage")+
  My_Theme
ggsave("fig9.jpg", units="in", width=10, height=7, dpi=300)

#5 Begin study period from Nov 2021

#Select months in study period: Nov 2021 to April 2023
dat2<- dat1|>
  filter(yearmon %in% c("2021-11", "2021-12",
                        "2022-01", "2022-02", "2022-03", "2022-04",
                        "2022-05", "2022-06", "2022-07", "2022-08", 
                        "2022-09", "2022-10", "2022-11", "2022-12", 
                        "2023-01", "2023-02", "2023-03", "2023-04")) %>%
  mutate(time = case_when(yearmon == "2021-11" ~ 1, yearmon == "2021-12" ~ 2,
                          yearmon == "2022-01" ~ 3, yearmon == "2022-02" ~ 4, yearmon == "2022-03" ~ 5,
                          yearmon == "2022-04" ~ 6, yearmon == "2022-05" ~ 7, yearmon == "2022-06" ~ 8, 
                          yearmon == "2022-07" ~ 9, yearmon == "2022-08" ~ 10, yearmon == "2022-09" ~ 11, 
                          yearmon == "2022-10" ~ 12, yearmon == "2022-11" ~ 13, yearmon == "2022-12" ~ 14, 
                          yearmon == "2023-01" ~ 15, yearmon == "2023-02" ~ 16, yearmon == "2023-03" ~ 17, 
                          yearmon == "2023-04" ~ 18))

#Linear interpolation for missing data in Vaccination Coverage and Stringency Index 
library(zoo)
dat3 <- dat2 %>%
  group_by(Continent, location) %>%
  mutate(new_vacc = na.approx(fully_vac, na.rm = F)) |> #Fill in NA by linear interpolation
  fill(new_vacc, .direction = "down")|> #Replace trailing NA's with the preceding value
  mutate(stringg = case_when(time==18 ~ 0, .default = strin)) |> #Assign zero stringency to the last month
  mutate(string = na.approx(stringg, na.rm = F))|> #Fill in NA by linear interpolation
  select(-c(fully_vac, strin, stringg)) |>
  ungroup() 

#Create categories of Vaccination Coverage
dat3<-
  dat3 %>%
  mutate(vac = case_when(new_vacc >= 80 ~ "e. 80% or more", new_vacc < 80 & new_vacc >= 70 ~ "d. 70 to 79%", new_vacc < 70 & new_vacc >= 60 ~ "c. 60 to 69%", new_vacc < 60 & new_vacc >= 50 ~ "b. 50 to 59%", new_vacc < 50 ~ "a. <50%", .default = NA))


#Include Testing Data (Total tests per thousand in each country in 2021)
dat4 <- dat3 %>%
  full_join(test, by = "location")

#Retain complete cases
dat4 <- dat4 %>%
  filter(complete.cases(.))
unique(dat4$location)

#One month lag for exposure and stringency index
dat5 <- dat4%>%
  group_by(Continent, location) %>%
  mutate(vac_l = dplyr::lag(vac, n=1, order_by = time, default = NA), #categorical
         vacc_l = dplyr::lag(new_vacc, n=1, order_by = time, default = NA), #continuous
         string_l = dplyr::lag(string, n=1, order_by = time, default = NA)) #stringency
#In later sensitivity tests, we vary lag to 0 and 2 months.

library(glmmTMB) #For random effect negative binomial models
library(broom)   #Tidying for mixed effect models

#Random Effects Negative Binomial Models (Errors are clustered at the country level)

#OUTCOME: COVID-19 CASES
#Continuous Exposure Variable
mod1a <- glmmTMB(cases  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1a, exponentiate = T)

#Categorical Exposure Variable
mod1b <- glmmTMB(cases  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + Continent + (1 | location) + (1 | location:time),
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1b, exponentiate = T)

#OUTCOME: COVID-19 DEATHS
#Continuous Exposure Variable
mod1c <- glmmTMB(deaths  ~ vacc_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1c, exponentiate = T)

#Categorical Exposure Variable
mod1d <- glmmTMB(deaths  ~ vac_l + offset(log(pop)) + pop_den + card + diab + med_age + string_l + gdp + life + tests + beds + month + (1 | Continent/location) + (1 | location:time), 
                 data = dat5, 
                 family = nbinom2)
tbl_regression(mod1d, exponentiate = T)

#MODEL DOT-AND-WHISKER PLOT
mod_labels <- c("vac_le. 80% or more" = "80% or more",
                "vac_ld. 70 to 79%" = "70 to 79%",
                "vac_lc. 60 to 69%" = "60 to 69%",
                "vac_lb. 50 to 59%" = "50 to 59%")

models <- list("New Cases" = mod1b,
               "New Deaths" = mod1d)

My_Theme = theme(
  axis.title.x = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 13, face = "italic", margin = margin(t = 10)),
  legend.text=element_text(size=16))
models %>% modelplot(coef_omit = "Interc|Cont|bed|age|test|life|gdp|strin|diab|card|pop|loc|month",
                     size = 1,
                     linewidth = 1,
                     coef_map = mod_labels,
                     exponentiate = T) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_colour_manual(values = c("blue", "red"))+
  labs(x = "Rate Ratio and 95% Confidence Interval",
       y = "Full vaccination coverage")+
  My_Theme
ggsave("fig10.jpg", units="in", width=10, height=7, dpi=300)


#The End