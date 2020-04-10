library(tidyverse)
library(zoo)
library(ggrepel)
library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(prophet)

setwd("~/Projects/covid-eda/")

# # Map of Counties

mapdat <- as.data.frame(read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
current_date <- last(mapdat$date)
current_date

mapdat1 <- filter(mapdat, date == current_date) %>% dplyr::select(fips, deaths)

# mapdat1 <- mapdat %>%
#   group_by(state, fips) %>%
#   summarise(total_deaths = sum(deaths, na.rm = TRUE),
#             total_cases = sum(cases, na.rm = TRUE)) %>%
#   ungroup() %>%
#   select(fips, total_deaths) %>%
#   filter(!is.na(fips))


names(mapdat1) <- c("region", "value")

mapdat1$region <- str_remove(mapdat1$region, "^0+")

# Remove fips with 999
mapdat1 <- mapdat1[-which(substr(mapdat1$region, nchar(mapdat1$region) - 1, nchar(mapdat1$region)) == 99), ]

# Recode to numeric
mapdat1$region <- as.numeric(mapdat1$region)


mapdat2 <- mapdat1
mapdat2$value <- cut(mapdat1$value, breaks = c(1, 10, 25, 50, 75, 100, 150, 200, 1000),
                     labels = c("1-10", "10-25", "25-50", "50-75", "75-100", "100-150", "150-200", ">200"))
mapdat2 <- drop_na(mapdat2)

cvalues <- brewer.pal(n=9, "Oranges")[2:9]
# cvalues <- viridis(option = "D", 10)[3:10]

choro = CountyChoropleth$new(mapdat2)
choro$title = paste0("US Covid-19 County-level Deaths (Updated ", last(mapdat$date), ") \n Total Deaths (", sum(mapdat1$value), ")")
choro$set_num_colors(7)
# choro$ggplot_scale = scale_fill_manual(values = cvalues, na.value="white", na.translate=FALSE)
choro$ggplot_scale = scale_fill_manual(values = cvalues, na.value="white", na.translate=FALSE)
choro$render() + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       plot.title = element_text(hjust = 0.5))

ggsave("~/Projects/covid-eda/figures/0-US_County_Death_Map.png", width = 10, height = 10)

 
# 
# 
# 
# 
# 
# covid_map <- county_choropleth(mapdat1)
# #covid_map
# 
# covid_map$data$value <- ifelse(is.na(covid_map$data$value), 0, covid_map$data$value)
# 
# covid_map <- covid_map + 
#   # scale_fill_manual(values=cc) +
#   # scale_fill_brewer(palette = "YlOrRd", 7, na.value = "Yl") +
#   # scale_fill_gradient(cc) +
#   # scale_fill_brewer(palette = "Spectral") +
#   scale_fill_gradientn(colors = viridis(5), 
#                        breaks = c(100, 200, 350, 400, 500),
#                        limit = c(0, 200)) +
#   # lims(fill = c(0, 500)) +
#   theme_tufte(base_size = 10)+
#   xlab(NULL) + ylab(NULL)  +
#   # theme(legend.position = "none",
#   #                      axis.text.x = element_blank(),
#   #                      axis.text.y = element_blank(),
#   #                      axis.ticks.x = element_blank(),
#   #                      axis.ticks.y = element_blank(),
#   #                      panel.border = element_rect(fill = NA))
#   NULL
# covid_map






# ---------------------------------------------------------------
# Non-New-York Data
nonny <- read_csv("http://covidtracking.com/api/states/daily.csv")
nonny <- dplyr::select(nonny, state, date, death)
nonny <- filter(nonny, state != "NY")
names(nonny) <- c("country", "date", "value")
nonny <- nonny %>% 
  group_by(date) %>% 
  summarise(value = sum(value, na.rm= TRUE)) %>% 
  mutate(country = "US(non-NY)") %>% 
  dplyr::select(country, date, value) %>% 
  ungroup()

nonny$date <- as.Date(paste0(substr(nonny$date, 1, 4), "-", substr(nonny$date, 5, 6), "-", substr(nonny$date, 7, 8)))
nonny$date <- as.Date(as.character(nonny$date), "%Y-%m-%d")
nonny$date <- format(nonny$date, "%m/%d/%Y")

cdat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
cdat <- dplyr::select(cdat, -Lat, -Long)
cdat <- gather(cdat, key = date, value=value, -`Province/State`, -`Country/Region`)
names(cdat) <- c("state", "country", "date", "value")
cdat <- dplyr::select(cdat, country, date, value)

cdat <- rbind(cdat, nonny)

cdat$date <- as.Date(cdat$date, "%m/%d/%y")

# World data -----------------------------------------------------
ccdat <- cdat %>% 
  filter(value >= 10 & (country %in% c("US", "US(non-NY)", "China", "Italy", "Iran", "Spain", "United Kingdom", "Japan", "Korea, South", "France"))) %>% 
  group_by(country, date) %>% 
  arrange(date) %>%
  summarise(value = sum(value)) %>%
  mutate(daily_deaths = value - lag(value),
         ndays = seq(1, n(), 1),
         value_rm3 = rollmean(daily_deaths, k = 3, align = "right", na.pad = TRUE)) %>% 
  ungroup()
ccdat

ccdat$country <- ifelse(ccdat$country == "United Kingdom", "UK", ccdat$country)

ccdat$value <- log(ccdat$value)

clabels <- ccdat %>% 
  group_by(country) %>% 
  filter(row_number()==n()) 
clabels

  
ccdat$cat <- as.character(ifelse(ccdat$country == "US", 1, 0))
ccdat$cat <- as.character(ifelse(ccdat$country == "US(non-NY)", 2, ccdat$cat))

ggplot(ccdat, aes(x=ndays, y=value, color=factor(country))) + 
  # scale_alpha_manual(values = c(0.5, 1), guide = FALSE) +
  geom_point(size=0.75) +
  geom_line() +
  # scale_color_manual(values=c("2" = "RoyalBlue", "1" = "DarkGrey", "0"="LightGrey")) +
  theme_bw(12) +
  labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as absolute values)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none") +
  scale_y_continuous(breaks = c(min(ccdat$value), 3, 4, 5, 6, 7, 8, 9, 10),
                     labels = round(c(min(exp(ccdat$value)), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8), exp(9), exp(10)), 0),
                     expand=c(0, 0),
                     limits = c(min(ccdat$value), 10)) +
  scale_x_continuous(breaks = seq(0, 70, 5),
                     expand= c(0,0),
                     limits = c(0, 80)) +
  scale_color_manual(values=c("US" = "royalblue", 
                              "US(non-NY)" = "royalblue",
                              "Spain" = "darkgrey", 
                              "Italy"="darkgrey",
                              "France" = "darkgrey",
                              "Iran" = "darkgrey",
                              "Japan" = "darkgrey",
                              "Korea, South" = "darkgrey",
                              "UK" = "darkgrey",
                              "China" = "darkgrey")) +  
  geom_text_repel(data=clabels, aes(label = country),
          # force=1,
          point.padding=unit(1,'lines'),
          direction = 'x') +
          # xlim = c(80, 100)) +
  NULL

ggsave("~/Projects/covid-eda/figures/1-World-Rate.png", width = 10, height = 6)




ccdat$ndays_rm <- ccdat$ndays - 3
ccdat1 <- drop_na(ccdat)
ccdat1 <- filter(ccdat1, country != "China" & country != "Japan" & country != "Korea, South")

unique(ccdat1$country)

clabels$ndays_rm <- clabels$ndays - 3

# World Rolling Mean Death Rate
ggplot(ccdat1, aes(x=ndays_rm, y=(value_rm3), color=factor(country))) + 
  geom_point(size=0.75) +
  geom_line() +
  scale_color_manual(values=c("US" = "royalblue", 
                              "US(non-NY)" = "royalblue",
                              "Spain" = "darkgrey", 
                              "Italy"="darkgrey",
                              "France" = "darkgrey",
                              "Iran" = "darkgrey",
                              "Japan" = "darkgrey",
                              "Korea, South" = "darkgrey",
                              "UK" = "darkgrey")) +
  scale_alpha_manual(values=c("US" = .1, 
                              "US(non-NY)" = 0.5,
                              "Spain" = 0.5, 
                              "Italy"=0.5,
                              "France" = 0.5,
                              "Iran" = 0.5,
                              "Japan" = 0.5,
                              "Korea, South" = 0.5,
                              "UK" = 0.5)) +  
  theme_bw(12) +
  labs(x="Number of days since 10th Death", y="Daily Number of Deaths \n (3-day Right-rolling mean)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 2500, 100)) +
  scale_x_continuous(breaks = seq(0, 50, 5),
                     expand= c(0,0),
                     limits = c(0, 50)) +
  geom_text_repel(data=filter(clabels, country != "China" & country != "Japan" & country != "Korea, South"), aes(label = country),
          # force=1,
          point.padding=unit(1,'lines'),
          direction = 'x',
          nudge_x = 1.5,
          segment.alpha = 0.75) +
  NULL


ggsave("~/Projects/covid-eda/figures/2-World-Daily-Death-Rate.png", width = 10, height = 6)



ggplot(filter(ccdat1, country == "US" | country == "US(non-NY)"), aes(date, daily_deaths, fill=country)) + 
  geom_bar(stat="identity") +
  geom_text_repel(data = filter(ccdat1, (date >= today() - 5) & (country == "US" | country == "US(non-NY)")), aes(label=daily_deaths), nudge_y = 20, size=3) +
  # geom_histogram(alpha=0.2, stat="identity") +
  theme_bw() +
  labs(x=NULL, y="Daily Death Count") +
  theme(legend.position = c(.085, .9),
        legend.title = element_blank()) +
  facet_wrap(~country) +
  NULL

ggsave("~/Projects/covid-eda/figures/3-US_Daily-Death-Rate_BarChart.png", width = 10, height = 6)


#

# County/State Data
uscdat <- read_csv("http://covidtracking.com/api/states/daily.csv")
uscdat$date <- as.Date(paste0(substr(uscdat$date, 1, 4), "-", substr(uscdat$date, 5, 6), "-", substr(uscdat$date, 7, 8)))

# Get US regions
regions <- data.frame(state = state.abb, regions = state.region)

uscdat2 <- uscdat %>% 
  filter(death >= 10) %>%
  group_by(date, state) %>% 
  summarise(value = sum(death, na.rm = TRUE)) %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  mutate(ndays = seq(1, n(), 1)) %>% 
  ungroup() %>% 
  left_join(regions, by="state") %>% 
  ungroup()

uscdat2

uscdat2$regions <- as.character(uscdat2$regions)

uscdat2$regions <- ifelse(is.na(uscdat2$regions), "Other", uscdat2$regions)
unique(uscdat2$regions)

uscdat2$value <- log(uscdat2$value)

ggplot(uscdat2, aes(x=ndays, y=value, color=factor(state))) + 
  geom_point(size=0.75) +
  geom_line() +
  theme_bw(12) +
  labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as real)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none") +
  scale_y_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 9.5),
                     labels = round(c(min(exp(uscdat2$value)), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8), exp(9), exp(0.5)), 0),
                     expand=c(0, 0),
                     limits = c(2, 9.5)) +
  scale_x_continuous(breaks = seq(0, 40, 5),
                     expand= c(0,0),
                     limits = c(0, 40)) +
  geom_text_repel(data = filter(uscdat2, date == last(uscdat2$date)), aes(label = state),
          force=1,
          point.padding=unit(1,'lines'),
          direction = 'x',
          nudge_x = 1.5,
          segment.alpha = 0.75) +
  facet_wrap(~regions, scales = "free") +
  NULL
  

ggsave("~/Projects/covid-eda/figures/4-US-State-Rate.png", width = 15, height = 10)


# US data --------------------------------------------------------
usdat <- filter(cdat, country == "US")
usdat
unique(usdat$state)

usdat$date <- as.Date(usdat$date, "%m/%d/%y")

usdat <- usdat %>% 
  filter(value > 0) %>% 
  group_by(date) %>% 
  summarise(value = sum((value))) %>% 
  arrange(date) %>% 
  ungroup

usdat$rm2 <- rollmean(usdat$value, k = 2, na.pad = TRUE, align = "right")
usdat$rm3 <- rollmean(usdat$value, k = 3, na.pad = TRUE, align = "right")
usdat$rm4 <- rollmean(usdat$value, k = 4, na.pad = TRUE, align = "right")
usdat

usdat$`R-RM1` <- (usdat$value - lag(usdat$value))/(lag(usdat$value))
usdat$`R-RM2` <- (usdat$rm2 - lag(usdat$rm2))/(lag(usdat$rm2))
usdat$`R-RM3` <- (usdat$rm3 - lag(usdat$rm3))/(lag(usdat$rm3))

usdat2 <- select(usdat, -value, -rm2, -rm3, -rm4) %>% 
  gather(key=rm, value=value, -date)

labelss <- usdat[nrow(usdat), ]
labelss <- select(labelss, -value, -rm2, -rm3, -rm4) %>% 
  gather(key=rm, value=value, -date)
labelss$label <- paste0(labelss$rm, ": ", 1 + round(labelss$value, 2), "x")
labelss

ggplot(filter(usdat2, date >= as.Date("2020-03-03")), aes(date, value*100, color=factor(rm))) + 
  geom_smooth(aes(date, value*100, group=1)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels=function(x) paste0((1 + x/100), "x"), 
                     breaks = seq(-100, 100, 25),
                     limits = c(-99, 99)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_x_date(date_labels = '%m/%d', 
               limits = c(as.Date("2020-03-03"), max(usdat$date) + 2), 
               breaks = seq(min(usdat$date), max(usdat$date) + 3, "day")) +
  labs(x=NULL, y="US Mortality Multiplier \n (Rolling Mean Windows 1-3)") +
  theme(legend.position = "none") +
  # geom_text(data=labelss, aes(date, value, label=label)) +
  geom_text_repel(data = labelss,
          aes(label = label),
          force=1,
          point.padding=unit(1,'lines'),
          direction = 'y',
          nudge_x = 1.5,
          segment.size=0.1,
          xlim = c(max(usdat$date) + 2.5)) +
  NULL
  
ggsave("~/Projects/covid-eda/figures/5-US-Mortality-Multiplier.png", width = 12.5, height = 6)

# usdat[nrow(usdat), ]
# 
# regdat <- filter(ccdat, country == "US") %>% arrange(date)
# regdat$week <- week(regdat$date)
# 
# mod <- glm(value ~ lag(value) + week , data = regdat)
# summary(mod)
# 
# newdat <- data.frame(value = tail(regdat$value, 2), week = 14)
# predict(mod, newdata = newdat)


usdat[nrow(usdat), ]
