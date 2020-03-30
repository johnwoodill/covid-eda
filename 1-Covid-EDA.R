library(tidyverse)
library(zoo)
library(ggrepel)
library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)

# # Map of Counties
# 
# mapdat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# current_date <- last(mapdat$date)
# current_date
# 
# mapdat1 <- mapdat %>% 
#   group_by(state, fips) %>% 
#   summarise(total_deaths = sum(deaths, na.rm = TRUE),
#             total_cases = sum(cases, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   select(fips, total_deaths) %>% 
#   filter(!is.na(fips))
# 
# 
# mapdat1 <- filter(mapdat1, total_deaths >= 5)
# 
# # mapdat1$total_deaths <- ifelse(is.na(mapdat1$total_deaths), 0, mapdat1$total_deaths)
# 
# 
# 
# names(mapdat1) <- c("region", "value")
# 
# # usfips <- unique(usmap$fips)
# mapdat1$region <- str_remove(mapdat1$region, "^0+")
# 
# # Remove fips with 999
# mapdat1 <- mapdat1[-which(substr(mapdat1$region, nchar(mapdat1$region) - 1, nchar(mapdat1$region)) == 99), ]
# # mapdat1 <- mapdat1[-which(substr(mapdat1$region, nchar(mapdat1$region) - 1, nchar(mapdat1$region)) == 98), ]
# 
# mapdat1$region <- as.numeric(mapdat1$region)
# # mapdat1$value <- cut(mapdat1$value, breaks = 9)
# 
# # mapdat1$value <- as.factor(mapdat1$value)
# 
# choro = CountyChoropleth$new(mapdat1)
# choro$title = "2012 Population Estimates"
# choro$set_num_colors(9)
# choro$ggplot_scale = scale_fill_brewer(name="Population", palette=7, drop=FALSE)
# choro$render()
# 
# 
# 
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

cdat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
cdat <- select(cdat, -Lat, -Long)

nonny <- read_csv("http://covidtracking.com/api/states/daily.csv")
nonny <- select(nonny, state, date, death)
nonny <- filter(nonny, state != "NY")
names(nonny) <- c("country", "date", "value")
nonny <- nonny %>% 
  group_by(date) %>% 
  summarise(value = sum(value, na.rm= TRUE)) %>% 
  mutate(country = "US(non-NY)") %>% 
  select(country, date, value) %>% 
  ungroup()

nonny$date <- as.Date(paste0(substr(nonny$date, 1, 4), "-", substr(nonny$date, 5, 6), "-", substr(nonny$date, 7, 8)))
nonny$date <- as.Date(as.character(nonny$date), "%Y-%m-%d")
nonny$date <- format(nonny$date, "%m/%d/%Y")



cdat <- gather(cdat, key = date, value=value, -`Province/State`, -`Country/Region`)
names(cdat) <- c("state", "country", "date", "value")
cdat <- select(cdat, country, date, value)

cdat <- rbind(cdat, nonny)

cdat$date <- as.Date(cdat$date, "%m/%d/%y")

# World data -----------------------------------------------------
ccdat <- cdat %>% 
  filter(value >= 10 & (country %in% c("US", "US(non-NY)", "China", "Italy", "Iran", "Spain", "United Kingdom", "Japan", "Korea, South", "France"))) %>% 
  group_by(country, date) %>% 
  arrange(date) %>%
  summarise(value = sum(value)) %>%
  mutate(ndays = seq(1, n(), 1)) %>% 
  ungroup()
ccdat

ccdat$country <- ifelse(ccdat$country == "United Kingdom", "UK", ccdat$country)

ccdat$value <- log(ccdat$value)

clabels <- ccdat %>% 
  group_by(country) %>% 
  filter(row_number()==n()) 
  # mutate(value = exp(value))
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
                     labels = round(c(min(exp(ccdat$value)), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8), exp(9), exp(9.5)), 0),
                     expand=c(0, 0),
                     limits = c(min(ccdat$value), 10)) +
  scale_x_continuous(breaks = seq(0, 70, 5),
                     expand= c(0,0),
                     limits = c(0, 70)) +
  geom_text_repel(data=clabels, aes(label = country),
          # force=1,
          point.padding=unit(1,'lines'),
          direction = 'x',
          nudge_x = 1.5,
          segment.alpha = 0.75) +
  NULL

ggsave("~/Projects/covid-eda/figures/1-World-Rate.png", width = 10, height = 6)
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
  left_join(regions, by="state")

uscdat2


uscdat2$value <- log(uscdat2$value)

ggplot(uscdat2, aes(x=ndays, y=value, color=factor(state))) + 
  geom_point(size=0.75) +
  geom_line() +
  theme_bw(12) +
  labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as real)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none") +
  scale_y_continuous(breaks = c(2, 3, 4, 5, 6, 7),
                     labels = round(c(min(exp(uscdat2$value)), exp(3), exp(4), exp(5), exp(6), exp(7)), 0),
                     expand=c(0, 0),
                     limits = c(2, 7)) +
  scale_x_continuous(breaks = seq(0, 20, 1),
                     expand= c(0,0),
                     limits = c(0, 20)) +
  geom_text_repel(data = filter(uscdat2, date == last(uscdat2$date)), aes(label = state),
          force=1,
          point.padding=unit(1,'lines'),
          direction = 'x',
          nudge_x = 1.5,
          segment.alpha = 0.75) +
  facet_wrap(~regions) +
  NULL
  
ggsave("~/Projects/covid-eda/figures/2-US-State-Rate.png", width = 15, height = 10)







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
               breaks = seq(min(usdat$date), max(usdat$date), "day")) +
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
  
ggsave("~/Projects/covid-eda/figures/3-US-Mortality-Multiplier.png", width = 12.5, height = 6)

usdat[nrow(usdat), ]
