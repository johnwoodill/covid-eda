library(tidyverse)
library(zoo)
library(ggrepel)
library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(prophet)
library(scales)
library(cowplot)


setwd("~/Projects/covid-eda/")

# ------------------------------------------------------------
# Figure 1 - Map of Counties
mapdat <- as.data.frame(read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

# Current date of data
current_date <- last(mapdat$date)
current_date

# Get county-level deaths
mapdat1 <- filter(mapdat, date == current_date) %>% dplyr::select(fips, deaths)

# Rename variables for chloroplethr
names(mapdat1) <- c("region", "value")

# Add zero
mapdat1$region <- str_remove(mapdat1$region, "^0+")

# Remove fips with 999
mapdat1 <- mapdat1[-which(substr(mapdat1$region, nchar(mapdat1$region) - 1, nchar(mapdat1$region)) == 99), ]

# Recode to numeric
mapdat1$region <- as.numeric(mapdat1$region)

# Recode variable and set breaks for map
mapdat2 <- mapdat1
mapdat2$value <- cut(mapdat1$value, breaks = c(1, 10, 25, 50, 75, 100, 150, 200, 1000),
                     labels = c("1-10", "10-25", "25-50", "50-75", "75-100", "100-150", "150-200", ">200"))

# Drop NA
mapdat2 <- drop_na(mapdat2)

# Color for maps
cvalues <- brewer.pal(n=9, "Oranges")[2:9]

# PLot map
choro = CountyChoropleth$new(mapdat2)
choro$title = paste0("US Covid-19 County-level Deaths \n Updated ", last(mapdat$date), " - Total Deaths: ", sum(mapdat1$value), "")
choro$set_num_colors(7)
choro$ggplot_scale = scale_fill_manual(values = cvalues, na.value="white", na.translate=FALSE)
choro$render() + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       plot.title = element_text(hjust = 0.5))

ggsave("figures/0-US_County_Death_Map.png", width = 10, height = 10)








# ------------------------------------------------------------
# Figure 1 - Cumulative Number of Deaths since 10th Death

# Non-New-York Data
# nonny <- read_csv("http://covidtracking.com/api/states/daily.csv")
# 
# # Get deaths
# nonny <- dplyr::select(nonny, state, date, death)
# 
# # Filter out NY
# nonny <- filter(nonny, state != "NY")
# 
# # Rename
# names(nonny) <- c("country", "date", "value")
# 
# # Aggregate count
# nonny <- nonny %>%
#   group_by(date) %>%
#   summarise(value = sum(value, na.rm= TRUE)) %>%
#   mutate(country = "US(non-NY)") %>%
#   dplyr::select(country, date, value) %>%
#   ungroup()
# 
# # Recode date
# nonny$date <- as.Date(paste0(substr(nonny$date, 1, 4), "-", substr(nonny$date, 5, 6), "-", substr(nonny$date, 7, 8)))
# nonny$date <- as.Date(as.character(nonny$date), "%Y-%m-%d")
# nonny$date <- format(nonny$date, "%m/%d/%Y")
# 
# # State level data
# cdat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# 
# # Remove lat long
# cdat <- dplyr::select(cdat, -Lat, -Long)
# 
# # Gather by state and region
# cdat <- gather(cdat, key = date, value=value, -`Province/State`, -`Country/Region`)
# 
# # Rename variables
# names(cdat) <- c("state", "country", "date", "value")
# 
# # Keep columns
# cdat <- dplyr::select(cdat, country, date, value)
# 
# # Bind non-Ny and NY
# cdat <- rbind(cdat, nonny)
# 
# # Recode date
# cdat$date <- as.Date(cdat$date, "%m/%d/%y")
# 
# # World data
# ccdat <- cdat %>%
#   filter(value >= 10 & (country %in% c("US", "US(non-NY)", "China", "Italy", "Iran", "Spain", "United Kingdom", "Japan", "Korea, South", "France"))) %>%
#   group_by(country, date) %>%
#   arrange(date) %>%
#   summarise(value = sum(value)) %>%
#   mutate(daily_deaths = value - lag(value),
#          ndays = seq(1, n(), 1),
#          value_rm3 = rollmean(daily_deaths, k = 3, align = "right", na.pad = TRUE)) %>%
#   ungroup()
# 
# # Rename UK
# ccdat$country <- ifelse(ccdat$country == "United Kingdom", "UK", ccdat$country)
# 
# # Log values
# ccdat$value <- log(ccdat$value)
# 
# # Get number of days
# clabels <- ccdat %>%
#   group_by(country) %>%
#   filter(row_number()==n())
# 
# # Recode for colors
# ccdat$cat <- as.character(ifelse(ccdat$country == "US", 1, 0))
# ccdat$cat <- as.character(ifelse(ccdat$country == "US(non-NY)", 2, ccdat$cat))
# 
# ggplot(ccdat, aes(x=ndays, y=value, color=factor(country))) +
#   geom_point(size=0.75) +
#   geom_line() +
#   theme_bw(12) +
#   labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as absolute values)") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
#         legend.position = "none") +
#   # scale_y_continuous(breaks = c(min(ccdat$value), 3, 4, 5, 6, 7, 8, 9, 10, 11, 11.5),
#   #                    labels = round(c(min(exp(ccdat$value)), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8), exp(9), exp(10), exp(11), exp(11.5)), 0),
#   #                    expand=c(0, 0),
#   #                    limits = c(min(ccdat$value), 11.5)) +
#   # scale_x_continuous(breaks = seq(0, 70, 5),
#   #                    expand= c(0,0),
#   #                    limits = c(0, 80)) +
#   scale_color_manual(values=c("US" = "royalblue",
#                               "US(non-NY)" = "royalblue",
#                               "Spain" = "darkgrey",
#                               "Italy"="darkgrey",
#                               "France" = "darkgrey",
#                               "Iran" = "darkgrey",
#                               "Japan" = "darkgrey",
#                               "Korea, South" = "darkgrey",
#                               "UK" = "darkgrey",
#                               "China" = "darkgrey")) +
#   geom_text_repel(data=clabels, aes(label = country),
#           point.padding=unit(1,'lines'),
#           direction = 'x') +
#   NULL
# 
# ggsave("~/Projects/covid-eda/figures/1-World-Rate.png", width = 10, height = 6)










# # ------------------------------------------------------------
# # Figure 2 - Daily Number of Deaths (3-day Right-rolling mean)
# 
# # Difference 3 days
# ccdat$ndays_rm <- ccdat$ndays - 3
# 
# # Drop NA
# ccdat1 <- drop_na(ccdat)
# 
# # Filter out
# ccdat1 <- filter(ccdat1, country != "China")
# 
# unique(ccdat1$country)
# 
# clabels$ndays_rm <- clabels$ndays - 3
# 
# # World Rolling Mean Death Rate
# ggplot(ccdat1, aes(x=ndays_rm, y=(value_rm3), color=factor(country))) + 
#   geom_point(size=0.75) +
#   geom_line() +
#   scale_color_manual(values=c("US" = "royalblue", 
#                               "US(non-NY)" = "royalblue",
#                               "Spain" = "darkgrey", 
#                               "Italy"="darkgrey",
#                               "France" = "darkgrey",
#                               "Iran" = "darkgrey",
#                               "Japan" = "darkgrey",
#                               "Korea, South" = "darkgrey",
#                               "UK" = "darkgrey")) +
#   scale_alpha_manual(values=c("US" = .1, 
#                               "US(non-NY)" = 0.5,
#                               "Spain" = 0.5, 
#                               "Italy"=0.5,
#                               "France" = 0.5,
#                               "Iran" = 0.5,
#                               "Japan" = 0.5,
#                               "Korea, South" = 0.5,
#                               "UK" = 0.5)) +  
#   theme_bw(12) +
#   labs(x="Number of days since 10th Death", y="Daily Number of Deaths \n (3-day Right-rolling mean)") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
#         legend.position = "none") +
#   scale_y_continuous(breaks = seq(0, 6000, 100)) +
#   scale_x_continuous(breaks = seq(0, 75, 5),
#                      expand= c(0,0),
#                      limits = c(0, 75)) +
#   geom_text_repel(data=filter(clabels, country != "China"), aes(label = country),
#           point.padding=unit(1,'lines'),
#           direction = 'x',
#           nudge_x = 1.5,
#           segment.alpha = 0.75) +
#   NULL
# 
# 
# ggsave("~/Projects/covid-eda/figures/2-World-Daily-Death-Rate.png", width = 10, height = 6)








# ------------------------------------------------------------
# Figure 3: US Daily Death Count since 10th Death
ccdat <- as.data.frame(read_csv("http://covidtracking.com/api/states/daily.csv"))

ccdat$date <- as.character(paste0(substr(ccdat$date, 1, 4), "-", substr(ccdat$date, 5, 6), "-", substr(ccdat$date, 7, 8)))

ccdat <- ccdat %>% 
  group_by(date) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative),
            death = sum(death)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(pos_diff = positive - lag(positive),
         neg_diff = negative - lag(negative),
         death_diff = death - lag(death),
         total_tests = pos_diff + neg_diff,
         pos_rate = ( (pos_diff) / (total_tests) ) * 100) %>% 
  mutate(rm_pos_diff = rollmean(pos_diff, k = 7, align = "right", na.pad = TRUE),
         rm_neg_diff = rollmean(neg_diff, k = 7, align = "right", na.pad = TRUE),
         rm_death_diff = rollmean(death_diff, k = 7, align = "right", na.pad = TRUE),
         rm_pos_rate = rollmean(pos_rate, k = 7, align = "right", na.pad = TRUE)) %>% 
  as.data.frame() %>% 
  dplyr::select(date, pos_diff, death_diff, pos_rate, total_tests, rm_pos_diff, rm_neg_diff, rm_death_diff, rm_pos_rate) %>% 
  arrange(date)

# ccdat <- filter(ccdat, death_diff >= 10)

p1 <- ggplot(ccdat, aes(as.POSIXct(date), rm_pos_diff)) + 
  geom_bar(stat="identity", fill="cornflowerblue") +
  geom_text_repel(data = filter(ccdat, (date >= today() - 2)), aes(label=round(rm_pos_diff, 0)), nudge_y = 200, size=3) +
  theme_bw() +
  labs(x=NULL, y="7-Day Average Daily Cases") +
  NULL




p2 <- ggplot(ccdat, aes(as.POSIXct(date), rm_pos_rate)) + 
  geom_bar(stat="identity", fill="darkgreen") +
  geom_text_repel(data = filter(ccdat, (date >= today() - 2)), aes(label=round(rm_pos_rate, 2)), nudge_y = 2, size=3) +
  theme_bw() +
  labs(x=NULL, y="7-Day Average Daily Test Rate (%)") +
  NULL





p3 <- ggplot(ccdat, aes(as.POSIXct(date), rm_death_diff)) + 
  geom_bar(stat="identity", fill="coral") +
  geom_text_repel(data = filter(ccdat, (date >= today() - 2)), aes(label=round(rm_death_diff, 0)), nudge_y = 50, size=3) +
  theme_bw() +
  labs(x=NULL, y="7-Day Average Daily Death") +
  NULL




plot_grid(p1, p2, p3, ncol=1)

ggsave("figures/US_Daily-Cases-Death-Rate_BarChart.png", width = 10, height = 8)








# 
# 
# # ------------------------------------------------------------
# # Figure 4: US Cumulative Number of Deaths since 10th Death
# 
# # County/State Data
# uscdat <- read_csv("http://covidtracking.com/api/states/daily.csv")
# 
# # Recode date
# uscdat$date <- as.Date(paste0(substr(uscdat$date, 1, 4), "-", substr(uscdat$date, 5, 6), "-", substr(uscdat$date, 7, 8)))
# 
# # Get US regions
# regions <- data.frame(state = state.abb, regions = state.region)
# 
# # Aggregate data
# uscdat2 <- uscdat %>% 
#   filter(death >= 10) %>%
#   group_by(date, state) %>% 
#   summarise(death = sum(death, na.rm = TRUE),
#             positive = sum(positive, na.rm = TRUE)) %>% 
#   group_by(state) %>% 
#   arrange(date) %>% 
#   mutate(ndays = seq(1, n(), 1)) %>% 
#   ungroup() %>% 
#   left_join(regions, by="state") %>% 
#   ungroup()
# 
# # Recode regions
# uscdat2$regions <- as.character(uscdat2$regions)
# 
# # Set other regions
# uscdat2$regions <- ifelse(is.na(uscdat2$regions), "Other", uscdat2$regions)
# 
# # Log values
# uscdat2$death <- log(uscdat2$death)
# 
# 
# ggplot(uscdat2, aes(x=ndays, y=death, color=factor(state))) + 
#   geom_point(size=0.75) +
#   geom_line() +
#   theme_bw(12) +
#   labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as real)") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
#         legend.position = "none") +
#   scale_y_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10.5),
#                      labels = round(c(min(exp(uscdat2$death)), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8), exp(9), exp(10), exp(10.5)), 0),
#                      expand=c(0, 0),
#                      limits = c(2, 10.5)) +
#   scale_x_continuous(breaks = seq(0, 75, 5),
#                      expand= c(0,0),
#                      limits = c(0, 75)) +
#   geom_text_repel(data = filter(uscdat2, date == last(uscdat2$date)), aes(label = state),
#           force=1,
#           point.padding=unit(1,'lines'),
#           direction = 'x',
#           nudge_x = 1.5,
#           segment.alpha = 0.75) +
#   facet_wrap(~regions, scales = "free") +
#   NULL
#   
# 
# ggsave("~/Projects/covid-eda/figures/4-US-State-Rate.png", width = 15, height = 10)
# 





# ------------------------------------------------------------
# Figure 5a: US Number of Deaths by State (last 30 days)

# Aggregate data

uscdat <- read_csv("http://covidtracking.com/api/states/daily.csv")
uscdat$date <- as.Date(paste0(substr(uscdat$date, 1, 4), "-", substr(uscdat$date, 5, 6), "-", substr(uscdat$date, 7, 8)))

# # Get US regions
regions <- data.frame(state = state.abb, regions = state.region)

# Aggregate data
uscdat2 <- uscdat %>%
  filter(death >= 10) %>%
  group_by(date, state) %>%
  summarise(death = sum(death, na.rm = TRUE),
            positive = sum(positive, na.rm = TRUE)) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(ndays = seq(1, n(), 1)) %>%
  ungroup() %>%
  left_join(regions, by="state") %>%
  ungroup()

# Recode regions
uscdat2$regions <- as.character(uscdat2$regions)

# Set other regions
uscdat2$regions <- ifelse(is.na(uscdat2$regions), "Other", uscdat2$regions)


uscdat3 <- uscdat2 %>% 
  arrange(date) %>% 
  group_by(state) %>% 
  mutate(daily_death = death - lag(death),
         daily_cases = positive - lag(positive)) %>% 
  arrange(regions, state) %>% 
  ungroup

# Arrange states by region
slabels <- data.frame(state = uscdat3$state, region = uscdat3$regions)

# Remove duplicates
slabels <- distinct(slabels) 

uscdat3$state <- factor(uscdat3$state, levels = slabels$state)

# Reset data
uscdat4 <- uscdat3

# Keep last 30 days
uscdat4 <- filter(uscdat4, date > today() - 30)
uscdat4 <- uscdat4 %>% 
  group_by(state) %>% 
  mutate(ndays = ndays - min(ndays)) %>% 
  ungroup()


ggplot(uscdat4, aes(ndays, daily_death, fill=regions)) + 
  geom_bar(stat="identity") + 
    theme_bw(12) +
  facet_wrap(~state, scales = 'free', ncol = 5) +
  labs(x="30-Days", y = "Deaths") +
  geom_smooth(se = FALSE, size=0.5) +
  NULL


ggsave("figures/5-US-State-Death-Dist_30.png", width = 15, height = 20)







ggplot(uscdat4, aes(ndays, daily_cases, fill=regions)) + 
  geom_bar(stat="identity") + 
    theme_bw(12) +
  facet_wrap(~state, scales = 'free', ncol = 5) +
  labs(x="Days", y = "Daily Cases") +
  geom_smooth(se = FALSE, size=0.5) +
  NULL

ggsave("figures/5-US-State-Cases-Dist_30.png", width = 15, height = 20)


# ------------------------------------------------------------
# Figure 5b: US Number of Deaths by Region (last 30 days)

# Aggregate data
uscdat5 <- uscdat4 %>% 
  group_by(date, regions) %>% 
  summarise(daily_death = sum(daily_death, na.rm = TRUE),
            daily_cases = sum(daily_cases, na.rm = TRUE))


ggplot(uscdat5, aes(date, daily_death, fill=regions)) + 
  geom_bar(stat="identity") + 
    theme_bw(12) +
  facet_wrap(~regions, scales = 'free') +
  labs(x="30-Days", y = "Daily Deaths") +
  theme(legend.position = "none") +
  geom_smooth(se = FALSE, size=0.5) +
  NULL

ggsave("figures/5-US-Region-Death-Dist_30.png", width = 18, height = 10)


# ------------------------------------------------------------
# Figure 5c: US Number of Cases by Region (last 30 days)


ggplot(uscdat5, aes(date, daily_cases, fill=regions)) + 
  geom_bar(stat="identity") + 
  theme_bw(12) +
  facet_wrap(~regions, scales = 'free') +
  labs(x="30-Days", y = "Daily Cases") +
  theme(legend.position = "none") +
  geom_smooth(se = FALSE, size=0.5) +
  NULL

ggsave("figures/5-US-Region-Cases-Dist_30.png", width = 18, height = 10)


# 
# # ------------------------------------------------------------
# # Figure 6: US Mortality Multiplier
# 
# usdat <- filter(cdat, country == "US")
# names(usdat)[3] <- "death"
# 
# # Recode date
# usdat$date <- as.Date(usdat$date, "%m/%d/%y")
# 
# # Aggregate data
# usdat <- usdat %>% 
#   filter(death > 0) %>% 
#   group_by(date) %>% 
#   summarise(death = sum((death))) %>% 
#   arrange(date) %>% 
#   ungroup
# 
# # Rollmean
# usdat$rm2 <- rollmean(usdat$death, k = 2, na.pad = TRUE, align = "right")
# usdat$rm3 <- rollmean(usdat$death, k = 3, na.pad = TRUE, align = "right")
# usdat$rm4 <- rollmean(usdat$death, k = 4, na.pad = TRUE, align = "right")
# 
# # Recode variables
# usdat$`R-RM1` <- (usdat$death - lag(usdat$death))/(lag(usdat$death))
# usdat$`R-RM2` <- (usdat$rm2 - lag(usdat$rm2))/(lag(usdat$rm2))
# usdat$`R-RM3` <- (usdat$rm3 - lag(usdat$rm3))/(lag(usdat$rm3))
# 
# # Gather data for plotting
# usdat2 <- select(usdat, -death, -rm2, -rm3, -rm4) %>% 
#   gather(key=rm, value=death, -date)
# 
# # Get labels
# labelss <- usdat[nrow(usdat), ]
# labelss <- select(labelss, -death, -rm2, -rm3, -rm4) %>% 
#   gather(key=rm, value=death, -date)
# labelss$label <- paste0(labelss$rm, ": ", 1 + round(labelss$death, 2), "x")
# 
# 
# ggplot(filter(usdat2, date >= as.Date("2020-03-03")), aes(date, death*100, color=factor(rm))) + 
#   geom_smooth(aes(date, death*100, group=1)) +
#   geom_point() + 
#   geom_line() +
#   theme_bw() +
#   scale_y_continuous(labels=function(x) paste0((1 + x/100), "x"), 
#                      breaks = seq(-100, 100, 25),
#                      limits = c(-99, 99)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   scale_x_date(date_labels = '%m/%d', 
#                limits = c(as.Date("2020-03-03"), max(usdat$date) + 2), 
#                breaks = seq(min(usdat$date), max(usdat$date) + 3, "day")) +
#   labs(x=NULL, y="US Mortality Multiplier \n (Rolling Mean Windows 1-3)") +
#   theme(legend.position = "none") +
#   # geom_text(data=labelss, aes(date, death, label=label)) +
#   geom_text_repel(data = labelss,
#           aes(label = label),
#           force=1,
#           point.padding=unit(1,'lines'),
#           direction = 'y',
#           nudge_x = 1.5,
#           segment.size=0.1,
#           xlim = c(max(usdat$date) + 2.5)) +
#   NULL
#   
# ggsave("~/Projects/covid-eda/figures/6-US-Mortality-Multiplier.png", width = 12.5, height = 6)








# ------------------------------------------------------------
# Figure 7: Model Predictions


# State level dat
dat <- read_csv("http://covidtracking.com/api/states/daily.csv")
dat <- dplyr::select(dat, state, date, death)
dat$date <- as.Date(paste0(substr(dat$date, 1, 4), "-", substr(dat$date, 5, 6), "-", substr(dat$date, 7, 8)))
dat$date <- as.Date(as.character(dat$date), "%Y-%m-%d")


dat1 <- dat %>% 
  group_by(date) %>% 
  summarise(y = sum(death, na.rm=TRUE))

dat1$cap <- 100000

names(dat1) <- c("ds", "y", "cap")

mod = prophet(dat1, growth='logistic')

# Predictions
future <- make_future_dataframe(mod, periods = 100)
future$cap <- 100000
fcst <- predict(mod, future)

# Density estimate
dss <- fcst$ds[1:(nrow(fcst) - 1)]
dss <- substr(dss, 1, nchar(dss))
dss <- as.Date.POSIXct(dss, format = "%Y-%m-%d", tz="")

diff_yhat <- diff(fcst$yhat)

ndat <- read_csv("http://covidtracking.com/api/states/daily.csv")

# Recode date
ndat$date <- as.Date(paste0(substr(ndat$date, 1, 4), "-", substr(ndat$date, 5, 6), "-", substr(ndat$date, 7, 8)))

ndat1 <- ndat %>% 
  group_by(date) %>% 
  summarise(death = sum(death, na.rm = TRUE)) %>% 
  mutate(lag_death = death - lag(death),
         rm_death = rollmean(lag_death, align="right", k=7, na.pad = TRUE)) %>% 
  ungroup


ggplot(NULL, aes(dss, diff_yhat)) + 
  # geom_bar(stat="identity", alpha=0.5) +
  geom_bar(data = ndat1, aes(date, rm_death), stat="identity", fill="cornflowerblue", alpha = 0.75) +
  geom_bar(stat="identity", alpha=0.5) +
  geom_vline(xintercept = today(), color="red") +
  theme_bw() +
  annotate("text", x = as.Date("2020-03-20"), y = 2200, label = "Predicted", color="darkgrey", size=6) +
  annotate("text", x = as.Date("2020-03-17"), y = 1500, label = "Actual", color="cornflowerblue", size=6) +
  annotate("text", x = today() + 10, y = 2800, label = "Today", color="red", size = 4) +
  labs(x=NULL, y="US Deaths \n (7-Day Rolling Average)") +
  scale_y_continuous(breaks = seq(0, 2700, 200), limits = c(0, 2600)) +
  scale_x_date(date_breaks="months", date_labels="%b") +
  NULL

ggsave("figures/7-Model_predictions_distr.png", width = 6, height = 4)





# ------------------------------------------------------------
# Figure 8: Percent positive tests by state

# State level dat
dat_raw <- as.data.frame(read_csv("http://covidtracking.com/api/v1/states/daily.csv"))

dat_per <- dat_raw %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  mutate(total_test = positive + negative) %>%
  group_by(state, date) %>%
  summarise(positive = sum(positive, na.rm = TRUE),
            total_test = sum(total_test, na.rm = TRUE)) %>%
  mutate(per_pos = (positive / total_test) * 100) %>%
  mutate(per_pos_7 = rollmean(per_pos, k = 7, align = "right", na.pad = TRUE)) %>%
  # Testing before april 15 was sporadic)
  filter(date > mdy("04-15-2020")) %>% 
  ungroup() %>% 
  as.data.frame()

# Code states into 3 risk groups
dat_risk <- dat_per %>%
  group_by(state) %>%
  summarise(curr_per = per_pos_7[which.max(date)],
            last_per = per_pos_7[which.max(date) - 1]) %>%
  mutate(ch_per = (curr_per - last_per) / last_per) %>%
  mutate(risk = if_else(ch_per < -0.004, 1,
                        if_else(ch_per >= -0.004 & ch_per < 0.004, 2, 3))) %>%
  dplyr::select(state, risk) %>% 
  ungroup()

dat_per <- dat_per %>%
  left_join(dat_risk, by = "state") %>%
  drop_na(risk)

ggplot(data = dat_per) +
  geom_bar(aes(date, per_pos, fill = as.factor(risk)), stat = "identity", show.legend = F) +
  scale_fill_manual(values = c("darkgreen", "gold", "red")) +
  facet_wrap(~state, scales = 'free', ncol = 5) +
  theme_bw() +
  labs(x = "Date", y = "Pecent of tests positive (%)")

ggsave("figures/8-Percent-positive-tests-state.png", width = 15, height = 20)








#### Figure of growth rates
dat_per2 <- left_join(dat_per, regions, by='state')
dat_per2 <- drop_na(dat_per2)

## Get last 30 days
dat_per2 <- filter(dat_per2, date >= today() - 30)

## Get most at risk states
dat_per2 <- filter(dat_per2, risk == 3)

ggplot(dat_per2, aes(date, per_pos, color=state)) + 
  geom_line() + 
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
       legend.position = "none") +
  scale_color_viridis_d() +
  geom_text_repel(data = filter(dat_per2, date == last(dat_per$date) & risk == 3), aes(label=state)) +
  labs(x=NULL, y="Pecent of tests positive (%)") +
  facet_wrap(~regions) +
  NULL

ggsave("figures/9-Percent-positive-tests-region-risk3.png", width=12, height=6)




filter(ndat, date >= first(ndat$date) - 30) %>% 
  group_by(date) %>% 
  summarise(positive = sum(positive),
            negative = sum(negative),
            death = sum(death)) %>% 
  ungroup() %>% 
  mutate(pos_diff = positive - lag(positive),
         neg_diff = negative - lag(negative),
         death_diff = death - lag(death),
         pos_rate = (pos_diff / (pos_diff + neg_diff) ) * 100) %>% 
  mutate(rm_pos_diff = rollmean(pos_diff, k = 7, align = "right", na.pad = TRUE),
         rm_death_diff = rollmean(death_diff, k = 7, align = "right", na.pad = TRUE),
         rm_pos_rate = rollmean(pos_rate, k = 7, align = "right", na.pad = TRUE)) %>% 
  as.data.frame() %>% 
  dplyr::select(date, pos_diff, death_diff, pos_rate, rm_pos_diff, rm_death_diff, rm_pos_rate) %>% 
  drop_na() %>% 
  tail(14)
  

