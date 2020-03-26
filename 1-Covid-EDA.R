library(tidyverse)
library(zoo)
library(ggrepel)

cdat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
cdat <- select(cdat, -Lat, -Long)

cdat <- gather(cdat, key = date, value=value, -`Province/State`, -`Country/Region`)
names(cdat) <- c("state", "country", "date", "value")

cdat$date <- as.Date(cdat$date, "%m/%d/%y")

# World data -----------------------------------------------------
ccdat <- cdat %>% 
  filter(value >= 10 & (country %in% c("US", "China", "Italy", "Iran", "Spain", "UK", "Japan", "Korea", "France"))) %>% 
  group_by(country, date) %>% 
  arrange(date) %>% 
  summarise(value = sum((value))) %>%
  mutate(ndays = seq(1, n(), 1)) %>% 
  ungroup()
ccdat

ccdat$value <- log(ccdat$value)

clabels <- ccdat %>% 
  group_by(country) %>% 
  filter(row_number()==n()) 
  # mutate(value = exp(value))
clabels
  


ggplot(ccdat, aes(x=ndays, y=value, color=factor(country))) + 
  geom_point(size=0.75) +
  geom_line() +
  theme_bw(12) +
  labs(x="Number of days since 10th Death", y="Cumulative Number of Deaths \n (Expressed in logs, displayed as real)") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1),
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
  # coord_trans(y = "exp") +
  NULL

ggsave("~/Projects/covid-eda/figures/1-World-Rate.png", width = 10, height = 6)
#




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

ggplot(usdat, aes(date, log(value), group=1)) + geom_line()

usdat$rm2 <- rollmean(usdat$value, k = 2, na.pad = TRUE, align = "right")
usdat$rm3 <- rollmean(usdat$value, k = 3, na.pad = TRUE, align = "right")
usdat$rm4 <- rollmean(usdat$value, k = 4, na.pad = TRUE, align = "right")
usdat

usdat$RM1 <- (usdat$value - lag(usdat$value))/(lag(usdat$value))
usdat$RM2 <- (usdat$rm2 - lag(usdat$rm2))/(lag(usdat$rm2))
usdat$RM3 <- (usdat$rm3 - lag(usdat$rm3))/(lag(usdat$rm3))

p1 <- ggplot(filter(usdat, date >= as.Date("2020-03-03")), aes(date, pc)) + 
  geom_point() + 
  geom_line() +
  NULL

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
          xlim = c(max(usdat$date) + 1.5)) +
  NULL
  
ggsave("~/Projects/covid-eda/figures/2-US-Mortality-Multiplier.png", width = 12.5, height = 6)

usdat[nrow(usdat), ]

usdat3 <- usdat2 %>% 
  group_by(rm) %>% 
  arrange(date) %>% 
  mutate(diff = value - lag(value))

ggplot(filter(usdat3, date >= as.Date("2020-03-05")), aes(date, diff, color=factor(rm))) + 
  geom_point() + 
  geom_line() +
  theme_bw(12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_labels = '%m/%d', 
               limits = c(as.Date("2020-03-05"), max(usdat$date) + 2), 
               breaks = seq(min(usdat$date), max(usdat$date), "day")) +
  labs(x=NULL, y="Difference in % Change") +
  theme(legend.position = "top") +
  NULL

usdat[nrow(usdat), ]

# Regression analysis

regdat <- cdat
regdat <- regdat %>% 
  filter(value > 0 & country == "US") %>% 
  arrange(date) %>% 
  ungroup

regdat <- usdat
regdat$date <- as.Date(regdat$date, "%m/%d/%y")
regdat$trend <- day(regdat$date)
regdat$trend_week <- week(regdat$date)
regdat$lag_value <- lag(regdat$value)
regdat

mod <- lm(log(value) ~ trend + I(trend^2), data = regdat)
summary(mod)
sum(mod$coefficients[2:3])*tail(regdat$trend, 1)

plot(exp(predict(mod)))

mod2 <- lm(log(value) ~ factor(trend_week), data = regdat)
summary(mod2)

sum(mod$coefficients)*tail(regdat$trend, 1)
