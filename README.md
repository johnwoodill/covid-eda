## Coronavirus EDA

Analysis of the coronavirus pandemic inspired by the great data visualiziation by John Burn-Murdoch at the Financial Times (https://www.ft.com/coronavirus-latest)

----------------------------
**Map 1: Total US Covid-19 County-level Deaths**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/0-US_County_Death_Map.png?raw=true">

------------------------------------


**Figure 1: Cumulative Number of Deaths since 10th Death**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/1-World-Rate.png?raw=true">

Figure provides the cumulative number of deaths since the 10th death in each country. The x-axis is the number of days since the 10th death and the y-axis is the real deaths for each country. The observations in the plot are expressed in log_10, which measures the exponential growth in the absolute values (linear increases in log_10 represent exponential growth of the absolute values). This allows for a comparative analysis across countries. We do not need population data when using log because the log of a fraction is simply the difference of the denominator (log(x/y) = log(x) - log(y) where log(y) is the constant population).

----------------------------

**Figure 2: Daily Death Rate since 10th Death**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/2-World-Daily-Death-Rate.png?raw=true">

Figure provides the daily number of deaths since the 10th death in each country. The x-axis is the number of days since the 10th death and the y-axis is the real daily deaths for each country. Values are calculated using a 3-day "right" rolling mean window. 

----------------------------

**Figure 3: US Daily Death Count since 10th Death**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/3-US_Daily-Death-Rate_BarChart.png?raw=true">


----------------------------


**Figure 4: US Cumulative Number of Deaths since 10th Death**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/4-US-State-Rate.png?raw=true">

Figure provides US cumulative number of deaths since the 10th death grouped by US regional states. The x-axis is the number of days since the 10th death and the y-axis is the real deaths for each country. The observations in the plot are expressed in log_10, which measures the exponential growth in the absolute values (linear increases in log_10 represent exponential growth of the absolute values).


----------------------------
**Figure 5: US Cumulative Number of Deaths since 10th Death by State**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/5-US-State-Death-Dist.png?raw=true">





----------------------------

**Figure 6: US Mortality Multiplier**

<p align="center">

<img align="center" width="800" src="https://github.com/johnwoodill/covid-eda/raw/master/figures/6-US-Mortality-Multiplier.png?raw=true">

Figure provides the US mortality multiplier across various right rolling mean windows. The multiplier rate describes how the death rate is increasing from the previous `n` days. A multiplier rate of 1.33 means deaths are doubling every 3 days. A loess trend line is fit through the data represented as a blue line with marginal error in grey.



