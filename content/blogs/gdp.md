---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: growth.jpg
keywords: ""
slug: gdp
title: Do we report GDP accurately?
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
```


# Challenge 3:GDP components over time and among countries

At the risk of oversimplifying things, the main components of gross domestic product, GDP are personal consumption (C), business investment (I), government spending (G) and net exports (exports - imports). You can read more about GDP and the different approaches in calculating at the [Wikipedia GDP page](https://en.wikipedia.org/wiki/Gross_domestic_product).

The GDP data we will look at is from the [United Nations' National Accounts Main Aggregates Database](https://unstats.un.org/unsd/snaama/Downloads), which contains estimates of total GDP and its components for all countries from 1970 to today. We will look at how GDP and its components have changed over time, and compare different countries and how much each component contributes to that country's GDP. The file we will work with is [GDP and its breakdown at constant 2010 prices in US Dollars](http://unstats.un.org/unsd/amaapi/api/file/6) and it has already been saved in the Data directory. Have a look at the Excel file to see how it is structured and organised


```{r read_GDP_data}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip
glimpse(UN_GDP_data)
```

The first thing you need to do is to tidy the data, as it is in wide format and you must make it into long, tidy format. Please express all figures in billions (divide values by `1e9`, or $10^9$), and you want to rename the indicators into something shorter.


```{r reshape_GDP_data}

# cleaning data. Making long grouping years into the same column
tidy_GDP_data = UN_GDP_data %>%
  select(1:51) %>% 
      pivot_longer(cols=4:51,
      names_to="year",
      values_to = "value")

# changing value to Billions of USD
tidy_GDP_data = tidy_GDP_data%>%
  mutate(value = value / 1000000000)

glimpse(tidy_GDP_data)


# Let us compare GDP components for these 3 countries
country_list <- c("United States","India", "Germany")
```

First, can you produce this plot?


```{r gdp1, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp1.png"), error = FALSE)
```

```{r}
# List of indicators of interest to replicate the graph. These are stored under the Indicator name column
indicator_list = c("Gross capital formation", 
                   "Exports of goods and services", 
                   "General government final consumption expenditure", 
                   "Household consumption expenditure (including Non-profit institutions serving households)", 
                   "Imports of goods and services")

tidy_GDP_data %>%
  # filter by country and indicator name using both lists
  filter(Country %in% country_list)%>%
  filter(IndicatorName %in% indicator_list)%>%
  # grouping by indicator
  group_by(IndicatorName)%>%
  # plotting
  ggplot(aes(x=year, y = value, color=IndicatorName, group=IndicatorName))+
  geom_line(aes(x=year, y = value, color=IndicatorName))+
  facet_wrap(~ Country)+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.direction="vertical")+
  scale_x_discrete(breaks=seq(1970, 2017, 10))+
  labs(title = "GDP Components over time",
         subtitle = "In constant 2010 USD",
         x = "", 
         y = "Billion US$")+
  scale_shape_discrete(
    limits = c(
      "Gross capital formation", 
      "Exports of goods and services", 
      "General government final consumption expenditure", 
      "Household consumption expenditure (including Non-profit institutions serving households)", 
      "Imports of goods and services"),
    labels = c(
      "Gross capital formation", 
      "Exports", 
      "Government expenditure",
      "Household expenditure", 
      "Imports")) +
  NULL
```


Secondly, recall that GDP is the sum of Household Expenditure (Consumption *C*), Gross Capital Formation (business investment *I*), Government Expenditure (G) and Net Exports (exports - imports). Even though there is an indicator `Gross Domestic Product (GDP)` in your dataframe, I would like you to calculate it given its components discussed above.

```{r}
# changing tidy_data to wide. Degrouping Indicator names to allow for easier calculations between these. 
UN_GDP_estimation = tidy_GDP_data%>%
  select(1:5)%>%
  pivot_wider(
    names_from = IndicatorName, 
    values_from = value
  )

# Creation of new column, expected_GDP, which is the result of the euquation provided above. 
UN_GDP_estimation = UN_GDP_estimation %>%
  mutate(expected_GDP = 
           UN_GDP_estimation$`Household consumption expenditure (including Non-profit institutions serving households)`+
           UN_GDP_estimation$`Gross capital formation`+
           UN_GDP_estimation$`General government final consumption expenditure`+
           UN_GDP_estimation$`Exports of goods and services`-
           UN_GDP_estimation$`Imports of goods and services`)

# Creation of new column, percentage deviation, which is the percentage deviation between the expected_GDP column, and the GDP column reported
UN_GDP_estimation = UN_GDP_estimation %>%
  mutate(percentage_deviation = ((expected_GDP/UN_GDP_estimation$`Gross Domestic Product (GDP)`)-1)*100)

# Plot
UN_GDP_estimation %>%
  filter(Country %in% country_list)%>%
  ggplot(aes(x=year, y=percentage_deviation))+
  geom_line(group=1, size = 0.8)+
  geom_line(group=1, y=0, size = 0.8)+
  facet_wrap(~ Country)+
  theme_bw()+
  theme(legend.position="none")+
  scale_x_discrete(breaks=seq(1970, 2017, 10))+
  geom_ribbon(aes(ymin = 0, ymax = pmin(0, percentage_deviation), group=1),fill = "red", alpha=0.2) +
  geom_ribbon(aes(ymin = percentage_deviation, ymax = pmin(0, percentage_deviation), group=1),fill = "green", alpha=0.2)+
  labs(title = "Deviation of Sum of GDP Components vs Reported GDP",
         subtitle = "Percentage Deviation",
         x = "Year", 
         y = "Percentage Deviation", 
         caption = "Green: Higher Calculated GDP than reported GDP \
         Red: Lower Calculated GDP than reported GDP")+
  NULL

# This code can be used to see the percentage deviation by country and year. It is commented out so it doesn't appear on the knitted file. 
#UN_GDP_estimation%>%
#  filter(Country %in% country_list)%>%
#  select("Country", "year", "percentage_deviation")
```


> What is the % difference between what you calculated as GDP and the GDP figure included in the dataframe?

For both Germany and the US, the calculated GDP was higher than the reported GDP from the 1970's to the 2000. This difference was highest in the 70's, of around a 4%, and has steadily decreased over time. Since the 2000's, both countries report a GDP that is consistent with the sum of its components, thus having an almost 0% deviation. 

India on the other hand still has fluctuations on the percentage difference between the reported and calculated GDP. From 1970 until 1990, it reported a lower GDP than its calculated, peaking at a difference of 7.41% in 1979. However, from 1990 until this day, India reports a higher GDP than the sum of its components, except two exceptions in 2007 and 2010. In 2017, the last datapoint available, India's reported GDP was 2% higher than its calculated.

```{r gdp2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp2.png"), error = FALSE)
```



> What is this last chart telling you? Can you explain in a couple of paragraphs the different dynamic among these three countries?

In Germany, during the past years, the proportion of their GDP attributed to net exports has increased, while the proportions of all other elements of GDP have decreased. This might be a result of Germany's strong exports in industries such as automobile, driving the economy of the country. The US has seen a steady increase in the proportion of household spending, reducing government expenditure. Lastly, India has seen a sharp increase in the proportion of gross capital formation, with a decrease in household expenditure. This might suggest an entrepreneurial trend among Indians, who prefer to invest capital than to spend it. 

Furthermore, household income is the largest contributor of GDP in all countries, while net exports is the lower. Government expenditure and gross capital formation represent a similar proportion in the US and Germany, while in India gross capital formation appears in a larger proportion. 

> If you want to, please change `country_list <- c("United States","India", "Germany")` to include your own country and compare it with any two other countries you like