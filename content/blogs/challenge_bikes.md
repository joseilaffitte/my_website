---
title: Look at this beautiful code
date: '2017-10-31T22:42:51-05:00'
description: Nullam et orci eu lorem consdequat tincidunt vivamus et sagittis magna
  sed nunc rhoncus condifdmentum vcvcsem. In efficitur ligula tate urna. Maecenas
  massa sed magna lacinia magna pelldfentesque lorem ipsum dolor. Nullam et orci eu
  lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: no
image: bikes.jpg
keywords: ''
slug: challenge_bikes
categories:
- ''
- ''
---


```{r, setup, include=FALSE}
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


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)

```


# Challenge 1: Excess rentals in TfL bike sharing

Recall the TfL data on how many bikes were hired every single day. We can get the latest data by running the following

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"
library(disk.frame)
# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))
```

We can easily create a facet grid that plots bikes hired by month and year.

```{r tfl_month_year_grid, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_distributions_monthly.png"), error = FALSE)
```

Look at May and Jun and compare 2020 with the previous years. What's happening?

> It becomes clear that the effect of national restrictive measures such as lockdowns have greatly influenced the bikes rent. This helps us explain the difference between the average number of rented bikes in May and June 2020 with previous years.

```{r}
actual_bike <- bike %>%
               filter (year >= 2016) %>%
               group_by(year,month) %>%
               summarise(actual = mean(bikes_hired))

expected_bike <- actual_bike %>%
                 group_by(month) %>%
                 summarise(expected = mean(actual))

comparison_bike <- left_join(actual_bike, expected_bike, by = "month")

comparison_bike
```


```{r, tfl_absolute_monthly_change, out.width="100%"}
comparison_bike %>%
  ggplot(aes(x = month, group = 1)) +
  geom_line(aes(x = month, y = actual), color = "black", size = 0.1) +
  geom_line(aes(x = month, y = expected), color = "blue", size = 0.8) +
  geom_ribbon(aes(ymin = expected, ymax = pmin(expected, actual)),fill = "red", alpha=0.2)  +
  geom_ribbon(aes(ymin = actual, ymax = pmin(expected, actual)),fill = "green", alpha=0.2)+
  facet_wrap(~ year) +
  theme_bw()+
  labs(
    title= "Montly changes in Tfl bikes rentals",
    y="bike rentals",
    x="Months"
  )
```

```{r}
actual_bike_w <- bike %>%
               filter (year >= 2016) %>%
               group_by(year, week) %>%
               summarise(actual = mean(bikes_hired))

expected_bike_w <- actual_bike_w %>%
                 group_by(week) %>%
                 summarise(expected = mean(actual))

comparison_bike_w <- left_join(actual_bike_w, expected_bike_w, by = "week") %>%
                     group_by(week) %>%
                     mutate(dchanges = (actual - expected) / expected )

comparison_bike_w = comparison_bike_w %>%
  filter(!(year ==2021 & week ==53))

```
```{r tfl_absolute_monthly_changemj, out.width="100%"}
comparison_bike_w %>%
  ggplot(aes(x = week, group = 1)) +
  geom_line(aes(x = week, y = dchanges, fill = "black")) +
  geom_ribbon(aes(ymin = 0, ymax = pmin(0, dchanges)),fill = "red", alpha=0.2)  +
  geom_ribbon(aes(ymin = dchanges, ymax = pmin(0, dchanges)),fill = "green", alpha=0.2)+
  facet_wrap(~ year) +
  theme_bw()+
  labs(
    title= "Weekly changes in Tfl bikes rentals",
    y= "Bikes rentals",
    x="Weeks"
  )
```

Should you use the mean or the median to calculate your expected rentals? Why?
> In order to calculate the expected rentals we used the mean of rented bikes/montly since we thought this was a better measurement. Since the monthly data of the actual rented bikes does not seem to be heavily right/left skewed, the mean is a good tool to calcukate the expected rentals. If the data were heavily skewed, we would have changed to the median.

