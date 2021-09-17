---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: Nullam et orci eu lorem consdequat tincidunt vivamus et sagittis magna
  sed nunc rhoncus condifdmentum vcvcsem. In efficitur ligula tate urna. Maecenas massa
  sed magna lacinia magna pelldfentesque lorem ipsum dolor. Nullam et orci eu lorem
  consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: picjose.jpg
keywords: ""
slug: homework
title: "Look at this beautiful code"
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



# Climate change and temperature anomalies 


If we wanted to study climate change, we can find data on the *Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies* in the Northern Hemisphere at [NASA's Goddard Institute for Space Studies](https://data.giss.nasa.gov/gistemp). The [tabular data of temperature anomalies can be found here](https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt)

To define temperature anomalies you need to have a reference, or base, period which NASA clearly states that it is the period between 1951-1980.

Run the code below to load the file:

```{r weather_data, cache=TRUE}
#cache=TRUE, I have to go on the internet and download it the first time, but then i do not have to do it anymore !! 
weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

```

Notice that, when using this function, we added two options: `skip` and `na`.

1. The `skip=1` option is there as the real data table only starts in Row 2, so we need to skip one row. 
1. `na = "***"` option informs R how missing observations in the spreadsheet are coded. When looking at the spreadsheet, you can see that missing data is coded as "***". It is best to specify this here, as otherwise some of the data is not recognized as numeric data.

Once the data is loaded, notice that there is a object titled `weather` in the `Environment` panel. If you cannot see the panel (usually on the top-right), go to `Tools` > `Global Options` > `Pane Layout` and tick the checkbox next to `Environment`. Click on the `weather` object, and the dataframe will pop up on a seperate tab. Inspect the dataframe.

For each month and year, the dataframe shows the deviation of temperature from the normal (expected). Further the dataframe is in wide format. 

You have two objectives in this section:

1. Select the year and the twelve month variables from the `weather` dataset. We do not need the others (J-D, D-N, DJF, etc.) for this assignment. Hint: use `select()` function.

```{r}
weather %>% 
  select(1:13) 
```


1. Convert the dataframe from wide to 'long' format. Hint: use `gather()` or `pivot_longer()` function. Name the new dataframe as `tidyweather`, name the variable containing the name of the month as `month`, and the temperature deviation values as `delta`.


```{r tidyweather}
tidyweather <- weather %>% 
             select(1:13) %>% 
            pivot_longer(cols=2:13,
             names_to="Month",
             values_to = "delta")
tidyweather

```

Inspect your dataframe. It should have three variables now, one each for 

1. year, 
1. month, and 
1. delta, or temperature deviation.

## Plotting Information

Let us plot the data using a time-series scatter plot, and add a trendline. To do that, we first need to create a new variable called `date` in order to ensure that the `delta` values are plot chronologically. 


> In the following chunk of code, I used the `eval=FALSE` argument, which does not run a chunk of code; I did so that you can knit the document before tidying the data and creating a new dataframe `tidyweather`. When you actually want to run this code and knit your document, you must delete `eval=FALSE`, **not just here but in all chunks were `eval=FALSE` appears.**


```{r scatter_plot} 
#this tells us not to run the code, when knitting we have to delete it

tidyweather <- tidyweather %>%
  mutate(date = ymd(paste(as.character(Year), Month, "1")),
         month = month(date, label=TRUE),
         year = year(date))

ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  labs (
    title = "Weather Anomalies"
  )

```

Is the effect of increasing temperature more pronounced in some months? Use `facet_wrap()` to produce a seperate scatter plot for each month, again with a smoothing line. Your chart should human-readable labels; that is, each month should be labeled "Jan", "Feb", "Mar" (full or abbreviated month names are fine), not `1`, `2`, `3`. 

```{r facet_wrap, echo=FALSE}

tidyweather <- tidyweather %>%
  mutate(date = ymd(paste(as.character(Year), Month, "1")),
         month = month(date, label=TRUE),
         year = year(date))

ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  facet_wrap("Month")+
  theme_bw() +
  labs (
    title = "Weather Anomalies"
  )

```


It is sometimes useful to group data into different time periods to study historical data. For example, we often refer to decades such as 1970s, 1980s, 1990s etc. to refer to a period of time. NASA calculaltes a temperature anomaly, as difference form the base period of 1951-1980. The code below creates a new data frame called `comparison` that groups data in five time periods: 1881-1920, 1921-1950, 1951-1980, 1981-2010 and 2011-present. 

We remove data before 1800 and before using `filter`. Then, we use the `mutate` function to create a new variable `interval` which contains information on which period each observation belongs to. We can assign the different periods using `case_when()`.


```{r intervals}

comparison <- tidyweather %>% 
  filter(Year>= 1881) %>%     #remove years prior to 1881
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    Year %in% c(1881:1920) ~ "1881-1920",
    Year %in% c(1921:1950) ~ "1921-1950",
    Year %in% c(1951:1980) ~ "1951-1980",
    Year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))
comparison
```

Inspect the `comparison` dataframe by clicking on it in the `Environment` pane.

Now that we have the `interval` variable, we can create a density plot to study the distribution of monthly deviations (`delta`), grouped by the different time periods we are interested in. Set `fill` to `interval` to group and colour the data by different time periods.

```{r density_plot}

ggplot(comparison, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with tranparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density"         #changing y-axis label to sentence case
  )

```

So far, we have been working with monthly anomalies. However, we might be interested in average annual anomalies. We can do this by using `group_by()` and `summarise()`, followed by a scatter plot to display the result. 

```{r averaging}

#creating yearly averages
average_annual_anomaly <- tidyweather %>% 
  group_by(Year) %>%   #grouping data by Year
  
  # creating summaries for mean delta 
  # use `na.rm=TRUE` to eliminate NA (not available) values 
   summarise(annual_average_delta = mean(delta),  na.rm = TRUE) 

#plotting the data:
ggplot(average_annual_anomaly, aes(x=Year, y= annual_average_delta))+
  geom_point()+
  
  #Fit the best fit line, using LOESS method
  geom_smooth() +
  
  #change to theme_bw() to have white background + black frame around plot
  theme_bw() +
  labs (
    title = "Average Yearly Anomaly",
    y     = "Average Annual Delta"
  )                         


```


## Confidence Interval for `delta`

[NASA points out on their website](https://earthobservatory.nasa.gov/world-of-change/decadaltemp.php) that 

> A one-degree global change is significant because it takes a vast amount of heat to warm all the oceans, atmosphere, and land by that much. In the past, a one- to two-degree drop was all it took to plunge the Earth into the Little Ice Age.

Your task is to construct a confidence interval for the average annual delta since 2011, both using a formula and using a bootstrap simulation with the `infer` package. Recall that the dataframe `comparison` has already grouped temperature anomalies according to time intervals; we are only interested in what is happening  between 2011-present.

```{r, calculate_CI_using_formula}

formula_ci <- comparison %>% 
  # choose the interval 2011-present
  filter(Year >=2011)%>% 
  # what dplyr verb will you use?
  # calculate summary statistics for temperature deviation (delta) 
  summarise(mean = mean(delta,  na.rm = TRUE), SD = sd(delta, na.rm = TRUE), count=n(),
  t_critical = qt(0.975, count-1), 
  SE = SD/sqrt(count),
  margin_error = t_critical * SE,
  lower = mean - margin_error,
  upper = mean + margin_error)
  # calculate mean, SD, count, SE, lower/upper 95% CI
  # what dplyr verb will you use? 

#print out formula_CI
formula_ci

```



```{r, calculate_CI_using_bootstrap}
set.seed(12345)
boot_weather <- comparison %>%
  specify(response =delta ) %>%
  generate (reps = 1000, type = "bootstrap") %>% 
  calculate(stat = c("mean"))

boot_weather_CI <- boot_weather %>%
  get_confidence_interval(level=0.95, type = "percentile")
boot_weather_CI

formula_ci %>%
  select(lower,upper)

boot_weather_CI

# use the infer package to construct a 95% CI for delta
#try to have lower standard errors -> more power in our tests! When we bootstrap we are not sure that the data is too valid, since you might just increase the number of observations of a flawed starting sample

```

> What is the data showing us? Please type your answer after (and outside!) this blockquote. You have to explain what you have done, and the interpretation of the result. One paragraph max, please!

It gives us very different confidence intervals : they do not overlap, which suggests that one of the method is getting flawed results. We could explain that by the fact that bootstrapping is affected by any skewness or any flaws (sample not representative enough of the whole population) in the sample. By looking back at the graph on "2011-present", we can note that the data is in fact skewed at the right.

# General Social Survey (GSS)

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.


In this assignment we analyze data from the **2016 GSS sample data**, using it to estimate values of *population parameters* of interest about US adults. The GSS sample data file has 2867 observations of 935 variables, but we are only interested in very few of these variables and you are using a smaller file.


```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))
```

You will also notice that many responses should not be taken into consideration, like "No Answer", "Don't Know", "Not applicable", "Refused to Answer".

We will be creating 95% confidence intervals for population parameters. The variables we have are the following:

- hours and minutes spent on email weekly. The responses to these questions are recorded in the `emailhr` and `emailmin` variables. For example, if the response is 2.50 hours, this would be recorded as emailhr = 2 and emailmin = 30.
- `snapchat`, `instagrm`, `twitter`: whether respondents used these social media in 2016
- `sex`: Female - Male
- `degree`: highest education level attained

## Instagram and Snapchat, by sex

Can we estimate the *population* proportion of Snapchat or Instagram users in 2016?

1. Create a  new variable, `snap_insta` that is *Yes* if the respondent reported using any of Snapchat (`snapchat`) or Instagram (`instagrm`), and *No* if not. If the recorded value was NA for both of these questions, the value in your new variable should also be NA.

```{r}
glimpse(gss)

gss_combined <- gss %>%
  mutate(snapchat = recode(snapchat, "No"=0, "Yes"=1)) %>%
  mutate(instagrm = recode(instagrm, "No"=0, "Yes"=1))%>%
  mutate(snap_insta_temporary = snapchat+instagrm)%>%
  mutate(snap_insta = ceiling(snap_insta_temporary/2))

```




2. Calculate the proportion of Yes’s for `snap_insta` among those who answered the question, i.e. excluding NAs.


```{r}
gss_combined %>%
  summarise(yesses= count(snap_insta==1), noses=count(snap_insta==0), proportion=yesses/noses)

```


3. Using the CI formula for proportions, please construct 95% CIs for men and women who used either Snapchat or Instagram


```{r}
formula_ci_FM <- gss_combined %>%
  group_by(sex) %>%
  filter(na.rm=TRUE) %>%
  summarise(mean = mean(snap_insta,  na.rm = TRUE), SD = sd(snap_insta, na.rm = TRUE), count=n(),
  t_critical = qt(0.975, count-1), 
  SE = SD/sqrt(count),
  margin_error = t_critical * SE,
  lower = mean - margin_error,
  upper = mean + margin_error)

formula_ci_FM
  
```


## Twitter, by education level

Can we estimate the *population* proportion of Twitter users by education level in 2016?. 

There are 5 education levels in variable `degree` which, in ascending order of years of education, are Lt high school, High School, Junior college, Bachelor, Graduate. 

1. Turn `degree` from a character variable into a factor variable. Make sure the order is the correct one and that levels are not sorted alphabetically which is what R by default does. 

```{r}
gss_combined = gss_combined %>%
  mutate(degree = factor(degree, levels = c("Lt high school", "High School", "Junior college", "Bachelor", "Graduate")))%>%
  arrange(degree) 
```

1. Create a  new variable, `bachelor_graduate` that is *Yes* if the respondent has either a `Bachelor` or `Graduate` degree. As before, if the recorded value for either was NA, the value in your new variable should also be NA.

```{r}
gss_combined <- gss_combined %>% 
  mutate(bachelor_graduate = if_else(degree %in% c("Bachelor", "Graduate"), "Yes", if_else(degree %in% c("Lt high school", "High School", "Junior college"), "No", "NA")))

```



1. Calculate the proportion of `bachelor_graduate` who do (Yes) and who don't (No) use twitter. 

```{r}
gss_twitter = gss_combined%>%
  filter(bachelor_graduate=="Yes")%>%
  summarise(
    twitter_yes = count(twitter == "Yes"), 
    twitter_no = count(twitter=="No"), 
    twitter_count = n(),
    proportion_twitter_yes = twitter_yes/twitter_count,
    proportion_twitter_no = twitter_no/twitter_count)
    
proportion_twitter_no = as.numeric(gss_twitter["proportion_twitter_no"])
proportion_twitter_yes = as.numeric(gss_twitter["proportion_twitter_yes"])

proportion_twitter_no
proportion_twitter_yes

```


1. Using the CI formula for proportions, please construct two 95% CIs for `bachelor_graduate` vs whether they use (Yes) and don't (No) use twitter. 

```{r}

formula_ci_twitter_yes <- gss_combined %>% 
  filter(bachelor_graduate == "Yes")%>%
  summarise(count = n(),
            t_critical = qt(0.975, count-1),
            SE = sqrt(proportion_twitter_yes*(1-proportion_twitter_yes)/count),
            margin_error= t_critical*SE,
            lower = (proportion_twitter_yes - margin_error),
            upper =(proportion_twitter_yes + margin_error))

formula_ci_twitter_no <- gss_combined %>% 
  filter(bachelor_graduate == "Yes")%>%
  summarise(count = n(),
            t_critical = qt(0.975, count-1),
            SE = sqrt(proportion_twitter_no*(1-proportion_twitter_no)/count),
            margin_error= t_critical*SE,
            lower = (proportion_twitter_no - margin_error),
            upper =(proportion_twitter_no + margin_error))

formula_ci_twitter_yes
formula_ci_twitter_no

```


1. Do these two Confidence Intervals overlap?

No. Significantly different. 


## Email usage

Can we estimate the *population* parameter on time spent on email weekly?

1. Create a new variable called `email` that combines `emailhr` and `emailmin` to reports the number of minutes the respondents spend on email weekly.

```{r}

gss_combined = gss_combined%>%
  mutate(email = as.numeric(emailhr)*60 + as.numeric(emailmin))

```


1. Visualise the distribution of this new variable. Find the mean and the median number of minutes respondents spend on email weekly. Is the mean or the median a better measure of the typical amoung of time Americans spend on email weekly? Why?

```{r}
gss_combined%>%
  summarise(mean = mean(email, na.rm=TRUE), median = median(email, na.rm = TRUE))
                        
gss_combined%>%
  ggplot(aes(x=email))+
  geom_density(fill="cyan")
```


1. Using the `infer` package, calculate a 95% bootstrap confidence interval for the mean amount of time Americans spend on email weekly. Interpret this interval in context of the data, reporting its endpoints in “humanized” units (e.g. instead of 108 minutes, report 1 hr and 8 minutes). If you get a result that seems a bit odd, discuss why you think this might be the case.

```{r}

set.seed(45678)

boot_email = gss_combined %>%
  specify(response = email) %>%
  generate(reps = 1000, type = "bootstrap")%>%
  calculate(stat = c("mean"))


boot_email_CI <- boot_email %>%
  get_confidence_interval(level=0.95, type = "percentile")

boot_email_CI

boot_email_CI%>%
  summarise(lower_hour = lower_ci%/%60, 
            lower_minute = floor(lower_ci %%60),
            upper_hour = upper_ci%/%60, 
            upper_minute = ceiling(upper_ci %%60))%>%
  mutate(lower_time = paste(as.character(lower_hour),"h",as.character(lower_minute),"min"))%>%
  mutate(upper_time = paste(as.character(upper_hour),"h",as.character(upper_minute),"min"))%>%
  select(lower_time, upper_time)


```


1. Would you expect a 99% confidence interval to be wider or narrower than the interval you calculated above? Explain your reasoning.

```{r}
Wider. 
```



# Biden's Approval Margins

As we saw in class, fivethirtyeight.com has detailed data on [all polls that track the president's approval ](https://projects.fivethirtyeight.com/biden-approval-ratings)

```{r, cache=TRUE}
# Import approval polls data directly off fivethirtyeight website
approval_polllist <- read_csv('https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

# Use `lubridate` to fix dates, as they are given as characters.
```

## Create a plot

What I would like you to do is to calculate the average net approval rate (approve- disapprove) for each week since he got into office. I want you plot the net approval, along with its 95% confidence interval. There are various dates given for each poll, please use `enddate`, i.e., the date the poll ended.

```{r}

approval_weeks <- approval_polllist %>%
          mutate(week = week(mdy(enddate))-3)%>%
          group_by(week)%>%
          select(!startdate)%>%
          select(!enddate)%>%
          mutate(net_approval = approve - disapprove) %>%
          select(!approve)%>%
          select(!disapprove)%>%
          select(!multiversions)%>%
          select(!tracking)%>%
          select(!modeldate)%>%
          select(!timestamp)
  
approval_weeks


formula_ci_approval <- approval_weeks %>% 
  group_by(week) %>% 
  summarise(count = n(),
            mean = mean(net_approval,na.rm=TRUE),
            SD = sd(net_approval,na.rm=TRUE),
            t_critical = qt(0.975, count-1),
            SE = SD/sqrt(count),
            margin_error= t_critical*SE,
            lower = (mean - margin_error),
            upper =(mean + margin_error))

formula_ci_approval

formula_ci_approval %>%
  ggplot(aes(x = week, y=mean))+
  geom_point()+
  geom_line()+
  geom_line(aes(x = week, y=upper), color = "red")+
  geom_line(aes(x = week, y=lower), color = "red")+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill="grey", alpha=0.5)+
  geom_smooth(color = "blue", se=FALSE)+
  geom_line(y=0,color = "orange", size=2)
  

```


Also, please add an orange line at zero. Your plot should look like this:

```{r trump_margins, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "biden_approval_margin.png"), error = FALSE)
```



## Compare Confidence Intervals

Compare the confidence intervals for `week 3` and `week 25`. Can you explain what's going on? One paragraph would be enough.

```{r}



```


# Gapminder revisited

Recall the `gapminder` data frame from the gapminder package. That data frame contains just six columns from the larger [data in Gapminder World](https://www.gapminder.org/data/). In this part, you will join a few dataframes with more data than the 'gapminder' package. Specifically, you will look at data on 


- Life expectancy at birth (life_expectancy_years.csv)
- GDP per capita in constant 2010 US$ (https://data.worldbank.org/indicator/NY.GDP.PCAP.KD)
- Female fertility: The number of babies per woman (https://data.worldbank.org/indicator/SP.DYN.TFRT.IN)
- Primary school enrollment as % of children attending primary school (https://data.worldbank.org/indicator/SE.PRM.NENR)
- Mortality rate, for under 5, per 1000 live births (https://data.worldbank.org/indicator/SH.DYN.MORT)
- HIV prevalence (adults_with_hiv_percent_age_15_49.csv): The estimated number of people living with HIV per 100 population of age group 15-49.

You must use the `wbstats` package to download data from the World Bank. The relevant World Bank indicators are `SP.DYN.TFRT.IN`, `SE.PRM.NENR`, `NY.GDP.PCAP.KD`, and `SH.DYN.MORT`

```{r, get_data, cache=TRUE}

# load gapminder HIV data
hiv <- read_csv(here::here("data","adults_with_hiv_percent_age_15_49.csv"))
life_expectancy <- read_csv(here::here("data","life_expectancy_years.csv"))

# get World bank data using wbstats
indicators <- c("SP.DYN.TFRT.IN","SE.PRM.NENR", "SH.DYN.MORT", "NY.GDP.PCAP.KD")


library(wbstats)

worldbank_data <- wb_data(country="countries_only", #countries only- no aggregates like Latin America, Europe, etc.
                          indicator = indicators, 
                          start_date = 1960, 
                          end_date = 2016)

# get a dataframe of information regarding countries, indicators, sources, regions, indicator topics, lending types, income levels,  from the World Bank API 
countries <-  wbstats::wb_cachelist$countries

```

You have to join the 3 dataframes (life_expectancy, worldbank_data, and HIV) into one. You may need to tidy your data first and then perform [join operations](http://r4ds.had.co.nz/relational-data.html). Think about what type makes the most sense **and explain why you chose it**.


```{r}
tidy_hiv = hiv %>%
   select(1:34) %>% 
   pivot_longer(cols=2:34,
   names_to="year",
   values_to = "hiv")%>%
   drop_na()
tidy_hiv


tidy_life_expectancy = life_expectancy %>%
   select(1:302) %>% 
   pivot_longer(cols=2:302,
   names_to="year",
   values_to = "life_expectancy")%>%
   drop_na()

tidy_life_expectancy

tidy_worldbank_data = worldbank_data%>%
  mutate(year = as.character(date))%>%
  select(!date)

tidy_worldbank_data 

joined_table <- inner_join(left_join(left_join(tidy_hiv, tidy_worldbank_data, by = c("country" = "country", "year" = "year") ), tidy_life_expectancy, by = c("country" = "country", "year" = "year")), countries, "country" = "country")
joined_table

```



1. What is the relationship between HIV prevalence and life expectancy? Generate a scatterplot with a smoothing line to report your results. You may find faceting useful

```{r}

joined_table%>%
  ggplot(aes(x=hiv, y = life_expectancy))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~region)

joined_table%>%
  group_by(region)%>%
  summarise(count=n())

```


1. What is the relationship between fertility rate and GDP per capita? Generate a scatterplot with a smoothing line to report your results. You may find facetting by region useful

```{r}
joined_table%>%
  ggplot(aes(x=NY.GDP.PCAP.KD, y = SP.DYN.TFRT.IN))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~region)

```



1. Which regions have the most observations with missing HIV data? Generate a bar chart (`geom_col()`), in descending order.

```{r}

hiv_na = hiv %>%
   select(1:34) %>% 
   pivot_longer(cols=2:34,
   names_to="year",
   values_to = "hiv")

hiv_na_country = left_join(hiv_na, countries)

hiv_na_country %>%
  filter(!is.na(region))%>%
  group_by(region) %>%
  summarise(na = sum(is.na(hiv))) %>%
  ggplot(aes(y = reorder(region, --na), x=na))+
  geom_col()

```

1. How has mortality rate for under 5 changed by region? In each region, find the top 5 countries that have seen the greatest improvement, as well as those 5 countries where mortality rates have had the least improvement or even deterioration.

```{r}

endpoints <- joined_table %>%
  group_by(country) %>%
  summarise(min_year = min(year), max_year = max(year))

endpoints

joiner_min <- left_join(joined_table, endpoints, "country"="country") %>%
  select(country, year, min_year, max_year, SH.DYN.MORT)%>%
  mutate(mortality_origin = ifelse(year == min_year, SH.DYN.MORT, 0))%>%
  select(!year)%>%
  filter(!mortality_origin==0)%>%
  select(!SH.DYN.MORT)

joiner_min

joiner_max <- left_join(joined_table, endpoints, "country"="country") %>%
  select(country, year, min_year, max_year, SH.DYN.MORT)%>%
  mutate(mortality_end = ifelse(year == max_year, SH.DYN.MORT, 0))%>%
  select(!year)%>%
  filter(!mortality_end==0)%>%
  select(!SH.DYN.MORT)

joiner_max

join_joiners <- left_join(joiner_min, joiner_max, "country"="country")
join_joiners

by_region <- left_join(join_joiners, countries, "country"="country") %>%
  select(country,region,min_year,max_year,mortality_origin,mortality_end)
by_region

evolution_included <- by_region %>%
  mutate(min_year = as.numeric(min_year),max_year = as.numeric(max_year),mortality_origin = as.numeric(mortality_origin),mortality_end = as.numeric(mortality_end),evolution_of_mortality = (mortality_end-mortality_origin)/mortality_origin)%>%
  arrange(desc(evolution_of_mortality))

evolution_included

evolution_by_region <- evolution_included %>%
  group_by(region) %>%
  summarise(mean_mortality_evol = mean(evolution_of_mortality))

evolution_by_region


evolution_by_region %>%
  ggplot(aes(y = reorder(region, --mean_mortality_evol), x=100*mean_mortality_evol))+
  geom_col()


region_list = c("East Asia & Pacific"	,"Europe & Central Asia"	,"Latin America & Caribbean","Middle East & North Africa","North America","South Asia","Sub-Saharan Africa")


prepared_data <- left_join(join_joiners, countries, "country"="country") %>%
  
  mutate(min_year = as.numeric(min_year),max_year = as.numeric(max_year),mortality_origin = as.numeric(mortality_origin),mortality_end = as.numeric(mortality_end),evolution_of_mortality = (mortality_end-mortality_origin)/mortality_origin)%>%
  
  arrange(desc(evolution_of_mortality))%>%
  
  select(country,region,min_year,max_year,mortality_origin,mortality_end, evolution_of_mortality)

prepared_data %>%
  filter(region == region_list[1])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[2])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[3])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[4])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[5])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)

prepared_data %>%
  filter(region == region_list[6])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)

prepared_data %>%
  filter(region == region_list[7])%>%
  slice_max(order_by = evolution_of_mortality,n = 5)

prepared_data %>%
  filter(region == region_list[1])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[2])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[3])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[4])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)


prepared_data %>%
  filter(region == region_list[5])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)

prepared_data %>%
  filter(region == region_list[6])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)

prepared_data %>%
  filter(region == region_list[7])%>%
  slice_max(order_by = -evolution_of_mortality,n = 5)

```


1. Is there a relationship between primary school enrollment and fertility rate?

```{r}

skim(joined_table)

education <- left_join(joined_table, evolution_included, "country"="country") 

education %>%
  group_by(country) %>%
  ggplot(aes(x=SE.PRM.NENR,y=evolution_of_mortality))+
  geom_point()+
  facet_wrap(~region)+
  geom_smooth(method = "lm")


```



# Challenge 1: Excess rentals in TfL bike sharing

Recall the TfL data on how many bikes were hired every single day. We can get the latest data by running the following

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

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

```{r tfl_month_year_grid, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_distributions_monthly.png"), error = FALSE)
```

Look at May and Jun and compare 2020 with the previous years. What's happening?

However, the challenge I want you to work on is to reproduce the following two graphs.

```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%", eval=FALSE}
knitr::include_graphics(here::here("images", "tfl_monthly.png"), error = FALSE)
```


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

```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%"}

comparison_bike %>%
  ggplot(aes(x = month, group = 1)) +
  geom_line(aes(x = month, y = actual), color = "black", size = 0.1) +
  geom_line(aes(x = month, y = expected), color = "blue", size = 0.8) +
  geom_ribbon(aes(ymin = expected, ymax = pmin(expected, actual)),fill = "red", alpha=0.2)  +
  geom_ribbon(aes(ymin = actual, ymax = pmin(expected, actual)),fill = "green", alpha=0.2)+
  facet_wrap(~ year) +
  theme_bw()

```

The second one looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to Q2 (weeks 14-26) and Q4 (weeks 40-52).

```{r tfl_percent_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
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

```{r}
comparison_bike_w %>%
  ggplot(aes(x = week, group = 1)) +
  geom_line(aes(x = week, y = dchanges, fill = "black")) +
  geom_ribbon(aes(ymin = 0, ymax = pmin(0, dchanges)),fill = "red", alpha=0.2)  +
  geom_ribbon(aes(ymin = dchanges, ymax = pmin(0, dchanges)),fill = "green", alpha=0.2)+
  facet_wrap(~ year) +
  theme_bw()
```


For both of these graphs, you have to calculate the expected number of rentals per week or month between 2016-2019 and then, see how each week/month of 2020-2021 compares to the expected rentals. Think of the calculation `excess_rentals = actual_rentals - expected_rentals`. 

Should you use the mean or the median to calculate your expected rentals? Why?

In creating your plots, you may find these links useful:

- https://ggplot2.tidyverse.org/reference/geom_ribbon.html
- https://ggplot2.tidyverse.org/reference/geom_tile.html 
- https://ggplot2.tidyverse.org/reference/geom_rug.html


# Deliverables

As usual, there is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Knit the edited and completed R Markdown file as an HTML document (use the "Knit" button at the top of the script editor window) and upload it to Canvas.

# Details

- Who did you collaborate with: TYPE NAMES HERE
- Approximately how much time did you spend on this problem set: ANSWER HERE
- What, if anything, gave you the most trouble: ANSWER HERE


**Please seek out help when you need it,** and remember the [15-minute rule](https://mam202.netlify.app/syllabus/#the-15-minute-rule){target=_blank}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!  

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 


# Rubric

Check minus (1/5): Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed. 

Check (3/5): Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output). 

Check plus (5/5): Finished all components of the assignment correctly and addressed both challenges. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output.