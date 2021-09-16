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
image: pic07.jpg
keywords: ""
slug: jose
title: About me
---

```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(gapminder)  # gapminder dataset
library(here)
library(janitor)
```

The goal is to test your software installation, to demonstrate
competency in Markdown, and in the basics of `ggplot`.

> Please delete all the intro text I wrote from line 22 to line 69 and
> start writing your short biography after this blockquote.

# My Life

My name is Jose Laffitte. I was born in Sevilla, Spain, where I lived
most of my life. It is a really beautiful city, and through this
[link](https://www.youtube.com/watch?v=AgBS7sIM6I8&t=138s&ab_channel=Enrique%C3%81lex)
you can take a look at it! When I was 16 I did an exchange to the US,
and at 18 I moved to the Netherlands to complete my university studies.

I come from a big family. We are 6 siblings in total. We are all very
close to each other, which made living with them a lot of fun! There was
never a dull moment in my house growing up! Their names are: - Carlos -
Felipe - Fernando - Luis - Carlota

Now I am beggining to write a new chapter in my life, by moving to
London and starting the MAM Program with a group of great people! I am
**very excited** to get started, and can't wait live great experiences
with the LBS Community!

# Task 2: `gapminder` country comparison

You have seen the `gapminder` dataset that has data on life expectancy,
population, and GDP per capita for 142 countries from 1952 to 2007. To
get a glimpse of the dataframe, namely to see the variable names,
variable types, etc., we use the `glimpse` function. We also want to
have a look at the first 20 rows of data.

```{r}
glimpse(gapminder)

head(gapminder, 20) # look at the first 20 rows of the dataframe

```

Your task is to produce two graphs of how life expectancy has changed
over the years for the `country` and the `continent` you come from.

I have created the `country_data` and `continent_data` with the code
below.

```{r}
country_data <- gapminder %>% 
            filter(country == "Spain")

continent_data <- gapminder %>% 
            filter(continent == "Europe")
```

First, create a plot of life expectancy over time for the single country
you chose. Map `year` on the x-axis, and `lifeExp` on the y-axis. You
should also use `geom_point()` to see the actual data points and
`geom_smooth(se = FALSE)` to plot the underlying trendlines. You need to
remove the comments **\#** from the lines below for your code to run.

```{r, lifeExp_one_country}
plot1 <- ggplot(data = country_data, mapping = aes(x = year, y = lifeExp))+
   geom_point() +
   geom_smooth(se = FALSE)+
   NULL 

 plot1
```

Next we need to add a title. Create a new plot, or extend plot1, using
the `labs()` function to add an informative title to the plot.

```{r, lifeExp_one_country_with_label}
 plot1<- plot1 +
   labs(title = "Life Expectancy Over Time, Spain ",
       x = "Years",
       y = "Life Expectancy") +
   NULL


 plot1
```

Secondly, produce a plot for all countries in the *continent* you come
from. (Hint: map the `country` variable to the colour aesthetic. You
also want to map `country` to the `group` aesthetic, so all points for
each country are grouped together).

```{r lifeExp_one_continent}
ggplot(data = continent_data, mapping = aes(x = year , y = lifeExp , colour=country))+
   geom_line() +
   geom_smooth(se = FALSE) + 
   NULL
```

Finally, using the original `gapminder` data, produce a life expectancy
over time graph, grouped (or faceted) by continent. We will remove all
legends, adding the `theme(legend.position="none")` in the end of our
ggplot.

```{r lifeExp_facet_by_continent}
 ggplot(data = gapminder, mapping = aes(x = year , y = lifeExp , colour = continent))+
   geom_line ()+ 
   geom_smooth(se = FALSE) +
   facet_wrap(~continent) +
   theme(legend.position = "none")
   NULL
```

Given these trends, what can you say about life expectancy since 1952?
Again, don't just say what's happening in the graph. Tell some sort of
story and speculate about the differences in the patterns.

> Type your answer after this blockquote.

Thanks to the developments and improvements in medicine, nutrition, and
economic conditions, the life expectancy has increased in all continents
through the years. We can see that the increase has been smaller in
Africa. This might be because of their lagging position in terms of
development.

# Task 3: Brexit vote analysis

We will have a look at the results of the 2016 Brexit vote in the UK.
First we read the data using `read_csv()` and have a quick glimpse at
the data

```{r load_brexit_data, warning=FALSE, message=FALSE}
brexit_results <- read_csv(here::here("data","brexit_results.csv"))


glimpse(brexit_results)
```

The data comes from [Elliott Morris](https://www.thecrosstab.com/), who
cleaned it and made it available through his [DataCamp class on
analysing election and polling data in
R](https://www.datacamp.com/courses/analyzing-election-and-polling-data-in-r).

Our main outcome variable (or y) is `leave_share`, which is the percent
of votes cast in favour of Brexit, or leaving the EU. Each row is a UK
[parliament
constituency](https://en.wikipedia.org/wiki/United_Kingdom_Parliament_constituencies).

To get a sense of the spread, or distribution, of the data, we can plot
a histogram, a density plot, and the empirical cumulative distribution
function of the leave % in all constituencies.

```{r brexit_histogram, warning=FALSE, message=FALSE}

# histogram
ggplot(brexit_results, aes(x = leave_share)) +
  geom_histogram(binwidth = 2.5) +
  labs(title ="Histogram of Leave Share")

# density plot-- think smoothed histogram
ggplot(brexit_results, aes(x = leave_share)) +
  geom_density()+
  labs(title ="Density of Leave Share")

# The empirical cumulative distribution function (ECDF) 
ggplot(brexit_results, aes(x = leave_share)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  scale_y_continuous(labels = scales::percent)+
  labs(title ="ECDF of Leave Share")
  


```

One common explanation for the Brexit outcome was fear of immigration
and opposition to the EU's more open border policy. We can check the
relationship (or correlation) between the proportion of native born
residents (`born_in_uk`) in a constituency and its `leave_share`. To do
this, let us get the correlation between the two variables

```{r brexit_immigration_correlation}
brexit_results %>% 
  select(leave_share, born_in_uk) %>% 
  cor()
```

The correlation is almost 0.5, which shows that the two variables are
positively correlated.

We can also create a scatterplot between these two variables using
`geom_point`. We also add the best fit line, using
`geom_smooth(method = "lm")`.

```{r brexit_immigration_plot}
ggplot(brexit_results, aes(x = born_in_uk, y = leave_share)) +
  geom_point(alpha=0.3) +
  
  # add a smoothing line, and use method="lm" to get the best straight-line
  geom_smooth(method = "lm") + 
  
  # use a white background and frame the plot with a black box
  theme_bw() +
  labs(title ="Relationship Between British Born population and Vote Leave")
  NULL
```

You have the code for the plots, I would like you to revisit all of them
and use the `labs()` function to add an informative title, subtitle, and
axes titles to all plots.

What can you say about the relationship shown above? Again, don't just
say what's happening in the graph. Tell some sort of story and speculate
about the differences in the patterns.

> Type your answer after, and outside, this blockquote.

It seems that the higher the density of british-born voters, the higher
proportion of the population voted leave in that area. Thus, one could
infer that british-born voters were more likely to vote leave. This
could be because of fear to loose european inmigration policies.
Non-british born voters would be more understanding of these policies as
they are inmigrants themselves.

# Task 4: Animal rescue incidents attended by the London Fire Brigade

[The London Fire
Brigade](https://data.london.gov.uk/dataset/animal-rescue-incidents-attended-by-lfb)
attends a range of non-fire incidents (which we call 'special
services'). These 'special services' include assistance to animals that
may be trapped or in distress. The data is provided from January 2009
and is updated monthly. A range of information is supplied for each
incident including some location information (postcode, borough, ward),
as well as the data/time of the incidents. We do not routinely record
data about animal deaths or injuries.

Please note that any cost included is a notional cost calculated based
on the length of time rounded up to the nearest hour spent by Pump,
Aerial and FRU appliances at the incident and charged at the current
Brigade hourly rate.

```{r load_animal_rescue_data, warning=FALSE, message=FALSE}

url <- "https://data.london.gov.uk/download/animal-rescue-incidents-attended-by-lfb/8a7d91c2-9aec-4bde-937a-3998f4717cd8/Animal%20Rescue%20incidents%20attended%20by%20LFB%20from%20Jan%202009.csv"

animal_rescue <- read_csv(url,
                          locale = locale(encoding = "CP1252")) %>% 
  janitor::clean_names()


glimpse(animal_rescue)
```

One of the more useful things one can do with any data set is quick
counts, namely to see how many observations fall within one category.
For instance, if we wanted to count the number of incidents by year, we
would either use `group_by()... summarise()` or, simply
[`count()`](https://dplyr.tidyverse.org/reference/count.html)

```{r, instances_by_calendar_year}

animal_rescue %>% 
  dplyr::group_by(cal_year) %>% 
  summarise(count=n())

animal_rescue %>% 
  count(cal_year, name="count")

```

Let us try to see how many incidents we have by animal group. Again, we
can do this either using group_by() and summarise(), or by using count()

```{r, animal_group_percentages}
animal_rescue %>% 
  group_by(animal_group_parent) %>% 
  
  #group_by and summarise will produce a new column with the count in each animal group
  summarise(count = n()) %>% 
  
  # mutate adds a new column; here we calculate the percentage
  mutate(percent = round(100*count/sum(count),2)) %>% 
  
  # arrange() sorts the data by percent. Since the default sorting is min to max and we would like to see it sorted
  # in descending order (max to min), we use arrange(desc()) 
  arrange(desc(percent))


animal_rescue %>% 
  
  #count does the same thing as group_by and summarise
  # name = "count" will call the column with the counts "count" ( exciting, I know)
  # and 'sort=TRUE' will sort them from max to min
  count(animal_group_parent, name="count", sort=TRUE) %>% 
  mutate(percent = round(100*count/sum(count),2))


```

Do you see anything strange in these tables?

Finally, let us have a loot at the notional cost for rescuing each of
these animals. As the LFB says,

> Please note that any cost included is a notional cost calculated based
> on the length of time rounded up to the nearest hour spent by Pump,
> Aerial and FRU appliances at the incident and charged at the current
> Brigade hourly rate.

There is two things we will do:

1.  Calculate the mean and median `incident_notional_cost` for each
    `animal_group_parent`
2.  Plot a boxplot to get a feel for the distribution of
    `incident_notional_cost` by `animal_group_parent`.

Before we go on, however, we need to fix `incident_notional_cost` as it
is stored as a `chr`, or character, rather than a number.

```{r, parse_incident_cost,message=FALSE, warning=FALSE}

# what type is variable incident_notional_cost from dataframe `animal_rescue`
typeof(animal_rescue$incident_notional_cost)

# readr::parse_number() will convert any numerical values stored as characters into numbers
animal_rescue <- animal_rescue %>%  

  # we use mutate() to use the parse_number() function and overwrite the same variable
  mutate(incident_notional_cost = parse_number(incident_notional_cost))

# incident_notional_cost from dataframe `animal_rescue` is now 'double' or numeric
typeof(animal_rescue$incident_notional_cost)

```

Now that incident_notional_cost is numeric, let us quickly calculate
summary statistics for each animal group.

```{r, stats_on_incident_cost,message=FALSE, warning=FALSE}

animal_rescue %>% 
  
  # group by animal_group_parent
  group_by(animal_group_parent) %>% 
  
  # filter resulting data, so each group has at least 6 observations
  filter(n()>6) %>% 
  
  # summarise() will collapse all values into 3 values: the mean, median, and count  
  # we use na.rm=TRUE to make sure we remove any NAs, or cases where we do not have the incident cos
  summarise(mean_incident_cost = mean (incident_notional_cost, na.rm=TRUE),
            median_incident_cost = median (incident_notional_cost, na.rm=TRUE),
            sd_incident_cost = sd (incident_notional_cost, na.rm=TRUE),
            min_incident_cost = min (incident_notional_cost, na.rm=TRUE),
            max_incident_cost = max (incident_notional_cost, na.rm=TRUE),
            count = n()) %>% 
  
  # sort the resulting data in descending order. You choose whether to sort by count or mean cost.
  arrange(count)

```

Compare the mean and the median for each animal group. waht do you think
this is telling us? Anything else that stands out? Any outliers?

Finally, let us plot a few plots that show the distribution of
incident_cost for each animal group.

```{r, plots_on_incident_cost_by_animal_group,message=FALSE, warning=FALSE}

# base_plot
base_plot <- animal_rescue %>% 
  group_by(animal_group_parent) %>% 
  filter(n()>6) %>% 
  ggplot(aes(x=incident_notional_cost))+
  facet_wrap(~animal_group_parent, scales = "free")+
  theme_bw()

base_plot + geom_histogram()
base_plot + geom_density()
base_plot + geom_boxplot()
base_plot + stat_ecdf(geom = "step", pad = FALSE) +
  scale_y_continuous(labels = scales::percent)



```

Which of these four graphs do you think best communicates the
variability of the `incident_notional_cost` values? Also, can you please
tell some sort of story (which animals are more expensive to rescue than
others, the spread of values) and speculate about the differences in the
patterns.

I think the histogram best communicates the variability.It seems that
the higher the size of the animal, the more expensive it is to rescue.
This makes sense, as the bigger the size, the more laborious it is to
handle the animal. Not many people would like to deal with a lost tiger,
but I am sure most of us would be willing to help a little rabbit out
;).

# Submit the assignment

Knit the completed R Markdown file as an HTML document (use the "Knit"
button at the top of the script editor window) and upload it to Canvas.

## Details

If you want to, please answer the following

-   Who did you collaborate with: TYPE NAMES HERE
-   Approximately how much time did you spend on this problem set:
    ANSWER HERE
-   What, if anything, gave you the most trouble: ANSWER HERE
