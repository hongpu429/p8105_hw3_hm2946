hw3
================
min
2022-10-13

# problem 1

## preparation

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggridges)
library(patchwork)
library(p8105.datasets)
data("instacart")
```

#### Introduce the data

``` r
instacart = 
  instacart %>% 
  as_tibble(instacart)

instacart
```

    ## # A tibble: 1,384,617 × 15
    ##    order_id product_id add_to_…¹ reord…² user_id eval_…³ order…⁴ order…⁵ order…⁶
    ##       <int>      <int>     <int>   <int>   <int> <chr>     <int>   <int>   <int>
    ##  1        1      49302         1       1  112108 train         4       4      10
    ##  2        1      11109         2       1  112108 train         4       4      10
    ##  3        1      10246         3       0  112108 train         4       4      10
    ##  4        1      49683         4       0  112108 train         4       4      10
    ##  5        1      43633         5       1  112108 train         4       4      10
    ##  6        1      13176         6       0  112108 train         4       4      10
    ##  7        1      47209         7       0  112108 train         4       4      10
    ##  8        1      22035         8       1  112108 train         4       4      10
    ##  9       36      39612         1       0   79431 train        23       6      18
    ## 10       36      19660         2       1   79431 train        23       6      18
    ## # … with 1,384,607 more rows, 6 more variables: days_since_prior_order <int>,
    ## #   product_name <chr>, aisle_id <int>, department_id <int>, aisle <chr>,
    ## #   department <chr>, and abbreviated variable names ¹​add_to_cart_order,
    ## #   ²​reordered, ³​eval_set, ⁴​order_number, ⁵​order_dow, ⁶​order_hour_of_day

*The data has 15 columns (variables) and 1384617 rows (observations).
The variables starts with order_id, product_id for identification and
variables like product_name, aisle, departments specify the goods. For
example, product_id 49302 is Bulgarian Yogurt, which is in aisle yogurt
department dairy eggs.*

In total, there are 39123 different products in 131209 different orders
from 131209 different users.

#### answer the questions

count aisles

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

*There are 134 aisles, and fresh vegetables is where the most items
ordered from*

plot of the number of items ordered in each aisle, only in which more
than 10000 items ordered

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(aisle = fct_reorder(aisle, n)) %>% 
  ggplot(aes(x = aisle,y = n)) + 
  geom_point() + 
  labs(title = "number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](hw3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Table of the three most popular items in aisles “baking ingredients”,
“dog food care”, and “packaged vegetables fruits”

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(desc(n)) %>%
  knitr::kable()
```

| aisle                      | product_name                                  |    n | rank |
|:---------------------------|:----------------------------------------------|-----:|-----:|
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |

Table of the mean hour of the day at which Pink Lady Apples and Coffee
Ice Cream are ordered everyday

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow + 1, value = mean_hour) %>%
  knitr::kable(digits = 2)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| product_name     | order_dow | 11.36 | 11.551724137931 | 11.7021276595745 | 11.9375 | 12.2631578947368 | 12.7843137254902 | 13.4411764705882 | 13.7741935483871 | 13.8333333333333 | 14.25 | 14.3157894736842 | 15.2173913043478 | 15.3181818181818 | 15.3809523809524 |
|:-----------------|----------:|------:|----------------:|-----------------:|--------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|------:|-----------------:|-----------------:|-----------------:|-----------------:|
| Coffee Ice Cream |         0 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |            13.77 |               NA |    NA |               NA |               NA |               NA |               NA |
| Coffee Ice Cream |         1 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |            14.32 |               NA |               NA |               NA |
| Coffee Ice Cream |         2 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |            15.38 |
| Coffee Ice Cream |         3 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |            15.32 |               NA |
| Coffee Ice Cream |         4 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |            15.22 |               NA |               NA |
| Coffee Ice Cream |         5 |    NA |              NA |               NA |      NA |            12.26 |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Coffee Ice Cream |         6 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |            13.83 |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         0 |    NA |              NA |               NA |      NA |               NA |               NA |            13.44 |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         1 | 11.36 |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         2 |    NA |              NA |             11.7 |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         3 |    NA |              NA |               NA |      NA |               NA |               NA |               NA |               NA |               NA | 14.25 |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         4 |    NA |           11.55 |               NA |      NA |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         5 |    NA |              NA |               NA |      NA |               NA |            12.78 |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |
| Pink Lady Apples |         6 |    NA |              NA |               NA |   11.94 |               NA |               NA |               NA |               NA |               NA |    NA |               NA |               NA |               NA |               NA |

# problem 2

## tidy the data

``` r
acc_data = read.csv("./accel_data.csv") %>% 
  janitor::clean_names() %>% 
  rename(day_of_the_week = day, day = day_id) %>% 
  pivot_longer(activity_1:activity_1440,
               names_to = "minute_from_midnight",
               names_prefix = "activity.",
               values_to = "activity_counts") %>% 
  mutate(minute_from_midnight = as.numeric(minute_from_midnight)) %>% 
  mutate(weekday_vs_weekend = 
           ifelse(day_of_the_week == c("Saturday","Sunday"),"weekend","weekday"))

acc_data
```

    ## # A tibble: 50,400 × 6
    ##     week   day day_of_the_week minute_from_midnight activity_counts weekday_vs…¹
    ##    <int> <int> <chr>                          <dbl>           <dbl> <chr>       
    ##  1     1     1 Friday                             1            88.4 weekday     
    ##  2     1     1 Friday                             2            82.2 weekday     
    ##  3     1     1 Friday                             3            64.4 weekday     
    ##  4     1     1 Friday                             4            70.0 weekday     
    ##  5     1     1 Friday                             5            75.0 weekday     
    ##  6     1     1 Friday                             6            66.3 weekday     
    ##  7     1     1 Friday                             7            53.8 weekday     
    ##  8     1     1 Friday                             8            47.8 weekday     
    ##  9     1     1 Friday                             9            55.5 weekday     
    ## 10     1     1 Friday                            10            43.0 weekday     
    ## # … with 50,390 more rows, and abbreviated variable name ¹​weekday_vs_weekend

## aggregate table

``` r
acc_data %>% 
  group_by(week, day_of_the_week) %>% 
  summarize(total = sum(activity_counts)) %>% 
  pivot_wider(names_from = day_of_the_week,
              values_from = total) %>% 
  select(week, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
```

    ## `summarise()` has grouped output by 'week'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 8
    ## # Groups:   week [5]
    ##    week  Monday Tuesday Wednesday Thursday  Friday Saturday Sunday
    ##   <int>   <dbl>   <dbl>     <dbl>    <dbl>   <dbl>    <dbl>  <dbl>
    ## 1     1  78828. 307094.   340115.  355924. 480543.   376254 631105
    ## 2     2 295431  423245    440962   474048  568839    607175 422018
    ## 3     3 685910  381507    468869   371230  467420    382928 467052
    ## 4     4 409450  319568    434460   340291  154049      1440 260617
    ## 5     5 389080  367824    445366   549658  620860      1440 138421

``` r
acc_data %>% 
  group_by(day, minute_from_midnight,weekday_vs_weekend) %>% 
  summarize(total = sum(activity_counts)) %>% 
  arrange(desc(total)) %>% 
  head(20)
```

    ## `summarise()` has grouped output by 'day', 'minute_from_midnight'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 20 × 4
    ## # Groups:   day, minute_from_midnight [20]
    ##      day minute_from_midnight weekday_vs_weekend total
    ##    <int>                <dbl> <chr>              <dbl>
    ##  1    14                 1172 weekday             8982
    ##  2     4                  691 weekday             7866
    ##  3    14                 1171 weekday             7866
    ##  4    16                 1176 weekday             7358
    ##  5    16                 1249 weekday             7236
    ##  6     1                  550 weekday             6997
    ##  7    10                 1192 weekday             6879
    ##  8    10                 1201 weekend             6539
    ##  9    29                 1315 weekday             6539
    ## 10     4                  663 weekday             6214
    ## 11     8                 1229 weekday             6214
    ## 12     8                 1230 weekday             6109
    ## 13     8                 1231 weekday             6109
    ## 14    29                 1314 weekday             6109
    ## 15     8                 1242 weekday             6007
    ## 16     1                 1260 weekday             5906
    ## 17    14                 1174 weekday             5906
    ## 18    18                  451 weekday             5805
    ## 19    33                  415 weekday             5805
    ## 20     8                 1274 weekday             5706

*There is no apparent trend in the first table, but in the second, we
can see it tends to have higher activity counts in weekdays, and it is
more likely to have higher activity counts at around 1170 minutes from
midnight.*

## plot

``` r
acc_data %>% 
  ggplot(aes(x = minute_from_midnight,y = activity_counts, color = day_of_the_week)) +
  geom_point(alpha = .1) +
  geom_smooth(se = FALSE) +
  ylim(NA,2500)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 202 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 202 rows containing missing values (geom_point).

![](hw3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> *The activity
counts are low and steady in the first place, then it starts to rise at
minute 250, achieve a climax around minute 600, decreases, rises again,
achieves another climax around 1100, especially in Friday and Sunday.*

# problem 3

``` r
data("ny_noaa")
str(ny_noaa)
```

    ## tibble [2,595,176 × 7] (S3: tbl_df/tbl/data.frame)
    ##  $ id  : chr [1:2595176] "US1NYAB0001" "US1NYAB0001" "US1NYAB0001" "US1NYAB0001" ...
    ##  $ date: Date[1:2595176], format: "2007-11-01" "2007-11-02" ...
    ##  $ prcp: int [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ snow: int [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ snwd: int [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ tmax: chr [1:2595176] NA NA NA NA ...
    ##  $ tmin: chr [1:2595176] NA NA NA NA ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   id = col_character(),
    ##   ..   date = col_date(format = ""),
    ##   ..   prcp = col_integer(),
    ##   ..   snow = col_integer(),
    ##   ..   snwd = col_integer(),
    ##   ..   tmax = col_character(),
    ##   ..   tmin = col_character()
    ##   .. )

``` r
ny_noaa %>% drop_na()
```

    ## # A tibble: 1,222,433 × 7
    ##    id          date        prcp  snow  snwd tmax  tmin 
    ##    <chr>       <date>     <int> <int> <int> <chr> <chr>
    ##  1 USC00300023 1981-01-03     0     0     0 -122  -206 
    ##  2 USC00300023 1981-01-05     0     0     0 -56   -178 
    ##  3 USC00300023 1981-01-12     0     0     0 -122  -306 
    ##  4 USC00300023 1981-01-13     0     0     0 -67   -289 
    ##  5 USC00300023 1981-01-15     0     0     0 -50   -106 
    ##  6 USC00300023 1981-01-17     0     0     0 -11   -150 
    ##  7 USC00300023 1981-01-20     0     0     0 61    -67  
    ##  8 USC00300023 1981-01-21     0     0     0 17    -106 
    ##  9 USC00300023 1981-01-22     0     0     0 6     -50  
    ## 10 USC00300023 1981-01-23   117   127    76 22    -44  
    ## # … with 1,222,423 more rows

*There are 7 variables and 2595176 observations in this data, only
1222433 observations contain no missing values. The dataset identify
each observation with id and date, and then describes prcp,snow,snwd and
temperature min&max in each observation.*

## tidy the data

``` r
tidy_ny = ny_noaa %>% 
  separate(col = date, into = c("year","month","day")) %>% 
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>%
  mutate(tmax = as.numeric(tmax)/10, tmin = as.numeric(tmin)/10, prcp = prcp/10)

str(tidy_ny)
```

    ## tibble [2,595,176 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ id   : chr [1:2595176] "US1NYAB0001" "US1NYAB0001" "US1NYAB0001" "US1NYAB0001" ...
    ##  $ year : int [1:2595176] 2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
    ##  $ month: int [1:2595176] 11 11 11 11 11 11 11 11 11 11 ...
    ##  $ day  : int [1:2595176] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ prcp : num [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ snow : int [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ snwd : int [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ tmax : num [1:2595176] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ tmin : num [1:2595176] NA NA NA NA NA NA NA NA NA NA ...

*the most commonly observed values for snowfall is 0, as normally, there
is no snow.*

## plot

``` r
tidy_ny %>% 
  filter(month == c(1,7), !is.na(tmax)) %>% 
  group_by(id,year,month) %>% 
  summarize(ave_tmax = mean(tmax)) %>% 
  ggplot(aes(x = year, y = ave_tmax)) + 
  geom_point() + 
  facet_grid(.~month)
```

    ## `summarise()` has grouped output by 'id', 'year'. You can override using the
    ## `.groups` argument.

![](hw3_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> *The ave_tmax
in July is significantly higher than ave_tmax in January. Outliers
exist*

## tmax vs tmin

``` r
tidy_ny %>% 
  drop_na(c(tmax,tmin)) %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_hex()
```

![](hw3_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## snowfall distribution

``` r
tidy_ny %>% 
  filter(snow > 0,snow < 100) %>% 
  ggplot(aes(x = snow, y = factor(year))) + 
  geom_density_ridges(scale = .85)
```

    ## Picking joint bandwidth of 3.76

![](hw3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
