El Vaquero Analysis
================
Phillip Sanderell

## Import Data

Load the scraped review data.

``` r
library(tidyverse)
el_vaq <- read_csv("el vaquero yelp reviews.csv")
```

## Clean Data

``` r
el_vaq <- 
  el_vaq %>% 
  select(-...1) %>% 
  mutate(original_link = link) %>% 
  mutate(link = str_remove(link, "https://www.yelp.com/biz/el-vaquero-mexican-restaurant-columbus-")) %>% 
  mutate(link = str_extract(link, "^\\d+")) %>% 
  mutate(location = case_when(link == 10 ~ "Riverside",
                              link == 15 ~ "Olentangy",
                              TRUE ~ "check")) %>% 
  select(-link) %>% 
  mutate(state_or_country = str_remove(user_locations, "(.*)\\s")) %>% 
  mutate(city = if_else(str_count(user_locations, ",") > 1,
                              str_extract(user_locations, ",\\s(.*),"),
                              str_extract(user_locations, "(.*),"))) %>% 
  mutate(city = str_remove(city, "^,") %>% 
           str_remove(",$") %>% 
           str_trim())
```

## Quick data vizes

Make the vizes pretty

``` r
library(MetBrewer)
theme_set(theme_minimal())
```

Side-by-side proportional bar plot of ratings distribution

``` r
el_vaq %>% 
  mutate(ratings = fct_reorder(as.factor(ratings), -ratings)) %>% 
  ggplot(aes(x = location, fill = ratings)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of star ratings",
       x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = met.brewer("Tam", 5))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)

``` r
el_vaq %>% 
  mutate(international = state_or_country %in% c("France", "Ireland", "Singapore")) %>% 
  mutate(state_or_country = fct_lump_min(state_or_country, 5)) %>% 
  mutate(state_or_country = if_else(international, 
                                    "International", 
                                    state_or_country %>% as.character())) %>% 
  count(location, state_or_country, sort = TRUE) %>% 
  group_by(location) %>% 
  mutate(state_or_country = fct_reorder(state_or_country, n)) %>% 
  mutate(n = if_else(location == "Olentangy", -n, n)) %>% 
  ggplot(aes(n, state_or_country, fill = location)) +
  geom_col() + 
  labs(title = "Count of reviews by people from different locations",
       y = NULL, x = NULL, fill = NULL,
       subtitle = "Olentangy has a lot more visitors outside of Ohio") +
  scale_x_continuous(limits = c(-140, 140), labels = c("150", "100", "50", "0", "50", "100", "150")) +
  theme(legend.position="bottom") +
  scale_fill_manual(values = met.brewer("Tam", 2))
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)

``` r
el_vaq %>% 
  mutate(year = lubridate::year(dates)) %>% 
  filter(year > 2022 - 10) %>% 
  mutate(year = fct_reorder(as.factor(year), year)) %>% 
  mutate(ratings = fct_reorder(as.factor(ratings), -ratings)) %>% 
  ggplot(aes(x = year, fill = ratings)) +
  geom_bar(position = "fill") +
  facet_wrap(~location, ncol = 1) +
  labs(title = "Distribution of star ratings",
       x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = met.brewer("Tam", 5))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)

I think it would be easier to read if it was simpler

``` r
el_vaq %>% 
  mutate(year = lubridate::year(dates)) %>% 
  filter(year > 2022 - 10) %>% 
  mutate(year = fct_reorder(as.factor(year), year)) %>% 
  mutate(ratings_simple = if_else(ratings > 3.5, "4 or 5", "1, 2, or 3")) %>% 
  mutate(ratings_simple = fct_reorder(ratings_simple, -ratings)) %>% 
  ggplot(aes(x = year, fill = ratings_simple)) +
  geom_bar(position = "fill") +
  facet_wrap(~location, ncol = 1) +
  labs(title = "Distribution of star ratings",
       x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = met.brewer("Tam", 2))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)

``` r
el_vaq %>% 
  mutate(review_location = if_else(state_or_country == "OH", "Ohio",
                                   "Outside Ohio")) %>% 
  mutate(ratings = fct_reorder(as.factor(ratings), -ratings)) %>% 
  ggplot(aes(x = location, fill = ratings)) +
  geom_bar(position = "fill") +
  facet_wrap(~review_location, ncol = 1) +
  labs(title = "Distribution of star ratings",
       x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = met.brewer("Tam", 5))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)