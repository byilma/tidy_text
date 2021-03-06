Tidy Text
================
Binyam Yilma
12/28/2020

Setup

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidytext)

library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Get Data

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_reviews = 
  tibble(
    page = 1:100,
    urls = str_c(url_base, page)) %>% 
  mutate(reviews = map(urls, read_page_reviews)) %>% 
  unnest(reviews) %>%
  mutate(review_num = row_number()) %>% 
  relocate(page, review_num)
```

## Do some tidy text stuff

Extracting words from the `text` field from the dynamite\_reviews
dataframe. 1 - can I extract the words? Yes.

``` r
dynamite_words = 
  dynamite_reviews %>% 
  unnest_tokens(word, input = text) %>% 
  select(-urls)
```

2 - can I remove unnecessary words?

``` r
dynamite_words = 
  anti_join(dynamite_words, stop_words) # getting rid of stop words from our words - the anti-join here is saying, keep everything in this data frame that's not in the list of the "stop_words"
```

    ## Joining, by = "word"

``` r
stop_words %>% View()
```

Usual tidyverse stuff …

``` r
dynamite_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(
    word = fct_reorder(word, n)
  ) %>% 
  ggplot(aes(y = word, x =n)) +
  geom_bar(stat = "identity")
```

    ## Selecting by n

<img src="tidytext_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

# Compare across groups

Groups I want to compare are 1 star and 5 star reviews.

How many words are coming out of 1 star reviews? 5 star reviews?

``` r
dynamite_words %>% 
  filter(stars %in% c(1,5)) %>% 
  group_by(stars) %>% 
    summarize (n = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   stars     n
    ##   <dbl> <int>
    ## 1     1   665
    ## 2     5  4478

Let’s count words in these groups

What are the top 5 words that are appear the most

``` r
dynamite_words %>% 
  filter(stars %in% c(1, 5)) %>% 
  group_by(stars) %>% 
  count(word) %>% 
  top_n(5)
```

    ## Selecting by n

    ## # A tibble: 10 x 3
    ## # Groups:   stars [2]
    ##    stars word        n
    ##    <dbl> <chr>   <int>
    ##  1     1 dumb        8
    ##  2     1 dvd        10
    ##  3     1 movie      50
    ##  4     1 time        9
    ##  5     1 watch       8
    ##  6     5 classic   102
    ##  7     5 funny     123
    ##  8     5 love      136
    ##  9     5 movie     430
    ## 10     5 time       99

Now, let’s say we’re interested in something like: does the word “dumb”
appear more often in 1 star reviews relative to 5 star reviews? So, this
is where the “odds ratio”-esque thing comes in.

We now need an “odds ratio”

``` r
word_ratios = dynamite_words %>% 
  filter(stars %in% c(1, 5)) %>% 
  count(word, stars) %>% # we're going to focus on words that get used somehow frequently
  group_by(word) %>%  
  filter(sum(n) >= 5) %>% # focusing on words that appear at least 5 times 
  ungroup(word) %>% 
  pivot_wider(
    names_from = stars, 
    values_from = n, 
    names_prefix = "stars_",
    values_fill = 0
  ) %>% 
  mutate(
    stars_1_odds = (stars_1 + 1) / (sum(stars_1) + 1),
    stars_5_odds = (stars_5 + 1) / (sum(stars_5) + 1),
    log_OR = log(stars_5_odds / stars_1_odds) #high values of these means that the word appears more times in 5 stars than 1 stars; lower values than 0 means that the word appears more times in the 1 stars
  )
```

Can we do something useful with this ^^ ?

Trying to identify words that are more likely to happen in the 1 star
reviews than 5 star reviews

``` r
word_ratios %>% 
  mutate(
    pos_log_OR = ifelse(log_OR > 0, "5 star > 1 star", "1 star > 5 star")
  ) %>%
  group_by(pos_log_OR) %>% 
  top_n(10, abs(log_OR)) %>%  # getting the most extreme values of the pos_log_OR stuff
  ungroup() %>% 
  mutate(word = fct_reorder(word, log_OR)) %>% 
  ggplot(aes(y = word, x = log_OR, fill = pos_log_OR)) + 
  geom_bar(stat = "identity")
```

<img src="tidytext_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

## Sentiment Analysis

Load lexicon

``` r
bing_sentiments = get_sentiments("bing")
```

``` r
dynamite_sentiments = inner_join(dynamite_words, bing_sentiments) %>% 
  count(review_num, sentiment) %>% 
    pivot_wider(
      names_from = sentiment, 
      values_from = n,
      values_fill = 0
    )  %>% 
    mutate(
      review_sentiment = positive - negative
    ) %>% 
    select(review_num, review_sentiment)
```

    ## Joining, by = "word"

Combine with full data

``` r
dynamite_sentiments = 
  left_join(dynamite_sentiments, dynamite_reviews) %>% 
    select(-urls)
```

    ## Joining, by = "review_num"

Plot

``` r
dynamite_sentiments %>% 
  mutate(
    review_num = factor(review_num),
    review_num = fct_reorder(review_num, review_sentiment)
  ) %>% 
  ggplot(aes(x = review_num, y = review_sentiment, fill = stars)) + 
  geom_bar(stat = "identity") + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
```

<img src="tidytext_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

Can we look at positive and negative reviews

``` r
dynamite_sentiments %>% 
  filter(review_sentiment == max(review_sentiment))
```

    ## # A tibble: 1 x 6
    ##   review_num review_sentiment  page title       stars text                      
    ##        <int>            <int> <int> <chr>       <dbl> <chr>                     
    ## 1        701                6    71 love love …     5 love love love love love.…
