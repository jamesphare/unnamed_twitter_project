Unnamed Twitter Project
================
James Hare
7/11/2020

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rtweet)
```

    ## 
    ## Attaching package: 'rtweet'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
library(tidytext)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

I wonder who my favorite tweeters are.

``` r
#This is the code to get favorites, but it shouldn't run automatically due to
#rate limits
#
#james <- get_favorites("ProsccoSocialst", n = 3000)
#write_as_csv(james, "data/james.csv")

james <- read_csv("data/james.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   created_at = col_datetime(format = ""),
    ##   display_text_width = col_double(),
    ##   is_quote = col_logical(),
    ##   is_retweet = col_logical(),
    ##   favorite_count = col_double(),
    ##   retweet_count = col_double(),
    ##   quote_count = col_logical(),
    ##   reply_count = col_logical(),
    ##   symbols = col_logical(),
    ##   ext_media_type = col_logical(),
    ##   quoted_created_at = col_datetime(format = ""),
    ##   quoted_favorite_count = col_double(),
    ##   quoted_retweet_count = col_double(),
    ##   quoted_followers_count = col_double(),
    ##   quoted_friends_count = col_double(),
    ##   quoted_statuses_count = col_double(),
    ##   quoted_verified = col_logical(),
    ##   retweet_status_id = col_logical(),
    ##   retweet_text = col_logical(),
    ##   retweet_created_at = col_logical()
    ##   # ... with 21 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
my_fav_tweeters <- james %>%
        count(screen_name, sort = TRUE)

head(my_fav_tweeters, 10)
```

    ## # A tibble: 10 x 2
    ##    screen_name         n
    ##    <chr>           <int>
    ##  1 akbarjenkins      223
    ##  2 kevinbaker        159
    ##  3 sarahljaffe       153
    ##  4 UrsulaLawrence     96
    ##  5 PatBlanchfield     87
    ##  6 WaywardWinifred    76
    ##  7 StephenMolldrem    70
    ##  8 KateAronoff        57
    ##  9 JoelBordeaux       52
    ## 10 socialistdogmom    48

I wonder who their favorites are.

``` r
#Code that shouldn't run automatically due to rate limits
#
#my_favs_favs <- get_favorites(my_fav_tweeters$screen_name, n = 300)
#write_as_csv(my_favs_favs, "data/my_favs_favs.csv")

my_favs_favs <- read_csv("data/my_favs_favs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   created_at = col_datetime(format = ""),
    ##   display_text_width = col_double(),
    ##   is_quote = col_logical(),
    ##   is_retweet = col_logical(),
    ##   favorite_count = col_double(),
    ##   retweet_count = col_double(),
    ##   quote_count = col_logical(),
    ##   reply_count = col_logical(),
    ##   symbols = col_logical(),
    ##   ext_media_type = col_logical(),
    ##   quoted_created_at = col_datetime(format = ""),
    ##   quoted_favorite_count = col_double(),
    ##   quoted_retweet_count = col_double(),
    ##   quoted_followers_count = col_double(),
    ##   quoted_friends_count = col_double(),
    ##   quoted_statuses_count = col_double(),
    ##   quoted_verified = col_logical(),
    ##   retweet_status_id = col_logical(),
    ##   retweet_text = col_logical(),
    ##   retweet_created_at = col_logical()
    ##   # ... with 21 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 1 parsing failure.
    ##  row     col           expected actual                    file
    ## 2182 symbols 1/0/T/F/TRUE/FALSE   TSLA 'data/my_favs_favs.csv'

``` r
my_favs_fav_tweeters <- my_favs_favs %>%
        count(screen_name, sort = TRUE)

head(my_favs_fav_tweeters, 10)
```

    ## # A tibble: 10 x 2
    ##    screen_name         n
    ##    <chr>           <int>
    ##  1 WaywardWinifred    82
    ##  2 cushbomb           36
    ##  3 re_colston         26
    ##  4 triofrancos        23
    ##  5 BrandyLJensen      18
    ##  6 BigMeanInternet    17
    ##  7 tgracchus1848      17
    ##  8 onesarahjones      16
    ##  9 AyoCaesar          14
    ## 10 melissagira        14

I’m going to go ahead and follow anyone on this list who I’m not already
following.

Let’s see which words show up most often in tweets that I have
favorited.

``` r
remove_reg <- "&amp;|&lt;|&gt;"
tidy_james_favs <- james %>%
        filter(!str_detect(text, "^RT")) %>%
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(word, text, token = "tweets") %>%
        filter(
                !word %in% stop_words$word,!word %in% str_remove_all(stop_words$word, "'"),
                str_detect(word, "[a-z]")
        )
```

    ## Using `to_lower = TRUE` with `token = 'tweets'` may not preserve URLs.

``` r
word_count <- tidy_james_favs %>%
        filter(!str_detect(word, "^@")) %>%
        count(word, sort = TRUE) %>%
        #"people" shoes up way to often and doesn't tell us much
        filter(word != "people")

head(word_count, 20)
```

    ## # A tibble: 20 x 2
    ##    word        n
    ##    <chr>   <int>
    ##  1 police    161
    ##  2 time      128
    ##  3 black     115
    ##  4 white      99
    ##  5 day        89
    ##  6 trump      78
    ##  7 bernie     75
    ##  8 fucking    75
    ##  9 bad        63
    ## 10 love       63
    ## 11 biden      62
    ## 12 shit       58
    ## 13 cops       57
    ## 14 life       56
    ## 15 racist     56
    ## 16 read       54
    ## 17 vote       53
    ## 18 lives      51
    ## 19 left       49
    ## 20 real       49

I guess I’m interested in Left politics and the uprising against racist
police violence.

Let’s see what words my favorites are favoriting.

``` r
tidy_favs_favs <- my_favs_favs %>%
        filter(!str_detect(text, "^RT")) %>%
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(word, text, token = "tweets") %>%
        filter(
                !word %in% stop_words$word,!word %in% str_remove_all(stop_words$word, "'"),
                str_detect(word, "[a-z]")
        )
```

    ## Using `to_lower = TRUE` with `token = 'tweets'` may not preserve URLs.

``` r
favs_word_count <- tidy_favs_favs %>%
        filter(!str_detect(word, "^@")) %>%
        count(word, sort = TRUE) %>%
        #"people" shoes up way to often and doesn't tell us much
        filter(word != "people")

head(favs_word_count, 20)
```

    ## # A tibble: 20 x 2
    ##    word        n
    ##    <chr>   <int>
    ##  1 time      119
    ##  2 black      99
    ##  3 love       88
    ##  4 cancel     73
    ##  5 letter     72
    ##  6 culture    70
    ##  7 day        68
    ##  8 white      68
    ##  9 read       64
    ## 10 life       63
    ## 11 police     63
    ## 12 climate    62
    ## 13 history    58
    ## 14 bad        57
    ## 15 trump      57
    ## 16 lot        54
    ## 17 real       54
    ## 18 feel       49
    ## 19 fuck       49
    ## 20 book       47

A lot of overalap, but they seem more interested in that whole cancel
culture letter than I am.

Now let’s take a look at the words that I use in my own tweets.

``` r
#james_tweets <- get_timeline("ProsccoSocialst", n = 3000)
#write_as_csv(james_tweets, "data/james_tweets.csv")
james_tweets <- read_csv("data/james_tweets.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   created_at = col_datetime(format = ""),
    ##   display_text_width = col_double(),
    ##   is_quote = col_logical(),
    ##   is_retweet = col_logical(),
    ##   favorite_count = col_double(),
    ##   retweet_count = col_double(),
    ##   quote_count = col_logical(),
    ##   reply_count = col_logical(),
    ##   symbols = col_logical(),
    ##   ext_media_type = col_logical(),
    ##   quoted_created_at = col_datetime(format = ""),
    ##   quoted_favorite_count = col_double(),
    ##   quoted_retweet_count = col_double(),
    ##   quoted_followers_count = col_double(),
    ##   quoted_friends_count = col_double(),
    ##   quoted_statuses_count = col_double(),
    ##   quoted_verified = col_logical(),
    ##   retweet_created_at = col_datetime(format = ""),
    ##   retweet_favorite_count = col_double(),
    ##   retweet_retweet_count = col_double()
    ##   # ... with 22 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
tidy_james_tweets <- james_tweets %>%
        filter(!str_detect(text, "^RT")) %>%
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(word, text, token = "tweets") %>%
        filter(
                !word %in% stop_words$word,!word %in% str_remove_all(stop_words$word, "'"),
                str_detect(word, "[a-z]")
        )
```

    ## Using `to_lower = TRUE` with `token = 'tweets'` may not preserve URLs.

``` r
tweets_word_count <- tidy_james_tweets %>%
        filter(!str_detect(word, "^@")) %>%
        count(word, sort = TRUE) %>%
        #"people" shoes up way to often and doesn't tell us much
        filter(word != "people")

head(tweets_word_count, 20)
```

    ## # A tibble: 20 x 2
    ##    word           n
    ##    <chr>      <int>
    ##  1 day           33
    ##  2 trump         25
    ##  3 bernie        19
    ##  4 time          19
    ##  5 bush          15
    ##  6 hard          14
    ##  7 worse         14
    ##  8 care          12
    ##  9 president     12
    ## 10 read          12
    ## 11 twitter       12
    ## 12 vote          12
    ## 13 real          11
    ## 14 social        11
    ## 15 guess         10
    ## 16 lot           10
    ## 17 post          10
    ## 18 pretty        10
    ## 19 book           9
    ## 20 democratic     9
