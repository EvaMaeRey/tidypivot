
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
knitr::opts_chunk$set(message = F, warning = F)
```

# Step 00. prep some data, records and flat

``` r
library(tidyverse)
library(magrittr)

Titanic %>% 
  data.frame() %>% 
  uncount(weights = Freq) ->
tidy_titanic ; tidy_titanic %>% head()
#>     Class  Sex   Age Survived
#> 3     3rd Male Child       No
#> 3.1   3rd Male Child       No
#> 3.2   3rd Male Child       No
#> 3.3   3rd Male Child       No
#> 3.4   3rd Male Child       No
#> 3.5   3rd Male Child       No

Titanic %>% 
  data.frame() ->
flat_titanic ; flat_titanic %>% head()
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0
```
