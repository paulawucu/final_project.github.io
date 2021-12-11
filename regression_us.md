regression for us
================

``` r
library(tidyverse)
library(viridis)
library(modelr)
library(plyr)
library(readxl)


knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 6,
  out.width = "90%"
)

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

load the data for poverty

## Build a read function

``` r
file_base = "data_regression_us/povety/poverty_us_"

read_function_p = function(file) {
  
  poverty_data = 
    file_name = str_c(file_base, file)
    read_csv(file_name) %>% 
    select(NAME, S1701_C02_001E)
    
  poverty_data =
    poverty_data[-1,] %>% 
    mutate(
      poverty_population = as.numeric(S1701_C02_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C02_001E)) %>% 
    relocate(state)
    
  
}
```

## load the data for poverty

``` r
poverty_2015 = 
  read_csv("data_regression_us/povety/poverty_us_2015.csv") %>% 
  select(NAME, S1701_C03_001E)
```

    ## Rows: 53 Columns: 368

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  poverty_2015 =
    poverty_2015[-1,] %>% 
    mutate(
      poverty_population_2015 = as.numeric(S1701_C03_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C03_001E)) %>% 
    relocate(state)
  
poverty_2016 = 
  read_csv("data_regression_us/povety/poverty_us_2016.csv") %>% 
  select(NAME, S1701_C03_001E)
```

    ## Rows: 53 Columns: 368

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  poverty_2016 =
    poverty_2016[-1,] %>% 
    mutate(
      poverty_population_2016 = as.numeric(S1701_C03_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C03_001E)) %>% 
    relocate(state)
  
poverty_2017 = 
  read_csv("data_regression_us/povety/poverty_us_2017.csv") %>% 
  select(NAME, S1701_C03_001E)
```

    ## Rows: 53 Columns: 368

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  poverty_2017 =
    poverty_2017[-1,] %>% 
    mutate(
      poverty_population_2017 = as.numeric(S1701_C03_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C03_001E)) %>% 
    relocate(state)
  
poverty_2018 = 
  read_csv("data_regression_us/povety/poverty_us_2018.csv") %>% 
  select(NAME, S1701_C03_001E)
```

    ## Rows: 53 Columns: 368

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  poverty_2018 =
    poverty_2018[-1,] %>% 
    mutate(
      poverty_population_2018 = as.numeric(S1701_C03_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C03_001E)) %>% 
    relocate(state)
  
poverty_2019 = 
  read_csv("data_regression_us/povety/poverty_us_2019.csv") %>% 
  select(NAME, S1701_C03_001E)
```

    ## Rows: 53 Columns: 368

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (368): GEO_ID, NAME, S1701_C01_001E, S1701_C01_001M, S1701_C01_002E, S17...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  poverty_2019 =
    poverty_2019[-1,] %>% 
    mutate(
      poverty_population_2019 = as.numeric(S1701_C03_001E),
      state = NAME
    ) %>% 
    select(-c(NAME, S1701_C03_001E)) %>% 
    relocate(state)
  
poverty_overall =
  join_all(list(poverty_2019, poverty_2018, poverty_2017, poverty_2016, poverty_2015)) %>% 
  group_by(state) %>% 
  mutate(ave_poverty = 
           0.01 * ave(poverty_population_2019,
               poverty_population_2018,
               poverty_population_2017,
               poverty_population_2016,
               poverty_population_2015)) %>% 
  select(state, ave_poverty)
```

    ## Joining by: state

    ## Joining by: state
    ## Joining by: state
    ## Joining by: state

## load the population data

``` r
population_us = 
  read_csv("data_regression_us/population/ACSDT5Y2019.B01003_data_with_overlays_2021-11-16T170855.csv")
```

    ## Rows: 53 Columns: 4

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): GEO_ID, NAME, B01003_001E, B01003_001M

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
population_us = 
  population_us[-1,] %>% 
  mutate(
    state = NAME,
    population = as.numeric(B01003_001E)
  ) %>% 
select(state, population)
```

## load the education data

load the education data for 2015

``` r
education_2015 =
  read_csv("data_regression_us/education/education_us_2015.csv")
```

    ## Rows: 53 Columns: 52

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (52): B15003_001E, B15003_001M, B15003_002E, B15003_002M, B15003_003E, B...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
education_2015 = 
  education_2015[-1,] %>% 
  select(B15003_022E, B15003_023E, B15003_024E, B15003_025E, NAME) %>% 
  mutate(
    number_bachelor = as.numeric(B15003_022E),
    number_master = as.numeric(B15003_023E),
    number_profesional = as.numeric(B15003_024E),
    number_doctor = as.numeric(B15003_025E),
    state = NAME
  ) %>% 
  select(state, number_bachelor, number_master, number_profesional, number_doctor) %>% 
  mutate(sum_high_educ_2015 = number_bachelor + number_master + number_profesional + number_doctor) %>% 
  select(state, sum_high_educ_2015)
```

load the education data for 2016

``` r
education_2016 =
  read_csv("data_regression_us/education/education_us_2016.csv")
```

    ## Rows: 53 Columns: 52

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (52): B15003_001E, B15003_001M, B15003_002E, B15003_002M, B15003_003E, B...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
education_2016 = 
  education_2016[-1,] %>% 
  select(B15003_022E, B15003_023E, B15003_024E, B15003_025E, NAME) %>% 
  mutate(
    number_bachelor = as.numeric(B15003_022E),
    number_master = as.numeric(B15003_023E),
    number_profesional = as.numeric(B15003_024E),
    number_doctor = as.numeric(B15003_025E),
    state = NAME
  ) %>% 
  select(state, number_bachelor, number_master, number_profesional, number_doctor) %>% 
  mutate(sum_high_educ_2016 = number_bachelor + number_master + number_profesional + number_doctor) %>% 
  select(state, sum_high_educ_2016)
```

load the education data for 2017

``` r
education_2017 =
  read_csv("data_regression_us/education/education_us_2017.csv")
```

    ## Rows: 53 Columns: 52

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (52): B15003_001E, B15003_001M, B15003_002E, B15003_002M, B15003_003E, B...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
education_2017 = 
  education_2017[-1,] %>% 
  select(B15003_022E, B15003_023E, B15003_024E, B15003_025E, NAME) %>% 
  mutate(
    number_bachelor = as.numeric(B15003_022E),
    number_master = as.numeric(B15003_023E),
    number_profesional = as.numeric(B15003_024E),
    number_doctor = as.numeric(B15003_025E),
    state = NAME
  ) %>% 
  select(state, number_bachelor, number_master, number_profesional, number_doctor) %>% 
  mutate(sum_high_educ_2017 = number_bachelor + number_master + number_profesional + number_doctor) %>% 
  select(state, sum_high_educ_2017)
```

load the education data for 2018

``` r
education_2018 =
  read_csv("data_regression_us/education/education_us_2018.csv")
```

    ## Rows: 53 Columns: 52

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (52): B15003_001E, B15003_001M, B15003_002E, B15003_002M, B15003_003E, B...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
education_2018 = 
  education_2018[-1,] %>% 
  select(B15003_022E, B15003_023E, B15003_024E, B15003_025E, NAME) %>% 
  mutate(
    number_bachelor = as.numeric(B15003_022E),
    number_master = as.numeric(B15003_023E),
    number_profesional = as.numeric(B15003_024E),
    number_doctor = as.numeric(B15003_025E),
    state = NAME
  ) %>% 
  select(state, number_bachelor, number_master, number_profesional, number_doctor) %>% 
  mutate(sum_high_educ_2018 = number_bachelor + number_master + number_profesional + number_doctor) %>% 
  select(state, sum_high_educ_2018)
```

load the education data for 2019

``` r
education_2019 =
  read_csv("data_regression_us/education/education_us_2019.csv")
```

    ## Rows: 53 Columns: 52

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (52): B15003_001E, B15003_001M, B15003_002E, B15003_002M, B15003_003E, B...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
education_2019 = 
  education_2019[-1,] %>% 
  select(B15003_022E, B15003_023E, B15003_024E, B15003_025E, NAME) %>% 
  mutate(
    number_bachelor = as.numeric(B15003_022E),
    number_master = as.numeric(B15003_023E),
    number_profesional = as.numeric(B15003_024E),
    number_doctor = as.numeric(B15003_025E),
    state = NAME
  ) %>% 
  select(state, number_bachelor, number_master, number_profesional, number_doctor) %>% 
  mutate(sum_high_educ_2019 = number_bachelor + number_master + number_profesional + number_doctor) %>% 
  select(state, sum_high_educ_2019)
```

overall education data

``` r
education_overall =
  join_all(list(education_2019, education_2018, education_2017, education_2016, education_2015)) %>% 
  group_by(state) %>% 
  mutate(ave_education = 
           ave(sum_high_educ_2019,
               sum_high_educ_2018,
               sum_high_educ_2017,
               sum_high_educ_2016,
               sum_high_educ_2015)) %>% 
  select(state, ave_education)
```

    ## Joining by: state
    ## Joining by: state
    ## Joining by: state
    ## Joining by: state

## education rate

``` r
education_rate = 
  left_join(education_overall, population_us) %>% 
  mutate(education_rate = ave_education / population)
```

    ## Joining, by = "state"

## unemployment

load the unemployment 2015-2019

``` r
unemployment_2015 =
  read_csv("data_regression_us/unemployment/unemployment_2015.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2015 = 
  unemployment_2015[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2015 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2015)



unemployment_2016 =
  read_csv("data_regression_us/unemployment/unemployment_2016.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2016 = 
  unemployment_2016[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2016 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2016)


unemployment_2017 =
  read_csv("data_regression_us/unemployment/unemployment_2017.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2017 = 
  unemployment_2017[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2017 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2017)


unemployment_2018 =
  read_csv("data_regression_us/unemployment/unemployment_2018.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2018 = 
  unemployment_2018[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2018 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2018)


unemployment_2019 =
  read_csv("data_regression_us/unemployment/unemployment_2019.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2019 = 
  unemployment_2019[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2019 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2019)
```

overall unemployment

## crude divorce rate

load the divorce count data from 2015-2019

``` r
unemployment_2015 =
  read_csv("data_regression_us/unemployment/unemployment_2015.csv")
```

    ## Rows: 53 Columns: 282

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (282): GEO_ID, NAME, S2301_C01_001E, S2301_C01_001M, S2301_C01_002E, S23...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_2015 = 
  unemployment_2015[-1,] %>% 
  select(NAME, S2301_C04_001E) %>% 
  mutate(
    unemployment_rate_2015 = as.numeric(S2301_C04_001E),
    state = NAME
  ) %>% 
  select(state, unemployment_rate_2015)
```

## regression overall data

``` r
overall_regression = 
  left_join(education_rate, poverty_overall) %>% 
  select(education_rate, ave_poverty)
```

    ## Joining, by = "state"

    ## Adding missing grouping variables: `state`
