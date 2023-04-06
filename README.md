# Master's Thesis
This repository has the codes and data used in my Master's in Economics Thesis. The code is written in R.

Description of files:

1. DATABASE1.xlsx: Macro-financial database.

2. maininf.R: R routine for monthly inflation forecasting. 

3. maininfI(0).R: R routine for monthly inflation forecasting after differencing acording to ADF test. 

4. maintrm.R: R routine for monthly depreciation rate forecasting. For the model in I(0) space just change the database used from dftrm2 to dftrm3.


****************************************************************************
As an alternative to create the lagged dataset (as the funs_() was deprecated) use: 
lag_functions1 <- 1:h %>% lapply(function(x) function(col) dplyr::lag(col, x))

vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions1))
    
The names of the lags are different, but this does not affect the models

For the issue see: https://github.com/tidyverse/dplyr/issues/5213
****************************************************************************
