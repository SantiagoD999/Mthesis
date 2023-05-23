# Master's Thesis
This repository has the codes and data used in my Master's in Economics Thesis. The code is written in R.

Description of files:

1. DATABASE1.xlsx: Macro-financial database.

2. maininf.R: R routine for monthly inflation forecasting. 

3. maininfI(0).R: R routine for monthly inflation forecasting after differencing acording to ADF test. 

4. maintrm.R: R routine for monthly depreciation rate forecasting. For the model in I(0) space just change the database used from dftrm2 to dftrm3.

5. Forecast evaluation results in Excel spreadsheets. 


****************************************************************************
As an alternative to create the lagged dataset (as the funs_() is deprecated) use: 

lag_functions1 <- lags %>% lapply(function(x) function(col) dplyr::lag(col, x)) # For max(lags)>1

vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), lag_functions1)
    
The names of the lags are different, but this does not affect the models

For the source see the issue: https://github.com/tidyverse/dplyr/issues/5213

Now, for univarate models (for which max(lags)=1), set lags=1:2 and before X<-zoo(vintage1) just use grepl to eliminate all columns that end with "fn2".
****************************************************************************

For including all seasonal dummies for some models could be tried (instead of omitting one category), just use function seasall at the beging of the maininf f.R (an small variation on seasonaldummy)instead of seasonaldummy (https://pkg.robjhyndman.com/forecast/reference/seasonaldummy.html) :





