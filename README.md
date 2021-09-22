README for BestModelAICBIC
================

1.  Find best model among all independent variables based on AIC or BIC
    values.
2.  The underlying model is a simple multiple linear regression.
3.  Function to finding AIC and BIC values to identify best model for
    GLM models is underconstruction.

``` r
BestModelAICBIC::bm_aic_bic(data = mtcars, depvar = "mpg", crit = "BIC")
```

    ## $BIC
    ## [1] 161.4481
    ## 
    ## $Select_model
    ## (Intercept)          wt        qsec          am 
    ##    9.617781   -3.916504    1.225886    2.935837 
    ## 
    ## $Adj_R2
    ## [1] 0.8335561

\#Prepare dataset for different scenarios

``` r
#Curtailed dataset with reduced data and interaction terms
new_df <- dplyr::mutate(mtcars[c("mpg","disp","drat","vs","am")], interaction.term = disp*drat)

#Run model for BIC
BestModelAICBIC::bm_aic_bic(data = new_df, depvar = "mpg", crit = "AIC")
```

    ## $AIC
    ## [1] 166.1929
    ## 
    ## $Select_model
    ##      (Intercept)             disp             drat interaction.term 
    ##       3.00513602       0.05269061       6.91014671      -0.02540659 
    ## 
    ## $Adj_R2
    ## [1] 0.7572688

``` r
#Run model for AIC
BestModelAICBIC::bm_aic_bic(data = new_df, depvar = "mpg", crit = "BIC")
```

    ## $BIC
    ## [1] 172.4672
    ## 
    ## $Select_model
    ##      (Intercept)             drat interaction.term 
    ##      15.18383951       3.69892169      -0.01070707 
    ## 
    ## $Adj_R2
    ## [1] 0.747296

Notice that according to the AIC the best model includes: “disp”, “drat”
and “interaction.term”. Whereas the BIC best model only contains: “drat”
and “interaction.term”.
