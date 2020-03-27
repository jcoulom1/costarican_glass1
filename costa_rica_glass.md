Costa Rica Glass
================
Julie M. Coulombe
2/25/2020

Create a new column to sort by bulk rocks into a column for Rockname

Use the below chunk to filter for analysis constraints

``` r
rock_data[rock_data == ""] <- NA

rock_data_wt <- rock_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 95.0 & Total < 101.0) %>% #select rows based on Total
  filter(is.na(V2O3) & SiO2 < 90.0 & Al2O3 < 22.0 & Al2O3 > 10 & K2O > 1.0) #select rows based on elements
```

Create plot for SiO2 vs Na2O + K2O by each rock
![](costa_rica_glass_files/figure-gfm/silica%20vs%20alkali-1.png)<!-- -->

Create plot for SiO2 vs Mg\# by each rock
![](costa_rica_glass_files/figure-gfm/si%20vs%20mg%20all-1.png)<!-- -->

Create plot for SiO2 vs FeO by each rock
![](costa_rica_glass_files/figure-gfm/si%20vs%20fe%20all-1.png)<!-- -->

Create plot for Al2O3 vs TiO by each rock
![](costa_rica_glass_files/figure-gfm/al%20vs%20ti%20all-1.png)<!-- -->

Create a TAS Diagram as a background layer

![](costa_rica_glass_files/figure-gfm/print%20TAS-1.png)<!-- -->

Plot Alkali by Rock Name
![](costa_rica_glass_files/figure-gfm/alkali%20by%20rockname-1.png)<!-- -->

Overlay Alkali Plot on TAS Diagram

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](costa_rica_glass_files/figure-gfm/overlay-1.png)<!-- -->

Now to look at plots of all rocks together

Plot Silica vs Mg\# by rock
![](costa_rica_glass_files/figure-gfm/si%20vs%20mg-1.png)<!-- -->

Plot Silica vs Iron by rock
![](costa_rica_glass_files/figure-gfm/si%20vs%20fe-1.png)<!-- -->

Plot Aluminum vs Titanium by rock
![](costa_rica_glass_files/figure-gfm/al%20vs%20ti-1.png)<!-- -->
