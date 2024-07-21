Drug Consumption Data Analysis
================

``` r
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
```

> ## load dataset

``` r
data = read.csv("drug.csv")
head(data, n=3)
```

    ##   X     Age Gender                         Education Country         Ethnicity
    ## 1 0 35 - 44 Female Professional Certificate/ Diploma      UK Mixed-White/Asian
    ## 2 1 25 - 34   Male                  Doctorate Degree      UK             White
    ## 3 2 35 - 44   Male Professional Certificate/ Diploma      UK             White
    ##     Nscore   Escore   Oscore   Ascore   Cscore Impulsive       SS Alcohol
    ## 1  0.31287 -0.57545 -0.58331 -0.91699 -0.00665  -0.21712 -1.18084     CL5
    ## 2 -0.67825  1.93886  1.43533  0.76096 -0.14277  -0.71126 -0.21575     CL5
    ## 3 -0.46725  0.80523 -0.84732 -1.62090 -1.01450  -1.37983  0.40148     CL6
    ##   Amphet Amyl Benzos Caff Cannabis Choc Coke Crack Ecstasy Heroin Ketamine
    ## 1    CL2  CL0    CL2  CL6      CL0  CL5  CL0   CL0     CL0    CL0      CL0
    ## 2    CL2  CL2    CL0  CL6      CL4  CL6  CL3   CL0     CL4    CL0      CL2
    ## 3    CL0  CL0    CL0  CL6      CL3  CL4  CL0   CL0     CL0    CL0      CL0
    ##   Legalh LSD Meth Mushrooms Nicotine Semer VSA
    ## 1    CL0 CL0  CL0       CL0      CL2   CL0 CL0
    ## 2    CL0 CL2  CL3       CL0      CL4   CL0 CL0
    ## 3    CL0 CL0  CL0       CL1      CL0   CL0 CL0

> ## give factor levels to education

``` r
data$Education = factor(data$Education, levels=c("Doctorate Degree", "Masters Degree",
                                "University Degree", "Professional Certificate/ Diploma",
                                "Some College,No Certificate Or Degree", "Left School at 18 years",
                                "Left School at 17 years", "Left School at 16 years",
                                "Left School Before 16 years"))
```

> ## create seperate datasets for features and labels

``` r
labels = data[, 14:32]
features = data[, 1:13]
```

> ## check the usage level (addiction) of each drug

``` r
drug.tb = gather(labels, key="drug")

ggplot(drug.tb, aes(y=value)) +
  geom_bar() +
  facet_wrap(~drug) +
  scale_color_ipsum()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

> ## create a dataframe **temp** of indivuals who actively uses dangerous drugs

``` r
drugs = colnames(labels)
dng.drugs = c("Heroin", "Meth", "Crack", "Coke")
recent.usage = c("CL6", "CL5", "CL4")

temp = data %>%
  filter(Heroin == recent.usage | Meth == recent.usage | Crack == recent.usage | Coke == recent.usage)
```

> ## plot the distribution in education levels in **temp**

``` r
ggplot(temp, aes(y=Education)) +
  geom_bar(aes(fill=Education)) +
  scale_fill_brewer(palette="RdBu") +
  labs(title="dangerous drug usage in education groups")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

> ## plot the distribution in age levels in **temp**

``` r
ggplot(temp, aes(y=Age)) +
  geom_bar(aes(fill=Age)) + 
  scale_fill_ft() +
  labs(title="dagerous drugs usage in age groups")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
