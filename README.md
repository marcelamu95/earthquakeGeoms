# Package earthquakeGeoms

---
title: package earthquakeGeoms
author: Lina Marcela Garzon Muñoz
email: lgarzon@grupobit.net
date: 2019-11-15
---

## Data used in the package


This package will be centered around a dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA) on significant earthquakes around the world. This dataset contains information about 5,933 earthquakes over an approximately 4,000 year time span

## Included data

The functions of  this package using NOAA data frame (NOAA Significant Earthquake Database) (https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).


## earthquakeGeoms functions:
  Functions clean data:
* `eq_clean_data` and `eq_location_clean`: Two functions that takes raw NOAA data frame and returns a clean data frame 

Functions geoms to visualize some of the information in the NOAA earthquakes dataset: 
  * `geom_timeline` to visualize the times, the magnitudes and the number of deaths associated to earthquakes within certain countries.
   * `geom_timelinelabel` for adding annotations to the earthquake data and an option to subset the n_max largest earthquakes by magnitude.


Functions maps earthquake:
*  `eq_map`: A function that interactively maps (`leaflet`) the earthquakes epicenters (LATITUDE/LONGITUDE) with the desired annotation (eg Date)
* `eq_create_label`: A function that creates an HTML label that can be used as the annotation text ("Location", "Total deaths", and "Magnitude") in the leaflet map

## Aplication

Download the data from the NOAA website, saved it to your working directory and transform it to a data frame using the `read_delim` function:

```{r eval = FALSE}
filename <- system.file("extdata", "signif.txt", package="earthquakeGeoms")
library(readr)
input <- readr::read_delim(filename, delim = "\t")
```

Before using the visualization package you must clean the data with the functions "clean_data" y eq_location_clean


```{r eval = FALSE}
clean_data <- eq_clean_data(input = input )
clean_data <- eq_location_clean(clean_data)
```
To visualize the times when earthquakes occur within certain countries, the dates earthquakes occur, the magnitudes (on the Richter scale) and the number of deaths associated with each earthquake you must use the functions "geom_timeline" y geom_timeline_label


```{r eval = FALSE}
sample <- input %>%
  eq_clean_data() %>%
  eq_location_clean("LOCATION_NAME")%>%
  filter(YEAR >= 2000) %>%
  filter(COUNTRY %in% c("USA", "MEXICO"))

ggplot(data = sample, aes(x=date,y=COUNTRY, label=LOCATION_NAME)) + geom_timeline() +
 geom_timeline_label(data=sample_USA,aes(label=LOCATION_NAME),n_max=8) + 
 scale_size_continuous(name = 'Richter scale value', guide = guide_legend(order = 1)) +
  geom_timeline(aes(y = COUNTRY, color=DEATHS, size = EQ_PRIMARY)) +
 scale_color_continuous(name = '# of Deaths', guide = guide_colorbar(order = 2)) +
  new_theme()
```

for visualize aerthquake in space use the function "eq_map" for mapping the earthquake epicenters and providing some annotations with the mapped data  ( Each earthquake shown with a circle, and the radius of the circle is proportional to the earthquake's magnitude (EQ_PRIMARY)

 ```{r eval = FALSE}
clean_data %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
  eq_map(annot_col = "date")
```

