---
title: "Final Project"
author: "Ali Haidar"
output: 
  html_document:
    theme: cerulean
    highlight: default
editor_options: 
  chunk_output_type: console
---

```{r, include = FALSE}
library(tidyverse)
library(viridis)
evpop <- read_csv("Electric_Vehicle_Population_Data.csv")
```

We would like to use our data to answer questions about how electric vehicles have improved over time based on battery range, how environmentally sustainable vehicles are, and affordability. Electric vehicles are defined as vehicles "that can be powered by an electric motor that draws electricity from a battery and is capable of being charged from an external source. An EV includes both a vehicle that can only be powered by an electric motor that draws electricity from a battery (EV) and a vehicle that can be powered by an electric motor that draws electricity from a battery and by an internal combustion engine (plug-in hybrid electric vehicle)." by the US Department of Energy.^[“Electric Vehicle (EV) Definition.” Alternative Fuels Data Center: Electric Vehicle (EV) Definition, afdc.energy.gov/laws/12660. Accessed 25 Nov. 2024.] This data set will be helpful for answering questions about the efficiency and prevalence of electric vehicles since it has over 200,000 individual observations of electric vehicles of varying ages and observes many helpful variables such as battery range and the number of electric vehicles that are manufactured. Many different researchers have asked similar questions and have posted their own findings online, a quick Google search of "electric vehicle efficiency since 2000" found several articles on the topic and many of which point to similar data. One such article that we found was published by *Sustainability by Numbers*^[Ritchie, Hannah. “The End of Range Anxiety: How Has the Range of Electric Cars Changed over Time?” The End of Range Anxiety: How Has the Range of Electric Cars Changed over Time?, Sustainability by numbers, 27 Feb. 2023, www.sustainabilitybynumbers.com/p/electric-car-range.] and looks at the battery ranges of electric vehicles over time since 2010. The use of battery range and environmental sustainability variables will give us an easy way to measure efficiency as those are two factors that are commonly associated with engine efficiency. 

The data for in this data set was collected from the state of Washington^[“State of Washington - Electric Vehicle Population Data.” Catalog, Publisher data.wa.gov, 22 Nov. 2024, catalog.data.gov/dataset/electric-vehicle-population-data.], there are vehicles as old as 1997 and as new as 2025 and the data is frequently updated, as recently as 11/22/2024. The data was collected by finding every electric/hybrid vehicle registered through the state of Washington's department of licensing. There are 205,439 individual vehicles in the data set with 17 observations tied to each vehicle. Some of the important observations that we will likely use in our work are model year (the year of vehicle production), Electric Range (a good measure of the efficiency/quality of electric vehicles), and Clean Alternative Fuel Vehicle (CAFV) Eligibility (whether or not each vehicle qualifies as a CAFV which is another measure of efficiency).


```{r, fig.cap = "Fig 1, Electric vehicle manufacturing has greatly increased over time. This data were collected by the Washington State Department of Licensing (DOL), Year refers to the year that each vehicle was manufactured and the y axis represents the number of electric vehicles produced in that year.", fig.alt = "This figure is bar graph showing amount of electric vehicles manufactured over time. The x axis represents the year in which the vehicles were manufactured and it ranges from 1997 to 2025. The y axis represents the amount of electric vehicles manufactured and ranges from 0 to 60,000. The two different types of electic vehicles manufactured are Battery electric vehicle (BEV) which are represented with a red border and Plug-in hybrid (PHEV) which are represented with a blue border. the figure shows a sharp increase in the amount of electric vehicles over time, with a peak around 2023. The figure also shows that there are more BEVs manufactured than PHEVs throughout time but especially after 2012.", echo = FALSE}
ggplot(evpop, aes(x = `Model Year`, fill = `Electric Vehicle Type`)) +
  geom_bar() + 
  labs(x = "Year", y = "Amount of Electric Vehicles Manufactured", title = "Electric vehicles Manufactured Over Time") +
  scale_fill_viridis_d()
```
Interpretation: Our first figure shows that electric vehicle production has increased greatly over time, with a peak around 2023, and that Battery electric vehicles are manufactured more than plug-in hybrid electric vehicles. The first PHEVs were manufactured around 2010 while BEVs have been manufactured as early as 1997. Between 2010 and 2013, BEVs and PHEVs were manufactured at almost the same rate but after 2013 BEVs were manufactured at a much higher rate. 

```{r fig.cap = "Fig 2, Electric vehicle manufacturing has greatly increased over time. This data were collected by the Washington State Department of Licensing (DOL), Vehicle Make refers to the maufacturer for each vehicle and the y axis represents the number of electric vehicles produced by each manufacturer", fig.alt = "This figure displays a series of line graphs illustrating the Clean Alternative Fuel Vehicle (CAFV) eligibility trends for the top five electric vehicle manufacturers over the years. Each facet represents a different manufacturer. The x-axis shows the model years, and the y-axis indicates the number of vehicles. The lines are color-coded based on CAFV eligibility: yellow for not eligible due to low battery range, green for unknown eligibility due to unresearched battery range, and blue for CAFV eligible. The graph shows that Tesla manufactured the most electric vehicles in Washington State. Additionally, most vehicles fall under unknown CAFV eligibility, followed by CAFV-eligible vehicles, with a small proportion ineligible due to low battery range.", echo = FALSE, message = FALSE, warning = FALSE}

# Summarize data for top 5 makes, grouped by year and CAFV eligibility

top_5_makes <- evpop %>%
  count(Make, name = "total_manufactured") %>%
  arrange(desc(total_manufactured)) %>%
  slice_head(n = 5)

# Filter the original dataset to include only the top 5 makes
evpop_top_5 <- evpop %>%
  filter(Make %in% top_5_makes$Make)

evpop_top_5_curve <- evpop %>%
  filter(Make %in% top_5_makes$Make) %>%
  count(Make, `Model Year`, `Clean Alternative Fuel Vehicle (CAFV) Eligibility`, name = "count")

# Plot the data as a curve with facets
ggplot(evpop_top_5_curve, aes(x = `Model Year`, y = count, color = `Clean Alternative Fuel Vehicle (CAFV) Eligibility`)) +
  geom_line(size = 1) +
  facet_wrap(~Make, scales = "free_y") +
  labs(
    x = "Year",
    y = "Number of Vehicles",
    title = "(CAFV) Eligibility Over the Years for Top 5 Electric Vehicle Makes",
    color = "CAFV Eligibility"
  ) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()
```

Interpretation: Our second figure shows that over the last 5 years in the state of Washington, Tesla was the most company to manufacture electric vehicles and we can also see that among all the kind of electric vehicles the Clean Alternative Fuel Vehicle (CAFV) eligibility was unkown as battery range has not been researched more than the eligible clean alternative fuel vehicles. Nonetheless, a small amount of cars wasn't eligible due to the low battery range.


```{r fig.cap = "Plug in hybrid electric vehicles are often not Clean Alternative Fuel Vehicle eligible while battery powered electric vehicles are eligible more often. This data was collected by the Washington State Department of Licensing (DOL), The x axis represents the CAFV eligibility of different electric vehicles and the y axis represents how many of each power type have been tested. ", fig.alt = "The graph is a bar graph with three bars representing the CAFV eligibility level of electric vehicles with levels of Clean Alternative Fuel Vehicle Eligible, Not eligible due to low battery range, and Eligibility unknown as battery range has not been researched. The y axis represents the number of cars for each of the three levels of eligibility and ranges from 0 to around 120,000. The three bars are separated by electric vehicle type, with plug-in hybrid being represented in yellow and Battery electric vehicles represented with dark blue. The graph shows that lots of plug-in hybrid vehicles are not eligible for CAFV while lots of Battery powered electric vehicles have yet to be tested enough to earn eligibility. Both battery electric vehicles and plug-in hybrids have been determined as CAFV eligible but there are more battery powered vehicles than hybrids in that category.", echo = FALSE, message = FALSE}

ggplot(evpop, aes(x = `Clean Alternative Fuel Vehicle (CAFV) Eligibility`, fill = `Electric Vehicle Type`)) +
  geom_bar() +
  labs( x = "CAFV Eligibility", 
        y =  "Count", 
        title = "CAFV Eligibility by Vehicle Type") +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
```

