# Hotels-Data-Analysis
For this EDA project, “Hotel booking demand” dataset is used. This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things.

title: "Data Wrangling Final Project"
author: "Group 1: James Stratton, Colton Dolezal, Pulkit Chauhan, Nikhil Khandelwal, and Ajay Dhariwal"
date: "12/2/2021"
output: html_document
---

# Hotels Data Analysis {.tabset .tabset-pills}

## Introduction

There are many decisions that a hotel operator needs to make. They need to choose where to promote their hotel. Then resources most be allocated to engaging with various distribution channels. Prices need to be set for the guest accommodations. And knowing what languages are needed at a hotel is helpful during the hiring process. Finally, revenue from the bookings needs to be determined.

The operator's questions can be summarized into four questions: Where are the hotel bookings coming from? What is the frequency of cancellations for my guests, and how likely are they to be no-shows? What is the trend in the rates that I can charge my guests? How much revenue is my hotel earning per booking, and how much is coming from each country?

Our team decided to gain insight into these issues by analyzing the Hotel Booking Demand Dataset by Antonio *et al* [1]. This dataset was compiled from the databases of a hotel located in the Algarve resort region ("Resort hotel") and one located in Lisbon ("City hotel") [1]. It is structured in terms of bookings.

Counts by country and distribution channel will be used to answer where the bookings are coming from. The proportion of guests who are repeat guests will be analyzed using a time-series analysis. Cancellation and no-show frequency will be assessed by dividing the number of offending bookings by the total number of bookings. Cancellation rate will be calculated for each country to determine which countries have the highest cancellation rates.

A time-series will be used to assess trends over time in booking prices. Prices from successful bookings will give an estimate of what the market will bear. Average rates for each month will be calculated and used to train an ARIMA model for forecasting future rates.

Revenue will be calculated from the booking data. This will involve multiplying the stay length by the average rate for the guest. Revenue salvaged from cancelled bookings will be tabulated. Average booking revenue will be used to compare bookings from different countries.

There are several ways that these analyses will help the hotel operator. Revenue and bookings by country can be used to make decisions about where to target advertising and what languages to invest in. Knowing the cancellation rate by country and distribution channel will make it easier for the hotel operator to adjust their prices to offset their risk. Setting prices using forecasts based on historical data should move the operator out of a reactionary role in setting prices and also help mitigate risk.

## Packages Required

```{r load_packages, message=FALSE, warning=FALSE}
library(tidyverse)
library(knitr)
library(lubridate)
library(patchwork)
library(ISOcodes)
library(maps)
library(forecast)
library(tseries)
library(sarima)
```

**Table 1: Package Summaries**

Package | Version | Summary
:--------|:---------|:--------
tidyverse | 1.3.1 | A compilation of R tools for data manipulation and visualization
knitr | 1.3.6 | Dynamic report generation in R, esp. for displaying tables
lubridate | 1.8.0 | A package that makes handling dates easier
patchwork | 1.1.1 | A package for combining multiple ggplot2 plots into a single figure
ISOcodes | 2021.02.24 | Contains datasets used to translate ISO country codes into country names
maps | 3.4.0 | Used to prepare and display geographical maps
forecast | 8.15 | Forecasting functions for time series modeling
tseries | 0.10-49 | Time series analysis
sarima | 0.8.5 | Generation of ARIMA models for time series modeling

## Data Preparation {.tabset .tabset-pills}

In this section, our data preparation process is described. This includes importing the data, handling missing values, and refactoring several of the variables. Due to the large number of variables, they were grouped into subsections as summarized below in Table 2. Each variable will also be briefly described, including the data gathering steps taken by Antonio et al.

**Table 2: Variable Groupings**

Subsection | Variables
:-----|:---------
Arrival Date | arrival_date_year, arrival_date_month, arrival_date_week_number, arrival_date_day_of_month
Guest Demographics | adults, children, babies, country
Reservation Status | reservation_status, reservation_status_date, is_canceled, lead_time, days_in_waiting_list
User Karma | is_repeated_guest, previous_cancellations, previous_bookings_not_cancelled
Guest Accommodations | stays_in_weekend_nights, stays_in_week_nights, reserved_room_type, assigned_room_type, meals, required_car_parking_spaces, total_of_special_requests
Booking Information | hotel, market_segment, distribution_channel, booking_changes, deposit_type, agent, company, customer_type
Average Daily Rate | adr

### Data Import

First, we need to import the data. This data was generated by Antonio *et al* [1]. Our copy of the data was obtained from the [Data Wrangling Final Project website](https://zzz1990771.github.io/data_wrangling/final-project) [3].

```{r hotels.csv import}
# Read in the hotels dataset from disk.
hotels <- read_csv(file = "./hotels.csv")
# Check the structure of the data
glimpse(hotels)
```

It looks like the Hotels dataset got imported correctly. However, there are clear issues that require correction during data preparation. For example, both `agent` and `company` are meant to be id numbers [2]. But they were imported as character variables due to the presence of `NULL` values in the original data.

### Arrival date

`arrival_date_year`, `arrival_date_month`, `arrival_date_week_number`, and `arrival_date_day_of_month` all indicate the point in time that the guest arrived at their hotel [1], [2]. No issues with the values in these variables were detected in our analysis of the dataset. Each of these variables is discussed separately, and then consolidated into a single `arrival_date` variable.

#### arrival_date_year

`arrival_date_year` stores the year that the guest arrived. In this dataset, the years of arrival range from 2015 to 2017 [1]. However, this variable was not changed to a factor variable. This is because we would have to add levels every time we added a new year. In practice, that would be necessary if we were working with a live database.

#### arrival_date_month

This variable contains information about the month that the booking arrived [1], [2]. Note that this data is stored as character strings corresponding to each month. No changes were necessary for `lubridate` to understand the month values.

#### arrival_date_week_number

`arrival_date_week_number` is a variable indicating the week that the guest arrived [1], [2]. No preparation was required for this variable.

#### arrival_date_day_of_month

`arrival_date_day_of_month` is the day of the month that a guest arrived [1], [2]. This is a numeric variable. No issues requiring data preparation were found for this variable. But it will be replaced by a consolidated `arrival_date` variable.

#### Consolidation of the Arrival Date Variables

For ease of use, we decided to merge the `arrival_date_x` variables into a single `arrival_date` variable using `lubridate`. `lubridate` functions can be used later to easily extract data by year, month, day, quarter, etc.

```{r arrival date merger}
# Consolidate the arrival dates into a single variable
hotels %>%
  # Initialize an arrival_date variable with the year, month, and day of month
  mutate(arrival_date = paste(arrival_date_year,
                              arrival_date_month,
                              arrival_date_day_of_month,
                              sep = "-")
         ) %>% 
  # Convert the arrival_date from a character to a date variable using lubridate
  mutate(arrival_date = ymd(arrival_date)) %>% 
  # Drop the redundant arrival_date variables
  select(-c(arrival_date_year,
            arrival_date_month,
            arrival_date_week_number,
            arrival_date_day_of_month)
         ) -> hotels
```

### Guest Demographics

This section deals with the `adults`, `children`, `babies`, and `country` variables. Removal of observations with zero guests and observations with `NA` for country were the main data preparation steps applied in this step.

#### adults

`adults` is the number of adults associated with a given booking [1], [2]. No preparation steps need to be taken for this variable on its own. The boxplot and histogram in Figure 1 below shows that most of the observations only had 2 guests. However, there were some larger parties. This happened because several of the bookings were made by travel agents or tour operators [1]. Therefore, we can't safely remove these observations from the dataset.

```{r adults boxplot and histogram, message=FALSE, warning=FALSE}
# Generate the boxplot for adults
hotels %>% 
  ggplot(aes(x = adults)) + 
    geom_boxplot() +
    labs(x = "Number of Adults", title = "Boxplot") -> adults_box_plot
# Generate the histogram for adults
hotels %>% 
  ggplot(aes(x = adults)) + 
    geom_histogram() +
    labs(x = "Number of Adults", title = "Histogram") -> adults_histogram
# Combine them into a single plot
adults_box_plot + adults_histogram -> adults_box_plot_and_histogram
# Add title to the combined plot and output it
adults_box_plot_and_histogram +
  plot_annotation(title = "Figure 1: Adults Boxplot and Histogram")
```

#### children

`children` is a variable indicating the number of children for a booking [1], [2]. There were `r sum(is.na(hotels[["children"]]))` `NA`s for this variable. We decided to presume that these are data entry errors for bookings that had no children. Thus, these `NA` values were imputed to be 0.

```{r children NA imputation}
# Replace children with 0 if it is NA
hotels %>% 
  mutate(children = case_when(is.na(children) ~ 0,
                              TRUE ~ children)
         ) -> hotels
```

After carrying out this preparation step, there were `r sum(is.na(hotels[["children"]]))` `NA`s remaining.

```{r children boxplot and histogram, warning=FALSE, message=FALSE}
# Generate the boxplot for children
hotels %>% 
  ggplot(aes(x = children)) + 
    geom_boxplot() +
    labs(x = "Number of Children", title = "Boxplot") -> children_box_plot
# Generate the histogram for children
hotels %>% 
  ggplot(aes(x = children)) + 
    geom_histogram() +
    labs(x = "Number of Children", title = "Histogram") -> children_histogram
# Combine them into a single plot
children_box_plot + children_histogram -> children_box_plot_and_histogram
# Add title to the combined plot and output it
children_box_plot_and_histogram +
  plot_annotation(title = "Figure 2: Children Boxplot and Histogram")
```

As the plots in Figure 2 show, the vast majority of guests didn't bring children. The largest number of children observed was `r max(hotels[["children"]])`. We decided to leave these outliers in place because it is plausible for a party to bring that many children on their trip.

#### babies

`babies` is a variable showing the number of babies associated with a given booking. The boxplot and histogram in Figure 3 was used to assess whether outlier handling was required for `babies`.

```{r babies histogram, warning=FALSE, message=FALSE}
# Generate the boxplot for babies
hotels %>% 
  ggplot(aes(x = babies)) + 
    geom_boxplot() +
    labs(x = "Number of Babies", title = "Boxplot") -> babies_box_plot
# Generate the histogram for babies
hotels %>% 
  ggplot(aes(x = babies)) + 
    geom_histogram() +
    labs(x = "Number of Babies", title = "Histogram") -> babies_histogram
# Combine them into a single plot
babies_box_plot + babies_histogram -> babies_box_plot_and_histogram
# Add title to the combined plot and output it
babies_box_plot_and_histogram +
  plot_annotation(title = "Figure 3: Babies Boxplot and Histogram")
```

Figure 3 shows that the vast majority of guests didn't bring babies with them to these hotels. The largest number of babies observed was `r max(hotels[["babies"]])`. Again, we decided not to consider these results as outliers because it is plausible that a group could bring that many babies with them. Especially for a large booking by a tour group.

#### Removal of Bookings without Guests

There were `r hotels %>% select(adults, children, babies) %>% filter(adults + children + babies < 1) %>%  nrow` observations where *no* guests were recorded for a booking. This is clearly nonsensical. We decided to remove these nonsense observations considering that only a tiny fraction of the observations have this issue.

```{r zero guest booking removal}
# Only include entries with at least one guest
hotels %>% 
  filter(adults + children + babies >= 1) -> hotels
```

After performing this manipulation, there are now `r hotels %>% select(adults, children, babies) %>% filter(adults + children + babies < 1) %>%  nrow` entries without guests.

#### country

`country` is the country of origin for the guests [1], [2]. These values are in the ISO 3166-3:2013 format [1]. This data wasn't always available in the PMS at the time of booking, and was often added after the guests checked in [1]. Note that both Antonio *et al* and the tidytuesday data dictionary incorrectly list `country` as being in ISO 31*55*-3:2013 format.

First, we converted the `r hotels %>% select(country) %>% filter(country == "NULL") %>% nrow` `NULL` values to `NA`s.

```{r Countries NULL to NA}
hotels %>% 
  mutate(country = na_if(country, "NULL")) -> hotels
```

After performing this step, all `r sum(is.na(hotels[["country"]]))` `NULL` values were converted to `NA`s. We decided to simply remove these `NA` values because each guest has to come from a country and there are only a few `NA`s compared to the size of our dataset. We also converted `country` to a factor variable.

```{r countries to factor}
# Remove NAs and convert to factor
hotels %>% 
  filter(!is.na(country)) %>% 
  mutate(country = as.factor(country)) -> hotels
```

### Reservation Status

This section discusses the `reservation_status`, `reservation_status_date`, `is_canceled`, `lead_time`, and `days_in_waiting_list`. Basically, the variables describing the guest's reservation.

#### reservation_status

`reservation_status` is a variable indicating whether the booking was canceled by the customer ("Canceled"); the guest checked in and then checked out ("Check-Out"); or never showed up without explanation to the hotel ("No-Show") [1], [2]. The only preparation this variable needed was conversion to a factor variable.

```{r conversion of reservation_status to a factor variable}
# Convert reservation_status to a factor variable
hotels %>% 
  mutate(reservation_status = as.factor(reservation_status)) -> hotels
```

#### reservation_status_date

`reservation_status_date` indicates the last time that a booking observation was changed in the PMS [1], [2]. This can be used to determine when a guest canceled or checked-out, as applicable [2]. Dates were extracted in `YYYY-MM-DD` format character strings (it is unclear whether this was how they were stored in the PMS). No changes were necessary for this variable because `read_csv` correctly interpreted it as a date variable.

#### is_canceled

`is_canceled` is a dummy variable indicating whether the booking was canceled (1) or not (0) [1], [2]. The only data preparation step for this data was conversion to a logical variable.

```{r is_cancelled boolean}
# Convert is_canceled to a logical variable
hotels %>% 
  mutate(is_canceled = as.logical(is_canceled)) -> hotels
```

#### lead_time

`lead_time` is the number of days between the booking and guest arrival at the hotel [1], [2]. This was calculated by subtracting reservation date from arrival date [1]. Looking at the boxplot for `lead_time` shown in Figure 4, we decided that any observations with a `lead_time` > 625 days. This was motivated by the gap in outliers around 625 days, and 625 days seemed like a reasonable cutoff because it corresponds to nearly 2 years worth of lead time. We handled these outliers by simply removing them from the dataset.

```{r lead_time boxplot}
# Generate a boxplot for lead_time
hotels %>% 
  ggplot(aes(lead_time)) +
    geom_boxplot() +
    labs(x = "Lead Time (days)",
         title = "Figure 4: Lead Time Boxplot")
```

Removing observations with a `lead_time` > 625 will remove only `r hotels %>% filter(lead_time > 625) %>% nrow` observations from the dataset. Therefore, we should be able to remove them safely.

```{r lead_time outlier removal}
# Only include observations with a lead time less than or equal to 625
hotels %>% 
  filter(lead_time <= 625) -> hotels
```

#### days_in_waiting_list

`days_in_waiting_list` is a variable indicating the number of days the booking was in the PMS before confirmation for the guest [1], [2]. This is a numeric variable with no `NA`s. It was calculated by subtracting the booking confirmation date from the date the booking entered the PMS [1]. We did remove any outliers that went above 200 based on the boxplot shown in Figure 5. This only removes `r hotels %>% filter(days_in_waiting_list > 200) %>% nrow()`. Thus, we should be able to safely remove them.

```{r waiting list boxplot}
# Generate a boxplot for lead_time
hotels %>% 
  ggplot(aes(days_in_waiting_list)) +
    geom_boxplot() +
    labs(x = "Days in Waiting List (days)",
         title = "Figure 5: Days in Waiting List Boxplot")
```

```{r days in waiting list removal}
hotels %>% 
  filter(days_in_waiting_list <= 200) -> hotels
```

### User Karma

The User Karma section contains variables that reflect on user behavior towards the hotel. Note that because there are travel agencies, we called it user behavior because agents have to act on behalf of guests. This section includes the `is_repeated_guest`, `previous_cancellations`, and `previous_bookings_not_cancelled` variables.

#### is_repeated_guest

`is_repeated_guest` is a variable indicating whether a booking was made by a repeat guest [1], [2]. Guests were assumed to be repeat guests if they had a profile that predated creation of the booking in question [1]. The only modification required was conversion to a logical variable.

```{r is repeated guest logical conversion}
# Convert is_repeated_guest to logical
hotels %>% 
  mutate(is_repeated_guest = as.logical(is_repeated_guest)) -> hotels
```

#### previous_cancellations

`previous_cancellations` is a numeric variable indicating how many times the user has cancelled a booking in the past [1], [2]. For bookings without an associated customer profile, a value of 0 was used by default [1]. There were no `NA` values. `previous_cancellations` above 10 were implied to be outliers based on the boxplot in Figure 6. However, we did not remove these outliers to avoid penalizing travel agents and booking companies that would have to cancel on behalf of guests.

```{r previous_cancellations boxplot}
# previous_cancellations boxplot
hotels %>% 
  ggplot(aes(previous_cancellations)) +
    geom_boxplot() +
    labs(x = "Previous Cancellations",
         title = "Figure 6: Previous Cancellations Boxplot")
```

#### previous_bookings_not_cancelled

`previous_bookings_not_cancelled` is the number of prior bookings for a customer profile that were not canceled [1], [2]. 0 was the default value for bookings that were made without a customer profile [1]. `previous_bookings_not_cancelled` did not have any `NA` values that needed removal. We tried using the boxplot in Figure 7 to assess outliers. However, it failed to indicate a clear cutoff for outliers.

```{r previous_bookings_not_cancelled}
# previous_bookings_not_cancelled boxplot
hotels %>% 
  ggplot(aes(previous_bookings_not_canceled)) +
    geom_boxplot() +
    labs(x = "Previous Bookings not Cancelled",
         title = "Figure 7: Previous Bookings not Cancelled Histogram")
```

### Guest Accommodations

This section covers the variables describing the accommodations each booking receive. Specifically, `stays_in_weekend_nights`, `stays_in_week_nights`, `reserved_room_type`, `assigned_room_type`, `meals`, `required_car_parking_spaces`, and `total_of_special_requests` are the variables included in this section.

#### stays_in_weekend_nights

`stays_in_weekend_nights` is the number of weekend nights (Saturday & Sunday) the guest stayed at the hotel, calculated by totaling the weekend nights out of the guest's stay [1], [2]. Observations over 5 weekend days were implied to be outliers by the Figure 8 boxplot. However, we decided to keep those entries, because extended stays are possible. Furthermore, they are clearly desirable to study.

```{r stays_in_weekend_nights boxplot}
# stays_in_weekend_nights boxplot
hotels %>% 
  ggplot(aes(stays_in_weekend_nights)) +
    geom_boxplot() +
    labs(x = "Stays in Weekend Nights",
         title = "Figure 8: Stays in Weekend Nights Boxplot")
```

#### stays_in_week_nights

`stays_in_week_nights` is the number of week nights the guest stayed at the hotel, calculated by totaling the week nights (Monday - Friday) out of the guest's stay [1], [2]. It did not have any `NA` values. According to the boxplot shown in Figure 9 below, anything above about 5 days could be treated as an outlier. Again, we decided not to tamper with these values because it would amputate the data for stays that last for longer than a single business week.

```{r stays_in_week_nights boxplot}
# stays_in_week_nights boxplot
hotels %>% 
  ggplot(aes(stays_in_week_nights)) +
    geom_boxplot() +
    labs(x = "Stays in Week Nights",
         title = "Figure 9: Stays in Week Nights Boxplot")
```

#### reserved_room_type

`reserved_room_type` is a variable indicating the guest's reserved room type at booking [1], [2]. The only data preparation task required for this variable was converting it to a factor variable.

```{r reserved_room_type conversion to factor}
# Convert reserved room type to a factor variable
hotels %>% 
  mutate(reserved_room_type = as.factor(reserved_room_type)) -> hotels
```

#### assigned_room_type

`assigned_room_type` is the code for the room assigned at booking [1], [2]. Hotel operations can dictate that this varies from the `reserved_room_type` [2]. The only preparation task required for this variable was converting it to a factor variable.

```{r assigned_rm_type conversion to factor}
# Convert assigned room type to a factor variable
hotels %>% 
  mutate(assigned_room_type = as.factor(assigned_room_type)) -> hotels
```

#### meal

`meal` is a categorical variable indicating which meal plan a guest chose during booking [1], [2]. HB meant "half board", a meal plan where guests received both breakfast and dinner [1]. FB meant "full board", a meal plan for all three meals [1]. BB stands for "bed and breakfast" [1]. Finally, "Undefined" and "SC" both indicate that the guest had no meal plan [1].

We prepared the `meals` variable by merging "Undefined" and "SC" into "No Meal Plan". To make the data more human readable, each abbreviation was expanded into a full label. We then converted the resulting data into a factor variable.

```{r meals data preparation}
# Relabel meal entries and then convert to a factor variable.
hotels %>% 
  mutate(meal = case_when(meal == "Undefined" ~ "No Meal Plan",
                          meal == "SC" ~ "No Meal Plan",
                          meal == "BB" ~ "Bed and Breakfast",
                          meal == "HB" ~ "Half Board",
                          meal == "FB" ~ "Full Board")
         ) %>% 
  mutate(meal = as.factor(meal)) -> hotels
```

#### required_car_parking_spaces

There were no anomalies in this data. Since the maximum value is only `r max(hotels[["required_car_parking_spaces"]])`, we deemed it prudent to ignore the outliers.

#### total_of_special_requests

`total_of_special_requests` is the number of special requests made by the customer [1], [2]. This included requests such as asking for a twin bed or a high floor [1]. The maximum number of requests was `r hotels %>% select(total_of_special_requests) %>% max(na.rm = TRUE)`. This seems like a reasonable number of requests, especially for a large booking. Therefore, we decided not to remove the potential outliers.

### Booking Information

This section discusses the `hotel`, `market_segment`, `distribution_channel`, `booking_changes`, `deposit_type`, `agent`, `company`, and `customer_type`. In short, the variables that describe the entities involved in the booking process.

#### hotel

`hotel` is a variable indicating which hotel (the Resort Hotel or City Hotel) an observation is for [2]. The resort hotel was located in Portugal's Algarve region and the city hotel was located in Lisbon, Portugal [1]. Conversion to factor was the only data processing step for `hotel`.

```{r hotel factor conversion}
# Convert the hotel variable to a factor
hotels %>% 
  mutate(hotel = as.factor(hotel)) -> hotels
```

#### market_segment

`market_segment` indicates which market segment the transaction fell into [1], [2]. Note that 'TA' means travel agent and 'TO' indicates tour operator [1], [2]. In this variable, there were originally values marked as "Undefined". We chose to simply remove these missing values because there are only `r sum(hotels[["market_segment"]] == "Undefined")`, a tiny fraction of the data. We also converted this variable to a factor.

```{r market segment to factor}
# Remove the undefined observations and convert to factor.
hotels %>% 
  filter(market_segment != "Undefined") %>% 
  mutate(market_segment = as.factor(market_segment)) -> hotels
```

#### distribution_channel

`distribution_channel` is the booking distribution channel [1], [2]. Note that 'TA' means travel agent and 'TO' indicates tour operator [1], [2]. There are values marked "Undefined" for this variable. Since there are only `r sum(hotels[["distribution_channel"]] == "Undefined")` such values remaining in our dataset, we can simply filter them out.

```{r distribution_channel "Undefined" removal}
# Filter out "Undefined" entries, and convert to a factor variable
hotels %>% 
  filter(distribution_channel != "Undefined") %>% 
  mutate(distribution_channel = as.factor(distribution_channel)) -> hotels
```

#### booking_changes

`booking_changes` is a numerical variable counting the number of amendments to the other variables made prior to check-in or cancellation [1], [2]. Multiple changes would be counted as a single amendment if entered together [1]. `booking_changes` didn't require any modifications. We tried using the boxplot in Figure 10 to find outliers, but it indicated that any values above 0 are outliers. We decided to leave the outliers alone, because it is not unreasonable to expect booking changes.

```{r booking_changes boxplot}
# booking_changes boxplot
hotels %>% 
  ggplot(aes(booking_changes)) +
    geom_boxplot() +
    labs(x = "Number of Booking Changes",
         title = "Figure 10: Booking Changes Boxplot")
```

#### deposit_type

`deposit_type` is a categorical variable indicating what type of deposit was made to secure the booking [1], [2]. "No Deposit" indicates that no deposit was made, "Non Refund" indicates that the deposit was for the entire stay, and "Refundable" indicates that the deposit was less than the value of the entire stay [1], [2]. These categories were determined based on the amount of payment made prior to the arrival or cancellation date [1]. The only preparation required was conversion to a factor variable.

```{r deposit type factorization}
# Convert deposit_type to a factor variable.
hotels %>% 
  mutate(deposit_type = as.factor(deposit_type)) -> hotels
```

#### agent

`agent` contains id numbers for the various travel agents [1], [2]. For privacy reasons, anonymized id numbers were used to indicate `agent` [1]. The main data preparation step was converting the `NULL` values to `NA` Then, we converted it to a numeric data type. We did not remove the `NA`s because this indicates that the booking was made without a travel agent [1].

```{r agent cleanup}
# Convert NULLs in agent to NAs and convert agent to numeric.
hotels %>% 
  mutate(agent = na_if(agent, "NULL")) %>% 
  mutate(agent = as.numeric(agent)) -> hotels
```

#### company

`company` contains id numbers for the various travel agents [1], [2]. For privacy reasons, anonymized id numbers were used to indicate `company` [1]. The main data preparation step was converting the `NULL` values to `NA` Then, we converted it to a numeric data type. However, we did not remove `NA`s because this indicated a guest independently making a booking [1].

```{r company cleanup}
# Convert NULLs in company to NAs and convert company to numeric.
hotels %>% 
  mutate(company = na_if(company, "NULL")) %>% 
  mutate(company = as.numeric(company)) -> hotels
```

#### customer_type

`customer_type` divides the customers into 4 categories: "Contract" is for customers when the booking was done by allotment or contract; "Group" for customers associated with a group; "Transient" for customers that aren't associated with a group, contract, or other transient booking; and "Transient-party" for transient bookings that are associated with other transient bookings [1], [2]. The only data preparation step necessary for `customer_type` was conversion to a factor variable.

```{r customer type factorization}
# Convert customer_type to a factor variable.
hotels %>% 
  mutate(customer_type = as.factor(customer_type)) -> hotels
```

### Average Daily Rate

`adr` stands for "Average Daily Rate". It was calculated by dividing the total transactions associated with a booking by the total number of nights [1], [2]. Denomination was not specified in either data dictionary. We assumed that the hotels use Euros because that is Portugal's currency. Only outliers needed to be dealt with to prepare `adr`. We made our choices about outliers using the summary below.

```{r adr summary}
# Summarize the adr distribution
hotels %>% 
  select(adr) %>% 
  summary
```

We decided not to mess with the positive values, as they all seem reasonable for hotels. Not to mention, removing the highest paying customers should be approached with caution. However, we did remove the negative `adr` because there was only `r hotels %>% filter(adr < 0) %>% nrow` such observation in the entire dataset.

```{r adr negative value elimination}
# Only include non-negative values for adr
hotels %>% 
  filter(adr >= 0) -> hotels
```

## Dataset after Preparation {.tabset .tabset-pills}

This section summarizes the structure of the prepared data set. It also shows summaries for each variable after data preparation has been completed. These summaries are split up as described in Table 2 above.

First, `glimpse` is used to get an overview of the structure of the data.

```{r hotels post prep}
# Check the structure of the data
glimpse(hotels)
```

These results indicate that the dataset was prepared as described above. After removal of `NA` values, the dataset has `r nrow(hotels)` observations remaining. Only `r round(100 - 100 * nrow(hotels) / 119390, 2)`% of the observations were removed during data preparation.

### Arrival Date

Two of the years only had data for half the year [1], [2]. Thus, we chose to summarize `arrival_date` by counting how many bookings there were in a given arrival month. That way, the data from the two partial years would be a good stand in for a full year of data.

```{r arrival_date count by month table}
# Count the number of bookings with each arrival month
hotels %>% 
  count(month(arrival_date, label = TRUE, abbr = FALSE)) %>% 
  kable(format = "pipe",
        col.names = c("Arrival Month", "Total Number of Bookings"),
        caption = "Table 3: Total Number of Bookings by Month")
```

According to Table 3, the summer months were the busiest for the hotels based on the booked arrival dates. Winter months were significantly less popular, with January have the fewest bookings at 5866. This implies that these Portuguese hotels are not locations people visit to escape the winter cold.

### Guest Demographics

#### adults, children, and babies Summaries

All three of these variables hold similar data. Thus, they are handled in a single subsection. Their distributions are summarized in Table 4 below.

```{r adults, children, and babies summary table, warning=FALSE}
# Calculate summary statistics for each of these variables, and output them as a
# single table.
hotels %>% 
  select(adults, children, babies) %>% 
  summarise(across(.fns = summary)) -> guest_age_summary
# Add a column of identifiers for the summary statistics
tibble(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
       guest_age_summary) -> guest_age_summary
# Output a table summarizing this data
guest_age_summary %>% 
  kable(format = "pipe",
        col.names = c("", "Adults", "Children", "Babies"),
        caption = "Table 4: Guest Age Demographics")
```

According to Table 4, over 75% of the guests did not bring children with them or babies. However, some parties did bring a large number of children with each other. Typical bookings appeared to consist of 2 adults booking a room, with some entries where only 1 person booked a room. The minimum value was `r min(hotels %>% select(adults))`. This implies that there were guests who were booking separate rooms for their children.

#### country

There are `r length(levels(hotels$country))` unique values. This is too many to list them all. Instead, we will summarize this variable using a top ten list in Table 5.

```{r countries top ten list}
# Make a top ten list of the ten countries with the largest number of bookings.
hotels %>% 
  count(country) %>% 
  # Get the country names
  left_join(y = ISO_3166_1, by = c("country" = "Alpha_3")) %>% 
  # Subset to the columns of interest
  select(country, Name, n) %>% 
  # Get the top ten countries
  slice_max(order_by = n, n = 10) %>% 
  kable(format = "pipe",
        col.names = c("Abbr.",
                      "Country",
                      "Number of Bookings"),
        row.names = TRUE,
        caption = "Table 5: Top Ten Countries by Number of Bookings")
```

Bookings from Portugal made up the largest number of bookings. Most of the countries that made the top ten list are located in Europe. This is no surprise considering that Portugal is both located in Europe and a member of the European Union. Brazil was the only country outside Europe to appear in the Table 5, being located in South America. Together, Portugal, the United Kingdom, and France constituted over half of the bookings.

### Reservation Status

#### reservation_status

This is a factor that comes in 3 categories. Table 6 counts the number of observations within each category. Note that this includes all of the observations across both hotels.

```{r reservation_status table}
# Tabulate the different reservation status categories
hotels %>% 
  count(reservation_status) %>% 
  kable(format = "pipe",
        col.names = c("Reservation Status", "Number of Bookings"),
        caption = "Table 6: Number of Bookings by Reservation Status"
        )
```

A majority of the bookings ended with the guests checking in and then proceeding to check out. Cancellations accounted for a little under half of the bookings. However, only `r sum(hotels$reservation_status == "No-Show")` bookings ended with the guest failing to show up for their reservation.

#### reservation_status_date

Again, some of the years only have data for half the year [1], [2]. Counting the number of observations for each month in `reservation_status_date` will be the most effective way to summarize this variable. Table 7 shows these results.

```{r reservation_status_date tables}
# Count the number of final status updates by month
hotels %>% 
  count(month(reservation_status_date, label = TRUE, abbr = FALSE)) %>% 
  kable(format = "pipe",
        col.names = c("Month", "Number of Bookings"),
        caption = "Table 7: Number of Booking Final Updates by Month")
```

July had the most final updates. This happened despite the fact that August had more reservations according to Table 3. Guests cancelling well before a reservation could have shifted `reservation_status_date` out of the month they arrived. Any guests who arrived and checked out in different months would also have a `reservation_status_date` month that didn't match their arrival month. No clear patterns are evident from this table.

#### is_canceled

There were `r hotels %>% select(is_canceled) %>% sum` bookings that ended in cancellations, or about `r hotels %>% select(is_canceled) %>% sum / nrow(hotels) * 100`% of bookings. This indicates that the cancelling guests are a small yet disruptive minority for the hotels. However, the guests who made bookings after the end date of this dataset may have cancelled, so this number is likely to be an underestimate.

#### lead_time and days_in_waiting_list

These are both numeric variables. `lead_time` and `days_in_waiting_list` are summarized together in Table 8.

```{r lead_time and days_in_waiting list table}
# Generate a table of statistical summaries for lead_time and
# days_in_waiting_list
hotels %>% 
  select(lead_time, days_in_waiting_list) %>% 
  summarise(across(.fn = summary)) %>% 
  # Add a column of names for the statistical summaries
  mutate(stat_labels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                         "Max.")
         ) %>% 
  # Put the columns in the desired order
  select(stat_labels, lead_time, days_in_waiting_list) %>% 
  kable(format = "pipe",
        col.names = c("", "Lead Time", "Days in Waiting List"),
        caption = "Table 8: Lead Time and Days in Waiting List Summaries")
```

The median `lead_time` was 69 days. In other words, guests were booking about 2 months ahead of their `arrival_date`. Only about 25% of guests were making their reservations over 5 months in advance. 25% of guests were making their reservations 18 days or less before their stays. These last minute guests would be helpful for filling cancelled bookings on short notice.

Less than 25% of guests went on the wait list at all based on Table 8. This could be because guests are backing out of the reservation system when they realize that the hotels are booked. Unfortunately, we can't make any inferences about this because our data doesn't show how many people backed out of the system without getting a reservation.

### User Karma

#### is_repeated_guest

There were `r hotels %>% select(is_repeated_guest) %>% sum` bookings made by repeat guests in this dataset. This constituted about `r round(hotels %>% select(is_repeated_guest) %>% sum * 100 / nrow(hotels), 2)`. Focusing on enticing new guests appears to be more important than getting repeat stays from past guests.

#### previous_cancellations and previous_bookings_not_cancelled

Statistical summaries of these numerical variables are tabulated below in Table 9.

```{r previous_cancellations and prev_bookings_not_cancelled summary table}
# Build a summary table for previous_cancellations and
# previous_bookings_not_cancelled.
hotels %>% 
  select(previous_cancellations, previous_bookings_not_canceled) %>% 
  summarise(across(.fn = summary)) %>% 
  # Add a column of names for the summary statistics
  mutate(stat_labels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                         "Max.")
         ) %>% 
  # Move the labels to the rightmost column
  select(stat_labels,
         previous_cancellations,
         previous_bookings_not_canceled) %>% 
  kable(format = "pipe",
        col.names = c("",
                      "Previous Cancellations",
                      "Previous Bookings not Cancelled"),
        caption = "Table 9: Previous Cancellations and Previous Bookings not Cancelled Summary")
```

According to Table 9, most of the booking guests did not have prior cancellations. However, some of the guests have a high number of cancellations, with the largest observed cancellations being `r hotels %>% select(previous_cancellations) %>% max()`. However, most of the guests did not have previous bookings that were not cancelled. This makes sense because most of the bookings were by new guests.

### Guest Accommodations

#### stays_in_weekend_nights, stays_in_week_nights, required_car_parking_spaces,  and total_of_special_requests

All four of these variables are numerical variables. Therefore, they will all be summarized in Table 10 below.

```{r numerical guest accomodation variable summary table}
# Build a summary table for stays_in_weekend_nights, stays_in_week_nights,
# required_car_parking_spaces, and total_of_special_requests
hotels %>% 
  select(stays_in_weekend_nights,
         stays_in_week_nights,
         required_car_parking_spaces,
         total_of_special_requests) %>% 
  summarise(across(.fn = summary)) %>% 
  # Add a column of names for the summary statistics
  mutate(stat_labels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                         "Max.")
         ) %>% 
  # Move the labels to the rightmost column
  select(stat_labels,
         stays_in_weekend_nights,
         stays_in_week_nights,
         required_car_parking_spaces,
         total_of_special_requests) %>% 
  kable(format = "pipe",
        col.names = c("",
                      "Stays in Weekend Nights",
                      "Stays in Week Nights",
                      "Required Parking Spaces",
                      "Total Special Requests"),
        caption = "Table 10: Guest Accommodation Numerical Variable Summaries")
```

According to Table 10, the median number of weekend nights and week nights was 1 and 2 nights, respectively. A potential explanation for this is that guests were staying at the hotel on Friday and Saturday before checking out on Sunday. There were some long stays, with the longest consisting of an 8 week stay.

Most of the guests didn't request parking spaces or have any special requests. The fact that the guests didn't use parking could imply that they are sticking close to the hotels or that they found the local public transit adequate.

#### Room Type

`reserved_room_type` and `assigned_room_type` both deal with the same room types. These variables are summarized in Table 11 to ease comparison.

```{r reserved_room_type and assigned room type summary table}
hotels %>% 
  # Count the number of room types in each category for reserved_room_type
  count(reserved_room_type) %>% 
  # Join this data with a count for the room types in assigned_room_type
  full_join(y = count(hotels, assigned_room_type),
            by = c("reserved_room_type" = "assigned_room_type")) %>% 
  kable(format = "pipe",
        col.names = c("Room Type", "Reserved", "Assigned"),
        caption = "Table 11: Reserved vs. Assigned Room Type Comparison")
```

Almost all of the guests were assigned to Type A and D rooms. These were also the most popular accommodations at the time of booking. Thus, it appears that these are the most desirable rooms for the guests. Despite no one making a reservation for Type I and K rooms, several guests were assigned to these rooms. Interestingly, only 6 bookings were made for Type L rooms, with only one booking successfully obtaining such a room. Unfortunately, it is hard to figure out why this is happening because no data is provided about what the room codes mean in real terms.

### Booking Information

#### hotel

A table of counts would be the most effective way to summarize this variable. This is shown below in Table 12.

```{r hotel category table}
# Generate a table of counts for each level of the hotel variable
hotels %>% 
  count(hotel) %>% 
  kable(format = "pipe",
        col.names = c("Hotel", "Number of Bookings"),
        caption = "Table 12: Number of Bookings by Hotel")
```

The majority of the bookings were made for the city hotel. However, both hotels received a large number of bookings.

#### market_segment

There are a large number of levels for this categorical variable. However, not so many as to make it impractical to tabulate all of the levels. The number of bookings in each segment are tabulated in Table 13 in descending order.

```{r market_segment tabulation}
# Tabulate the booking market segments
hotels %>% 
  count(market_segment) %>% 
  # Put the results in descending order
  arrange(desc(n)) %>% 
  # Add a percentage of bookings column
  mutate(percent_of_bookings = round(100 * n / sum(n),
                                     2)
         ) %>% 
  kable(format = "pipe",
        col.name = c("Market Segment",
                     "Number of Bookings",
                     "Percent of Bookings"),
        caption = "Table 13: Bookings by Market Segment")
```

Travel agents and tour operators were by far the largest market segment, at ~67% of bookings. Complementary and Aviation were both tiny market shares. This makes sense, because a hotel is not going to routinely hand out free stays if it wants to stay in business. Direct bookings by guests made up about 10% of the bookings.

#### distribution_channel

This categorical variable is summarized in Table 14 below.

```{r distribution_channel tabulation}
# Tabulate the booking market segments
hotels %>% 
  count(distribution_channel) %>% 
  # Put the results in descending order
  arrange(desc(n)) %>% 
  # Add a percentage of bookings column
  mutate(percent_of_bookings = round(100 * n / sum(n),
                                     2)
         ) %>% 
  kable(format = "pipe",
        col.name = c("Distribution Channel",
                     "Number of Bookings",
                     "Percent of Bookings"),
        caption = "Table 14: Bookings by Distribution Channel")
```

Over 80% of the bookings were generated to travel agents and tour operators. Marketing to these entities should be an effective method to boost sales. Direct bookings by guests is the next largest channel. Corporate bookings only made up about 5% of the bookings.

#### booking_changes

This numerical variable is summarized in Table 15 below.

```{r booking_changes summary}
# Generate a summary for booking_changes
hotels %>% 
  select(booking_changes) %>% 
  summarise(summary(booking_changes)) -> changes_summary
# Add row names to the summary and output as a table
tibble(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
       changes_summary) %>% 
  kable(format = "pipe",
        col.names = c("", ""),
        caption = "Table 15: Booking Changes Summary Statistics")
```

Over 75% of the guests did not make a change to their reservation after making it. However, there were some guests that made a large number of changes. In light of the fact that `r round(hotels %>% select(is_canceled) %>% sum / nrow(hotels) * 100, 2)`% of bookings were cancelled, we can conclude that cancellations are not treated as a booking change by the hotel.

#### deposit_type

`deposit_type` is a categorical variable and is summarized in Table 16 below.

```{r deposit_type tabulation}
# Tabulate the deposit_types
hotels %>% 
  count(deposit_type) %>% 
  # Put the results in descending order
  arrange(desc(n)) %>% 
  # Add a percentage of bookings column
  mutate(percent_of_bookings = round(100 * n / sum(n),
                                     2)
         ) %>% 
  kable(format = "pipe",
        col.name = c("Deposit Type",
                     "Number of Bookings",
                     "Percent of Bookings"),
        caption = "Table 16: Bookings by Deposit Type")
```

Almost all of the bookings were made with no up front deposit. These guests are going to be the biggest challenge in terms of cancellations, since they aren't generating any revenue from them while they are holding their room. Where a deposit was made, it was almost always made as a non-refundable deposit. Only a tiny fraction of the guests made a refundable deposit.

#### agent and company

Both of these variables are anonymized id numbers [1], [2]. Thus, no useful information can be gained by analyzing these variables on their own.

#### customer_type

Table 17 summarizes this categorical variable below.

```{r customer_type tabulation}
# Tabulate the customer_type
hotels %>% 
  count(customer_type) %>% 
  # Put the results in descending order
  arrange(desc(n)) %>% 
  # Add a percentage of bookings column
  mutate(percent_of_bookings = round(100 * n / sum(n),
                                     2)
         ) %>% 
  kable(format = "pipe",
        col.name = c("Customer Type",
                     "Number of Bookings",
                     "Percent of Bookings"),
        caption = "Table 17: Bookings by Customer Type")
```

Over 95% of the bookings were of the Transient and Transient-Types. While travel agents and tour operators make up a large proportion of the market segments, they are not making group bookings. This makes sense considering that the typical number of guests associated with a booking is only 2 people.

### Average Daily Rate

Statistical summaries for `adr` are shown below in Table 18.

```{r prepared adr table}
# Generate a summary for adr
hotels %>% 
  summarise(summary(adr)) -> adr_summary
# Add row names to the summary and output as a table
tibble(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
       adr_summary) %>% 
  kable(format = "pipe",
        col.names = c("", ""),
        caption = "Table 18: Average Daily Rate Summary Statistics (\u20ac)")
```

`r median(hotels$adr)` was the median average daily rate for the guests. 0 was the minimum non-negative daily rate. This is problematic because such guests are not generating profit for their hotel. The maximum of `r max(hotels$adr)` is very good, and we would want to attract such bookings in the future.

## Exploratory Data Analysis {.tabset .tabset-pills}

For the exploratory data analysis, we will be performing a statistical analysis by analyzing the different variables and have divided our analysis into the following parts:

### Analysis of Bookings

Number of bookings is the first subject to be considered in this exploratory analysis. In this section, bookings are examined by country, hotel type, and distribution channel. Further assessment of the number of bookings in each month is also conducted.

#### Bookings by Country

Knowing where your guests are coming from is important for efficient targeted marking and cultural training of hotel staff. A world map is the best way to visualize guest countries of origin. To generate a world map, country names were mapped to the `country` code associated with each booking. In several cases, the region names in `map_data`'s world map did not match with the `ISOCodes` dataset. Where possible, the country's name was used. Bookings from countries that were below the granularity of the world map data were allocated to the controlling country or nearest neighbor. For example, bookings from Gibraltar were allocated to Spain.

```{r world map visualization}
# Load the world map data
world_map = map_data(map = "world")
# We need to add country names that match the regions in the world_map
# map_data. Since this will need be done every time a world_map is made, it
# will save a lot of space to just add a country name variable to the hotels
# data.
hotels %>% 
  # Add the full names for each country
  left_join(y = ISO_3166_1, by = c("country" = "Alpha_3")) %>% 
  # Correct the Names to match regions in world_map
  mutate(Name = case_when(Name == "French Southern Territories" ~ "French Southern and Antarctic Lands",
                          Name == "Bolivia, Plurinational State of" ~ "Bolivia",
                          Name == "Côte d'Ivoire" ~ "Ivory Coast",
                          Name == "CN" ~ "China",
                          Name == "Cabo Verde" ~ "Cape Verde",
                          Name == "Czechia" ~ "Czech Republic",
                          Name == "United Kingdom" ~ "UK",
                          # There was no entry for Gibraltar in the map
                          Name == "Gibraltar" ~ "Spain",
                          # Hong Kong didn't have its own entry, added to China
                          Name == "Hong Kong" ~ "China",
                          Name == "Iran, Islamic Republic of" ~ "Iran",
                          Name == "Saint Kitts and Nevis" ~ "Saint Kitts",
                          Name == "Korea, Republic of" ~ "South Korea",
                          Name == "Lao People's Democratic Republic" ~ "Laos",
                          # Macao didn't have its own entry, added to China
                          Name == "Macao" ~ "China",
                          Name == "Russian Federation" ~ "Russia",
                          Name == "Syrian Arab Republic" ~ "Syria",
                          Name == "Taiwan, Province of China" ~ "Taiwan",
                          country == "TMP" ~ "Timor-Leste",
                          Name == "Tanzania, United Republic of" ~ "Tanzania",
                          Name == "United States Minor Outlying Islands" ~ "USA",
                          Name == "United States" ~ "USA",
                          Name == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
                          Name == "Virgin Islands, British" ~ "Virgin Islands",
                          Name == "Viet Nam" ~ "Vietnam",
                          country == "CN" ~ "China",
                          TRUE ~ Name)
         ) %>% 
  rename(country_name = Name, country_abbr = country) %>% 
  # Drop the superfluous variables inherited from ISO_3166_1
  select(-Alpha_2, -Numeric, -Official_name, -Common_name) -> hotels
# Make a plot of bookings vs country
hotels %>% 
  # Count the bookings by country
  count(country_name) %>% 
  # Make the world map
  ggplot() +
    # Add a base layer to show all countries, even ones we don't have data for
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             fill = "white",
             color = "black"
             ) +
    # Color the map based on the number of bookings per country
    geom_map(aes(map_id = country_name, fill = n), map = world_map) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      scale_fill_continuous(name = "", type = "viridis") +
      labs(title = "Figure 11: World Map of Bookings",
           x = "Longitude (°)",
           y = "Lattitude (°)")
```

Figure 11 shows that the largest proportion of bookings originated from Portugal and Europe. These Portuguese hotels were receiving bookings from every continent. However, Antarctica is presumably included due to its inclusion with France's Southern Territories rather than guests making bookings from Antarctica.

#### Bookings by Hotel Type

We now turn our attention to the number of bookings associated with each hotel. Time dependence of booked `arrival_dates` is also assessed. Figure 12 below compares the number of bookings for the City hotel and Resort hotel from 2015 through 2017. Then, Figure 13 compares the number of bookings by both arrival year and month.

```{r Analysis of Bookings}
# Create a barplot of the bookings by arrival year and hotel type
hotels %>% 
  mutate(arrival_year = year(arrival_date)) %>% 
  group_by(arrival_year) %>%
  ggplot() +
    geom_bar(aes(x = hotel, fill = hotel)) + 
    facet_wrap(~ arrival_year, nrow = 1) +
    labs(x = "",
         y = "Number of Bookings",
         fill = "Hotel",
         title = "Figure 12: Hotel Bookings by Year")
# Create a barplot of the bookings by arrival month and hotel type
hotels %>% 
  mutate(arrival_month = month(arrival_date, label = TRUE, abbr = TRUE)) %>% 
  ggplot() +
    geom_bar(aes(x = arrival_month, fill = hotel),
             position = "dodge") + 
    facet_wrap(~ year(arrival_date), nrow = 3) +
    labs(x = "",
         y = "Number of Bookings",
         fill = "Hotel",
         title = "Figure 13: Hotel Bookings by Month")
```

These figures show that the City hotel had more bookings than the Resort hotel in every year. The year 2016 saw maximum number bookings for both hotels. However, 2016 had more bookings because 2015 and 2017 we don't have the data for the entire year but only for specific months.

#### Bookings by Distribution Channel

Thirdly, the number of bookings by `distribution_channel` is considered. Knowing which distribution channel is most productive will hopefully enable the hotels to focus on that channel to drive bookings.

```{r total bookings by distribution channel}
# Make a barplot of the total number of bookings by distribution channel
hotels %>% 
  ggplot(aes(distribution_channel, fill = (hotel))) +
    geom_bar(position = 'dodge') +
    scale_y_continuous(name = "Number of Bookings",labels = scales::comma) +
    xlab("Distribution Channel") +
    ggtitle("Figure 14: Total Hotel Bookings by Distribution Channel") +
    labs(fill = 'Hotel')
```


According to Figure 14, travel agents and tour operators are the largest channels in terms of bookings for both hotels. This is reasonable, as these entities have a constant stream of vacationers to direct to the hotels. The City hotel had more direct bookings than the Resort hotel.

### Analysis of Cancellations

Booking cancellations are analyzed in this section. First, we compared the number of cancelled bookings to bookings that were not cancelled. This includes comparisons across hotels and year. Then, we compare the number of cancelled bookings across distribution channels. Finally, we compare the proportion of cancelled bookings from each country.

#### Bookings vs Cancellations 

Comparisons of total bookings and cancellations are conducted in this section. Figure 15 compares the number of total bookings to cancellations over the entire dataset. These counts are broken down by year in Figure 16.

```{r Bookings vs Cancellations, message=FALSE}
# Barplot of Total Cancellations by Hotel for the Observed Period
hotels %>% 
  ggplot() +
    geom_bar(aes(x = is_canceled, fill = hotel),
             position = "dodge") +
    labs(x = "Cancelled",
         y = "Number of Bookings",
         fill = "Hotel",
         title = "Figure 15: Total Booking Cancellations by Hotel")
# Barplot of bookings and cancellations per year by hotel type
hotels %>% 
  mutate(arrival_year = year(arrival_date)) %>% 
  ggplot() +
    geom_bar(aes(x = is_canceled, fill = hotel),
             position = "dodge") +
    facet_wrap(~ arrival_year, nrow = 1) +
    labs(x = "Cancelled",
         y = "Number of Bookings",
         fill = "Hotel",
         title = "Figure 16: Booking Cancellations by Hotel and Year")
```

Figures 15 and 16 both indicate that the number of bookings is larger than the number of cancellations for both hotels. Due to incomplete data in 2015 and 2017, 2016 had more bookings and cancellations than either of these years. The City hotel had more cancellations, but also had more bookings. To minimize these issues, Table 19 below shows the percentages of cancelled and not cancelled bookings by year.

```{r hotel-year-cancellation contingency table}
# Tabular summary of the percentage of cancellations by hotel and year
hotels %>% 
  # Find the counts by arrival year
  mutate(arrival_year = year(arrival_date)) %>% 
  group_by(hotel, arrival_year) %>% 
  count(is_canceled) %>% 
  # Convert the counts to percentages
  group_by(hotel, arrival_year) %>% 
  summarise(is_canceled = is_canceled,
            percentages = round(100 * n / sum(n), 2)
            ) %>%
  # Structure output as a contingency table
  pivot_wider(names_from = arrival_year, values_from = percentages) %>% 
  kable(format = "pipe",
        col.names = c("Hotel",
                      "Cancellation Status",
                      "2015 (%)",
                      "2016 (%)",
                      "2017 (%)"),
        caption = "Table 19: Hotel Cancellation and Year Contingency Table")
```

`r round(hotels %>% filter(is_canceled == TRUE) %>% nrow() * 100 / nrow(hotels), 2)`% of bookings were cancelled across both types of hotels. It is evident from Table 19 that the ratio of cancellations to bookings is higher for the City hotel than the Resort hotel. This trend holds across all time periods in the study. A potential explanation for this is that the City hotel's extra cancellations are due to it having proportionally more corporate bookings than the Resort hotel as shown in Figure 14 above. Further research should be done to isolate the cause of the extra cancellations and rectify those issues.

#### Analysis of No-Shows vs. Formal Cancellations

In this section, the proportions of guests who cancel by not showing up ("No-Show") are compared to guests who call ahead to formally cancel their booking ("Canceled"). This is useful information because it indicates how likely the hotel is to have the cancellation. First, we compare the proportion of No-Shows to Canceled bookings using Table 20 below.

```{r Analysis of no-shows vs formal cancellations}
# Cancellation contingency table
hotels %>% 
  filter(is_canceled == TRUE) %>% 
  group_by(reservation_status) %>% 
  count(is_canceled) %>% 
  # Calculate percentages for each row
  ungroup() %>% 
  mutate(percentage = round(100 * n / sum(n), 2)) %>% 
  # Remove the redundant is_canceled variable from the table
  select(-is_canceled) %>% 
  kable(format = "pipe",
        col.names = c("Reservation Status",
                      "Number of Bookings",
                      "Percentage of Cancellations"),
        caption = "Table 20: Cancellation Contingency Table")
```

Out of the 44,142 cancellations, 42,940 (~97%) are actual cancellations and the remaining 1202 are no shows. This means that the guests are likely to give prior notice to their hotel before cancelling. Prior notice makes it more likely that the booking can be reassigned. Table 21 and Figure 17 compare these results between the City and Resort hotel.

```{r assessment of cancellations by hotel type}
# Cancellations by hotel type contingency table
hotels %>% 
  filter(is_canceled == TRUE) %>% 
  # Find the number of cancelled bookings by hotel and reservation status
  group_by(hotel, reservation_status) %>% 
  count(is_canceled) %>% 
  # Add a percentage of booking column
  ungroup() %>% 
  mutate(percentage = round(100 * n / sum(n), 2)) %>% 
  # Remove the redundant is_canceled variable
  select(-is_canceled) %>% 
  kable(format = "pipe",
        col.names = c("Hotel",
                      "Reservation Status",
                      "Number of Bookings",
                      "Percentage of Cancellations"),
        caption = "Table 21: Cancellations by Hotel and Cause")
# Plotting cancellations by hotel type and year
hotels %>% 
  filter(is_canceled == TRUE) %>% 
  mutate(arrival_year = year(arrival_date)) %>% 
  ggplot(aes(x = reservation_status, fill = hotel)) + 
    geom_bar(position = "dodge") +
    facet_grid(cols = vars(arrival_year)) +
    labs(x = "Cause",
         y = "Number of Bookings",
         fill = "Hotel",
         title = "Figure 17: Cancellations by Hotel, Year, and Cause")
```

Out of the total cancellations, around 72.3% are for city hotels and 24.5% for Resort Hotels. There are around 2% no-shows for City hotels and less than 1% for resort hotels. Again, more cancellations were observed for 2016 due to the half years representing 2015 and 2017.

#### Cancellations by Distribution Channel

In this section, the booking cancellations are assessed by distribution channel. Distribution channel cancellations are tallied by hotel in terms of total number of cancellations (Table 22) and percentages of each hotel's cancellations (Table 23). Table 24 shows the percentage of cancellations that are attributable to each distribution channel considering both hotels.

```{r Distribution channel, message=FALSE}
# Cancellations by Distribution Channel
hotels %>% 
  group_by(hotel, distribution_channel) %>% 
  summarise(total_cancellations = sum(is_canceled, na.rm = TRUE)) %>% 
  pivot_wider(names_from = distribution_channel,
              values_from = total_cancellations) %>% 
  kable(format = "pipe",
        caption = "Table 22: Cancellations by Hotel and Distribution Channel")
# Percentage of Cancellations for each hotel
hotels %>% 
  filter(is_canceled == TRUE) %>% 
  group_by(hotel, distribution_channel) %>% 
  count(is_canceled) %>% 
  # Calculate the percentage of cancellations per distribution hotel by hotel
  group_by(hotel) %>% 
  mutate(percentage = round(100 * n / sum(n), 2)) %>% 
  # Drop the unneeded variables from the table
  select(-c(is_canceled, n)) %>% 
  # Output as a contingency table
  pivot_wider(names_from = distribution_channel, values_from = percentage) %>% 
  kable(format = "pipe",
        caption = "Table 23: Percentage of Hotel Cancellations by Distribution Channel")
# Percent cancellations by Distribution channel
hotels %>% 
  filter(is_canceled == TRUE) %>% 
  group_by(distribution_channel) %>% 
  count(is_canceled) %>% 
  # Calculate a percentage of cancellations variable
  ungroup() %>% 
  mutate(percentage = round(100 * n / sum(n), 2)) %>% 
  # Drop the is_cancelled variable from the table
  select(-is_canceled) %>% 
  kable(format = "pipe",
        col.names = c("Distribution Channel",
                      "Cancellations",
                      "Percentage of Cancellations"),
        caption = "Table 24: Percentage of Cancellations by Distribution Channel")
```

From Table 23, we can see that over 80% of the cancellations came from the travel agent and tour operator distribution channel. Direct was the second most common source of cancellations. Interestingly, the Resort hotel had far more cancellations from the Corporate and Direct distribution channels than the city hotel, but a lower proportion of cancellations from the travel agents and tour operators. According to Table 24, over 90% of all cancellations came from travel agents and tour operators.

One weakness of these tables is that they fail to provide any indication of the proportion of bookings that were canceled. Figure 18 gives the proportion of bookings that were canceled for each distribution channel, by hotel.

```{r visualization of the proportional cancellations by distribution channel, message=FALSE}
# Proportion of Cancellations by Distribution Channel - Plot
hotels %>%
  # Find the percentage of cancellations by hotel and distribution channel.
  group_by(hotel, distribution_channel) %>% 
  summarise(n_canceled = sum(is_canceled),
            n_total = n(), percentage = 100 * n_canceled / n_total) %>% 
  # Make a column plot to display this data
  ggplot() +
    geom_col(aes(x = distribution_channel,
                 y = percentage,
                 fill = hotel),
             position = "dodge") +
    labs(x = "Distribution Channel",
         y = "Percentage of Bookings Cancelled",
         title = "Figure 18: Percentage of Bookings Cancelled by Distribution Channel",
         fill = "Hotel")
```

For both hotels travel agents and tour operators had the highest rate of booking cancellations. Corporate bookings had the next highest cancellation rate. While travel agents and tour operators constituted over 90% of cancellations, they only had a cancellation rate of less than 50% for both hotels. This suggests that travel agents and tour operators are heavily represented among cancellations due to making a large population of bookings to be cancelled.

#### Cancellations by Country

Finally, let's find the countries with the highest cancellation rate. This was evaluated by finding the percentage of bookings from each country that resulted in a cancellation. Data from all time periods and both hotels were used for this calculation.

Table 25 shows a top ten list of countries by highest numbers of cancellations. Percentage of cancellations was not used for ranking because all of the countries on that list had 100% cancellation rates. This was an issue because all of the countries listed had a small number of cancellations, but all of their bookings were canceled. Showing cancellation rates for the countries with the largest number of cancellations was deemed more informative. Figure 19 maps the cancellation rate as a percentage for each country.

```{r cancellation proportions for each country}
# First, generate a countries with the highest cancellation rate list
hotels %>% 
  group_by(country_name) %>% 
  # Count the number of cancelled and find the cancellation rate (%)
  summarise(n_cancelled = sum(is_canceled),
            n_total = n(),
            percentage_cancellations = round(100 * n_cancelled / n_total,
                                             2)
            ) %>% 
  slice_max(n_cancelled, n = 10) %>% 
  # Only include the columns of interest
  select(country_name,
         n_cancelled,
         n_total,
         percentage_cancellations) %>% 
  kable(format = "pipe",
        col.names = c("Country Name",
                      "Cancelled Bookings",
                      "Total Bookings",
                      "Percentage Cancellations"),
        caption = "Table 25: Cancellations by Country")
# Next, let's get a more global picture by plotting the cancellation rate
# on the world map.
hotels %>%
  group_by(country_name) %>%
  # Count the proportion of cancelled bookings for each country
  summarise(n_cancelled = sum(is_canceled),
            n_total = n(),
            percentage_cancellations = 100 * n_cancelled / n_total) %>% 
  # Map the results
  ggplot() +
    # Add a base layer to show all countries, even ones we don't have data for
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             fill = "white",
             color = "black"
             ) +
    # Color the map based on the proportion of cancellations for that country
    geom_map(aes(map_id = country_name,
                 fill = percentage_cancellations),
             map = world_map) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      scale_fill_continuous(name = "", type = "viridis") +
      labs(title = "Figure 19: Percentage of Bookings Cancelled World Map",
           x = "Longitude (°)",
           y = "Lattitude (°)"
           )
```

Again, Antarctica has data due to the French Southern Territories including Antarctica. Portugal was the worst offender on the top ten list, with a 56.6% cancellation rate. It is interesting that the hotel's home country would have a number of cancellations an order of magnitude bigger than the rest of the countries. This may indicate that Portuguese customers are more likely to book at their local hotels when their plans are uncertain. And then are more willing to cancel when their plans change because they didn't have to arrange international travel to reach their hotel.

Cancellation rates do appear to be higher outside of Europe. Countries in the southern hemisphere also appear to be more likely to cancel. This could be because of the distance to these countries. Any disruptions to travel arrangements would also be more problematic for guests living outside the Shengen Area.

### Analysis of Repeated Guests

Time series plots are used to assess the proportion of bookings that were made by repeated guests. Percentage of bookings was used so that the time series would not be affected by the partial data in 2015 and 2017. Figure 20 shows the percentage of bookings made by repeat guests taking both hotels into account.

```{r analysis of repeated guests, message=FALSE}
# Make a time-series plot of repeated guest bookings as a proportion of
# bookings.
hotels %>% 
  # Count the number of bookings made by repeat guests over time.
  group_by(arrival_date) %>%
  count(is_repeated_guest) %>% 
  # Calculate the percentage of bookings made by repeat and new guests
  mutate(percentage = 100 * n / sum(n)) %>% 
  # Only include the repeated guests
  filter(is_repeated_guest == TRUE) %>% 
  ggplot() +
    geom_line(aes(x = arrival_date,
                  y = percentage)
              ) +
    labs(x = "Arrival Date",
         y = "Percentage of Repeat Guests",
         title = "Figure 20: Repeat Guests Time-Series")
```

Over half of the bookings were made by guests who were not repeated guests. There doesn't appear to be any long term trend in the proportion of repeated guests over time. However, cyclic trends are present in this data. It appears that the proportion of repeated guests increases in the winter months and is lower in the summer months. Similar trends appear in the plots for the individual hotels shown in Figure 21 below.

```{r analysis of repeated guests by hotel, message=FALSE}
# Make a time-series plot of repeated guest bookings as a proportion of
# bookings by hotel.
hotels %>% 
  # Count the number of bookings made by repeat guests over time by hotel.
  group_by(arrival_date, hotel) %>%
  summarise(n_repeated_guests = sum(is_repeated_guest),
            n_total = n(),
            percent_of_bookings = round(100 * n_repeated_guests / n_total,
                                        2)
            ) %>% 
  # Subset to only include data for repeated guests
  filter(n_repeated_guests > 0) %>% 
  ggplot() +
    geom_line(aes(x = arrival_date,
                  y = percent_of_bookings,
                  colour = hotel)
              ) +
    facet_wrap(~ hotel, ncol = 2) +
    labs(x = "Arrival Date",
         y = "Percentage of Repeat Guests",
         colour = "",
         title = "Figure 21: Repeat Guests Time-Series by Hotel")
```

### Analysis of Average Daily Rate

The effect of time on `adr` is assessed in this section. `adr` averaged over the course of each month is used to gauge how the amount guests are willing to spend at the hotel changes over the course of the year. Figure 22 below shows the time series for this data.

```{r adr time-series analysis, message=FALSE}
# Find the mean adr by month
hotels %>% 
  filter(is_canceled == FALSE) %>% 
  # Subset the arrival_date to the year and month
  mutate(arrival_year = year(arrival_date),
         arrival_month = month(arrival_date),
         arrival_time = ym(paste(arrival_year, arrival_month))
         ) %>% 
  # Drop the arrival_year and arrival_month variables, as they are redundant
  select(-arrival_year, -arrival_month) %>% 
  group_by(arrival_time) %>% 
  summarise(monthly_mean_adr = mean(adr)) -> monthly_adr
# Make a time series plot of monthly mean adr
monthly_adr %>% 
  ggplot() +
    geom_line(aes(x = arrival_time, y = monthly_mean_adr)) +
    labs(x = "Month",
         y = "Monthly Average of ADR (€)",
         title = "Figure 22: Monthly ADR Time-Series")
```

A strong seasonal trend in monthly average `adr` is heavily implied by this data. It appears that guests are willing to spend more in the summer months than in the winter ones. Forecasting the monthly average of `adr` would give a good baseline for setting prices. We used an ARIMA model for our forecast.

```{r forecast of adr, message=FALSE}
# Build an ARIMA model for average monthly ADR
monthly_adr %>%
  # Convert the monthly mean ADR data into a time-series
  select(monthly_mean_adr) %>% 
  as.ts() %>% 
  # Build the ARIMA model out of the time-series
  auto.arima() -> monthly_adr_model
# Plot a forecast of the monthly average ADR over the next 12 months
monthly_adr_model %>% 
  forecast(h = 24) %>%
  as_tibble() %>% 
  # Add Date type dates to the time series
  mutate(forecast_month_index = 1:n(),
         forecast_month = add_with_rollback(ym("2017-08"),
                                               months(forecast_month_index),
                                            roll_to_first = TRUE)
         ) %>% 
  ggplot() +
    # Add the historical data to the plot
    geom_line(data = monthly_adr, aes(x = arrival_time,
                                      y = monthly_mean_adr),
              color = "black") +
    # Add the forecast data to the plot
    geom_line(aes(x = forecast_month, y = `Point Forecast`), color = "blue") +
    labs(x = "Year",
         y = "Monthly Average ADR (€)",
         title = "Figure 23: Monthly ADR Forecast")
```

The forecast in Figure 23 predicts that the monthly average `adr` will decline as the summer wanes. But it then predicts that next year monthly average `adr` will reach an asymptote. This result doesn't match the trend of the known data. Furthermore, the implication that the hotels will not raise their rates during peak demand is not credible. Unfortunately, it appears that we do not have enough data to build a useful ARIMA forecast.

### Analysis of Revenue

Revenue is another important factor to consider. We've created a new variable, `total_stay` to calculate the total number of days stayed which is the sum of `stays_in_weekend_nights` and `stays_in_week_nights`.  We've also created a `total_revenue` variable by multiplying the average daily rate by the total number of days stayed. Table 26 and Figure 24 both show total revenue per year by hotel.

```{r Anlaysis of Revenue, message=FALSE}
# Introduce a total_stay and revenue variable
hotels %>% 
  mutate(total_stay = stays_in_weekend_nights + stays_in_week_nights,
         revenue = adr * total_stay) -> hotels
#Total revenue by hotel type and year
hotels %>% 
  filter(is_canceled == FALSE) %>% 
  mutate(arrival_year = year(arrival_date)) %>% 
  group_by(hotel, arrival_year) %>% 
  summarise(total_revenue = sum(revenue)) %>% 
  pivot_wider(names_from = arrival_year, values_from = total_revenue) %>% 
  kable(format = "pipe",
        caption = "Table 26: Total Hotel Revenue (€)")
# Revenue by hotel type and year
hotels %>% 
  mutate(arrival_year = year(arrival_date)) %>% 
  filter(is_canceled == FALSE) %>%  
  ggplot() + 
    stat_summary(aes(x = arrival_year, y = revenue, fill = hotel), 
                 fun = function(x) sum(x), 
                 geom = "bar", position = "dodge") +
    scale_y_continuous(name = "Revenue", labels = scales::dollar_format(prefix = "", suffix = "€")) +
    xlab("Year") +
    ggtitle("Figure 24: Hotel Revenue vs Year") +
    labs(fill = 'Hotel Type')
```

In every year except 2015, the City hotel made more revenue than the resort hotel. We do not have complete data for 2017 and 2015. For the first half of 2017, we have revenue of around 5.6 million for city hotels and around 4.1 million for resort hotels in 2017. Assuming similar performance in the second half of 2017 to that in 2015, total revenue should be higher in 2017 than in any other year.

The total revenue shown is not necessarily the actual revenue obtained. Cancellations must be taken into account. One interesting phenomenon was that cancelled bookings could generate revenue, as shown in Table 27 below.

```{r revenue from cancelled bookings, message=FALSE}
# Revenue from cancelled bookings and bookings that ended in a no-show
hotels %>% 
  filter(reservation_status != "Check-Out") %>% 
  group_by(reservation_status, deposit_type) %>% 
  summarise(total_revenue = sum(revenue)) %>% 
  pivot_wider(names_from = deposit_type, values_from = total_revenue) %>% 
  kable(format = "pipe",
        caption = "Table 27: Revenue from Cancelled and No-Show Bookings (€)")
```

Canceled bookings generated several times more revenue than no-shows regardless of the deposit type. Strangely, it was possible for the hotels to earn over 12 million € on bookings that were marked as "no deposit". It is unclear how that is possible, since the hotels were not getting payed in advance. One possibility is that these were the result of bookings where the guests had to cancel partway through their stay. Non-refundable and refundable generating revenue makes more sense, since the hotel is earning revenue through the guest's deposit.

#### Analysis of Revenue by Country

Average revenue per booking was calculated for each country. This data is summarized below in Figure 25.

```{r average revenue by country}
# Make a plot of bookings vs country
hotels %>% 
  # Aggregate revenue per booking by country using the mean
  group_by(country_name) %>% 
  summarise(mean_revenue = mean(revenue)) %>% 
  # Plot this data on a world map
  ggplot() +
    # Add a base layer to show all countries, even ones we don't have data for
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             fill = "white",
             color = "black"
             ) +
    # Color the map based on the proportion of cancellations for that country
    geom_map(aes(map_id = country_name,
                 fill = mean_revenue),
             map = world_map) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      scale_fill_continuous(name = "", type = "viridis") +
      labs(title = "Figure 25: Average Revenue per Booking World Map",
           x = "Longitude (°)",
           y = "Lattitude (°)"
           )  
```

Most of the countries generated approximately 500 € per booking on average. Saudi Arabia, Egypt, Ghana, and Angola are all examples of countries that generate high revenue bookings. Madagascar is an example of a country that had a low average revenue per booking.

## Summary

There were four problems we sought to address. Firstly, we sought to identify the sources of the hotel bookings. Secondly, we set out to determine the frequency of cancellations, whether the cancellation was a no-show or not, and where the cancellations originate from. Thirdly, we built a model to forecast future booking prices. Finally, we set out to calculate the revenue generated by each booking, assess the amount of revenue salvaged from cancelled bookings, and identify the countries that generate the most revenue per booking.

### Methodology

Sources of bookings were found by counting the number of bookings associated with each `country` and `distribution channel`. Additionally, the proportion of bookings associated with repeated guests was evaluated using a time-series plot. The `country`, `distribution_channel`, and `is_repeated_guest` variables were the main sources of data for these analyses.

Frequency of cancellations was assessed by counting the number of canceled bookings according to `is_canceled`. Comparisons of cancellation rates were made between hotels and over time. Data from the `reservation_status` variable was used to determine the proportion of cancellations that were the result of no-shows. The sources of cancellations were found by comparing the proportion of cancellations between distribution channels and countries. These were found using the `distribution_channel` and `country` variables, respectively.

We used time-series analyses to identify trends in the `adr` data. We took `adr` to be a representation of what the market will bear in terms of booking pricing. Before producing the time-series plot, we calculated the average `adr` for each month to smooth out fluctuations within each month. Then, we generated an ARIMA model for forecasting future monthly mean `adr`s. This model was used to produce a 2-year forecast of the mean `adr` for each month.

A revenue variable was added to the dataset by multiplying the `total_stay_length` by `adr` for their booking. `total_stay_length` was calculated by summing `stays_in_weekend_nights` and `stays_in_week_nights` for each booking. Revenue from the bookings was then tabulated by both `hotel` and the year of the `arrival_date`. To assess the impact of `deposit_type` on revenue generation from cancelled booking, revenue for cancelled bookings was tabulated by `reservation_status` and `deposit_type`. Finally, average `revenue` per booking was calculated for each `country` and plotted on a world map.

### Results

Portugal had the largest proportion of bookings out of all countries that had bookings. The hotels are located in Portugal, so it makes sense that there would be a large number of bookings from that country. While Europe had the most bookings, guests were visiting from every continent. This implies that advertising should focused in the European market, since the hotels are already attracting guests from this region. Travel agents and tour operators are the largest distribution channel by bookings for both hotels. Building customer relations with these entities will be essential to the success of the hotels.

Only 3% of guests are repeat guests. Repeat guests are not driving repeat bookings directly. Seasonal trends are present in the time-series for the proportion of guests that are repeat guests. Specifically, the proportion of repeat guests increases each winter. This implies that repeat guests are less deterred by the winter weather than new guests. Targeted advertising towards these former guests may be an effective way to drive up bookings in the winter.

37.13% of all of the bookings in the dataset ended in a cancellation. Guests almost always gave their hotels prior notice before canceling. However, around 2.7% of the bookings were no-shows. The City hotel consistently had more cancellations than the Resort hotel. Furthermore, most of the no-show cancellations were at the City hotel. These trends suggest that corrective action should be taken at the City hotel to reduce the cancellations.

Travel agents and tour operators were the distribution channel that had the largest proportion of cancellations. Cancellation rates were 45% and ~32% for the City and Resort hotels for this channel, respectively. Despite having less than a 50% cancellation rate, bookings made by travel agents and tour operators constituted over 80% of all cancellations. This is likely due to the large proportion of bookings that were made using this channel. Reducing the cancellations from this channel could be very productive for reducing cancellations. Alternatively, the cancellation rates can be used to set prices to offset the losses due to cancellations.

27,345 cancellations were from bookings originating in Portugal. This was an order of magnitude larger than the next highest country on the list. However, due to the large number of bookings made by Portuguese guests the cancellation rate was only ~56.6%. Finding and eliminating the causes of these Portuguese cancellations should be beneficial because of the sheer number of cancellations. Countries that were farther away from Portugal tended to have higher cancellation rates, as did countries located in the southern hemisphere.

Monthly average `adr` displayed strong cyclic trends when plotted as a time-series. It was highest in the summer months and lowest in the winter months. Our ARIMA model forecasted that the monthly average `adr` would decrease going into winter 2018 and then level off.

The City hotel made more revenue than the Resort hotel in 2016 and 2017, but not 2015. If the hotels enjoy the same success they had in 2015 for the rest of 2017, they should both surpass their revenues from 2016. The hotels managed to recoup some of their revenue lost to cancelled bookings. No-show bookings earned revenues 2-3 orders of magnitude lower than canceled bookings. This implies that stringent efforts should be made to avoid no-show cancellations. Alternatively, no-show bookings should be filled as soon as possible. Strangely, no-deposit cancelled bookings had the highest revenues out of all of the deposit types.

Most countries generated an average booking revenue of ~500€. Saudi Arabia, Egypt, Ghana, Angola, and Russia are all countries that generated high average revenues per booking. This implies that an advertising campaign in these countries would be very lucrative.

### Future Work

A general limitation of this dataset is that it only contains data from part of 2015, 2016, and part of 2017. That makes it difficult to assess any long term trends. Identification of long term trends would require gathering new data to extend the time period under consideration.

Another inherent limitation of the hotels dataset is that there is only the City hotel and the Resort hotel. Making generalizations about the hospitality industry in Portugal from these two hotels is not reasonable. A larger sample would need to be taken from the population of Portuguese hotels to support such an analysis.

All three maps are limited by the `map_data` from the `maps` package. Lack of granularity was the main issue. There were several countries such as Gibraltar that were not represented in the regions of the map. To compensate for that, they were allocated to their controlling country or a geographically close country (Spain in the case of Gibraltar). This may have affected the results where calculations were performed by country. In the future, it would be best to use a more comprehensive map.

While we were able to identify cyclic trends in the monthly average `adr`, forecasting was not feasible. This is likely because our ARIMA model was based on the 26 monthly average `adr` results for this dataset. It shows a declining monthly average `adr` at the start of the forecast, matching the cyclic trend. But then it reaches an asymptote. For that to happen, a hotel would have to apply their off-peak rates to their peak demand. Clearly, this is an unrealistic result. Expanding the dataset to longer periods should give more realistic results. Alternatively, model performance might be improved by training on the daily average `adr`.

One of the strange results that was obtained in this analysis was that no-deposit cancelled bookings earned over 12 million € in revenue. However, no-deposit bookings are supposed to be made without making a payment to secure the booking [1]. It is unclear where the revenue from the no-deposit cancelled bookings is coming from. That would be a good matter to discuss with subject matter experts and further analysis.

## References

[1] N. Antonio, A. de Almeida and L. Nunes, "Hotel booking demand datasets," *Data in Brief*, vol. 22, pp. 41-49, 2019.

[2] T. Mock and A. Bichat. "Hotels." rfordatascience / tidytuesday. https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-11/readme.md (accessed Nov. 4, 2021).

[3] T. Zu. "Final Project". Course: Data Wrangling with R. https://zzz1990771.github.io/data_wrangling/final-project (accessed Nov. 4, 2021)
