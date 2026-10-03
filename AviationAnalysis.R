# Load the original BTS dataset
flights <- read.csv("data/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2025_1.csv")

# Packages install
install.packages("tidyverse")
library(tidyverse)
install.packages("gapminder")
library(gapminder)

# Table overview
summary(flights)
names(flights)
head(flights)

# Are there any NA
summary(flights$Cancelled)
summary(flights$Diverted)
summary(flights$ArrDelay)

flights$FlightDate <- as.Date(flights$FlightDate)
head(flights)
print(range(flights$FlightDate, na.rm = TRUE))

# Month overview
overview <- flights |>
  summarise(total_flights = n(),
            cancelled_flights = sum(Cancelled == 1),
            cancelled_pct = 100 * mean(Cancelled == 1),
            diverted_flights = sum(Diverted == 1))
print(overview)

# In BTS documentation, there is a 15-min delay threshold
# Select completed non-diverted flights with known arrival delay
arrivals <- flights |>
  filter(Cancelled == 0,
         Diverted == 0,
         !is.na(ArrDelay))
summary(arrivals$ArrDelay)
# Delays
#arrdelay = arrival delay
delay_summary <- arrivals |>
  summarise(flights_n = n(),
            delayed_15_pct = 100 * mean(ArrDelay >= 15),
            mean_arrdelay_min = mean(ArrDelay),
            median_arrdelay_min = median(ArrDelay))
print(delay_summary)
# Mean and median include early arrivals.
# This shows the schedule deviation for all flights in the selected group,
# not just the average duration of delays.

# The share of delayed arrivals is calculated for each day
daily_delays <- arrivals |>
  group_by(FlightDate) |>
  summarise(flights_n = n(),
            delayed_pct = 100 * mean(ArrDelay >= 15))

# The first graph will show in which had more frequent delays
p <- ggplot(daily_delays, aes(x = FlightDate, y = delayed_pct)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(title = "Arrival delays by day - January 2025",
       subtitle = "Completed non-diverted flights",
       x = "Date",
       y = "Arrivals with the delay of 15+ min, %") +
  theme_minimal()
print(p)

# Examine the arrival delay distribution
arrival_percentiles <- quantile(arrivals$ArrDelay,
                                probs = c(0.50, 0.75, 0.90, 0.95, 0.99))
print(arrival_percentiles)

# Measure early arrivals and long delays
delay_details <- arrivals |>
  summarise(early_arrivals_pct = 100 * mean(ArrDelay < 0),
            delayed_60plus_pct = 100 * mean(ArrDelay >= 60),
            delayed_180plus_pct = 100 * mean(ArrDelay >= 180),
            mean_delay_among_delayed_min = mean(ArrDelay[ArrDelay >= 15]))
print(delay_details)

# Airline comparison
# How do cancellation and arrival delay rates vary across airlines?
# Do arrival delay differences persist when comparing the same routes?

# Calculate flight totals and cancellation rates
airline_totals <- flights |>
  group_by(Reporting_Airline) |>
  summarise(total_flights = n(),
            cancelled_pct = round(100 * mean(Cancelled == 1), 2))

# Calculate arrival delay rates
airline_delays <- arrivals |>
  group_by(Reporting_Airline) |>
  summarise(arrival_flights = n(),
            delayed_15_pct = round(100 * mean(ArrDelay >= 15), 2),
            delayed_60_pct = round(100 * mean(ArrDelay >= 60), 2))
print(airline_delays, n = Inf)

# Combine the summaries using the airline code
airline_comparison <- airline_totals |>
  left_join(airline_delays, by = "Reporting_Airline") |>
  arrange(desc(delayed_15_pct))

# Display all rows and columns
print(airline_comparison, n = Inf, width = Inf)

# Table of the airline comparison
View(airline_comparison)

# Calculate delay rates by airline and directional route
route_delays <- arrivals |>
  filter(Reporting_Airline %in% c("UA", "WN")) |>
  group_by(Reporting_Airline, Origin, Dest) |>
  summarise(flights_n = n(),
            delayed_pct = 100 * mean(ArrDelay >= 15),
            .groups = "drop") |>
  filter(flights_n >= 30)

# Prepare United Airlines (UA) route statistics
ua_routes <- route_delays |>
  filter(Reporting_Airline == "UA") |>
  select(Origin, Dest, ua_flights = flights_n,
         ua_delayed_pct = delayed_pct)

# Prepare Southwest Airlines (WN) route statistics
wn_routes <- route_delays |>
  filter(Reporting_Airline == "WN") |>
  select(Origin, Dest, wn_flights = flights_n,
         wn_delayed_pct = delayed_pct)

# Compare airlines on shared routes
route_comparison <- ua_routes |>
  inner_join(wn_routes, by = c("Origin", "Dest")) |>
  mutate(delay_gap_pp = ua_delayed_pct - wn_delayed_pct) |>
  arrange(desc(delay_gap_pp))

# Check the number of shared routes
print(nrow(route_comparison))

# Comparison table
View(route_comparison)
# Positive delay_gap_pp means a higher arrival delay rate for UA

# Does the difference between UA and WN persist on shared routes?
# Summarise differences with equal weight for each route
route_summary <- route_comparison |>
  summarise(
    shared_routes = n(),
    ua_higher_delay_routes = sum(delay_gap_pp > 0),
    wn_higher_delay_routes = sum(delay_gap_pp < 0),
    equal_delay_routes = sum(delay_gap_pp == 0),
    ua_mean_route_delay_pct = mean(ua_delayed_pct),
    wn_mean_route_delay_pct = mean(wn_delayed_pct),
    mean_gap_pp = mean(delay_gap_pp))
print(route_summary, width = Inf)

# In the aggregate table, UA's delay rate is 2.42 percentage points higher.
# However, after switching to the selected shared routes with equal weighting,
# the difference shrinks to just about 0.15 points in the opposite direction. 
# This indicates that the comparison results heavily depend on the choice of routes and the calculation method.

# Plot the distribution of route-level delay rate differences
route_gap_plot <- ggplot(route_comparison,
  aes(x = delay_gap_pp)) + 
  geom_histogram(binwidth = 5,
                 boundary = 0,
                 fill = "steelblue",
                 color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Arrival delay differences on shared routes",
       subtitle = "UA vs WN | January 2025",
       x = "Delay rate difference: UA minus WN (percentage points)",
       y = "Number of routes",
       caption = "At least 30 eligible arrivals per airline per route.") +
  theme_minimal()
print(route_gap_plot)

# Now, let's check how sensitive the results are to the chosen threshold of 30 flights
# Select routes with higher flight counts
routes_60 <- route_comparison |>
  filter(ua_flights >= 60, wn_flights >= 60)
routes_100 <- route_comparison |>
  filter(ua_flights >= 100, wn_flights >= 100)

# Compare results across minimum flight thresholds
threshold_check <- data.frame(
  min_flights_per_airline = c(30, 60, 100),
  shared_routes = c(nrow(route_comparison),
                    nrow(routes_60),
                    nrow(routes_100)),
  mean_gap_pp = c(mean(route_comparison$delay_gap_pp),
                  mean(routes_60$delay_gap_pp),
                  mean(routes_100$delay_gap_pp)))
print(threshold_check)

# With equal weights on the selected shared routes, UA's average delay share is slightly lower.
# However, the size of the gap varies with the specific set of routes.
# Therefore, the outcome for shared routes does not stem exclusively from the 30-flight threshold.

# Next, examine how arrival delay rates change during the month.
# Compare UA and WN on the 139 shared routes selected earlier.
# Each daily percentage is calculated from the eligible flights that day.

daily_airline_delays <- arrivals |>
  filter(Reporting_Airline %in% c("UA", "WN")) |>
  inner_join(route_comparison, by = c("Origin", "Dest")) |>
  group_by(FlightDate, Reporting_Airline) |>
  summarise(flights_n = n(),
            delayed_pct = 100 * mean(ArrDelay >= 15),
            .groups = "drop")

# Show each airline in a separate panel
daily_airline_plot <- ggplot(daily_airline_delays,
                             aes(x = FlightDate, y = delayed_pct)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ Reporting_Airline, ncol = 1) +
  labs(title = "Daily arrival delay rates: UA and WN",
       subtitle = "January 2025 | Flights on 139 shared routes",
       x = "Date",
       y = "Arrivals delayed by 15+ minutes (%)") +
  theme_minimal()
print(daily_airline_plot)