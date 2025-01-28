library(marinecs100b)


# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.

site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day
# Calculates the average number of hours per day with extreme cold temps. Does
# this by grabbing the number of cold hours at a given site in a given season /
# site.

# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?
# average_cold_hours(site, season)

# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.
average_cold_hours <- function(site, season){
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_cold_per_day <- hours_cold / days_total
  return(hours_cold_per_day)
}
average_cold_hours("Harris", "Late winter")

# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_type <- "cold"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == 'hot') {
  is_extreme <- kefj_temperature >= 25
}
is_extreme

# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.
site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                is_extreme &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day

# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.
average_extreme_hours <- function(site, season, extreme_type){
  if (extreme_type == "cold") {
    is_extreme <- kefj_temperature <= -4
  } else if (extreme_type == 'hot') {
    is_extreme <- kefj_temperature >= 25
  }
  n_extreme <- sum(kefj_site == site &
                  kefj_season == season &
                  is_extreme &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_extreme<- n_extreme * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_extreme_per_day <- hours_extreme / days_total
  return(hours_extreme_per_day)
}

# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?
unique(kefj_season)

# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

seasons <- unique(kefj_season)
  for (season in seasons) {
    heat_exposure <- average_extreme_hours("Aialik", season, "hot")
    cold_exposure <- average_extreme_hours("Aialik", season, "cold")
    print(paste("Aialik", season, heat_exposure, cold_exposure))
}

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.
sites <- unique(kefj_site)
for(site in sites){
  for (season in seasons) {
    heat_exposure <- average_extreme_hours(site, season, "hot")
    cold_exposure <- average_extreme_hours(site, season, "cold")
    print(paste(site, season, heat_exposure, cold_exposure))
  }
}

# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?
# Fall at Harris and Nuka Bay
