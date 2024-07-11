## Extra functions I've found or made for this project:

## Found this state abbreviation amendment on Stack Overflow from Ben G.
st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))
# replaces the default:
# diaspora_data%>%
#   mutate(ab = state.abb[state])


## state.name function does not seem to be working for me
abbr2state <- function(abbr) {
  ab <- tolower(c(
    "AL",
    "AK", "AZ", "KS", "UT", "CO", "CT",
    "DE", "FL", "GA", "HI", "ID", "IL",
    "IN", "IA", "AR", "KY", "LA", "ME",
    "MD", "MA", "MI", "MN", "MS", "MO",
    "MT", "NE", "NV", "NH", "NJ", "NM",
    "NY", "NC", "ND", "OH", "OK", "OR",
    "PA", "RI", "SC", "SD", "TN", "TX",
    "CA", "VT", "VA", "WA", "WV", "WI",
    "WY", "DC"
  ))
  st <- c(
    "Alabama",
    "Alaska", "Arizona", "Kansas",
    "Utah", "Colorado", "Connecticut",
    "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois",
    "Indiana", "Iowa", "Arkansas",
    "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico",
    "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas",
    "California", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin",
    "Wyoming", "District of Columbia"
  )
  st[match(tolower(abbr), ab)]
}
## source: https://rdrr.io/cran/usdata/src/R/abbr2state.R