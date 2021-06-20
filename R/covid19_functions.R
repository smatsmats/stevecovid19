# for sure these
library("aws.s3")
library("zoo")
library("scales")

#library("tidyverse")   # maybe we don't need the whole -verse
# tidyverse things
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("stringr")

# mapping packages
library("ggmap")
library("maps")
library("mapdata")

# design decisions
# - don't combine plotting and making df"s
# - except where it makes sense, i.e. build all states
#
#

# tmp goes in tmp, everything is tmp
setwd("/tmp")

# Some flags
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
VERBOSE <- FALSE
KEEP_FILES <- FALSE      # don't remove files after being pushed
live_mode <- FALSE

# don't push to amazon if we don't have the environment vars
if (Sys.getenv("AWS_DEFAULT_REGION") == "") {
  cat("No AWS creds in environment\n")
  cat("turning off AWS pushes\n")
  PUSH_TO_AMAZON <- FALSE
} else {
  if (Sys.getenv("BUCKET") == "") {
    cat("Must set environment var BUCKET\n")
    quit()
  }
  else {
    bucket <- Sys.getenv("BUCKET")
  }
  PUSH_TO_AMAZON <- TRUE
}

# constants
plot_start_date <- "2020/3/1"  # not the earliest case in WA but ...
plot_end_date <-
  format(Sys.Date(), "%Y/%m/%d") #gets reset in newday function
cumulative_c19_cases_txt <- "Cumulative COVID-19 Cases"
daily_c19_cases_txt <- "Daily COVID-19 Cases"
fourteen_day_avrg_txt <- "14day Average"
fourteen_day_sum_txt <- "14day Sum"
redblue_txt <- "Red / Blue"
hundy_txt <- "per 100,000"
main_daily_cases_hundy_txt <- paste(daily_c19_cases_txt, hundy_txt)
main_daily_cases_hundy_14d_avrg_txt <-
  paste(daily_c19_cases_txt, hundy_txt, fourteen_day_avrg_txt)
main_daily_cases_hundy_14d_sum_txt <-
  paste(daily_c19_cases_txt, hundy_txt, fourteen_day_sum_txt)
main_14day_trend_txt <-
  "14day Trend (of cases per 100,000, 14day average)"
main_cases_hundy_txt <- paste(cumulative_c19_cases_txt, hundy_txt)
ylab_cases_txt <- "Cases"
ylab_daily_cases_txt <- "Daily Cases"
ylab_cases_hundy_txt <- "Cases / 100,000 Population"
ylab_daily_cases_hundy_txt <- "Daily Cases / 100,000 Population"
plot_file_width <- (480 * 2)
plot_file_height <- (310 * 2)

file_to_bucket <- function(file, unlink_after = TRUE) {
  if (PUSH_TO_AMAZON) {
    file <- str_replace_all(file, " ", "_")
    put_object(
      file = file,
      bucket = bucket,
      multipart = FALSE,
      acl = "public-read",
      headers = list(),
      verbose = TRUE,
      show_progress = FALSE
    )
  }
  if (unlink_after & !KEEP_FILES) {
    unlink(file)
  }
  
  return(0)
}

# reads in population file date and does some formating
get_population <- function() {
  uid_iso_fips_lookup <-
    read.csv(
      "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv?raw=true"
    )
  uid_iso_fips_lookup <<- mash_combined_key(uid_iso_fips_lookup)
  population <- uid_iso_fips_lookup
  
  return(population)
}

pop_format <- function(pop) {
  return(format(pop * 100000, big.mark = ","))
}

state_pop_txt <- function(s, df) {
  return(paste(s, " State (pop=", pop_format(df$pop[1]), ")", sep = ""))
}

onetime <- function() {
  # some datasets
  # 2016 presidential election results, by county
  if (ENABLE_RED_BLUE) {
    prez_2016 <<-
      read.csv(
        "https://raw.githubusercontent.com/mkearney/presidential_election_county_results_2016/master/pres.elect16.results.dec9.csv"
      )
    prez_2020 <<-
      read.csv(
        "https://github.com/kjhealy/us_elections_2020_csv/raw/master/results_current.csv"
      )
  }
  
  # don't use this anywhere
  #steve_usa <<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrt-fhmYSJ4BUiombXneAsK9BRLyRxwqxxu47pkpiFP6ZgRrXwm4V7frh_rtwPqQAIrCm4RrT8TFkM/pub?gid=931635221&single=true&output=csv")
  
  # info on wa counties
  wa_counties <<-
    read.csv(
      "https://docs.google.com/uc?id=19OOGc3UmvN77oqPP9JeRKFbGSzuxzxRQ&export=download"
    )
  
  # old filesystme grabs
  #  county_transformations <<- read.csv("/Users/willey/Google\ Drive/data/county_transformations.csv")
  #  wa_counties <<- read.csv("/Users/willey/Google\ Drive/data/wa_counties.csv")
  
  return(0)
}

newday <- function() {
  # reset end date
  plot_end_date <<- format(Sys.Date(), "%Y/%m/%d")
  
  # comes in wide
  usa_confirmed <<-
    read.csv(
      "https://github.com/CSSEGISandData//COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true"
    )
  global_confirmed <<-
    read.csv(
      "https://github.com/CSSEGISandData//COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv?raw=true"
    )
  
  # clean-up global-confirmed
  global_confirmed[c("Lat", "Long")] <- NULL
  # make long
  global_confirmed_t <<- pivot_longer(
    global_confirmed,
    cols = starts_with("X"),
    names_to = "dates",
    values_to = "cases",
    values_drop_na = FALSE
  )
  global_confirmed_t$dates <<-
    as.Date(global_confirmed_t$dates,  format = "X%m.%d.%y")
  global_confirmed_t$admin0 <<-
    tolower(global_confirmed_t$Country.Region)
  admin0_t <<-
    global_confirmed_t %>% group_by(Country.Region, dates) %>% summarise(cases =
                                                                           sum(cases))
  
  uc <- usa_confirmed
  
  # remove soem junk
  uc[c(
    "UID",
    "iso2",
    "iso3",
    "code3",
    "FIPS",
    "Country_Region",
    "Lat",
    "Long_",
    "Combined_Key"
  )] <- NULL
  usa_confirmed_t <<-
    pivot_longer(
      uc,
      cols = starts_with("X"),
      names_to = "dates",
      values_to = "cases",
      values_drop_na = FALSE
    )
  usa_confirmed_t$dates <<-
    as.Date(substr(usa_confirmed_t$dates, 2, 20),
            format = "%m.%d.%y")
  usa_confirmed_t$state_ <<- tolower(usa_confirmed_t$Province_State)
  usa_states <<-
    usa_confirmed_t %>% group_by(Province_State, dates) %>% summarise(cases =
                                                                        sum(cases))
  
  # pivot back wide to get the nice wide version
  us_states_wide_raw <<- pivot_wider(
    usa_states,
    id_cols = Province_State,
    names_from = dates,
    values_from = cases
  )
  
  return(0)
}

vax_data <- function() {
  vax_global_wide_raw <<-
    read.csv(
      "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv"
    )
  vax_us_wide_raw <<-
    read.csv(
      "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_doses_admin_US.csv"
    )
  vax_global_wide <- mash_combined_key(vax_global_wide_raw)
  vax_us_wide <- mash_combined_key(vax_us_wide_raw)
  latest_g <- dim(vax_global_wide_raw)[2] - 1
  latest_u <- dim(vax_us_wide_raw)[2] - 1
  
  vax_global_wide <<-
    summarize_vax_wide_data(vax_global_wide, latest_g)
  vax_us_wide <<- summarize_vax_wide_data(vax_us_wide, latest_u)
  
  write.csv(vax_us_wide, "vax_us_wide.csv")
  
  return(0)
  
}

get_pop_jhu <- function(province_state = "",
                        country = "US",
                        admin2 = "") {
  # legacy
  if (is.null(admin2)) {
    admin2 <- ""
  }
  # legacy
  if (admin2 == "Total") {
    admin2 <- ""
  }
  if (is.null(country)) {
    country <- "US"
  }
  if (is.null(province_state)) {
    province_state <- ""
  }
  
  if (VERBOSE) {
    cat(
      "get_pop_jhu: going to get pop for: country:",
      country,
      "state:",
      province_state,
      "admin2:",
      admin2,
      "\n"
    )
  }
  
  row <- subset(
    uid_iso_fips_lookup,
    grepl(country, Country_Region, ignore.case = TRUE) &
      grepl(province_state, Province_State, ignore.case = TRUE) &
      grepl(admin2, Admin2, ignore.case = TRUE)
  )
  
  hundy = row$Population[1] / 100000.0
  
  if (is.na(hundy)) {
    hundy <- 0
  }
  
  return(hundy)
}

get_pop <- function(state = NULL,
                    county = NULL,
                    country = NULL,
                    province_state = NULL,
                    admin2 = NULL) {
  if (!is.null(province_state)) {
    state <- province_state
  }
  if (!is.null(admin2)) {
    county <- admin2
  }
  admin2 <- county
  province_state <- state
  
  return(get_pop_jhu(
    country = country,
    province_state = province_state,
    admin2 = admin2
  ))
  
}

# cleans-up some goofy county names - needed this with US Census pops and
# election results
get_full_county_name <- function(state = "not alaska",  county) {
  if (state == "Alaska") {
    full_county_name <- county
  } else if (state == "District of Columbia") {
    full_county_name <- "District of Columbia"
  } else if (state == "Louisiana") {
    full_county_name = paste(county, "Parish")
  } else {
    mystate <- state
    trans <-
      subset(county_transformations,
             state == mystate & county_in == county)
    if (nrow(trans) == 1) {
      full_county_name <- trans$county_out
    } else {
      full_county_name = paste(county, "County")
    }
  }
  return(full_county_name)
}

# old function where we only returned winning prez candidate
get_2016_prez <- function(state, county) {
  mycounty <- county
  if (state == "Alaska") {
    hill_trump <- subset(prez_2016, state.name == state)$lead[1]
  } else if (state == "District of Columbia") {
    hill_trump <- subset(prez_2016, county == county)$lead[1]
  } else {
    hill_trump <-
      subset(prez_2016, state.name == state &
               county == mycounty)$lead[1]
  }
  if (is.na(hill_trump)) {
    return("unkown")
  } else {
    return(hill_trump)
  }
}

get_redblue2016 <- function(state, county) {
  red_cand <- "Donald Trump"
  blue_cand <- "Hillary Clinton"
  if (state == "Louisiana") {
    mycounty <- paste(county, "Parish")
  } else {
    mycounty <- get_full_county_name(state, county)
  }
  
  if (state == "District of Columbia") {
    red_pct_t <- subset(prez_2016, county == state &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016, county == state &
                           cand == blue_cand)$pct[1]
    
  } else if (state == "Alaska" || state == "District of Columbia") {
    red_pct_t <- subset(prez_2016, state.name == state &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016, state.name == state &
                           cand == blue_cand)$pct[1]
    
  } else {
    red_pct_t <- subset(prez_2016,
                        state.name == state &
                          county == mycounty &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016,
                         state.name == state &
                           county == mycounty &
                           cand == blue_cand)$pct[1]
  }
  #  print(paste(red_pct_t, blue_pct_t, ","))
  red_pct <- red_pct_t / (red_pct_t + blue_pct_t)
  blue_pct <- blue_pct_t / (red_pct_t + blue_pct_t)
  return(c(red_pct, blue_pct))
}

get_redblue <-  function(state, county) {
  get_redblue2016(state, county)
}

if (live_mode) {
  print(paste("Washington", "Island", get_redblue("Washington", "Island")))
  print(paste(
    "Washington",
    "Columbia",
    get_redblue("Washington", "Columbia")
  ))
  print(paste(
    "Washington",
    "Garfield",
    get_redblue("Washington", "Garfield")
  ))
  print(paste("Alaska", "bumfuck", get_redblue("Alaska", "bumfuck")))
  print(paste(
    "Louisiana",
    "Terrebonne",
    get_redblue("Louisiana", "Terrebonne")
  ))
  print(paste(
    "District of Columbia",
    "dc",
    get_redblue("District of Columbia", "dc")
  ))
}

make_plot <- function(df,
                      loc_txt,
                      main_txt = NULL,
                      cases_per_hundy = TRUE,
                      cases = TRUE,
                      daily_cases = FALSE,
                      file_base = NULL) {
  # maybe bail
  if (is.null(df)) {
    return(NULL)
  }
  
  # bail if we have no population
  if (df[1, ]$pop == 0) {
    return(NULL)
  }
  
  # maybe override the global cumulative_c19_cases_txt
  if (!is.null(main_txt)) {
    cumulative_c19_cases_txt = main_txt
  }
  
  if (cases_per_hundy) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }
    
    p <- ggplot(data = df, aes(x = dates, y = cases_per_hundy)) +
      geom_line(colour = "purple", na.rm = FALSE) +
      labs(
        title = paste(loc_txt, cumulative_c19_cases_txt, hundy_txt),
        subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
        x = "Dates",
        y = ylab_cases_hundy_txt
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
      )
    print(p)
    
    if (!is.null(file_base)) {
      dev.off()
    }
    
  }
  
  if (cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }
    
    p <- ggplot(data = df, aes(x = dates, y = cases)) +
      geom_line(colour = "purple", na.rm = FALSE) +
      labs(
        title = paste(loc_txt, cumulative_c19_cases_txt),
        subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
        x = "Dates",
        y = ylab_cases_txt
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
      )
    
    print(p)
    
    if (!is.null(file_base)) {
      dev.off()
    }
  } # if cases
  
  if (daily_cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_daily_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }
    
    p <- ggplot(data = df, aes(dates)) +
      geom_line(
        aes(y = daily_cases_per_hundy, colour = "Daily"),
        size = 0.3,
        na.rm = FALSE
      ) +
      scale_color_manual(values = c("14 Day Average / Sum" = "red",
                                    "Daily" = "mediumpurple1")) +
      labs(
        title = paste(loc_txt, main_daily_cases_hundy_txt),
        subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y"))
      ) +
      scale_x_date(name = "Dates") +
      scale_y_continuous(
        name =  ylab_daily_cases_hundy_txt,
        limits = c(0, max(df$daily_cases_per_hundy)),
        sec.axis = sec_axis(trans =  ~ . * 14, name = "14 Day Sum / 100,000 Population")
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.35, 0.87),
        legend.background = element_rect(
          linetype = "solid",
          size = 0.2,
          colour = "black"
        )
      ) +
      geom_line(aes(y = daily_cases_per_hundy_avrg14d,
                    colour = "14 Day Average / Sum"),
                na.rm = FALSE)
    
    print(p)
    
    if (!is.null(file_base)) {
      dev.off()
    }
  } # if daily cases
  
  # just return something not NULL
  return(p)
}

multi_make_plot <- function(df,
                            multi_cats,
                            loc_txt = "loc_txt",
                            main_txt = NULL,
                            cases_per_hundy = TRUE,
                            cases = TRUE,
                            daily_cases = FALSE,
                            file_base = NULL) {
  p <- ggplot(data = df, aes(dates)) +
    scale_color_manual(values = c("14 Day Average" = "red",
                                  "Daily" = "mediumpurple1")) +
    #   ylim(0,max(df$daily_cases)) +
    labs(
      title = paste(loc_txt, daily_c19_cases_txt),
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  for (cat in multi_cats) {
    p <- p + geom_line(aes(y = cat, colour = "Daily"), size = 0.3)
  }
  
  
  #    geom_line(aes(y = daily_cases_avrg14d, colour="14 Day Average"))
  
  print(p)
  
  if (!is.null(file_base)) {
    dev.off()
  }
  
} # multi_make_plot

build_cols <- function(df, pop) {
  if (pop > 0) {
    df$pop <- pop
    df$cases_per_hundy <- df$cases / pop
    # ifelse(df$cases_per_hundy < 0, 0, df$cases_per_hundy)
  } else {
    df$pop <- 0
    df$cases_per_hundy <- 0
  }
  
  # get the daily deltas
  df$daily_cases <- ave(
    df$cases,
    FUN = function(x)
      c(0, diff(x))
  )
  df$daily_cases_per_hundy <- df$daily_cases / pop
  #make na"s zero (first daily starts as an NA)
  #  df$daily_cases[is.na(df$daily_cases)] <- 0
  
  # rolling averages
  df$daily_cases_avrg7d <-
    zoo::rollmean(df$daily_cases,
                  k = 7,
                  fill = NA,
                  align = "right")
  df$daily_cases_avrg7d[is.na(df$daily_cases_avrg7d)] <- 0
  df$daily_cases_avrg14d <-
    zoo::rollmean(df$daily_cases,
                  k = 14,
                  fill = NA,
                  align = "right")
  df$daily_cases_avrg14d[is.na(df$daily_cases_avrg14d)] <- 0
  
  df$daily_cases_sum7d <-
    zoo::rollsum(df$daily_cases,
                 k = 7,
                 fill = NA,
                 align = "right")
  df$daily_cases_sum7d[is.na(df$daily_cases_avrg7d)] <- 0
  df$daily_cases_sum14d <-
    zoo::rollsum(df$daily_cases,
                 k = 14,
                 fill = NA,
                 align = "right")
  df$daily_cases_sum14d[is.na(df$daily_cases_avrg14d)] <- 0
  
  if (pop > 0) {
    df$daily_cases_per_hundy_avrg7d <- df$daily_cases_avrg7d / pop
    df$daily_cases_per_hundy_avrg14d <- df$daily_cases_avrg14d / pop
    df$daily_cases_per_hundy_sum7d <- df$daily_cases_sum7d / pop
    df$daily_cases_per_hundy_sum14d <- df$daily_cases_sum14d / pop
    #ifelse(df$cases_per_hundy < 0, 0, df$cases_per_hundy)
  } else {
    df$daily_cases_per_hundy_avrg7d <- 0
    df$daily_cases_per_hundy_avrg14d <- 0
    df$daily_cases_per_hundy_sum7d <- 0
    df$daily_cases_per_hundy_sum14d <- 0
  }
  
  if (ENABLE_RED_BLUE) {
    red_blue_pcts <- get_redblue(state, county)
    
    df$red_cases <- df$cases * red_blue_pcts[1]
    df$red_daily_cases <-
      df$daily_cases * red_blue_pcts[1]
    
    df$red_daily_cases_avrg7d <-
      df$daily_cases_avrg7d * red_blue_pcts[1]
    df$red_daily_cases_avrg14d <-
      df$daily_cases_avrg14d * red_blue_pcts[1]
    df$red_daily_cases_per_hundy_avrg7d <-
      df$daily_cases_per_hundy_avrg7d * red_blue_pcts[1]
    df$red_daily_cases_per_hundy_avrg14d <-
      df$daily_cases_per_hundy_avrg14d * red_blue_pcts[1]
    
    df$red_pop <- df$pop * red_blue_pcts[1]
    df$red_cases_per_hundy <- df$cases_per_hundy * red_blue_pcts[1]
    ifelse(df$red_cases_per_hundy < 0, 0, df$red_cases_per_hundy)
    
    df$blue_cases <- df$cases * red_blue_pcts[2]
    df$blue_daily_cases <-
      df$daily_cases * red_blue_pcts[2]
    
    df$blue_daily_cases_avrg7d <-
      df$daily_cases_avrg7d * red_blue_pcts[1]
    df$blue_daily_cases_avrg14d <-
      df$daily_cases_avrg14d * red_blue_pcts[1]
    df$blue_daily_cases_per_hundy_avrg7d <-
      df$daily_cases_per_hundy_avrg7d * red_blue_pcts[1]
    df$blue_daily_cases_per_hundy_avrg14d <-
      df$daily_cases_per_hundy_avrg14d * red_blue_pcts[1]
    
    df$blue_pop <- df$pop * red_blue_pcts[2]
    df$blue_cases_per_hundy <- df$cases_per_hundy * red_blue_pcts[2]
    ifelse(df$blue_cases_per_hundy < 0, 0, df$blue_cases_per_hundy)
  } #enable red blue
  
  return(df)
}

# selects a county
get_admin2 <- function(state, county) {
  cat("in get_admin2(", county, ")")
  
  if (county == "Total") {
    county_cases_t <- as.data.frame(subset(usa_states,
                                           Province_State == state))
  }
  else {
    # convert into a data frame instead of a tuple.  tuple has big performance impacts down the road
    county_cases_t <-
      as.data.frame(subset(usa_confirmed_t, Admin2 == county &
                             Province_State == state))
  }
  
  pop <- get_pop(state, county, country = "US")
  
  df <- build_cols(county_cases_t, pop)
  
  return(df)
  
}


if (live_mode) {
  b_ci_cases <-
    get_admin2("Maryland", "Baltimore City")
  make_plot(b_ci_cases, "bongo", "bingo")
  ic_cases <- get_admin2("Washington", "Island")
  
  make_plot(
    loc_txt = "Washington",
    "Island",
    df = ic_cases,
    daily_cases = TRUE,
    file_base = NULL
  )
  kc_cases <- get_admin2("Washington", "King")
  make_plot(
    loc_txt = "Washington",
    "King",
    df = kc_cases,
    daily_cases = TRUE,
    file_base = NULL
  )
  cc_cases <- get_admin2("Washington", "Columbia")
  ac_cases <- get_admin2("Washington", "Adams")
  wa_cases <- get_admin2("Washington", "Total")
  
  gc_cases <- get_admin2("Washington", "Garfield")
  tc_cases <- get_admin2("Louisiana", "Terrebonne")
  junk_new <- get_admin2("Virginia", "Lunenburg")
  
}

write_csv_file <- function(df, file_base) {
  write.csv(df, paste(file_base, ".csv", sep = ""))
}

make_redblue_plot <- function(df,
                              loc_txt,
                              main_txt = NULL,
                              cases_per_hundy = TRUE,
                              cases = TRUE,
                              file_base = NULL) {
  # maybe bail
  if (is.null(df)) {
    return(NULL)
  }
  
  # maybe override the global cumulative_c19_cases_txt
  if (!is.null(main_txt)) {
    cumulative_c19_cases_txt = main_txt
  }
  
  max_red <- 0
  max_blue <- 0
  try(max_red <- max(df$red_cases), silent = TRUE)
  try(max_blue <- max(df$blue_cases), silent = TRUE)
  max_y <- ifelse(max_red > max_blue, max_red, max_blue)
  
  max_red_cases_per_hundy <- 0
  max_blue_cases_per_hundy <- 0
  try(max_red_cases_per_hundy <-
        max(df$red_cases_per_hundy, 1),
      silent = TRUE)
  try(max_blue_cases_per_hundy <-
        max(df$blue_cases_per_hundy, 1),
      silent = TRUE)
  max_y_cases_per_hundy <-
    ifelse(
      max_red_cases_per_hundy > max_blue_cases_per_hundy,
      max_red_cases_per_hundy,
      max_blue_cases_per_hundy
    )
  #  print(max(df$red_per_hund, 1))
  #  print(paste(max_red_cases_per_hundy, max_blue_cases_per_hundy,max_y_cases_per_hundy, sep=", "))
  if (cases_per_hundy) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }
    plot(
      df$dates,
      df$red_cases_per_hundy,
      main = paste(loc_txt, redblue_txt, cumulative_c19_cases_txt, hundy_txt),
      ylab = ylab_cases_hundy_txt,
      xlab = "Dates",
      type = "l",
      col = "red",
      ylim = c(0, max_y_cases_per_hundy),
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    if (max_blue_cases_per_hundy > 0) {
      lines(df$dates, df$blue_cases_per_hundy, col = "blue")
    }
    if (!is.null(file_base)) {
      dev.off()
    }
    
  }
  
  if (cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
      #      print("on 2")
    }
    plot(
      df$dates,
      df$red_cases,
      main = paste(loc_txt, redblue_txt, cumulative_c19_cases_txt),
      ylab = ylab_cases_txt,
      xlab = "Dates",
      type = "l",
      col = "red",
      ylim = c(0, max_y),
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    if (max_blue > 0) {
      lines(df$dates, df$blue_cases, col = "blue")
    }
    if (!is.null(file_base)) {
      dev.off()
      #      print("off 2")
    }
    
  }
  
}


# var                           source          aggregate function
# pop                           get_pop         addition
# cases                         something       addition
# daily_cases                   something       addition
# cases_per_hundy               division        division
# daily_cases_per_hundy         division        division
# daily_cases_avgr7             rollmean        rollmean
# daily_cases_avgr7             rollmean        rollmean
# daily_cases_per_hundy_avrg7   division        division
# daily_cases_per_hundy_avrg14  division        division
#
aggregate_dfs <- function(in_df, new_df) {
  #    print(paste("in aggregate_dfs", in_df[nrow(in_df),"cases"], new_df[nrow(in_df),"cases"]))
  #    print(paste("in aggregate_dfs", in_df[1,"pop"], new_df[1,"pop"]))
  
  for (r in 1:nrow(in_df)) {
    #      print(paste("in:", in_df[r, "pop"], "new:", new_df[r, "pop"]))
    in_df[r, "pop"] <- in_df[r, "pop"] + new_df[r, "pop"]
    #      print(paste("combined:", in_df[r, "pop"]))
    
    in_df[r, "cases"] <- in_df[r, "cases"] + new_df[r, "cases"]
    in_df[r, "cases_per_hundy"] <-
      in_df[r, "cases"] / in_df[r, "pop"]
    in_df[r, "daily_cases"] <- in_df[r, "daily_cases"] +
      new_df[r, "daily_cases"]
    in_df[r, "daily_cases_per_hundy"] <-
      in_df[r, "daily_cases"] / in_df[r, "pop"]
    
    #      print(paste("where the fun starts", r))
    if (r < 7) {
      in_df[r, "daily_cases_avrg7d"] <- 0
      in_df[r, "daily_cases_per_hundy_avrg7d"] <- 0
      in_df[r, "daily_cases_sum7d"] <- 0
      in_df[r, "daily_cases_per_hundy_sum7d"] <- 0
    }
    else {
      #        print(in_df[(r-6):r, "daily_cases"])
      #        print(zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left"))
      #        print(zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left")[0])
      in_df[r, "daily_cases_avrg7d"] <-
        zoo::rollmean(in_df[(r - 6):r, "daily_cases"],
                      k = 7,
                      fill = NA,
                      align = "left")[1]
      in_df[r, "daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "daily_cases_avrg7d"] / in_df[r, "pop"]
      in_df[r, "daily_cases_sum7d"] <-
        zoo::rollsum(in_df[(r - 6):r, "daily_cases"],
                     k = 7,
                     fill = NA,
                     align = "left")[1]
      in_df[r, "daily_cases_per_hundy_sum7d"] <-
        in_df[r, "daily_cases_sum7d"] / in_df[r, "pop"]
    }
    if (r < 14) {
      in_df[r, "daily_cases_avrg14d"] <- 0
      in_df[r, "daily_cases_per_hundy_avrg14d"] <- 0
      in_df[r, "daily_cases_sum14d"] <- 0
      in_df[r, "daily_cases_per_hundy_sum14d"] <- 0
    }
    else {
      in_df[r, "daily_cases_avrg14d"] <-
        zoo::rollmean(in_df[(r - 13):r, "daily_cases"],
                      k = 14,
                      fill = NA,
                      align = "left")[1]
      in_df[r, "daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "daily_cases_avrg14d"] / in_df[r, "pop"]
      in_df[r, "daily_cases_sum14d"] <-
        zoo::rollsum(in_df[(r - 13):r, "daily_cases"],
                     k = 14,
                     fill = NA,
                     align = "left")[1]
      in_df[r, "daily_cases_per_hundy_sum14d"] <-
        in_df[r, "daily_cases_sum14d"] / in_df[r, "pop"]
    }
    
    
    #      in_df[r, "daily_cases_avrg7d"] <- in_df[r, "daily_cases_avrg7d"] +
    #        new_df[r, "daily_cases_avrg7d"]
    #      in_df[r, "daily_cases_avrg14d"] <- in_df[r, "daily_cases_avrg14d"] +
    #        new_df[r, "daily_cases_avrg14d"]
    #      in_df[r, "daily_cases_per_hundy_avrg7d"] <- in_df[r, "daily_cases_per_hundy_avrg7d"] +
    #        new_df[r, "daily_cases_per_hundy_avrg7d"]
    #      in_df[r, "daily_cases_per_hundy_avrg14d"] <- in_df[r, "daily_cases_per_hundy_avrg14d"] +
    #        new_df[r, "daily_cases_per_hundy_avrg14d"]
    
    
    #      print("kinda done")
    
    if (ENABLE_RED_BLUE) {
      in_df[r, "red_cases"] <-
        in_df[r, "red_cases"] + new_df[r, "red_cases"]
      #      print(r)
      if (r < 8) {
        in_df[r, "red_daily_cases_avrg7d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg7d"] <- 0
      }
      else {
        #        in_df[r, "red_daily_cases_avrg7d"] <- zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left")[0]
        #        in_df[r, "red_daily_cases_per_hundy_avrg7d"] <- in_df[r, "red_daily_cases_avrg7d"] / in_df[r, "pop"]
      }
      if (r < 15) {
        in_df[r, "red_daily_cases_avrg14d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- 0
      }
      else {
        #        in_df[r, "red_daily_cases_avrg14d"] <- zoo::rollmean(in_df[(r-13):r, "daily_cases"], k = 14, fill = NA, align="left")[0]
        #        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- in_df[r, "red_daily_cases_avrg14d"] / in_df[r, "pop"]
      }
      
      #      in_df[r, "red_daily_cases_avrg14d"] <- in_df[r, "red_daily_cases_avrg14d"] +
      #        new_df[r, "red_daily_cases_avrg14d"]
      
      in_df[r, "red_daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "red_daily_cases_per_hundy_avrg7d"] +
        new_df[r, "red_daily_cases_per_hundy_avrg7d"]
      in_df[r, "red_daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "red_daily_cases_per_hundy_avrg14d"] +
        new_df[r, "red_daily_cases_per_hundy_avrg14d"]
      
      
      in_df[r, "red_pop"] <-
        in_df[r, "red_pop"] + new_df[r, "red_pop"]
      in_df[r, "red_cases_per_hundy"] <-
        in_df[r, "red_cases_per_hundy"] +
        new_df[r, "red_cases_per_hundy"]
      in_df[r, "blue_cases"] <-
        in_df[r, "blue_cases"] + new_df[r, "blue_cases"]
      
      in_df[r, "blue_daily_cases_avrg7d"] <-
        in_df[r, "blue_daily_cases_avrg7d"] +
        new_df[r, "blue_daily_cases_avrg7d"]
      in_df[r, "blue_daily_cases_avrg14d"] <-
        in_df[r, "blue_daily_cases_avrg14d"] +
        new_df[r, "blue_daily_cases_avrg14d"]
      in_df[r, "blue_daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "blue_daily_cases_per_hundy_avrg7d"] +
        new_df[r, "blue_daily_cases_per_hundy_avrg7d"]
      in_df[r, "blue_daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "blue_daily_cases_per_hundy_avrg14d"] +
        new_df[r, "blue_daily_cases_per_hundy_avrg14d"]
      
      in_df[r, "blue_pop"] <-
        in_df[r, "blue_pop"] + new_df[r, "blue_pop"]
      in_df[r, "blue_cases_per_hundy"] <-
        in_df[r, "blue_cases_per_hundy"] +
        new_df[r, "blue_cases_per_hundy"]
    }
  } # enable red blue
  
  return(in_df)
  
}

get_admin1 <- function(admin1,
                       admin0 = "US") {
  cat("in get_admin1, state:",
      admin1,
      "country:",
      admin0,
      "\n")
  
  
  if (admin0 == "US") {
    state_cases_t <-
      as.data.frame(subset(
        usa_states,
        grepl(admin1, Province_State, ignore.case = TRUE)
      ))
  }
  else  {
    country <-
      admin0  #  ARRRG I don't understand why this is needed!!!!
    state_cases_t <- as.data.frame(subset(
      global_confirmed_t,
      grepl(country,
            Country.Region,
            ignore.case = TRUE) &
        grepl(admin1,
              Province.State,
              ignore.case = TRUE)
    ))
  }
  
  
  pop <- get_pop(admin1, country = admin0)
  
  df <- build_cols(state_cases_t, pop)
  
  return(df)
  
}


if (live_mode) {
  wa_cases <<- get_admin1("Washington")
  make_plot(wa_cases, "bongo", cases = TRUE)
  dp_cases <<- get_admin1("Diamond Princess")
  ca_bc_cases <<- get_admin1("British Columbia", admin0 = "Canada")
  make_plot(ca_bc_cases, "bongo", daily_cases = TRUE)
  dc_cases <<- get_admin1("District of Columbia")
  write.csv(wa_cases, "wa_cases.csv")
  pr_cases <- get_admin1("Puerto Rico")
}

get_admin0 <- function(country_in) {
  cat("in get_admin0:", country_in)
  
  # convert into a data frame instead of a tuple.
  # tuple has big performance impacts down the road
  country_cases_t <- as.data.frame(subset(
    admin0_t,
    grepl(country_in, Country.Region, ignore.case = TRUE)
  ))
  
  pop <- get_pop(country = country_in)
  
  df <- build_cols(country_cases_t, pop)
  
  return(df)
  
}


make_state_string <- function(state) {
  s <- tolower(state)
  s <- str_replace_all(tolower(s), " ", "_")
  return(s)
}

build_all_states <- function(combined = TRUE,
                             keep_dfs = FALSE,
                             write_dfs = FALSE,
                             plot_wa_and = FALSE,
                             plot_daily_cases = FALSE,
                             plot_state_cases_per_hundy = FALSE) {
  if (exists("usa_df")) {
    remove(usa_df, envir = .GlobalEnv)
  }
  
  if (plot_wa_and) {
    wa_cases <- get_admin1("Washington")
    max_wa_y = max(wa_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
    wa_s_txt <-
      paste("Washington (pop=", pop_format(wa_cases$pop[1]), ")", sep = "")
  }
  
  states <- unique(sort(usa_confirmed$Province_State))
  
  for (state in states) {
    if (VERBOSE) {
      print(paste("state is", state))
    }
    new_df <- get_admin1(state)
    
    if (is.null(new_df)) {
      next
    }
    
    file_base <- str_replace_all(tolower(state), " ", "_")
    if (write_dfs) {
      filename <- paste(file_base, "csv", sep = ".")
      write.csv(new_df, filename)
      file_to_bucket(filename)
    }
    
    if (keep_dfs) {
      st_string <- make_state_string(state)
      new_df_name <- paste(st_string, "_df", sep = "")
      assign(new_df_name, new_df, envir = .GlobalEnv)
      
      # make the text name for graphs
      txt_arg <- paste(st_string, "_s_txt", sep = "")
      txt_value <- paste(state,
                         " State (pop=",
                         pop_format(new_df$pop[1]),
                         ")",
                         sep = "")
      assign(txt_arg, txt_value, envir = .GlobalEnv)
    }
    
    if (combined) {
      if (exists("usa_df")) {
        usa_df <- aggregate_dfs(usa_df, new_df)
      }
      else {
        usa_df <- new_df
      }
    }
    
    # really no point if there isn"t anyone there
    if (new_df[1, ]$pop == 0) {
      next
    }
    
    if (plot_daily_cases) {
      ret <- make_plot(
        df = new_df,
        loc_txt = state,
        daily_cases = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "daily_cases.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }
    
    if (plot_state_cases_per_hundy) {
      ret <- make_plot(
        new_df,
        loc_txt = state,
        cases_per_hundy = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "cases_per_hundy.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }
    
    if (plot_wa_and) {
      # multiple counties 14 day
      filename <- paste("wa_and_", tolower(state), ".jpg", sep = "")
      filename <- str_replace_all(filename, " ", "_")
      
      jpeg(filename = filename,
           width = plot_file_width,
           height = plot_file_height)
      
      new_df$wa_cases <- wa_cases$cases
      new_df$wa_daily_cases <- wa_cases$daily_cases
      new_df$wa_daily_cases_per_hundy_avrg14d <-
        wa_cases$daily_cases_per_hundy_avrg14d
      s_txt <-
        paste(state, " (pop=", pop_format(new_df$pop[1]), ")", sep = "")
      
      p <- ggplot(data = new_df, aes(dates)) +
        geom_line(aes(y = daily_cases_per_hundy_avrg14d,
                      colour = s_txt)) +
        geom_line(aes(y = wa_daily_cases_per_hundy_avrg14d,
                      colour = wa_s_txt)) +
        scale_color_manual(values = c("black", "darkgreen")) +
        ylim(0, max(new_df$daily_cases_per_hundy_avrg14d)) +
        labs(
          title = "Daily Cases per 100,000, 14day Average",
          subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
          x = "Dates",
          y = ylab_daily_cases_hundy_txt
        ) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          #            panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = c(0.35, 0.87),
          legend.background = element_rect(
            linetype = "solid",
            size = 0.2,
            colour = "black"
          )
        )
      
      print(p)
      
      dev.off()
      file_to_bucket(filename)
    }
    
  } # for all states
  
  return(usa_df)
  
}

wa_east_west <- function(plot_casesned = FALSE,
                         plot_casesned_cases = FALSE,
                         file_base = NULL) {
  state = "Washington"
  loc_txt = "Eastern / Western Washington"
  
  cases <-
    as.data.frame(subset(usa_confirmed, Province_State == state))
  
  counties <- unique(sort(cases$Admin2))
  
  for (county in counties) {
    if (str_detect(county, "Out of ") |
        county == "" |
        county == "unkown" |
        county == "Unassigned") {
      next
    }
    
    if (wa_counties[which(wa_counties$county == county),]$eastwest == "eastern") {
      cat("east\n")
      east_df <- get_admin2(state = state, county = county)
      if (exists("combined_east_df")) {
        combined_east_df <- aggregate_dfs(combined_east_df, east_df)
      }
      else {
        combined_east_df <- east_df
      }
    } else {
      cat("west\n")
      west_df <- get_admin2(state = state, county = county)
      if (exists("combined_west_df")) {
        combined_west_df <- aggregate_dfs(combined_west_df, west_df)
      }
      else {
        combined_west_df <- west_df
      }
    }
  }
  
  if (!is.null(file_base)) {
    f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
    jpeg(filename = f,
         width = plot_file_width,
         height = plot_file_height)
  }
  
  plot(
    combined_east_df$dates,
    combined_east_df$cases_per_hundy,
    main = paste(loc_txt, cumulative_c19_cases_txt, hundy_txt),
    ylab = ylab_cases_hundy_txt,
    xlab = "Dates",
    type = "l",
    col = "gold",
    xlim = as.Date(c(plot_start_date, plot_end_date))
  )
  mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
  legend(
    "topleft",
    legend = c("Eastern", "Western"),
    col = c("gold", "green"),
    lty = 1
  )
  lines(combined_west_df$dates,
        combined_west_df$cases_per_hundy,
        col = "green")
  if (!is.null(file_base)) {
    dev.off()
  }
  
  filename = "east_west_daily.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(combined_east_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  east_txt <-
    paste("East of the Cascades, WA (pop=",
          pop_format(combined_east_df$pop[1]),
          ")",
          sep = "")
  west_txt <-
    paste("West of the Cascades, WA (pop=",
          pop_format(combined_west_df$pop[1]),
          ")",
          sep = "")
  
  ew_df <- data.frame(
    dates = combined_east_df$dates,
    east = combined_east_df$daily_cases_per_hundy_avrg14d,
    west = combined_west_df$daily_cases_per_hundy_avrg14d
  )
  p <- ggplot(data = ew_df, aes(dates)) +
    geom_line(aes(y = west, colour = west_txt)) +
    geom_line(aes(y = east, colour = east_txt)) +
    scale_color_manual(values = c("gold", "green")) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    scale_y_continuous(
      limits = c(0, maxy),
      sec.axis = sec_axis(trans =  ~ . * 14, name =
                            "14 Day Sum / 100,000 Population")
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  
  print(p)
  dev.off()
  file_to_bucket(filename)
  
}

if (live_mode) {
  a_cases <- get_admin2(state = "Washington", county = "Adams")
  wa_east_west(file_base = "waeastwest")
}

summarize_vax_wide_data <- function(df, latest_col) {
  df$latest <- df[, latest_col]
  df$vax_pct <- df$latest / df$Population
  df$week2ago <- df[, (latest_col - 14)]
  df$trend <-  df$latest - df$week2ago
  
  return(df)
}

summarize_wide_data <- function(df, latest_col) {
  df$latest <- df[, latest_col]
  # difference over 14days
  df$diff14 <- df$latest - df[, (latest_col - 14)]
  
  df$avrg14 <- df$diff14 / 14
  df$latest_per_hundy <- df$latest / df$Population * 100000
  df$avrg14_per_hundy <- df$avrg14 / df$Population * 100000
  
  df$week2ago <- df[, (latest_col - 14)]
  
  # change over 14days (two weeks ago)
  df$week2ago_diff14 <-
    df[, (latest_col - 14)] - df[, (latest_col - 28)]
  df$week2ago_avrg14 <- df$week2ago_diff14 / 14
  df$week2ago_per_hundy <- df$week2ago / df$Population * 100000
  df$week2ago_avrg14_per_hundy <-
    df$week2ago_avrg14 / df$Population * 100000
  df$trend <-  df$avrg14_per_hundy - df$week2ago_avrg14_per_hundy
  
  return(df)
}

# makes a consistent clean key for matching
mash_combined_key <- function(df) {
  df$combinedkeylc <- str_to_lower(df$Combined_Key)
  df$combinedkeylc <- str_replace_all(df$combinedkeylc, "[.]", "")
  df$combinedkeylc <- str_replace_all(df$combinedkeylc, "[ ]", "")
  return(df)
}

prep_wide_data <- function() {
  # if we only wonted WA
  #  us_counties_wide <- filter(usa_confirmed, Province_State == "Washington")
  us_counties_wide <- usa_confirmed
  
  # county data
  # before we add any columns get the last date column
  # then subtract one for the prior day since "today" might not be fully reported.
  latest <- dim(us_counties_wide)[2] - 1
  us_counties_wide <- merge(us_counties_wide,
                            uid_iso_fips_lookup[, c("Population", "Combined_Key")],
                            by = "Combined_Key",
                            all.x = TRUE)
  
  us_counties_wide <- summarize_wide_data(us_counties_wide, latest)
  
  us_counties_wide <- mash_combined_key(us_counties_wide)
  
  # put in global environment
  us_counties_wide <<- us_counties_wide
  
  write.csv(us_counties_wide, file = "us_counties_covid19_cases.csv")
  
  # states
  us_states_wide <- us_states_wide_raw
  latest <- dim(us_states_wide)[2] - 1
  # get the pops for just us states
  uid_iso_fips_lookup_states <-
    filter(uid_iso_fips_lookup, Admin2 == "" &
             Country_Region == "US")
  us_states_wide <- merge(us_states_wide,
                          uid_iso_fips_lookup_states[, c("Population", "Province_State")],
                          by = "Province_State")
  
  us_states_wide <<- summarize_wide_data(us_states_wide, latest)
}

if (live_mode) {
  prep_wide_data()
}

make_a_map_from_base <- function(df,
                                 key = NULL,
                                 var,
                                 base,
                                 title,
                                 midpoint = "mean",
                                 lowpoint = NULL,
                                 trans = NULL,
                                 border1_color = NULL,
                                 border1_df = NULL,
                                 border2_color = NULL,
                                 border2_df = NULL,
                                 caption = NULL,
                                 filebase = NULL) {
  # prepare to drop the axes and ticks but leave the guides and legends
  # We can't just throw down a theme_nothing()!
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
  
  # if we get a key then can make a df with only the key and values
  # to make available for on the the webpage
  if (!is.null(key)) {
    df4export <- unique(df[, c(key, var)])
    if (!is.null(filebase)) {
      filename <- paste(filebase, "csv", sep = ".")
      write.csv(df4export, filename)
      file_to_bucket(filename)
    }
  }
  
  meanv <- mean(df[, var], na.rm = TRUE)
  mean_txt <- paste("Mean =", round(meanv, digits = 1))
  med <- median(df[, var], na.rm = TRUE)
  
  iqr <- IQR(df[, var], na.rm = TRUE)
  if (is.null(lowpoint)) {
    data_range <- c(med - iqr * 1.5, iqr * 1.5 + med)
  }
  else {
    data_range <- c(lowpoint, iqr * 1.5 + med)
  }
  if (VERBOSE) {
    print(paste("iqr", iqr, "med", med, "range", data_range))
  }
  
  if (!is.null(filebase)) {
    filename <- paste(filebase, "jpg", sep = ".")
    jpeg(filename = filename,
         width = plot_file_width,
         height = plot_file_height)
  }
  
  mymap <- base +
    geom_polygon(data = df, aes(fill = get(var))) +
    theme_bw() +
    ditch_the_axes +
    labs(title = title,
         subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y"))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)
    )
  if (is.null(trans)) {
    if (midpoint == "mean") {
      mymap <- mymap +
        scale_fill_gradient2(
          midpoint = meanv,
          low = "blue",
          mid = "white",
          high = "red",
          name = paste(mean_txt, "\nCenter at mean"),
          limits = data_range,
          oob = scales::squish
        )
    }
    else {
      mymap <- mymap +
        scale_fill_gradient2(
          midpoint = midpoint,
          low = "blue",
          mid = "white",
          high = "red",
          name = paste(mean_txt, "\nCenter at 0 (no change)"),
          limits = data_range,
          oob = scales::squish
        )
    }
  }
  else {
    if (trans == "log10") {
      mymap <- mymap +
        scale_fill_gradient(
          breaks = c(2, 4, 10, 100, 1000, 10000),
          low = "white",
          high = "red",
          space = "Lab",
          na.value = "pink",
          name = mean_txt,
          trans = "log10"
        )
    }
  }
  if (!is.null(border1_df)) {
    if (is.null(border1_color)) {
      border1_color <- "black"
    }
    mymap <- mymap +
      geom_polygon(data = border1_df,
                   color = border1_color,
                   fill = NA)
  }
  if (!is.null(border2_df)) {
    if (is.null(border2_color)) {
      border2_color <- "black"
    }
    mymap <- mymap +
      geom_polygon(data = border2_df,
                   color = border2_color,
                   fill = NA)
  }
  if (!is.null(caption)) {
    mymap <- mymap +
      labs(caption = caption)
  }
  print(mymap)
  
  if (!is.null(filename)) {
    dev.off()
  }
  file_to_bucket(filename)
  
}


make_maps <- function() {
  usa <- map_data("usa")
  states <- map_data("state")
  
  # add Province_State to make merging easier
  states$Province_State = str_to_title(states$region)
  states_merged <-
    inner_join(states, us_states_wide, by = "Province_State")
  # use key = "Province_State"
  
  vax_states_merged <-
    inner_join(states, vax_us_wide, by = "Province_State")
  # use key = "Province_State"
  
  wa_df <- subset(states, region == "washington")
  wa_base <-
    ggplot(data = wa_df,
           mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(color = "black", fill = "gray")
  
  counties <- map_data("county")
  # make a combined key that matches our data
  counties$Combined_Key <- paste(
    str_to_title(counties$subregion),
    ", ",
    str_to_title(counties$region),
    ", US",
    sep = ""
  )
  counties <- mash_combined_key(counties)
  
  counties_merged <-
    inner_join(counties, us_counties_wide, by = "combinedkeylc")
  # use key = Combined_Key.x
  
  wa_counties_merged <-
    subset(counties_merged, region == "washington")
  # use key = Combined_Key.x
  
  make_a_map_from_base(
    df = wa_counties_merged,
    key = "Combined_Key.x",
    var = "avrg14_per_hundy",
    base = wa_base,
    lowpoint = 0,
    border1_color = "grey",
    border1_df = wa_counties_merged,
    border2_df = wa_df,
    title = paste("Washington",
                  main_daily_cases_hundy_14d_avrg_txt),
    filebase = "map_wa_14avrg"
  )
  make_a_map_from_base(
    df = wa_counties_merged,
    var = "trend",
    key = "Combined_Key.x",
    midpoint = 0,
    border1_color = "grey",
    border1_df = wa_counties_merged,
    border2_df = wa_df,
    base = wa_base,
    title = paste("Washington", main_14day_trend_txt),
    filebase = "map_wa_trend"
  )
  
  
  states_base <-
    ggplot(data = states,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "white") +
    coord_fixed(1.3)
  
  make_a_map_from_base(
    df = states_merged,
    var = "avrg14_per_hundy",
    key = "Province_State",
    lowpoint = 0,
    base = states_base,
    border1_color = "grey",
    border1_df = states,
    border2_df = usa,
    title = paste("USA", main_daily_cases_hundy_14d_avrg_txt, "States"),
    filebase = "map_usa_14avrg"
  )
  make_a_map_from_base(
    df = states_merged,
    var = "trend",
    key = "Province_State",
    midpoint = 0,
    base = states_base,
    border1_color = "grey",
    border1_df = states,
    border2_df = usa,
    title = paste("USA", main_14day_trend_txt, "States"),
    filebase = "map_usa_trend"
  )
  
  make_a_map_from_base(
    df = vax_states_merged,
    var = "vax_pct",
    key = "Province_State",
    lowpoint = 0,
    base = states_base,
    border1_color = "grey",
    border1_df = states,
    border2_df = usa,
    title = paste("USA", main_daily_cases_hundy_14d_avrg_txt, "States"),
    filebase = "vax1"
  )
  
  # us county maps
  counties_base <-
    ggplot(data = counties,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "black") +
    coord_fixed(1.3)
  
  make_a_map_from_base(
    df = counties_merged,
    var = "avrg14_per_hundy",
    key = "Combined_Key.x",
    lowpoint = 0,
    base = counties_base,
    title = paste("USA", main_daily_cases_hundy_14d_avrg_txt, "Counties"),
    #    trans = "log10",
    border1_color = "grey",
    border1_df = states,
    border2_df = usa,
    caption = "(black or grey represends missing data)",
    filebase = "map_usa_14avrg_c"
  )
  make_a_map_from_base(
    df = counties_merged,
    var = "trend",
    key = "Combined_Key.x",
    midpoint = 0,
    base = counties_base,
    title = paste("USA", main_14day_trend_txt, "Counties"),
    border1_color = "grey",
    border1_df = states,
    border2_df = usa,
    caption = "(black or grey represends missing data)",
    filebase = "map_usa_trend_c"
  )
  
  return()
}

if (live_mode) {
  make_maps()
}

doit <- function() {
  if (USA_ALL) {
    usa_cases <- build_all_states(
      combined = TRUE,
      keep_dfs = TRUE,
      write_dfs = TRUE,
      plot_state_cases_per_hundy = TRUE,
      plot_wa_and = TRUE,
      plot_daily_cases = TRUE
    )
    make_plot(
      usa_cases,
      loc_txt = "USA",
      cases_per_hundy = TRUE,
      daily_cases = TRUE,
      file_base = "USA"
    )
    file_to_bucket("USA_cases_per_hundy.jpg")
    file_to_bucket("USA_daily_cases.jpg")
  }
  
  ic_cases <- get_admin2("Washington", "Island")
  kc_cases <- get_admin2("Washington", "King")
  kc_txt <-
    paste("King County, WA (pop=", pop_format(kc_cases$pop[1]), ")", sep =
            "")
  wh_cases <- get_admin2("Washington", "Whitman")
  kit_cases <- get_admin2("Washington", "Kitsap")
  sno_cases <-
    get_admin2("Washington", "Snohomish")
  ska_cases <- get_admin2("Washington", "Skagit")
  b_co_cases <-
    get_admin2("Maryland", "Baltimore")
  b_ci_cases <-
    get_admin2("Maryland", "Baltimore City")
  # wic_cases <- get_admin2("Maryland", "Wicomico")
  mad_cases <- get_admin2("Virginia", "Madison")
  all_cases <-
    get_admin2("Pennsylvania", "Allegheny")
  yak_cases <- get_admin2("Washington", "Yakima")
  # oc_cases <- get_admin2("California", "Orange")
  che_cases <- get_admin2("Washington", "Chelan")
  doug_cases <-
    get_admin2("Washington", "Douglas")
  # lane_cases <- get_admin2("Oregon", "Lane")
  sji_cases <-
    get_admin2("Washington", "San Juan")
  jeff_cases <-
    get_admin2("Washington", "Jefferson")
  
  ca_bc_cases <-
    get_admin1(admin0 = "Canada", admin1 = "British Columbia")
  ca_bc_txt <-
    paste("British Columbia (pop=",
          pop_format(ca_bc_cases$pop[1]),
          ")",
          sep = "")
  ca_on_cases <- get_admin1(admin0 = "Canada", admin1 = "Ontario")
  
  yak_txt <-
    paste("Yakima County, WA (pop=",
          pop_format(yak_cases$pop[1]),
          ")",
          sep = "")
  wh_txt <-
    paste("Whitman County, WA (pop=",
          pop_format(wh_cases$pop[1]),
          ")",
          sep = "")
  ic_txt <-
    paste("Island County, WA (pop=",
          pop_format(ic_cases$pop[1]),
          ")",
          sep = "")
  kit_txt <-
    paste("Kitsap County, WA (pop=",
          pop_format(kit_cases$pop[1]),
          ")",
          sep = "")
  b_co_txt <-
    paste("Baltimore County, MD (pop=",
          pop_format(b_co_cases$pop[1]),
          ")",
          sep = "")
  b_ci_txt <-
    paste("Baltimore City, MD (pop=",
          pop_format(b_ci_cases$pop[1]),
          ")",
          sep = "")
  all_txt <-
    paste("Allegheny County, PA (pop=",
          pop_format(all_cases$pop[1]),
          ")",
          sep = "")
  mad_txt <-
    paste("Madison County, VA (pop=",
          pop_format(mad_cases$pop[1]),
          ")",
          sep = "")
  sji_txt <-
    paste("San Juan County, WA (pop=",
          pop_format(sji_cases$pop[1]),
          ")",
          sep = "")
  jeff_txt <-
    paste("Jefferson County, WA (pop=",
          pop_format(jeff_cases$pop[1]),
          ")",
          sep = "")
  sno_txt <-
    paste("Snohomish County, WA (pop=",
          pop_format(sno_cases$pop[1]),
          ")",
          sep = "")
  ska_txt <-
    paste("Skagit County, WA (pop=",
          pop_format(ska_cases$pop[1]),
          ")",
          sep = "")
  kit_txt <-
    paste("Kitsap County, WA (pop=",
          pop_format(kit_cases$pop[1]),
          ")",
          sep = "")
  if (USA_ALL) {
    usa_txt <-
      paste("USA (pop=", pop_format(usa_cases$pop[1]), ")", sep = "")
  }
  #  india_txt <-
  #    paste("India (pop=", pop_format(india$pop[1]), ")", sep = "")
  
  ##############################################################################
  filename = "my_perhundy_select.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  linetypes <- c(
    "wa" = "solid",
    "mt" =  "solid",
    "ic" = "solid",
    "kc" = "solid",
    "yak" = "solid",
    "all" = "solid",
    "mad" = "solid",
    "sji" = "solid",
    "jeff" = "solid",
    "kit" = "solid",
    "usa" = "dashed",
    "b_co" = "solid",
    "b_ci" = "solid"
  )
  
  #   linetypes <- c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid", "solid")
  temp_df <- data.frame(
    dates = washington_df$dates,
    wa = washington_df$cases_per_hundy,
    mt = montana_df$cases_per_hundy,
    ic = ic_cases$cases_per_hundy,
    kc = kc_cases$cases_per_hundy,
    yak = yak_cases$cases_per_hundy,
    all = all_cases$cases_per_hundy,
    mad = mad_cases$cases_per_hundy,
    sji = sji_cases$cases_per_hundy,
    jeff = jeff_cases$cases_per_hundy,
    kit = kit_cases$cases_per_hundy,
    b_co = b_co_cases$cases_per_hundy,
    b_ci = b_ci_cases$cases_per_hundy,
    usa = usa_cases$cases_per_hundy
  )
  p <-
    ggplot(data = temp_df, aes(dates, linetypes = "linetypes")) +
    geom_line(aes(y = ic, colour = ic_txt), linetype = "solid") +
    geom_line(aes(y = kc, colour = kc_txt), linetype = "solid") +
    geom_line(aes(y = wa, colour = washington_s_txt), linetype = "solid") +
    geom_line(aes(y = mt, colour = montana_s_txt), linetype = "solid") +
    geom_line(aes(y = b_co, colour = b_co_txt), linetype = "solid") +
    geom_line(aes(y = b_ci, colour = b_ci_txt), linetype = "solid") +
    geom_line(aes(y = kit, colour = kit_txt), linetype = "solid") +
    geom_line(aes(y = jeff, colour = jeff_txt), linetype = "solid") +
    geom_line(aes(y = sji, colour = sji_txt), linetype = "solid") +
    geom_line(aes(y = yak, colour = yak_txt), linetype = "solid") +
    geom_line(aes(y = all, colour = all_txt), linetype = "solid") +
    geom_line(aes(y = mad, colour = mad_txt), linetype = "solid") +
    geom_line(aes(y = usa, colour = usa_txt), linetype = "dashed") +
    scale_color_manual(
      values = c(
        "black",
        "darkred",
        "red",
        "blue",
        "pink",
        "green",
        "grey",
        "lightblue",
        "orange",
        "purple",
        "red",
        "darkgreen",
        "yellow"
      )
    ) +
    #    scale_linetype_manual( values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid", "solid")) +
    #      scale_linetype_manual( values = linetypes ) +
    ylim(0, max(usa_cases$cases_per_hundy, na.rm = TRUE)) +
    #      labs(title = main_cases_hundy_txt,
    #           subtitle = paste("created",format(Sys.Date(), "%m/%d/%Y")),
    #           x = "Dates",
    #           y = ylab_cases_hundy_txt) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      #        legend.title = element_blank(),
      legend.position = c(0.35, 0.80),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  # apple cup
  # daily rates
  filename = "apple_cup_daily.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(wh_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  apple_df <- data.frame(
    dates = kc_cases$dates,
    kc = kc_cases$daily_cases_per_hundy_avrg14d,
    wh = wh_cases$daily_cases_per_hundy_avrg14d,
    wa = washington_df$daily_cases_per_hundy_avrg14d
  )
  p <- ggplot(data = apple_df, aes(dates)) +
    geom_line(aes(y = kc, colour = kc_txt)) +
    geom_line(aes(y = wh, colour = wh_txt)) +
    scale_color_manual(values = c("purple", "darkred")) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    scale_y_continuous(
      limits = c(0, maxy),
      sec.axis = sec_axis(trans =  ~ . * 14, name =
                            "14 Day Sum / 100,000 Population")
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  
  print(p)
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  filename = "uw_v_wsu.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  apple_cup <- data.frame(wh_cases$dates,
                          wh_cases$cases_per_hundy,
                          kc_cases$cases_per_hundy)
  p <- ggplot(data = apple_cup, aes(wh_cases.dates)) +
    geom_line(aes(y = wh_cases.cases_per_hundy,
                  colour = wh_txt)) +
    geom_line(aes(y = kc_cases.cases_per_hundy,
                  colour = kc_txt)) +
    scale_color_manual(values = c("purple", "darkred")) +
    ylim(0, max(apple_cup$wh_cases.cases_per_hundy)) +
    labs(
      title = paste(cumulative_c19_cases_txt, hundy_txt),
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  wa_east_west(
    plot_casesned = TRUE,
    plot_casesned_cases = FALSE,
    file_base = "wa_east_west"
  )
  file_to_bucket(file = "wa_east_west_cases_per_hundy.jpg")
  
  ##############################################################################
  
  # 14 day moving plots
  make_plot(
    loc_txt = ic_txt,
    df = ic_cases,
    daily_cases = TRUE,
    file_base = "island_wa"
  )
  file_to_bucket(file = "island_wa_daily_cases.jpg")
  make_plot(
    loc_txt = kc_txt,
    df = kc_cases,
    daily_cases = TRUE,
    file_base = "king_wa"
  )
  file_to_bucket(file = "king_wa_daily_cases.jpg")
  make_plot(
    loc_txt = b_co_txt,
    df = b_co_cases,
    daily_cases = TRUE,
    file_base = "balto_co_md"
  )
  file_to_bucket(file = "balto_co_md_daily_cases.jpg")
  
  ##############################################################################
  filename = "is_king_balto.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy = max(b_co_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  
  temp_df <- data.frame(
    dates = wh_cases$dates,
    kc = kc_cases$daily_cases_per_hundy_avrg14d,
    ic = ic_cases$daily_cases_per_hundy_avrg14d,
    wa = washington_df$daily_cases_per_hundy_avrg14d,
    b_co = b_co_cases$daily_cases_per_hundy_avrg14d
  )
  p <- ggplot(data = temp_df, aes(dates)) +
    geom_line(aes(y = kc, colour = kc_txt)) +
    geom_line(aes(y = ic, colour = ic_txt)) +
    geom_line(aes(y = b_co, colour = b_co_txt)) +
    scale_color_manual(values = c("red", "blue", "green")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  filename <- "is_king_wa.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy <-
    max(washington_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  p <- ggplot(data = temp_df, aes(dates)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = ic, colour = ic_txt)) +
    geom_line(aes(y = kc, colour = kc_txt)) +
    scale_color_manual(values = c("blue", "green", "darkgreen")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  filename = "is_king_wa_sum.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  temp_df_sum <- data.frame(
    dates = wh_cases$dates,
    kc = kc_cases$daily_cases_per_hundy_sum14d,
    ic = ic_cases$daily_cases_per_hundy_sum14d,
    wa = washington_df$daily_cases_per_hundy_sum14d,
    ska = ska_cases$daily_cases_per_hundy_sum14d,
    kit = kit_cases$daily_cases_per_hundy_sum14d,
    sno = sno_cases$daily_cases_per_hundy_sum14d,
    b_co = b_co_cases$daily_cases_per_hundy_sum14d
  )
  maxy = max(washington_df$daily_cases_per_hundy_sum14d, na.rm = TRUE)
  today <- Sys.Date()
  start_graph <- today - months(2)
  
  p <- ggplot(data = temp_df_sum, aes(dates)) +
    geom_line(aes(y = sno, colour = sno_txt)) +
    geom_line(aes(y = ic, colour = ic_txt)) +
    geom_line(aes(y = ska, colour = ska_txt)) +
    geom_line(aes(y = kc, colour = kc_txt)) +
    geom_line(aes(y = kit, colour = kit_txt)) +
    geom_vline(xintercept = as.Date(c("2021/04/24", "2021/04/09")), linetype =
                 "dashed") +
    geom_hline(yintercept = 200, linetype = "dashed") +
    # geom_hline(yintercept = c(200, 150)) +
    scale_color_manual(values = c("blue", "green", "yellow", "purple", "pink")) +
    ylim(0, maxy) +
    scale_x_date(
      date_breaks = "3 week",
      labels = date_format("%b-%d-%Y"),
      limits = c(start_graph, today),
    ) +
    labs(
      title = main_daily_cases_hundy_14d_sum_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  # MISC graphs
  # multiple counties 14 day
  filename = "misc.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(montana_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  temp_df <- data.frame(
    dates = washington_df$dates,
    or = oregon_df$daily_cases_per_hundy_avrg14d,
    mi = michigan_df$daily_cases_per_hundy_avrg14d,
    wa = washington_df$daily_cases_per_hundy_avrg14d,
    ca = california_df$daily_cases_per_hundy_avrg14d,
    md = maryland_df$daily_cases_per_hundy_avrg14d,
    id = idaho_df$daily_cases_per_hundy_avrg14d,
    mt = montana_df$daily_cases_per_hundy_avrg14d,
    ca_bc = ca_bc_cases$daily_cases_per_hundy_avrg14d,
    usa = usa_cases$daily_cases_per_hundy_avrg14d,
    nebraska = nebraska_df$daily_cases_per_hundy_avrg14d,
    south_dakota = south_dakota_df$daily_cases_per_hundy_avrg14d,
    wyoming = wyoming_df$daily_cases_per_hundy_avrg14d,
    colorado = colorado_df$daily_cases_per_hundy_avrg14d,
    kansas = kansas_df$daily_cases_per_hundy_avrg14d,
    missouri = missouri_df$daily_cases_per_hundy_avrg14d,
    iowa = iowa_df$daily_cases_per_hundy_avrg14d
  )
  p <- ggplot(data = temp_df, aes(dates)) +
    geom_line(aes(y = or, colour = oregon_s_txt)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = mt, colour = montana_s_txt)) +
    geom_line(aes(y = mi, colour = michigan_s_txt)) +
    #      geom_line(aes(y = india, colour=india_txt)) +
    geom_line(aes(y = usa, colour = usa_txt), linetype = "dashed") +
    scale_color_manual(values = c("black", "orange", "lightgreen", "red", "darkgreen")) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "solid")) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    scale_y_continuous(
      limits = c(0, maxy),
      sec.axis = sec_axis(trans =  ~ . * 14, name =
                            "14 Day Sum / 100,000 Population")
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  
  ##############################################################################
  # MISC2222222222222222222222
  # multiple counties 14 day
  filename = "misc2.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(california_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  p <- ggplot(data = temp_df, aes(dates)) +
    geom_line(aes(y = or, colour = oregon_s_txt)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = ca_bc, colour = ca_bc_txt)) +
    geom_line(aes(y = id, colour = idaho_s_txt)) +
    geom_line(aes(y = ca, colour = california_s_txt)) +
    scale_color_manual(values = c("lightblue", "pink", "brown", "lightgreen", "darkgreen")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  # MISC2222222222222222222222bbbb
  # multiple counties 14 day
  filename = "misc2.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(south_dakota_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
  p <- ggplot(data = temp_df, aes(dates)) +
    geom_line(aes(y = nebraska, colour = nebraska_s_txt)) +
    geom_line(aes(y = south_dakota, colour = south_dakota_s_txt)) +
    geom_line(aes(y = wyoming, colour = wyoming_s_txt)) +
    geom_line(aes(y = colorado, colour = colorado_s_txt)) +
    #      geom_line(aes(y = kansas, colour = kansas_s_txt)) +
    geom_line(aes(y = missouri, colour = missouri_s_txt)) +
    geom_line(aes(y = iowa, colour = iowa_s_txt)) +
    #     scale_color_manual(values = c("lightblue", "pink", "brown", "lightgreen", "darkgreen", "black", "red")) +
    ylim(0, maxy) +
    labs(
      title = paste("Really Nebraska?", main_daily_cases_hundy_14d_avrg_txt),
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)
  
  dev.off()
  file_to_bucket(filename)
  
}  #doit