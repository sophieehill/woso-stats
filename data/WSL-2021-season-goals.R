## Set up ----

library(tidyverse)
library(rvest)

## Custom functions ----

# create pause function to avoid timeout errors
CatchupPause <- function(Secs) {
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

# extract team id
get_team_id <- function(html, team) {
  html %>%
    html_elements(".sp-c-fixture--live-session-header") %>%
    html_children() %>%
    html_elements(paste0(".sp-c-fixture__team.sp-c-fixture__team--", team)) %>%
    html_attr("data-team-id")
}

# extract team name
get_team_name <- function(html, team) {
  html %>%
    html_elements(".sp-c-fixture--live-session-header") %>%
    html_children() %>%
    html_elements(paste0(".sp-c-fixture__team.sp-c-fixture__team--", team)) %>%
    html_elements("abbr") %>%
    html_attr("title")
}

# extract number of goals
get_goals <- function(html, team) {
  html %>%
    html_elements(".sp-c-fixture--live-session-header") %>%
    html_elements(
      paste0(
        ".sp-c-fixture__number.sp-c-fixture__number--",
        team,
        ".sp-c-fixture__number--ft"
      )
    ) %>%
    html_text() %>%
    as.numeric()
}

# extract goal events (name and minute of goal scorer/assister)
get_events <- function(html, team, type) {
  # team = "home" or "away"
  # type = "scorers" or "assists"
  
  events <-
    html %>%
    html_elements(paste0(
      ".sp-c-fixture__player-action",
      paste0(".sp-c-fixture__", type),
      paste0(".sp-c-fixture__", type, "-", team)
    )) %>%
    html_text() %>%
    str_split(., "\\),") %>%
    unlist()
  
  
  if (is.null(events)){
    dat <- NULL
  } else if (events[1] == "") {
    dat <- NULL
  } else {
    
    # Note on this regex:
    # if a player is sent off, it is recorded in the goals section (why, BBC??!)
    # so let's remove any string that starts with "Dismissed" and ends with "minutes"
    # we use the lazy quantifier *? to select the shortest string with this end point
    events <- 
      events %>%
      str_remove_all(., "(D|d)ismissed.*?minutes")
    
    # Now delete any entries that were there only for a dismissal
    events_to_keep <- 
      events %>%
      str_detect(., "\\'") %>%
      which()
    
    if (length(events_to_keep) == 0) {
      dat <- NULL
    } else {
    
    events <- events[events_to_keep]

    events_n <-
      events %>%
      str_count(., "minutes")
    
    events_name <-
      events %>%
      word(., 1, 1) %>%
      unlist() %>%
      str_extract_all(., "\\S+") %>%
      unlist()
    
    events_name_reps <-
      rep(events_name, events_n)
    
    # This bit is slightly tricky:
    # goals in extra time are recorded as 90'+2 minutes (for example)
    # So we need to convert these into numeric format (e.g., 92)
    
    events_minutes <-
      events %>%
      # Reminder of what this regex means:
      # [0-9]+ = 1+ numbers
      # (?:'\\+)? = the pattern '+ 0 or 1 time
      # [0-9]* = 0+ numbers
      str_extract_all(., "[0-9]+(?:'\\+)?[0-9]*") %>%
      unlist() %>%
      str_split(., "'\\+") %>%
      lapply(., as.numeric) %>%
      lapply(., sum) %>%
      unlist()
    
    dat <-
      tibble(name = events_name_reps, minute = events_minutes)
    
    if (type == "scorers") {
      names(dat)[1] = "scorer"
    }
    
    if (type == "assists") {
      names(dat)[1] = "assist"
    }
    
    }
    
  }
  
  return(dat)
}

# combining events

combine_events <- function(n_goals, goal_events, assist_events) {
  if (n_goals == 0) {
    x <- NULL
  } else if (is.null(assist_events)) {
    x <- goal_events
  } else {
    x <-
      left_join(goal_events,
                assist_events,
                by = "minute")
  }
  
  return(x)
  
}

## Scrape WSL 2021-22 results from BBC website ----

# Use cached version of URL since BBC removes results that are 1y+ old
bbc_base_url <-
  "http://webcache.googleusercontent.com/search?q=cache%3Ahttps%3A%2F%2Fwww.bbc.co.uk%2Fsport%2Ffootball%2Fwomens-super-league%2Fscores-fixtures%2F"

# Add year and month to the base url
bbc_links_2122 <-
  paste0(bbc_base_url,
         c(rep(2021, 4), rep(2022, 5)),
         "-",
         c("09", "10", "11", "12", "01", "02", "03", "04", "05"))

## (i loop): For each month of the season, scrape all match URLs ----

goal_events <- list()
match_info <- list()

for (i in 7:length(bbc_links_2122)) {
  # Note: looks like BBC does not remove the actual match links
  # so using original URLs rather than cached versions here
  bbc_match_links <-
    bbc_links_2122[i] %>%
    read_html() %>%
    html_elements("a") %>%
    html_attr("href") %>%
    tibble() %>%
    filter(str_detect(., "^/sport/football/[0-9]{8}$")) %>%
    pull() %>%
    paste0("https://www.bbc.co.uk", .)
  
  CatchupPause(10)
  
  ## (j loop): For each match link, extract goal/assist info ----
  
  # Create empty lists to store output of j tibbles
  goal_events_j <- list()
  match_info_j <- list()
  
  for (j in 1:length(bbc_match_links)) {
    
    print(paste0("month ", i, ", scraping match ", j, "/", length(bbc_match_links)))
    
    match_id <-
      bbc_match_links[j] %>%
      substr(., nchar(.) - 7, nchar(.))
    
    # Save match HTML once (to avoid repeated requests)
    saved_html <-
      bbc_match_links[j] %>%
      read_html()
    
    CatchupPause(5)
    
    match_date <-
      saved_html %>%
      html_elements(".sp-c-fixture__date") %>%
      html_attr("datetime")
    
    home_team_id <- get_team_id(saved_html, "home")
    away_team_id <- get_team_id(saved_html, "away")
    
    home_team_name <- get_team_name(saved_html, "home")
    away_team_name <- get_team_name(saved_html, "away")
    
    home_team_goals <- get_goals(saved_html, "home")
    away_team_goals <- get_goals(saved_html, "away")
    
    home_scorers <- get_events(saved_html, "home", "scorers")
    home_assisters <- get_events(saved_html, "home", "assists")
    
    away_scorers <- get_events(saved_html, "away", "scorers")
    away_assisters <- get_events(saved_html, "away", "assists")
    
    home_goal_events <-
      combine_events(home_team_goals,
                     home_scorers,
                     home_assisters) 
    
    if (home_team_goals != 0) {
      home_goal_events <- 
        home_goal_events %>%
        mutate(team_ha = "home",
               team_name = home_team_name,
               team_id = home_team_id)
    }
    
    
    away_goal_events <-
      combine_events(away_team_goals,
                     away_scorers,
                     away_assisters) 
    
    if (away_team_goals != 0) {
      
      away_goal_events <- 
        away_goal_events %>%
      mutate(team_ha = "away",
             team_name = away_team_name,
             team_id = away_team_id)
      
    }
    
    goal_events_j[[j]] <-
      bind_rows(home_goal_events,
                away_goal_events) %>%
      mutate(match_date = match_date,
             match_id = match_id)
    
    match_info_j[[j]] <-
      tibble(
        match_id,
        match_date,
        home_team_name,
        home_team_id,
        away_team_name,
        away_team_id,
        home_team_goals,
        away_team_goals
      )
    
  }
  
  goal_events[[i]] <- do.call("bind_rows", goal_events_j)
  match_info[[i]] <- do.call("bind_rows", match_info_j)
  
  
}

goal_events_final <- do.call("bind_rows", goal_events)
match_info_final <- do.call("bind_rows", match_info)

goal_events_final %>%
  group_by(scorer, assist) %>%
  tally() %>%
  arrange(-n)

write.csv(goal_events_final, "wsl_2021_goals_assists.csv")
write.csv(match_info_final, "wsl_2021_match_info.csv")

