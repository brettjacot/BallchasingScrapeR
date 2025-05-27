get_replays_by_criteria <- function(start_date, end_date = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), match_type = NULL, whouploader = NULL, player_id = NULL, api_key, callspersecond = 2) {

  url <- "https://ballchasing.com/api/replays"
counter = 1
  # Initialize an empty list to store replay IDs
  all_replay_ids <- list()

    message("Processing request....")
    query = list( #making the query based on user-input criteria
      `replay-date-after` = start_date,
      `replay-date-before` = end_date
    )

    if (!is.null(match_type)) query$playlist <- match_type
    if (!is.null(whouploader)) query$uploader <- whouploader
    if (!is.null(player_id)) query$`player-id` <- player_id
    query$count <- as.integer(200)
    response <- GET( #querying ballchasing for 200 replay ids
      url,
      add_headers(Authorization = api_key),
      query = query
    )
    if (status_code(response) == 200) {
      replays <- content(response, as = "parsed", type = "application/json")
      if (length(replays$list) > 0) {
        replay_ids <- sapply(replays$list, function(x) x$id) #adding these IDs to a final list
        all_replay_ids <- (c(all_replay_ids, replay_ids))
        while(((length(all_replay_ids)) %% 200) == 0) { #if more than 200 replays, repeat process until you get to the end
          message("More than ", (200 * counter)," replays found, repeating process.")
          counter = counter + 1
          last_replay_id <- all_replay_ids[length(all_replay_ids)]
          url2 <- paste0("https://ballchasing.com/api/replays/", last_replay_id) #finding the date from last replay and using that for the next call
          response <- GET(url2, add_headers(Authorization = api_key))
          if (status_code(response) == 200) {

            content_text <- httr::content(response, as = "text")
            last_replay_data <- fromJSON(content_text, flatten = TRUE)

          }  else {
            stop("Failed to retrieve data: ", status_code(response))
          }
          Sys.sleep(1/callspersecond) #rate limiting
          new_end_date <- last_replay_data[["date"]]
          has_timezone <- last_replay_data[["date_has_timezone"]]

          if (has_timezone) { #Dates can be in two different formats, this is how I account for that
            # Format: "2025-01-07T22:14:23+01:00"
            time_zone <- str_sub(new_end_date, -6)
            new_end_date2 <- str_sub(new_end_date, end = -7)
            end_posix <- as_datetime(new_end_date2, format = "%Y-%m-%dT%H:%M:%S")
          } else {
            # Format: "2024-11-17T19:27:33Z"
            time_zone <- "Z"
            new_end_date2 <- str_sub(new_end_date, end = -2)  # Remove "Z"
            end_posix <- as_datetime(new_end_date2, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
          }

          # Subtract 1 second so the replay isn't re-run
          end_posix <- end_posix - 1

          # Format final date
          FinalEndDate <- paste0(as.character(end_posix), time_zone)
          FinalEndDate <- str_replace(FinalEndDate, " ", "T")

          message("Start of new call replay date: ", FinalEndDate)
          query = list() #making new call

            query$`replay-date-after` = start_date
            query$`replay-date-before` = FinalEndDate

          if (!is.null(match_type)) query$playlist <- match_type
          if (!is.null(whouploader)) query$uploader <- whouploader
          if (!is.null(player_id)) query$`player-id` <- player_id
          query$count <- as.integer(200)
          response2 <- GET(
            url,
            add_headers(Authorization = api_key),
            query = query
          )
          if (status_code(response) == 200) {
            replays2 <- content(response2, as = "parsed", type = "application/json")
            if (length(replays2$list) > 0) {
              replay_ids_new <- sapply(replays2$list, function(x) x$id)
              all_replay_ids <- (c(all_replay_ids, replay_ids_new))
            }
          }
        }
      }
    }
    else if (status_code(response) == 429) {
      stop("Rate limit hit. You've either hit Ballchasing's hourly limit or put in the wrong patron tier into the callspersecond variable.")}
      else {
      stop("Failed to retrieve replays, status code: ", status_code(response))

    }



  message("Returned ", length(all_replay_ids), " replay IDs.")
  return(unique(all_replay_ids))  # Return unique replay IDs directly
}

get_replay_data <- function(api_key = apikey, replay_id, callspersecond = 2, alldata = FALSE) {
  url <- paste0("https://ballchasing.com/api/replays/", replay_id) #add replay id to be processed to the link
  response <- GET(url, add_headers(Authorization = api_key))
  if (status_code(response) == 200) {
    Sys.sleep(1/callspersecond)
    content_text <- httr::content(response, as = "text")
    replay_data <- fromJSON(content_text, flatten = TRUE)

  }
  else if (status_code(response) == 429) {
    stop("Rate limit hit. You've either hit Ballchasing's hourly limit or put in the wrong patron tier into the callspersecond variable.")}
    else {
    stop("Failed to retrieve data: ", status_code(response))
  }
  if (alldata == FALSE) { #just main stats for each player
  if (!is.null(replay_data[["blue"]])){
    blue_df <- replay_data[["blue"]][["players"]]
    blue_core_stats_cols <- grep("stats.core.", names(blue_df), value = TRUE)
    blue_name_stats_cols <- subset(blue_df, select = c(name, id.id, stats.demo.inflicted, stats.demo.taken))
    blue_df_sub <- cbind(blue_name_stats_cols, blue_df[, blue_core_stats_cols])
    blue_df_sub$team <- "blue"
    blue_df_sub$win <- (sum(blue_df_sub[,"stats.core.mvp"]))
  }

  if (!is.null(replay_data[["orange"]])) {
    orange_df <- replay_data[["orange"]][["players"]]
    orange_core_stats_cols <- grep("stats.core.", names(orange_df), value = TRUE)
    orange_name_stats_cols <- subset(orange_df, select = c(name, id.id, stats.demo.inflicted, stats.demo.taken))
    orange_df_sub <- cbind(orange_name_stats_cols, orange_df[, orange_core_stats_cols])
    orange_df_sub$team <- "orange"
    orange_df_sub$win <- (sum(orange_df_sub[,"stats.core.mvp"]))
  }

  AllTogether <- rbind(blue_df_sub, orange_df_sub)
  AllTogether$match_guid <- replay_data[["match_guid"]]
  AllTogether$match_date <- as_datetime(replay_data[["date"]])
  AllTogether$BallchasingLink <- paste("https://ballchasing.com/replay/", replay_id, sep = "")

  return(AllTogether)
  }

  if (alldata == TRUE) { #if every stat is requested
    if (!is.null(replay_data[["blue"]])){
      blue_df2 <- as.data.frame(replay_data[["blue"]][["players"]])
      if (("stats.positioning.goals_against_while_last_defender" %in% colnames(blue_df2)) == FALSE) {
        blue_df2$stats.positioning.goals_against_while_last_defender = 0 #sometimes this is NA if shutout
      }
      if (("mvp" %in% colnames(blue_df2)) == FALSE) {
        blue_df2$mvp = FALSE #NA if loss sometimes
      }
    }
    if (!is.null(replay_data[["orange"]])) {
      orange_df2 <- as.data.frame(replay_data[["orange"]][["players"]])
      if (("stats.positioning.goals_against_while_last_defender" %in% colnames(orange_df2)) == FALSE) {
        orange_df2$stats.positioning.goals_against_while_last_defender = 0
      }
      if (("mvp" %in% colnames(orange_df2)) == FALSE) {
        orange_df2$mvp = FALSE
      }
    }

    AllTogether2 <- rbind(blue_df2, orange_df2)
    AllTogether2$match_guid <- replay_data[["match_guid"]]
    AllTogether2$match_date <- as_datetime(replay_data[["date"]])
    AllTogether2$BallchasingLink <- paste("https://ballchasing.com/replay/", replay_id, sep = "")

    return(AllTogether2)
  }

}

RankedStats <- function(results,  TopPlayerOnly = TRUE, RemoveNextUp = FALSE, rank = "Champion") {

  # Step 1: Count games for each player and summarize results
  Names <- Find_Player_Names(results)
  results <- results %>%
    count(id.id, name = "NumGames") %>%
    left_join(results, by = "id.id") %>%
    group_by(id.id) %>%
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      NumGames = first(NumGames)
    ) %>%
    mutate(NumGames = sqrt(NumGames)) %>%
    arrange(desc(NumGames))

  # Step 2: Normalize stats columns
  results <- results %>%
    mutate(
      NumGames = NumGames^2

    )
  results2 <- merge(Names, results, by = "id.id")
  results2 <- arrange(results2, desc(NumGames))
  results2 <- results2 %>%
    rename(
      PlayerID = id.id,
      PlayerName = most_common_name,
      `#Games` = NumGames,
      `Demos/G` = stats.demo.inflicted,
      `DemosTaken/G` = stats.demo.taken,
      `Shots/G` = stats.core.shots,
      `ShotsA/G` = stats.core.shots_against,
      `Goals/G` = stats.core.goals,
      `GoalsA/G` = stats.core.goals_against,
      `Saves/G` = stats.core.saves,
      `Assists/G` = stats.core.assists,
      `Score/G` = stats.core.score,
      `Shooting%` = stats.core.shooting_percentage,
      `Win%` = win
    )

  message("RankedStats Process Complete!")
  if(TopPlayerOnly == TRUE){
    ifelse(RemoveNextUp == TRUE, results_notop <- results2[3:nrow(results2),], results_notop <- results2[2:nrow(results2),])


    #Divide all numeric columns by NumGames, then do sum
    someDF <- results_notop %>%
      mutate(across(where(is.numeric), ~ . / `#Games`, .names = "{.col}")) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
    PlayerStats <- head(results2, n = 1)

    someDF[["Win%"]] = NA
    someDF[["#Games"]] = NA
    TotalAverages_2s <- data.frame(matrix(nrow = 8, ncol = 14))
    rownames(TotalAverages_2s) = c("Bronze", "Silver", "Gold", "Platinum", "Diamond", "Champion", "GC", "Pros") #these stats are from ballchasing
    colnames(TotalAverages_2s) = colnames(results2)
    TotalAverages_2s$`Demos/G` <- c(0.41, 0.42, 0.46, 0.50, 0.57, 0.64, 0.73, 0.81)
    TotalAverages_2s$`DemosTaken/G` <- c(0.41, 0.44, 0.49, 0.51, 0.56, 0.63, 0.71, 0.78)
    TotalAverages_2s$`Shots/G` <- c(2.11, 2.48, 2.77, 3.01, 3.22, 3.52, 3.85, 4.31)
    TotalAverages_2s$`ShotsA/G` <- TotalAverages_2s$`Shots/G` * 2
    TotalAverages_2s$`Goals/G` <- c(1.38, 1.48, 1.42, 1.26, 1.13, 1.06, 0.97, 1.33)
    TotalAverages_2s$`GoalsA/G` <- TotalAverages_2s$`Goals/G`*2
    TotalAverages_2s$`Saves/G` <- c(0.58, 0.77, 0.94, 1.11, 1.29, 1.52, 1.77, 2.01)
    TotalAverages_2s$`Assists/G`<- c(0.44, 0.5, 0.55, 0.59, 0.64, 0.69, 0.73, 0.78)
    TotalAverages_2s$`Score/G` <- c(314.87, 368.08, 403.65, 428.39, 450.14, 477.54, 507.23, 549.84)
    TotalAverages_2s$`Shooting%` <- (TotalAverages_2s$`Goals/G` / TotalAverages_2s$`Shots/G`)*100

    RankedAverageRow <- as.data.frame(TotalAverages_2s[rank, , drop = FALSE])
    # Combine the data frames
    Final <- bind_rows(
      PlayerStats,
      someDF,
      RankedAverageRow
    )
    Final <- as.data.frame(Final)


    rownames(Final) <- c("Player Stats", "Lobby Averages", paste0(rank, " Rank Average (2s)"))

    return(Final)

  }
  if (TopPlayerOnly == FALSE){
    return(results2)
  }
}

Find_Player_Names <- function(results) {
  replace(results$name, is.na(results$name), "(blank name)")
  results <- results %>%
    group_by(id.id) %>%
    reframe(
      most_common_name = paste0(names(which.max(table(name)))), # Find the most frequent name for each ID
    ) %>%
    ungroup()
  return(results)
}

RunReplays <- function(replay_id_data, api_key, callspersecond = 2, AllData = FALSE){
  results <- get_replay_data(api_key, replay_id_data[[1]], alldata = AllData)
  TotalReplayCount = length(replay_id_data)
  ReplayCount = 1
  #Fix the dates in the >200 replays part
  while (ReplayCount < TotalReplayCount) {
    message("Processing replay ", ReplayCount, " out of ", TotalReplayCount)
    ReplayCount <- ReplayCount + 1
    replay_results <- get_replay_data(api_key, replay_id_data[[ReplayCount]], callspersecond, alldata = AllData)
    results <- rbind(results, replay_results)

  }
  return(results)
}
# Function to get replay stats by ID

stats_from_group <- function(group_id, api_key, name = FALSE) {
  url <- paste0("https://ballchasing.com/api/groups/", group_id)

  # API request with query parameters
  response <- GET(
    url,
    add_headers(Authorization = api_key),
    query = list(count = 200)  # Request up to 200 replays
  )

  # Check for request success
  if (http_type(response) != "application/json") {
    stop(paste0("API request failed. Check the group ID and API key. Error code: ", status_code(response)))
  }

  # Parse JSON response
  content_data <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(content_data, flatten = TRUE)
  message("Group data collected!")
  datadf <- parsed_data[["players"]]
  core_stats_cols <- grep("game_average.core.", names(datadf), value = TRUE)
  name_stats <- subset(datadf, select = c(id, cumulative.games, cumulative.wins, cumulative.win_percentage, game_average.demo.taken, game_average.demo.inflicted))
  core_stats <- datadf[c(core_stats_cols)]
  if (name == TRUE) {
    player_names <- subset(datadf, select = name)
    core_stats <- cbind(player_names, core_stats)
    }
  overall_stats <- cbind(name_stats, core_stats)
  overall_stats$game_average.core.mvp = NULL
  return(overall_stats)
}
AccountCombiner <- function(data, main, alt1, alt2 = NULL, alt3 = NULL) {
  # Collect all non-null alt accounts
  alts <- Filter(Negate(is.null), list(alt1, alt2, alt3))

  # If no alts provided, return original data
  if (length(alts) == 0) return(data)

  # Get the rows to combine (main + all alts)
  fixDF <- data[data$id %in% c(main, alts), ]
  # Remove combined accounts from the dataset
  data2 <- data[!data$id %in% c(main, alts), ]
  # Summarise with weighted and summed stats
  combined <- fixDF %>%
    summarise(
      id = main,
      across(starts_with(c("game_average", "cumulative.win_percentage")),
             ~ weighted.mean(.x, fixDF$cumulative.games, na.rm = TRUE)),
      across(c("cumulative.games", "cumulative.wins"), sum, na.rm = TRUE)
    )
  # Recalculate shooting percentage
  combined$game_average.core.shooting_percentage <-
    (combined$game_average.core.goals / combined$game_average.core.shots) * 100

  # Reattach the new combined account to the rest of the data
  final <- rbind(data2, combined)
  return(final)
}
