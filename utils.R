library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)

#' Swap times from one to another time zone
#'
#' @param time in format: 2020-08-27 11:00:00
#' @param tz_time the timezone the time is in
#' @param tz_goal the timezone you want
#'
#' @return updated timezone
#' @export
#'
#' @examples
#' switch_timezone(time = "2020-08-27 11:00", tz_goal = "Europe/Helsinki")
#' switch_timezone(time = "2020-08-27 11:00:00", tz_goal = "Europe")
switch_timezone <-
  function(time, tz_goal) {

    my_timezone <-
      grep(tz_goal, OlsonNames(), value = TRUE)
    
    if (length(my_timezone) == 0) {
      warning("Your string did not match any name in OlsonNames(). Try another one.")
    }
    if (length(my_timezone) > 1) {
      warning(
        "Your string matched more than one name in OlsonNames(): ",
        paste(my_timezone, collapse = ', '),
        ". Be more specific."
      )
    }
    
    my_time <- with_tz(time, tzone = my_timezone)
    
    return(my_time)
    #pb.txt <- "2020-08-27 11:00"
    #pb.date <- as.POSIXct(pb.txt, tz="US/Eastern")
    #format(pb.date, tz=my_timezone, usetz=TRUE)
  }


import_schedule <- function(link, sheet, tz_schedule = "US/Eastern") {
  dat <- readxl::read_xlsx(link, sheet = sheet, col_names = TRUE)
  
  ## special treatment for Birds of a Feather Sessions
  ind <- grep("Birds of a Feather Sessions", dat$talk_speaker)
  if(length(ind) > 0) for (idx in ind) dat[idx, "talk_speaker"] <- gsub("\n", "#", dat[idx, "talk_speaker"] )
  
  ## take columns apart
  dat <-dat %>% 
    separate(talk_speaker, c("talk", "speaker"), sep = "\n") %>% 
    separate(from_to, c("from", "to"), sep = " – ")
  
  dat$speaker[is.na(dat$speaker)] <- ""
  
  ## add again the \n for birds
  if(length(ind) > 0) for (idx in ind) dat[idx, "talk"] <- gsub("#", "\n", dat[idx, "talk"] )
  
  
  ## change time format
  dat$from <- ymd_hm(paste0(sheet, dat$from), tz = tz_schedule)
  dat$to <- ymd_hm(paste0(sheet, dat$to), tz = tz_schedule)
  
  return(dat)
}


time_to_hour_minute <- function(time) {
  format(as.POSIXct(time,format="%H:%M:%S"),"%H:%M")
}

update_schedule <- function(link, sheet, tz_goal) {
  dat <- import_schedule(
    link = link, 
    sheet = sheet)
  dat$from_my <- switch_timezone(dat$from, tz_goal)
  dat$to_my <- switch_timezone(dat$to, tz_goal)
  
  ## combine from - to
  dat <- dat %>% mutate(from = time_to_hour_minute(from),
                        to = time_to_hour_minute(to),
                        from_my = time_to_hour_minute(from_my), 
                        to_my = time_to_hour_minute(to_my)) %>% 
    unite("time", c("from", "to"), sep = " - ") %>%
    unite("my_time", c("from_my", "to_my"), sep = " - ")

  
  ## order
  dat <- dat %>% select(`Time`= my_time, Talk = talk, Speaker = speaker, `Time (EDT)`= time)

  return(dat)  
}







