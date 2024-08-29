install.packages("baseballr")
install.packages("rlist")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("data.table")
install.packages("hablar")

library(baseballr)
library(rlist)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

#2023 season



analyzeYear <- function(year, step=20) {
  
  allseasons = mlb_seasons_all()
  
  season_start_date = filter(allseasons, season_id==year)["regular_season_start_date"]
  season_end_date = filter(allseasons, season_id==year)["regular_season_end_date"]
  
  qualifiedPlayers = bref_daily_batter(season_start_date, season_end_date)
  qualifiedPlayers = filter(qualifiedPlayers, G > 140)
  
  
  start = ymd(season_start_date)
  end = ymd(season_start_date) + step
  day = step
  
  dayofseason = list()
  wOBAvalues = list()
  
  while (end < ymd(season_end_date)) {
    
    
    playersubset = bref_daily_batter(start, end)
    playersubset = filter(playersubset, Name %in% qualifiedPlayers$Name)
    
    
    playersubset = playersubset %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
    playersubset[[2]][[length(playersubset[[2]])]] = year
    
    
    playersubset = woba_plus(playersubset)
    
    playersubset = filter(playersubset, Name == "Total")
    
    dayofseason = append(dayofseason, day)
    wOBAvalues = append(wOBAvalues, playersubset$wOBA[1])
    
    start = start + 1
    end = end + 1
    day = day + 1
    
    print(paste0(year, ": ", day))
  }
  
  wOBAframe <- data.frame(day=20:dayofseason[[length(dayofseason)]])
  
  wOBAframe$wOBA <- wOBAvalues
  
  wOBAframe$day=as.numeric(wOBAframe$day)
  wOBAframe$wOBA=as.numeric(wOBAframe$wOBA)
  
  return(wOBAframe)
  
}


testYear = 2023

data2023 = analyzeYear(2023)

wOBAplot = ggplot(data2023, aes(x=day, y=wOBA)) + geom_line() + 
  ggtitle("20 Game Rolling Average of Batter wOBA", subtitle="2023 Season") +
  xlab("Day of Season") + xlim(20,190) + scale_x_continuous(breaks = c(20, 40, 60, 80, 100, 120, 140, 160, 180))
plot(wOBAplot)
  
  
  
  