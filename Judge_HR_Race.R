library(ggplot2)
library(gganimate)
library(ggimage)
library(viridis)
library(hrbrthemes)
library(readr)
library(dplyr)
library(rvest)
library(strex)

# import raw data from baseball-reference
judge = read_html("https://www.baseball-reference.com/players/gl.fcgi?id=judgeaa01&t=b&year=2022") %>%
  html_nodes("table") %>%
  html_table()
judge = data.frame(judge[[5]])

maris = read_html("https://www.baseball-reference.com/players/gl.fcgi?id=marisro01&t=b&year=1961") %>%
  html_nodes("table") %>%
  html_table()
maris = data.frame(maris[[5]])

bonds = read_html("https://www.baseball-reference.com/players/gl.fcgi?id=bondsba01&t=b&year=2001") %>%
  html_nodes("table") %>%
  html_table()
bonds = data.frame(bonds[[5]])

# change "Date" column values fram characters to "Date" type
judge["Date"] <- as.Date(judge$Date, "%b %d")
maris["Date"] <- as.Date(maris$Date, "%b %d")
bonds["Date"] <- as.Date(bonds$Date, "%b %d")

# change "HR" column values from characters to numeric
judge["HR"] <- suppressWarnings(as.numeric(judge$HR))
maris["HR"] <- suppressWarnings(as.numeric(maris$HR))
bonds["HR"] <- suppressWarnings(as.numeric(bonds$HR))

# change "Gtm" column values from characters to numeric (and remove games they missed)
judge["Gtm"] <- as.numeric(str_first_number(judge$Gtm))
maris["Gtm"] <- as.numeric(str_first_number(maris$Gtm))
bonds["Gtm"] <- as.numeric(str_first_number(bonds$Gtm))

# select only the columns I want (and remove null values)
judge = na.omit(select(judge, Gtm, Date, HR))
maris = na.omit(select(maris, Gtm, Date, HR))
bonds = na.omit(select(bonds, Gtm, Date, HR))

# combine days with double headers
judge1 = aggregate(judge, by = list(judge$Date), FUN = max)
judge2 = aggregate(judge$HR, by = list(judge$Date), FUN = sum)
judge = cbind(judge1[2:3], HR = judge2[,2])

maris1 = aggregate(maris, by = list(maris$Date), FUN = max)
maris2 = aggregate(maris$HR, by = list(maris$Date), FUN = sum)
maris = cbind(maris1[2:3], HR = maris2[,2])

bonds1 = aggregate(bonds, by = list(bonds$Date), FUN = max)
bonds2 = aggregate(bonds$HR, by = list(bonds$Date), FUN = sum)
bonds = cbind(bonds1[2:3], HR = bonds2[,2])

# add column with cumulative homer total
cumHRs = cumsum(judge["HR"])
colnames(cumHRs) = "cumHRs"
judge = cbind(judge, cumHRs)

cumHRs = cumsum(maris["HR"])
colnames(cumHRs) = "cumHRs"
maris = cbind(maris, cumHRs)

cumHRs = cumsum(bonds["HR"])
colnames(cumHRs) = "cumHRs"
bonds = cbind(bonds, cumHRs)

# remove games in which they don't homer
judge = judge[which(judge$HR > 0 | judge$cumHRs == 0), ]
maris = maris[which(maris$HR > 0 | maris$cumHRs == 0), ]
bonds = bonds[which(bonds$HR > 0 | bonds$cumHRs == 0), ]

# add column with players' names (for graphing purposes)
judge = cbind(Player = "Judge", judge)
maris = cbind(Player = "Maris", maris)
bonds = cbind(Player = "Bonds", bonds)

# combine the player data
allGames = rbind(judge, maris, bonds)

# create animated graph
graph <- ggplot(allGames, aes(x=Gtm, y=cumHRs, group=Player, color=Player)) +
  geom_line() +
  geom_point() +
  ggtitle("MLB Home Run Race") +
  theme_ipsum() +
  ylab("Home Runs") +
  xlab("Game") +
  transition_reveal(Date) +
  theme(aspect.ratio=3/4)

animate(graph, fps = 7, end_pause = 10)
