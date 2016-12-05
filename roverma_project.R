#
# Author: Rohan Verma
#
# Purpose: Final Project - The Pioneers of The Video Game Industry
#

# Load necessary packages
library("plyr")
library("sqldf")
library("ggplot2")

####### Loading data and Preparing Datasets for Plots ########
# File downloaded from https://www.kaggle.com/gregorut/videogamesales/downloads/videogamesales.zip
# The file was unzipped in local machine and loaded into R

games.file <- file.choose()
games.data <- read.csv(games.file, header = TRUE, stringsAsFactors = FALSE)

# filtering dataset on the basis of year
games_data <- games.data[games.data$Year < 2016, ]

# Calculating the number of games released each year by genre
count_yr.genre <- count(games_data,c("Year","Genre"))

# Annual Global Sales figures for each Publisher
games.year.pub <- as.data.frame(tapply(games_data$Global_Sales
                                       , list(games_data$Year
                                              , games_data$Publisher)
                                       , sum))

games.year.pub$Year <- rownames(games.year.pub)

# Identifying Top 5 Publishers
total.sales <- aggregate(games_data$Global_Sales, by = list(games_data$Publisher), FUN = sum)
colnames(total.sales) <- c('Publisher','TotalGlobalSales')

head(total.sales[order(total.sales$TotalGlobalSales, decreasing = TRUE),],5)


top5pub <- data.frame(games.year.pub$Year, games.year.pub$Nintendo
                      , games.year.pub$`Electronic Arts`
                      , games.year.pub$Activision
                      , games.year.pub$`Sony Computer Entertainment`
                      , games.year.pub$Ubisoft)

# Sales per year for the Top 5 publishers
sel.query <- 'select Year, Publisher, NoOfGames, NetSales, NetSales*100/NoOfGames SalesPerGames from 
              (select Year, Publisher, count(1) as NoOfGames, sum(Global_Sales) as NetSales
              from games_data 
              where Publisher in ("Activision","Electronic Arts","Nintendo","Ubisoft"
              ,"Sony Computer Entertainment")
              group by Year, Publisher)'

Sales.per.Game <- sqldf(sel.query, stringsAsFactors = FALSE)

# Subsetting dataset for Top 5 Publishers
games_top5 <- games_data[which(games_data$Publisher == 'Activision' |
                                 games_data$Publisher == 'Electronic Arts' |
                                 games_data$Publisher == 'Nintendo' |
                                 games_data$Publisher == 'Sony Computer Entertainment' |
                                 games_data$Publisher == 'Ubisoft'), ]

# Global Sales for Top 5 Publishers in each Platform
Pub_Platform <- sqldf('select Publisher, Platform, sum(Global_Sales) Net_Sales from 
                      games_top5 group by Publisher, Platform')

# Global Sales for Top 5 Publishers in each Genre
Pub_Genre <- sqldf('select Publisher, Genre, sum(Global_Sales) Net_Sales from 
                   games_top5 group by Publisher, Genre')


# Sales by Genre and Region
GenreSales <- aggregate(games_data$Global_Sales, by = list(games_data$Genre), FUN = sum)
NA_Sales <- aggregate(games_data$NA_Sales, by = list(games_data$Genre), FUN = sum)
EU_Sales <- aggregate(games_data$EU_Sales, by = list(games_data$Genre), FUN = sum)
JP_Sales <- aggregate(games_data$JP_Sales, by = list(games_data$Genre), FUN = sum)
Other_Sales <- aggregate(games_data$Other_Sales, by = list(games_data$Genre), FUN = sum)

GenreSales$NA_Sales <- NA_Sales$x
GenreSales$EU_Sales <- EU_Sales$x
GenreSales$JP_Sales <- JP_Sales$x
GenreSales$Other_Sales <- Other_Sales$x
colnames(GenreSales) <- c('Genre', 'TotalSales','NA_Sales','EU_Sales','JP_Sales','Other_Sales')

GenreSalesRegion <- data.frame('North America', GenreSales$Genre, GenreSales$NA_Sales)
colnames(GenreSalesRegion) <- c('Region','Genre','Sales')

GenreSalesRegion <- rbind(GenreSalesRegion,data.frame(Region = 'Europe',Genre = GenreSales$Genre
                                                      ,Sales = GenreSales$EU_Sales))
GenreSalesRegion <- rbind(GenreSalesRegion,data.frame(Region = 'Japan',Genre = GenreSales$Genre
                                                      ,Sales = GenreSales$JP_Sales))
GenreSalesRegion <- rbind(GenreSalesRegion,data.frame(Region = 'Other',Genre = GenreSales$Genre
                                                      ,Sales = GenreSales$Other_Sales))

# Top 5 Platforms in each Decade from 1980s - 2015 on which Top 100 Video Games of that decade were played
DecadePerf <- as.data.frame(sqldf('select Platform, "1980s" Decade, NoOfGames, TotalSales 
                                  from (select Platform, count(1) NoOfGames, sum(Global_Sales) TotalSales from 
                                  (select * from games_data where Year >= 1980 and Year < 1990 limit 100)
                                  group by Platform
                                  order by 3 desc limit 5) 
                                  union 
                                  select Platform, "1990s" Decade, NoOfGames, TotalSales 
                                  from (select Platform, count(1) NoOfGames, sum(Global_Sales) TotalSales from 
                                  (select * from games_data where Year >= 1990 and Year < 2000 limit 100)
                                  group by Platform
                                  order by 3 desc limit 5)
                                  union
                                  select Platform, "2000s" Decade, NoOfGames, TotalSales 
                                  from (select Platform, count(1) NoOfGames, sum(Global_Sales) TotalSales from 
                                  (select * from games_data where Year >= 2000 and Year < 2010 limit 100)
                                  group by Platform
                                  order by 3 desc limit 5)
                                  union
                                  select Platform, "2010-2015" Decade, NoOfGames, TotalSales 
                                  from (select Platform, count(1) NoOfGames, sum(Global_Sales) TotalSales from 
                                  (select * from games_data where Year >= 2010 and Year <= 2015 limit 100)
                                  group by Platform
                                  order by 3 desc limit 5)'))


############# PLOTS #############
# Heat Map for Evolution of Different Genres by Year
(p <- ggplot(count_yr.genre, aes(Genre, Year)) + geom_tile(aes(fill = freq),
                                                           colour = "white") + scale_fill_gradient(low = "#FFD10A",high = "#E84B0C")
)

# Net Global Sales per Year for Top 5 Publishers
ggplot(data = Sales.per.Game, aes(x = Year, y = NetSales
                                  , shape = Publisher, group = Publisher)) + 
  geom_line(aes(colour = Publisher), size = 0.75, alpha = 0.2) +
  theme_classic() + theme(axis.text.x=element_text(angle=45)) +
  geom_smooth(aes(colour = Publisher, span =0.3))

# Radar Chart for Global Sales of Top 5 Publishers in each Platform
ggplot(Pub_Platform, aes(x = Platform, y = Net_Sales, group=Publisher, color=Publisher,fill=Publisher)) +
  geom_polygon(aes(group = Publisher, color = Publisher), size = 1, alpha=0.2) +
  coord_polar(theta="x") +   ggtitle('Sales by Platform for Top 5 Publishers')

# Radar Chart for Global Sales of Top 5 Publishers in each Genre
ggplot(Pub_Genre, aes(x = Genre, y = Net_Sales)) +
  geom_polygon(aes(color=Publisher, group=Publisher, fill = Publisher), size = 1, alpha=0.2) +
  coord_polar(theta="x") +   ggtitle('Sales by Genre for Top 5 Publishers')

# Top Platforms in Each Decade
ggplot(data = DecadePerf, aes(x = Decade, y= TotalSales)) + 
  geom_bar(stat = 'identity', aes(fill = Platform),position = 'dodge', size = 1) 

# Sales in each Region by Genre
ggplot(data = GenreSalesRegion, aes(x = Genre, y = Sales, fill = Region)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  theme_classic() +
  ggtitle('Sales by Region across Different Genres')

# Net Global Sales in each Platform since 1980
ggplot(games_data, aes(x = Platform, y = Global_Sales)) + 
  geom_bar(stat = 'identity')

####################################################################

