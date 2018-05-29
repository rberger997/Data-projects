## NBA playoff advanced player stats analysis
## Ryan Berger PhD
## 5-29-18


## Data was scraped from basketball reference and saved as csv
## Read in file if already saved
full <- read.csv('playoff_advanced_stats_1960-2018.csv')
full <- full[,-1]

## Arrange by win shares
full <- arrange(full, desc(WS))
head(full)

## Look at top 20 playoff PER (filter by 200 minutes to eliminate outliers)
filter(full[,1:10], MP > 200) %>% 
  arrange(-PER) %>% 
  head(., 20)
head(full)

filter(full, Player == 'Michael Jordan')

## Filter out cleveland 
cle <- filter(full, Tm == 'CLE' & MP > 100) %>% 
  arrange(-PER)

head(cle, 10)

## Compare career playoff PER
## Normalize PER by games played
full$PER_total <- full$PER * full$G
full$MP.G <- round(full$MP / full$G, 1)
full$VORP.G <- round(full$VORP / full$G, 5)

## Function to get career PER for each player
career_PER <- function(player){
  sum(filter(full, Player == player)$PER_total) / sum(filter(full, Player == player)$G)
}
career_PER('Kobe Bryant')
career_PER('Tim Duncan')
career_PER('Stephen Curry')
career_PER('Kevin Durant')
career_PER('Isaiah Thomas')
career_PER('Magic Johnson')

## Make dataframe of player career playoff PER
player.PER <- NULL
head(full)
full.1 <- filter(full, MP > 200)
for(player in unique(full.1$Player)){
  temp <- as.data.frame(list(player,round(career_PER(player),2),sum(filter(full.1, Player == player)$G)))
  colnames(temp) <- c('Player', 'PER', 'G')
  player.PER <- rbind(player.PER, temp) %>% 
    arrange(-PER)
}

write.csv(player.PER, 'career_playoff_PER_by_player.csv')
head(player.PER)

player.PER <- filter(player.PER, G > 25)  # Set minimum games played filter
head(player.PER)


## Look at the help on each player's team in finals runs
## Extract Jordan's teammates
mjhelp <- filter(full, Tm == 'CHI' & Year %in% c(1991:1993, 1996:1998) & MP.G > 15) %>% 
  filter(Player != 'Michael Jordan')
mean(mjhelp$PER)
mean(mjhelp$WS.48)

## Extract Lebron's Cleveland teammates
ljhelp.cle <- filter(full, Tm == 'CLE' & Year %in% c(2007,2015:2018) & Player != 'LeBron James' & MP.G > 15)
mean(ljhelp.cle$PER)
mean(ljhelp.cle$WS.48)

## Extract Lebron's Miami teammates
ljhelp.mia <- filter(full, Tm == 'MIA' & Year %in% 2011:2014 & Player != 'LeBron James' & MP.G > 15)
mean(ljhelp.mia$PER)
mean(ljhelp.mia$WS.48)

## Combine all Lebron's teammates together
ljhelp <- rbind(ljhelp.cle, ljhelp.mia)
mean(ljhelp$PER)
mean(ljhelp$WS.48)

## Combine Both MJ and LJ sets together
mjhelp$MJ.LJ <- 'MJ'
ljhelp$MJ.LJ <- 'LJ'
allhelp <- rbind(mjhelp, ljhelp)

boxplot(allhelp$PER)

boxplot(PER ~ MJ.LJ,
        data = allhelp,
        col = c('maroon', 'red'))
t.test(PER ~ MJ.LJ, data = allhelp)


## Plot teammates help
metric <- 'PER'
ggplot(allhelp, aes(y = PER, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red')) +
  geom_jitter(width = 0.1, size = 0.5) + 
  xlab('Player') +
  ylab(paste('Teammate help (',metric,')', sep = '')) + 
  ggtitle('Playoffs teammate help - Lebron vs. Jordan')
t.test(PER ~ MJ.LJ, data = allhelp)  ## Stats show non-significant


## Plot teammates help
metric <- 'VORP'
ggplot(allhelp, aes(y = VORP, x = MJ.LJ)) + geom_boxplot(fill = c('maroon', 'red')) +
  geom_jitter(width = 0.1, size = 0.5) + 
  xlab('Player') +
  ylab(paste('Teammate help (',metric,')', sep = '')) + 
  ggtitle('Playoffs teammate help - Lebron vs. Jordan')
t.test(VORP ~ MJ.LJ, data = allhelp)  ## Stats show highly significant (phacking)

a <- filter(allhelp, Year == 2016)
b <- filter(allhelp, Year == 1996)

gsw <- filter(full, Tm == 'GSW' & Year %in% 2015:2018)
mean(gsw$PER)

head(full)
mean(filter(allhelp, Year == 1991)$PER)
