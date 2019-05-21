library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3,6) +
  ylim(10, 30)
ggplot(data=mpg, aes(x = cty, y = hwy)) + 
  geom_point() 
ggplot(data=midwest, aes(x = poptotal, y = popasian)) +
  geom_point()+
  xlim(0, 500000)+
  ylim(0, 10000)
midwest <- as.data.frame(ggplot2::midwest)
midwest
library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) +
         geom_col()
ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data = mpg, aes(x = hwy)) + geom_bar()       
mpg_a <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(5)
ggplot(data = mpg_a, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + geom_col() 
mpg_a
ggplot(data = mpg, aes(x = class)) + geom_bar()
ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()

ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
mpg_b <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))
ggplot(data = mpg_b, aes(x = class, y = cty)) + geom_boxplot()  
  
install.packages("XML")
library(XML)
Sys.setlocale("LC_ALL","English")
df <- readHTMLTable("http://lol.inven.co.kr/dataninfo/match/playerList.php", 
                    header = T)
str(df)
df <- df[[2]]
str(df)
View(df)
df <- df[, -c(1,2,5)]
names(df) <- c("name", "cmp", "result", "k", "d", "a", "kda", "help")
str(df)
View(df)
df[, 4:7] <- sapply(df[, 4:7], function(a){as.numeric(as.character(a))})
df$name <- as.character(df$name)
df$help <- as.numeric(sub("%", "", df$help))/100
temp <- as.data.frame(do.call(rbind, strsplit(df$name, ' (?=[^ ]+$)', perl = TRUE)))
str(temp)
df$name <- temp$V2
df$team <- temp$V1
View(df)
library(ggplot2)
ggplot(df, aes(x = team)) + geom_bar(fil = "dark blue")
mean.df <-as.data.frame(tapply(df$kda, df$team, mean))
mean.df$team <- rownames(mean.df)
names(mean.df) <- c("kda", "team")
mean.df
ggplot(mean.df, aes(team, kda)) + geom_bar(stat="identity")




install.packages("XML")
library(XML)
Sys.setlocale("LC_ALL","English")
df <- readHTMLTable("http://lol.inven.co.kr/dataninfo/match/teamTotal.php", 
                    header = T)
df
df <- df[[2]]
str(df)
View(df)
df <- df[, -c(1,3,4,5,7,8,9,10,11,12,13)]
names(df) <- c("team name", "win ratio")
View(df)
arrange(desc(df))
df %>% arrange(desc(df)) %>% 
  head(5)







  