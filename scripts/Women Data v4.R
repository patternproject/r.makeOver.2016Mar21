library(dplyr)
library(ggplot2)
library(tidyr)

# --------------------------------------------------------------------
df.region = read.csv("Region Info.csv")

df.region <- df.region %>%
  select(Region,Country,Country.Code)
# --------------------------------------------------------------------
# Abortion
# --------------------------------------------------------------------
df.abortion = read.csv("Abortion.csv")

names(df.abortion) = c("Country","Q1","Q2","Q3","Q4","Q5","Q6","Q7")

df.m.abortion <- df.abortion %>%
  gather(key=Country)

names(df.m.abortion)[2] = "Question"

df.m.abortion.reduced <- df.m.abortion %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.abortion.region <- left_join(df.m.abortion.reduced, df.region)

df.abortion.2 = df.abortion.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)

df.abortion.2$Region = as.factor(df.abortion.2$Region)

p1.1 <- ggplot(df.abortion.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width=.2), size=2,alpha=0.2) +
  coord_flip() 

p1.2 = p1.1 + ggtitle("Abortion")

p1.3 <- p1.2 +
  theme_minimal() +
  theme(
    #panel.grid.minor = element_blank(),
    #panel.grid.major = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p1.4 <- p1.3 + 
  scale_y_continuous(breaks=seq(1, 7, 1))  # Ticks from 1-7, every 1

# final plot
p1.4

# --------------------------------------------------------------------
# unable to set the palette
library("RColorBrewer", lib.loc="/usr/local/lib/R/site-library")
display.brewer.pal(7,"Pastel1")

mypalette<-brewer.pal(7,"Pastel1")

# p7 <- p6 + 
#   scale_fill_brewer(mypalette)

# removing df not required anymore
rm(df.abortion,df.m.abortion,df.m.abortion.reduced,df.abortion.region)

# --------------------------------------------------------------------
# Constitution
# --------------------------------------------------------------------
df.constitution = read.csv("Constitution.csv")

names(df.constitution) = c("Country","Q1","Q2","Q3")

df.m.constitution <- df.constitution %>%
  gather(key=Country)

names(df.m.constitution)[2] = "Question"


df.m.constitution.reduced <- df.m.constitution %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.constitution.region <- left_join(df.m.constitution.reduced, df.region)

df.constitution.2 = df.constitution.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)


df.constitution.2$Region = as.factor(df.constitution.2$Region)

# p2.1 <- ggplot(df.constitution.2, aes(x=Region,y=YesCount)) +
#   geom_boxplot(aes(fill=Region,colour=Region),alpha=I(0.3)) + 
#   geom_jitter(position=position_jitter(width = 0.2, height = 0.01), size=2,alpha=0.5) +
#   coord_flip() 

p2.1 <- ggplot(df.constitution.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.3)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 



# p2.1 <- ggplot(df.constitution.2, aes(x=Region,y=YesCount)) +
#   geom_boxplot(aes(colour=Region),fill="white", alpha=I(0.2)) + 
#   geom_jitter(position=position_jitter(width = 0.2, height = 0.01), size=2,alpha=0.5) +
#   coord_flip() 

p2.2 = p2.1 + ggtitle("Constitution")

p2.3 <- p2.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p2.4 <- p2.3 + 
  scale_y_continuous(breaks=seq(1, 3, 1))  # Ticks from 1-3, every 1

# final plot
p2.4

# removing df not required anymore
rm(df.constitution,df.m.constitution,df.m.constitution.reduced,df.constitution.region)

# --------------------------------------------------------------------
# Domestic Violence
# --------------------------------------------------------------------
