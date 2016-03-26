library(dplyr)
library(ggplot2)
library(tidyr)

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

df.region = read.csv("Region Info.csv")

df.region <- df.region %>%
  select(Region,Country,Country.Code)

df.abortion.region <- left_join(df.m.abortion.reduced, df.region)

df.c = df.abortion.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)

df.c$Region = as.factor(df.c$Region)

p4 <- ggplot(df.c, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width=.2), size=2,alpha=0.2) +
  coord_flip() 

p4 = p4 + ggtitle("Abortion")

p5 <- p4 +
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

p6 <- p5 + 
  scale_y_continuous(breaks=seq(1, 7, 1))  # Ticks from 1-7, every 1


library("RColorBrewer", lib.loc="/usr/local/lib/R/site-library")
display.brewer.pal(7,"Pastel1")

mypalette<-brewer.pal(7,"Pastel1")

# p7 <- p6 + 
#   scale_fill_brewer(mypalette)

# --------------------------------------------------------------------

df.constitution = read.csv("Constitution.csv")

names(df.constitution) = c("Country","Q1","Q2","Q3")

df.m.constitution <- df.constitution %>%
  gather(key=Country)

names(df.m.constitution)[2] = "Question"
