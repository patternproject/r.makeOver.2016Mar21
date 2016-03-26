library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
install.packages("useful")
library(useful)

# read input file
df.abortion = read.csv(file = "Abortion.csv",stringsAsFactors = FALSE)
df.region = read.csv(file = "Region Info.csv",stringsAsFactors = FALSE)

names(df.abortion) = c("Country","Q1","Q2","Q3","Q4","Q5","Q6","Q7")
df.abortion.sub = df.abortion %>% filter(Country %in% c("Afghanistan","Albania"))

df.abortion.sub2 <- df.abortion.sub %>% 
  gather(Country)




rowSums(df.region.sub == "Yes")>0

rowSums(df.region.sub)

# ----------------------------------------------------------
# for 2012
# ----------------------------------------------------------
my.year = 2012

df.2012 <- df.2 %>%
  filter(Year == my.year) %>%
  select(Region, CPI)

df.2012.m <- df.2012 %>%
  group_by(Region) %>%
  summarise(mean=mean(CPI))

p2012 = ggplot(df.2012, aes(x=CPI, fill=Region, colour=Region)) +
  geom_density(alpha=.3) + 
  facet_grid(.~ Region)

p2012 = p2012 +
  geom_vline(data=df.2012.m, aes(xintercept=mean),
             linetype="dashed", size=1)

p2012 = p2012 + ggtitle("Y-2012")

p1 = p2012 + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p1 
# ----------------------------------------------------------
# year 2013
# ----------------------------------------------------------
my.year = 2013

df.2013 <- df.2 %>%
  filter(Year == my.year) %>%
  select(Region, CPI)

df.2013.m <- df.2013 %>%
  group_by(Region) %>%
  summarise(mean=mean(CPI))

p2013 = ggplot(df.2013, aes(x=CPI, fill=Region, colour=Region)) +
  geom_density(alpha=.3) + 
  facet_grid(.~ Region)

p2013 = p2013 +
  geom_vline(data=df.2013.m, aes(xintercept=mean),
             linetype="dashed", size=1)

p2013 = p2013 + ggtitle("Y-2013")

p2 = p2013 + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p2


# ----------------------------------------------------------
# year 2014
# ----------------------------------------------------------
my.year = 2014

df.2014 <- df.2 %>%
  filter(Year == my.year) %>%
  select(Region, CPI)

df.2014.m <- df.2014 %>%
  group_by(Region) %>%
  summarise(mean=mean(CPI))

p2014 = ggplot(df.2014, aes(x=CPI, fill=Region, colour=Region)) +
  geom_density(alpha=.3) + 
  facet_grid(.~ Region)

p2014 = p2014 +
  geom_vline(data=df.2014.m, aes(xintercept=mean),
             linetype="dashed", size=1)

p2014 = p2014 + ggtitle("Y-2014")

p3 = p2014 + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p3


# ----------------------------------------------------------
# year 2015
# ----------------------------------------------------------
my.year = 2015

df.2015 <- df.2 %>%
  filter(Year == my.year) %>%
  select(Region, CPI)

df.2015.m <- df.2015 %>%
  group_by(Region) %>%
  summarise(mean=mean(CPI))

p2015 = ggplot(df.2015, aes(x=CPI, fill=Region, colour=Region)) +
  geom_density(alpha=.3) + 
  facet_grid(.~ Region)

p2015 = p2015 +
  geom_vline(data=df.2015.m, aes(xintercept=mean),
             linetype="dashed", size=1)

p2015 = p2015 + ggtitle("Y-2015")

p4 = p2015 + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color="darkolivegreen")
  )

p4


# ----------------------------------------------------------
# combining them all 
# ----------------------------------------------------------
pdf("CPI3.pdf", width = 10, height = 20)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(5, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))

# npc = normalized parent corodinates
# ref: http://www.amstat.org/publications/jse/v18n3/zhou.pdf
grid.text("CPI", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar( col = "#A9A8A7", cex = 12, alpha = 0.3))

my.y=0.9
increment=0.02
grid.text("According to Transparency International:", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2))
my.y=my.y-increment
grid.text("The 2010 Corruption Perceptions Index shows that", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2)) 
my.y=my.y-(1*increment)
grid.text("nearly three quarters", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2)) 
my.y=my.y-(1.20*increment)
grid.text("of the 178 countries in the index score below five,", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2))
my.y=my.y-(1.40*increment)
grid.text("on a scale from 10 (highly clean) to 0 (highly corrupt).", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2))
my.y=my.y-(1.60*increment)
grid.text("These results indicate a serious corruption problem.", vjust = 0, hjust = .5, y = unit(my.y, "npc"), gp = gpar(col = "darkolivegreen", cex = 2))


#grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
#grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
print(p1, vp = vplayout(2, 1:3))
print(p2, vp = vplayout(3, 1:3))
print(p3, vp = vplayout(4, 1:3))
print(p4, vp = vplayout(5, 1:3))

dev.off()