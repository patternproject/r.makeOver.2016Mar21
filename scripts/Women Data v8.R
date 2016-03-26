library(dplyr)
library(ggplot2)
library(tidyr)

# --------------------------------------------------------------------
# Global Settings
# --------------------------------------------------------------------
title.color = "darkolivegreen"



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
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
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
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 

p2.2 = p2.1 + ggtitle("Constitution")

p2.3 <- p2.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
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
df.violence = read.csv("Domestic Violence.csv")

names(df.violence) = c("Country","Q1","Q2","Q3","Q4","Q5","Q6","Q7")

df.m.violence <- df.violence %>%
  gather(key=Country)

names(df.m.violence)[2] = "Question"


df.m.violence.reduced <- df.m.violence %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.violence.region <- left_join(df.m.violence.reduced, df.region)

df.violence.2 = df.violence.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)


df.violence.2$Region = as.factor(df.violence.2$Region)

p3.1 <- ggplot(df.violence.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 


p3.2 = p3.1 + ggtitle("Violence")

p3.3 <- p3.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
  )

p3.4 <- p3.3 + 
  scale_y_continuous(breaks=seq(1, 7, 1))  # Ticks from 1-7, every 1

# final plot
p3.4

# removing df not required anymore
rm(df.violence,df.m.violence,df.m.violence.reduced,df.violence.region)

# --------------------------------------------------------------------
# Other Equality
# --------------------------------------------------------------------
df.equality = read.csv("Other Equality.csv")

names(df.equality) = c("Country","Q1","Q2","Q3")

df.m.equality <- df.equality %>%
  gather(key=Country)

names(df.m.equality)[2] = "Question"


df.m.equality.reduced <- df.m.equality %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.equality.region <- left_join(df.m.equality.reduced, df.region)

df.equality.2 = df.equality.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)


df.equality.2$Region = as.factor(df.equality.2$Region)

p4.1 <- ggplot(df.equality.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 

p4.2 = p4.1 + ggtitle("Equality")

p4.3 <- p4.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
  )

p4.4 <- p4.3 + 
  scale_y_continuous(breaks=seq(1, 3, 1))  # Ticks from 1-3, every 1

# final plot
p4.4

# removing df not required anymore
rm(df.equality,df.m.equality,df.m.equality.reduced,df.equality.region)


# --------------------------------------------------------------------
# Property
# --------------------------------------------------------------------
df.property = read.csv("Property.csv")

names(df.property) = c("Country","Q1","Q2","Q3")

df.m.property <- df.property %>%
  gather(key=Country)

names(df.m.property)[2] = "Question"


df.m.property.reduced <- df.m.property %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.property.region <- left_join(df.m.property.reduced, df.region)

df.property.2 = df.property.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)


df.property.2$Region = as.factor(df.property.2$Region)

p5.1 <- ggplot(df.property.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 

p5.2 = p5.1 + ggtitle("Property")

p5.3 <- p5.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
  )

p5.4 <- p5.3 + 
  scale_y_continuous(breaks=seq(1, 3, 1))  # Ticks from 1-3, every 1

# final plot
p5.4

# removing df not required anymore
rm(df.property,df.m.property,df.m.property.reduced,df.property.region)


# --------------------------------------------------------------------
# Work
# --------------------------------------------------------------------
df.work = read.csv("Work.csv")

names(df.work) = c("Country","Q1","Q2","Q3","Q4","Q5","Q6","Q7")

df.m.work <- df.work %>%
  gather(key=Country)

names(df.m.work)[2] = "Question"


df.m.work.reduced <- df.m.work %>%
  group_by(Country) %>%
  filter(value=="Yes") %>%
  summarise(YesCount = n()) %>%
  arrange(YesCount)

df.work.region <- left_join(df.m.work.reduced, df.region)

df.work.2 = df.work.region %>%
  filter(!is.na(Region)) %>% ## excluding those with no Region
  select(Country,YesCount,Region)


df.work.2$Region = as.factor(df.work.2$Region)

p6.1 <- ggplot(df.work.2, aes(x=Region,y=YesCount)) +
  geom_boxplot(aes(fill=Region),alpha=I(0.5)) + 
  geom_jitter(position=position_jitter(width = 0.2, height = 0.09), size=2,alpha=0.2) +
  coord_flip() 

p6.2 = p6.1 + ggtitle("Work")

p6.3 <- p6.2 +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),    # Remove y - axis label
    axis.title.x = element_blank(),    # Remove x-axis label
    legend.position = "none", # np legend 
    plot.title=element_text(family="Times", face="bold", size=20, color=title.color)
  )

p6.4 <- p6.3 + 
  scale_y_continuous(breaks=seq(1, 7, 1))  # Ticks from 1-3, every 1

# final plot
p6.4

# removing df not required anymore
rm(df.work,df.m.work,df.m.work.reduced,df.work.region)



# --------------------------------------------------------------------
# Combining all
# --------------------------------------------------------------------
# install.packages("cowplot")
# require(cowplot)
# library(cowplot)

require(gridExtra)
# pdf("foo.pdf")
# grid.arrange(p1.4,p2.4, p3.4,p4.4, p5.4,p6.4,ncol=2, nrow=3)
# dev.off()


# hint from https://jonkimanalyze.wordpress.com/2014/03/26/ggplot2-arrangegrob-arrange-ggplots-on-a-page/
grobframe <- arrangeGrob(p1.4,p2.4, p3.4,p4.4, p5.4,p6.4,ncol=2, nrow=3,
main = textGrob("\nWomen's Rights", gp = gpar(fontsize=60, fontface="bold")),
sub = textGrob("*X-Axis depicts Total Number of Yes to Different Parameters*", x=0, hjust=-0.5, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 15)))

print(grobframe)
ggsave(filename="test.png",plot=grobframe,width = 20, height = 25, units = "in",dpi=400)
