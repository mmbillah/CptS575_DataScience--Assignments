### 1(a)
college = read.csv(url("https://scads.eecs.wsu.edu/wp-content/uploads/2017/09/College.csv"),
                   header = TRUE)
head(college)

### 1(b)
median(college$Books)

### 1(c)
library(ggplot2)
plot1 = ggplot(data=college, aes(x=perc.alumni, y=Outstate)) +
  geom_point(color="darkorange2", shape=5, size=1) +
  ggtitle("Does alumni donation help reduce out-of-state tuition?") + ylab("Out-of-state Tuition") + xlab("% Alumni who Donate") + labs(fill = "Time Zone") +
  theme(
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey60", size=0.25),
    panel.grid.minor = element_line(colour = "grey60", linetype = "dashed"),
    plot.title = element_text(size=15, hjust=0.5, vjust = 3.5, family = "Palatino", colour = "Black",
                              margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "Black",
                                margin = margin(0, 0, 20, 0)),
    axis.text.x = element_text(size = 10, family = "Palatino", colour = "grey50"),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "Black",
                                margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "grey50"),
  )

plot1

### 1(d)
#create column for overall undergrads
college$O.undergrad = college$P.Undergrad + college$F.Undergrad

plot2 = ggplot(data=college, aes(x=X, y=O.undergrad, fill=Private)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(ylim=c(0,40000)) + scale_fill_manual(labels = c("Public","Private"),values = c("olivedrab3", "deepskyblue3")) +
  ggtitle("Overall Undergrads (Public vs Private)") + ylab("Overall Number of Students") +   xlab("Schools") + #labs(fill = "Private?") +
  theme(
    panel.background = element_rect(fill = "grey20"),
    panel.grid.major = element_line(colour = "grey60", size=0.25),
    panel.grid.minor = element_line(colour = "grey60", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size=15, hjust=0.5, vjust = 1.5, family = "Palatino", colour = "Black", margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "Black", margin = margin(0, 0, 20, 0)),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "Black", margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "grey50"),
    legend.background = element_rect(fill = "transparent"),
    legend.title=element_blank(),
    legend.text = element_text(color = "White",family = "Palatino", size=10),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.direction = "horizontal"
  )

plot2


### 1(e)
Top = ifelse(college$Top10perc >75, "Yes", "No") 
table(Top)

college$Acceptance.Rate = college$Accept / college$Apps

plot3 = ggplot(data=college, aes(x=Top, y=Acceptance.Rate, color=Top, fill=Top)) +
  geom_boxplot(notch=TRUE, width=0.1) + 
  scale_color_manual(labels = c("Others","Top"), values = c("olivedrab3", "deepskyblue3")) +  
  scale_fill_manual(labels = c("Others","Top"), values = c("olivedrab3", "deepskyblue3")) +
  ggtitle("Acceptance Rate of Top vs Other Schools") + ylab("Acceptance Rate") + xlab("Schools") +
  theme(
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey60", size=0.25),
    panel.grid.minor = element_line(colour = "grey60", linetype = "dashed"),
    plot.title = element_text(size=15, hjust=0.5, vjust = 3.5, family = "Palatino", colour = "Black",
                              margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "Black",
                                margin = margin(0, 0, 20, 0)),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "Black",
                                margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "grey50"),
    legend.title = element_blank(),
    legend.position="right",
  )

plot3


###1(f)
plot4 = ggplot(data=college, aes(x=Private, y=Acceptance.Rate, color=Private, fill=Private)) +
  geom_boxplot(notch=TRUE, width=0.1) + 
  scale_color_manual(labels = c("Public", "Private"), values = c("orange", "orchid3")) +  
  scale_fill_manual(labels = c("Public", "Private"), values = c("orange", "orchid3")) +
  ggtitle("Acceptance Rate of Public vs Private Schools") + ylab("Acceptance Rate") + xlab("Schools") +
  theme(
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey60", size=0.25),
    panel.grid.minor = element_line(colour = "grey60", linetype = "dashed"),
    plot.title = element_text(size=15, hjust=0.5, vjust = 3.5, family = "Palatino", colour = "Black",
                              margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "Black",
                                margin = margin(0, 0, 20, 0)),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "Black",
                                margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "grey50"),
    legend.title = element_blank(),
    legend.position="right",
  )

plot4

# Histogram overlaid with kernel density curve
plot5 =ggplot(college, aes(x=Outstate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 
                 colour="white", fill="darkorange") +
  geom_density(alpha = .15, color = "white", fill="white") +
  ggtitle("Distribution of Out-of-state Tuition") + ylab("Density") + xlab("Outstate") +
  theme(
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey60", size=0.25),
    panel.grid.minor = element_line(colour = "grey60", linetype = "dashed"),
    plot.title = element_text(size=15, hjust=0.5, vjust = 3.5, family = "Palatino", colour = "Black",
                              margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "Black",
                                margin = margin(0, 0, 20, 0)),
    axis.text.x = element_text(size = 10, family = "Palatino", colour = "grey50"),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "Black",
                                margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "grey50"),
  )
plot5




#################

autodata = read.csv(url("https://scads.eecs.wsu.edu/wp-content/uploads/2017/09/Auto.csv"),
                    header = TRUE)
attach(autodata)

### 2(b)

horsepower = as.numeric(autodata$horsepower)
quant.var = data.frame(mpg,cylinders,displacement,horsepower,weight,acceleration)
summary(quant.var)
sd(quant.var$mpg)
sd(quant.var$cylinders)
sd(quant.var$displacement)
sd(quant.var$horsepower)
sd(quant.var$weight)
sd(quant.var$acceleration)


### 2(c)

autodata2 = autodata[-c(40:80),] # Data after discarding rows 40 to 80 inclusive
horsepower = as.numeric(autodata2$horsepower)
summary(autodata2)


### 2(d)

library(GGally)
ggpairs(quant.var)













