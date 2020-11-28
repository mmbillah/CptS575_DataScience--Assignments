library(ggplot2)
library(gridExtra)

skills = factor(c("Computer Science", "Data visualization", "Statistics" , "Communication", 
           "Math",  "Machine Learning", "Domain expertise"), ordered = FALSE )
percent_current = c(30, 45, 55, 70, 60, 55, 40)
percent_future = c(40, 60, 70, 80, 65, 70, 50)
df1 = data.frame(skills,percent_current)
df2 = data.frame(skills,percent_future)

# make skills an ordered factor
df1$skills = factor(df1$skills, levels = df1$skills)
df2$skills = factor(df2$skills, levels = df2$skills)

plot1 = ggplot(data=df1, aes(x=skills, y=percent_current)) + 
  geom_bar(stat="identity", width=0.8, fill="deepskyblue3") +
  geom_text(aes(label=percent_current), hjust=-0.3, size=3.5) +
  coord_cartesian(xlim=c(0,100)) + coord_flip() +
  ggtitle("Current DS Profile") + ylab("Percentage (%)") + xlab("Skills") + 
  theme(
    plot.title = element_text(size=20, hjust=0.5, vjust = 1.5, family = "Palatino"),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino"),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino")
  )
plot1

plot2 = ggplot(data=df2, aes(x=skills, y=percent_future)) + 
  geom_bar(stat="identity", width=0.8, fill="deepskyblue3") +
  geom_text(aes(label=percent_future), hjust=-0.3, size=3.5) +
  coord_cartesian(xlim=c(0,100)) + coord_flip() +
  ggtitle("Expected DS Profile") + ylab("Percentage (%)") + xlab("Skills") +
  theme(
    plot.title = element_text(size=20, hjust=0.5, vjust = 1.5, family = "Palatino"),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino"),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino")
  )

plot2

grid.arrange(plot1, plot2, nrow=2)

plot3 = ggplot(data=df1, aes(x=skills, y=percent_current)) + 
  geom_bar(stat="identity", width=0.8, fill="deepskyblue3") +
  geom_text(aes(label=percent_current), vjust=-0.3, size=3.5) +
  coord_cartesian(ylim=c(0,100)) + #coord_flip() +
  ggtitle("Current DS Profile") + ylab("Percentage (%)") + xlab("Skills") + 
  theme(
    plot.title = element_text(size=20, hjust=0.5, vjust = 1.5, family = "Palatino"),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino"),
    axis.text.x = element_text(size=10, angle = 90),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino")
  )
plot3

plot4 = ggplot(data=df2, aes(x=skills, y=percent_future)) + 
  geom_bar(stat="identity", width=0.8, fill="deepskyblue3") +
  geom_text(aes(label=percent_future), vjust=-0.3, size=3.5) +
  coord_cartesian(ylim=c(0,100)) + #coord_flip() +
  ggtitle("Expected DS Profile") + ylab("Percentage (%)") + xlab("Skills") +
  theme(
    plot.title = element_text(size=20, hjust=0.5, vjust = 1.5, family = "Palatino"),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino"),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino")
  )

plot4

grid.arrange(plot3, plot4, ncol=2)

df3 = data.frame(time=rep(c("Before CptS 575", "After CptS 575"), each=7),skills=rep(skills),
                 percent=c(percent_current,percent_future))
df3$skills = factor(df3$skills, as.ordered(skills))
head(df3,15)

# Stacked barplot with multiple groups
plot5 = ggplot(data=df3, aes(x=skills, y=percent, fill=time)) +
  geom_bar(stat="identity", width=0.7) + scale_fill_brewer(palette=15)

plot5

# Use position=position_dodge()
plot6 = ggplot(data=df3, aes(x=skills, y=percent, fill=time)) +
  geom_bar(stat="identity", width=0.7, position=position_dodge()) +
  geom_text(aes(label=percent), position = position_dodge(0.7), vjust=-0.3, size=3.5, color = "White", family = "Palatino") +
  coord_cartesian(ylim=c(0,100)) + scale_fill_manual(values = c("olivedrab3", "deepskyblue3")) +
  ggtitle("Expected DS Profile") + ylab("Percentage (%)") + xlab("Skills") + labs(fill = "Time Zone") +
  theme(
    plot.background = element_rect(fill = "grey20"), 
    panel.background = element_rect(fill = "grey20"),
    panel.grid.major = element_line(colour = "orange", size=0.5),
    panel.grid.minor = element_line(colour = "white", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size=25, hjust=0.5, vjust = 1.5, family = "Palatino", colour = "White", margin = margin(10, 0, 0, 0)),
    axis.title.x = element_text(size=14, vjust = -0.3, family = "Palatino", colour = "White", margin = margin(0, 0, 20, 0)),
    axis.text.x = element_text(size = 10, vjust = 5.0, family = "Palatino", colour = "White"),
    axis.title.y = element_text(size=14, vjust = 1.5, family = "Palatino", colour = "White", margin = margin(10, 0, 10, 10)),
    axis.text.y = element_text(size = 10, family = "Palatino", colour = "White"),
    # Change legend background color
    legend.background = element_rect(fill = "Grey20"),
    #legend.title= element_text(color = "White",family = "Palatino", size=14),
    legend.title=element_blank(),
    #legend.key = element_rect(fill = "lightblue", color = NA),
    legend.text = element_text(color = "White",family = "Palatino", size=14)
    # Change legend key size and key width
    #legend.key.size = unit(1.5, "cm"),
    #legend.key.width = unit(0.5,"cm") 
  )

plot6











