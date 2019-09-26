############################## Script check temperature record at the Hawkesbury region         ###############
############################## Created by: Mingkai Jiang                                        ###############
############################## First created on: 2019-09-26                                     ###############
##############################################################################################################
##############################################################################################################
##############################################################################################################
#### Prepare all functions and coordinate files
### clear workspace
rm(list=ls())

### Calls script containing all necessary functions
source("R/prepare_R.R")

##############################################################################################################
#### read in csv
minDF <- read.csv("data/Monthly_Min_Temperature.csv")
maxDF <- read.csv("data/Monthly_Max_Temperature.csv")

### Calculate percentiles
min.qt <- quantile(minDF$Jan, c(0.9, 0.95), na.rm=T) 
max.qt <- quantile(maxDF$Jan, c(0.9, 0.95), na.rm=T) 

### subset 2017 - 2019
min.sb <- subset(minDF, Year%in%c(2017, 2018, 2019))
max.sb <- subset(maxDF, Year%in%c(2017, 2018, 2019))

#### look at January value
p1 <- ggplot(minDF)+
    geom_point(aes(x=Year, y=Jan), fill="grey", pch=21)+
    geom_point(data=min.sb, aes(x=Year, y=Jan), fill="red", pch=21)+
    geom_hline(yintercept = min.qt, color=c("orange", "red"))+
    xlab("Year") + ylab(expression("January Min T (" * degree * "C)")) +
    annotate(geom="text", x=1935, y=min.qt[2]+0.2, label="95th percentile",
             color="red")+
    annotate(geom="text", x=1935, y=min.qt[1]+0.2, label="90th percentile",
             color="orange")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")

plot(p1)

p2 <- ggplot(maxDF)+
    geom_point(aes(x=Year, y=Jan), fill="grey", pch=21)+
    geom_point(data=max.sb, aes(x=Year, y=Jan), fill="red", pch=21)+
    geom_hline(yintercept = max.qt, color=c("orange", "red"))+
    xlab("Year") + ylab(expression("January Max T (" * degree * "C)")) +
    annotate(geom="text", x=1935, y=max.qt[2]+0.2, label="95th percentile",
             color="red")+
    annotate(geom="text", x=1935, y=max.qt[1]+0.2, label="90th percentile",
             color="orange")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")

plot(p2)

if(!dir.exists("output"))dir.create("output")

pdf("output/Hawkesbury_min_temperature_profile.pdf")
plot(p1)
dev.off()

pdf("output/Hawkesbury_max_temperature_profile.pdf")
plot(p2)
dev.off()

##############################################################################################################
#### End

