demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)

library(jsonlite)

demo_table2 <- fromJSON(txt='demo.json')

demo_table[3,"Year"]

demo_table[3,3]

demo_table$"Vehicle_Class"

demo_table$"Vehicle_Class"[2]

filter_table <- demo_table2[demo_table2$price > 10000,]

filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) #filter by price and drivetrain

filter_table3 <- demo_table2[("clean" %in% demo_table2$title_status) & (demo_table2$price > 10000) & (demo_table2$drive == "4wd"),]

sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)

demo_table[sample(1:nrow(demo_table), 3),]

library(dplyr)
library(tidyverse)
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame

summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer), .groups = 'keep') #create summary table

summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n(), .groups = 'keep') #create summary table with multiple columns

demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)

long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

wide_table <- long_table %>% spread(key="Metric",value="Score")

install.packages("ggplot2")
library(ggplot2)

plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot

plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot bar plot with labels

plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") + #plot a boxplot with labels
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2

plt + geom_line()

plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels

plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2

plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels

plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics

plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees

mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels

mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + #add heatmap with labels > theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + geom_point()
theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
geom_point() #overlay scatter plot on top

mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") #add scatter plot

mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ), .groups = 'keep')
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars

mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)

plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot with labels rotated 45 degrees

plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels
