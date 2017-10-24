#Using data from insideairbnb.com - data from the Santa Cruz, CA area, last updated October 2015
#Focus is on visualizing the data by room type

santacruz = read.csv("documents/airbnb_santacruz.csv")
head(santacruz)

#Find out types of rooms
unique(santacruz$room_type)

#price per night based on room type
private <- subset(santacruz, room_type=="Private room")$price
home <- subset(santacruz, room_type=="Entire home/apt")$price
shared <- subset(santacruz, room_type=="Shared room")$price
prices <- c(mean(private),mean(home),mean(shared))

#Without ggplot, can just use built-in barplot (but it takes longer to try and make error bars)
barplot(prices,xlab="Room Type",ylab="$/night", main="Santa Cruz Airbnb Daily Price by Rental Type",
        names.arg=c("Private Room", "Full House/Apt", "Shared Room"), col=c("Red","Yellow","Blue"))

#With ggplot
#bar chart based on mean price per night, separated by room type
install.packages("ggplot2")
library(ggplot2)
ggplot(santacruz,aes(x=room_type,y=price)) + geom_bar(
    aes(fill=room_type),stat = "summary", fun.y = "mean") + labs(
    title="Santa Cruz Airbnb Daily Price by Rental Type", x="Room Type", y="$/night", fill="Room Type") +
geom_errorbar(stat = "summary",
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
                  
#Is the difference in price between a private and a shared room significant?
#It certainly looks so in bar graph, but good to check (t should indeed be quite large)
t <- (mean(private)-mean(shared))/(sqrt(sd(private)^2/length(private)+sd(shared)^2/length(private)))
t
