tweets <- scan(file="data/tweets2.txt", what="character",sep=",")
sampletweets <- tweets[1:10]

library(lubridate)
library(stringr)
library(ggplot2)
library(scales)

fixed <- 0

for (i in 1:length(tweets)){
  split <- str_split(tweets[i], " ")
  joined <- paste(split[[1]][6], split[[1]][2], split[[1]][3], split[[1]][4], sep=" ")
  fixed[i] <- joined
}

dates <- ymd_hms(fixed)

p <- ggplot() + geom_histogram(aes(x=dates), binwidth=60*60*24*7)+ labs(title="AmeliaMN tweets")
p <- p + ylab("Tweets") + ggtitle("One thousand tweets by @AmeliaMN") + xlab("")
p <- p + scale_x_datetime(breaks="3 month", minor_breaks="1 month", labels=date_format("%B %Y"))
p <- p +  annotate("text", x = ymd("2012 Dec 15"), y = 205, label = "#compj") +annotate("text", x = ymd("2013 Apr 01"), y = 310, label = "#eyeo2013")+annotate("text", x = ymd("2013 Sep 01"), y = 50, label = "#futureofstats")
ggsave("plots/1000tweets.pdf", p, width=20, height=6)


r <- ggplot() + geom_line(aes(x=dates, y=1000:1))
r <- r + ylab("Total number of tweets") + xlab("") + scale_x_datetime(breaks="1 year", minor_breaks="3 month", labels=date_format("%Y"))
r <- r + ggtitle("Total number of tweets by @AmeliaMN over time")
r
ggsave("plots/cumulativetweets.pdf", r)



# People I follow
followtweets <- read.csv("data/followtweets.txt")
s <- ggplot(followtweets) + geom_histogram(aes(x=tweets)) + xlab("Total tweets") + ylab("Users")
s <- s + annotate("text", x = 112500, y = 6, label = "@znmeb") + annotate("text", x = 125000, y = 6, label = "@nytimes")
s + annotate("text", x = 40000, y = 10, label = "@VirginAmerica") + annotate("text", x = 50000, y = 6, label = "@StarTribune")  + annotate("text", x = 65000, y = 6, label = "@TechCrunch")
s + annotate("segment", x = 12000, xend = 12550, y = 15, yend = 10)
s <- s + ggtitle("Number of tweets per user followed by @AmeliaMN")
s <- s + scale_x_continuous(breaks=seq(from=0, to=150000, by=25000), minor_breaks=seq(from=-10000, to=150000, by=5000))
ggsave("plots/following.pdf", s, width=12, height=8)

s

# People who follow me
followertweets <- read.csv("data/followertweets.txt")
t <- ggplot(followertweets) + geom_histogram(aes(x=tweets)) + xlab("Total tweets") + ylab("Users")
t <- t + annotate("text", x = 112500, y = 4, label = "@znmeb") + annotate("text", x = 42500, y = 4, label = "@ibogost")
t <- t + ggtitle("Number of tweets per user following @AmeliaMN")
t <- t + scale_x_continuous(breaks=seq(from=0, to=150000, by=25000), minor_breaks=seq(from=-10000, to=150000, by=5000))

ggsave("plots/followers.pdf", t, width=12, height=8)

ggplot(followertweets) + geom_histogram(aes(x=tweets)) + xlab("total tweets") + ylab("users") + xlim(0, 5000)
t

# Boxplot
followertweets$type <- "Follower"
followtweets$type <- "Following"
allusers <- rbind(followertweets, followtweets)

u <- ggplot(allusers) + geom_boxplot(aes(type, tweets)) + xlab("") + ylab("Total tweets") 
u <- u + ggtitle("Number of tweets per user followed by or following @AmeliaMN")
u

ggsave("plots/boxplots.pdf", u, width=12, height=8)



# Grab Ramnath Vaidya's gist from github: https://gist.github.com/ramnathv/7793167
#' SparkBar Generator in R
#' 
#' Code based on SparkBlocks in Python
#' https://github.com/1stvamp/py-sparkblocks/blob/master/sparkblocks/__init__.py
#' 
#' @example
#' spark(30, 31, 32, 33)
#' "▁▃▅▇"
spark = function(...){
  numbers = c(...)
  min_value = min(numbers)
  max_value = max(numbers)
  value_scale = max_value - min_value
  nums = c()
  for (number in numbers){
    if ((number - min_value) != 0 && (value_scale != 0)){
      scaled_value = (number - min_value)/value_scale
    } else {
      scaled_value = 0
    }
    
    # Hack because 9604 and 9608 aren't vertically aligned the same as
    # other block elements
    num = floor(min(6, scaled_value * 7))
    if (num == 3){
      if ((scaled_value * 7) < 3.5){
        num = 2
      } else {
        num = 4
      }
    } else if (num == 7){
      num = 6
    }
    nums = c(nums, num)
  }
  intToUtf8(9601 + nums)
}


# Now we can make a sparkline!
res <- hist(followertweets$tweets)
spark(res$density)

