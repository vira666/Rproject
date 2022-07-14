library(tidyverse)
tweets <- read_csv("C:\\Users\\vira_\\Downloads\\tweet_emotions.csv")

tweets$Count <- ave(as.integer(tweets$sentiment), tweets$sentiment, FUN = length)

count_tweets <- aggregate(tweets$Count, by=list(tweets$sentiment), FUN=mean) 
colnames(count_tweets) <- c("Name of sentiment", "Count")
count_tweets <- count_tweets[order(count_tweets$Count, decreasing = TRUE),]
count_tweets$`Name of sentiment` <- factor(count_tweets$`Name of sentiment`, 
                                           levels = count_tweets$`Name of sentiment`)                             
head(count_tweets, 5)

library(ggplot2)
theme_set(theme_linedraw(base_size = 12))

library(RColorBrewer)
display.brewer.all(type="qual")
display.brewer.all(n=12)

require(RColorBrewer)
coul <- colorRampPalette( brewer.pal(12, "Set2") )(13)

plot <- ggplot(count_tweets, aes(x=`Name of sentiment`, y=Count)) + 
  geom_bar(stat="identity", width=.6, fill=coul) + 
  labs(title="Distribution of classes", 
       caption="source: tweets") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
plot + theme(axis.title.x = element_text(colour = "plum4"),
          axis.title.y = element_text(colour = "olivedrab4"))

sink("C:\\Users\\vira_\\Downloads\\output_statistics.txt", split=TRUE)

# описательные статистики  
table1 <- table(tweets$sentiment)
sort(table1)
sort(prop.table(table1))
sink()
