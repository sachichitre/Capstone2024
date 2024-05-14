library(ggplot2)

capstone <- function(start, end, interval, runs){
  
  df <- data.frame(mean = 0, max = 0, min = 0)
  x <- start
  for(z in seq(start, end, by=interval)){
    z1 <- rep(NA, runs)
    for(i in 1:runs){
      x1 <- as.matrix(runif(x, 0, 1))
      y1 <- as.matrix(runif(x, 0, 1))
    
    
      ny1 <- norm(y1, type ="1")
      nx1 <- norm(x1, type = "1")
      z1[i] <- t(y1)%*%x1/(ny1* nx1)
    }
    x <- x + interval
    df <- rbind(df, c(mean(z1), max(z1), min(z1)))
  }
  df <- df[-1,]
  
  return(df)
}

start <- 0
end <- 750
interval <- 25
runs <- 500
capstone(start, end, interval, runs)




df2 <- capstone(start, end, interval, runs)
df2$mean<- log(df2$mean)
#df2$min <- log(df2$min)
#df2$max <- log(df2$max)
df2 <- df2[-1,]
x = seq(25, 750, by=25)
ggplot(df2, aes(x = x, y = mean)) + geom_point(col = 'red') + 
  labs(x = "Dimension", y = "Cosine Similarity", title = "Cosine Similarity after Log Transformation")
  #geom_point(data = df2, aes(x = x, y = min), col = 'blue') + 
  #geom_point(data = df2, aes(x = x, y = max), col = 'green') +
  #labs(x = "Dimension", y = "Cosine Similarity", title = "Cosine Similarity after Log Transformation")


