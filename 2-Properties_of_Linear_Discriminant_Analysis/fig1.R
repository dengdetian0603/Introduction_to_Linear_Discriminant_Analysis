library(MASS)
n1 <- 10000
n2 <- 10000
M <- 10
grid.size<-50

Sigma1 <- matrix(c(1,-0.5,-0.5,1),nrow=2)
x1 <- mvrnorm(n = n1,c(2,1), Sigma1)
x2 <- mvrnorm(n = n2,c(1,-1), Sigma1)
x <- rbind(x1,x2)
d <- data.frame(features=x,class = c(rep(1,times=n1),rep(2,times=n2)))
y <- d$class-1


x.1.lim <- c(min(x[,1])-sd(x[,1]),max(x[,1])+sd(x[,1]))
x.2.lim <- c(min(x[,2])-sd(x[,2]),max(x[,2])+sd(x[,2]))
x.1.seq <- seq(from=x.1.lim[1], to=x.1.lim[2], length.out=grid.size)
x.2.seq <- seq(from=x.2.lim[1], to=x.2.lim[2], length.out=grid.size)
grid <- matrix(nrow=grid.size*grid.size,ncol=2)
for(i in (1:grid.size)){
    for(j in (1:grid.size)){
        index <- (i-1)*grid.size+j
        grid[index,] <- c(x.1.seq[i],x.2.seq[j])
    }
}
test <- data.frame(features.1=grid[,1],features.2=grid[,2])
train <- sample(1:(n1+n2), size=(n1+n2)/20)

z <- lda(class ~ ., d, subset = train)
v <- predict(z,test)$posterior[,2]
grid.v <- matrix(v,nrow=grid.size,ncol=grid.size)

plot(d[train,1:2], col=c('blue','red')[d[train,"class"]],xlim=x.1.lim, ylim=x.2.lim, main="Linear dicision boundary for LDA")
contour(x=x.1.seq,y=x.2.seq,t(grid.v), levels=0.5, add=TRUE, col="green")