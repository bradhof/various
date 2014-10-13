sim <- function(){
     min = numeric(0)
     crand = numeric(0)
     cfirst = numeric(0)
     
     for (i in 1:100000)
     {
          #flip 1000 coins 10 times each
          coins <- data.frame(matrix(rbinom(10000,1, .5), 1000,10))
          
          #get the rowmeans of each 
          coins$mn <- rowMeans(coins)
     
          cfirst[i] <- coins[1,]$mn
          cmin[i] <- min(coins$mn)
          crand[i] <- sample(coins$mn,1)
     }
     
     print(paste("c1:",mean(cfirst)))
     print(paste("crand:",mean(crand)))
     print(paste("cmin:",mean(cmin)))
}
