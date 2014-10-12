data.generate = function(n = 10, ext = 1){ 
     # Generate the points.
     x1 = runif(n, -ext, ext)
     x2 = runif(n, -ext, ext)
     
     # Draw a random line in the area.
     point = runif(2, -ext, ext)
     point2 = runif(2, -ext, ext)
     slope = (point2[2] - point[2]) / (point2[1] - point[1])
     intercept = point[2] - slope * point[1]
     
     #Assign the dependent values.
     y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
     
     # Return the values.
     data = data.frame(x1,x2,y)
     return(list(data = data,slope = slope, intercept = intercept))
}

pla <- function(samples=1000, nobs=10, drawPlot=T)
{
     iterCount = numeric(0)
     
     for (i in 1:samples){
          d <- data.generate(nobs)

          if (drawPlot){
               #draw the target function f
               plot(d$data$x1, d$data$x2, col=as.factor(d$data$y))
               abline(b=d$slope, a=d$intercept, col=4)
          }
          
          x <- d$data[,c(1,2)]
          w <- c(0,0,0)
          y <- d$data$y
          m <- 0; b<-0;
          
          tempy <- rep(0, nrow(x))
          hasMiss = T
          iter <- 0
          
          while (hasMiss)
          {
               iter <- iter + 1
          
               #select random unclassified point
               misPoints = d$data[d$data$y != tempy,]
               mis <- misPoints[sample(nrow(misPoints),1),]
               
               ##calculate new w
               w <- w + mis$y * c(1,mis$x1,mis$x2)
     
               ##relcaluate y
               tempy = apply(x, 1, FUN=function(e){sign(1*w[1] + e[1]*w[2] + e[2]*w[3])})
               
               hasMiss <- !all(y == tempy)


          }
          ##draw the selected hypothesis (g)
          if (drawPlot){
               m <- -1 * w[2]/w[3]
               b <- -1 * w[1]/w[3]
               abline(b=m, a=b, col="green", lwd=4)
               Sys.sleep(2)
          }
          
          iterCount[i] <- iter
          print(w)
     }     
     
     mean(iterCount)
}

