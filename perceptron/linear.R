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

data.generate.out = function(n = 1000, ext = 1, slope, intercept){ 
     # Generate the points.
     x1 = runif(n, -ext, ext)
     x2 = runif(n, -ext, ext)

     #Assign the dependent values.
     y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
     
     # Return the values.
     return(data.frame(x1,x2,y))
}


lin <- function(samples=1000, nobs=10, drawPlot=T, calcPla=F)
{
     errs <- numeric(0)
     eout <- numeric(0)
     plaIter <- numeric(0)
     
     for (i in 1:samples){
          d <- data.generate(nobs)

          if (drawPlot){
               #draw the target function f
               plot(d$data$x1, d$data$x2, col=as.factor(d$data$y))
               abline(b=d$slope, a=d$intercept, col=4)
          }
          
          x <- cbind(1,d$data[,c(1,2)])
          y <- d$data$y
          m <- 0; b<-0;
          
          w <- pinv(as.matrix(x)) %*% as.matrix(d$data$y)
          
          ##relcaluate y
          tempy = apply(x, 1, FUN=function(e){sign(1*w[1,1] + e[2]*w[2,1] + e[3]*w[3,1])})
          errs[i] <- sum(tempy != y)/nobs

          ##draw the selected hypothesis (g)
          if (drawPlot){
               m <- -1 * w[2,1]/w[3,1]
               b <- -1 * w[1,1]/w[3,1]
               abline(b=m, a=b, col="green", lwd=4)
               Sys.sleep(.5)
          }

          print(paste("ein:",errs[i]))
          
          if (calcPla)
          {
               plaIter[i] <-  pla(d, w, drawPlot)               
          }
          else
          {
               #generate 1000 more points
               dout <- data.generate.out(n = 1000, slope=d$slope, intercept=d$intercept)
               calcy <- apply(dout, 1, FUN=function(e){sign(1*w[1,1] + e[1]*w[2,1] + e[2]*w[3,1])})
               eout[i] <- sum(calcy != dout$y)/1000
               print(paste("eout:",eout[i]))
          }
     
     }
     
     print(paste("mean ein:",mean(errs)))
     print(paste("mean eout:", mean(eout)))
     print(paste("mean pla iter:", mean(plaIter)))
}

pla <- function(d, w, drawPlot=T)
{
     x <- d$data[,c(1,2)]
     y <- d$data$y

     tempy = apply(x, 1, FUN=function(e){sign(1*w[1] + e[1]*w[2] + e[2]*w[3])})
     hasMiss <- !all(y == tempy)
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
          abline(b=m, a=b, col="red", lwd=3)
          Sys.sleep(2)
     }
     
     iter
}
