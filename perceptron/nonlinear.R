non.data.generate = function(n = 10, ext = 1){ 
     # Generate the points.
     x1 = runif(n * .9, -ext, ext)
     x2 = runif(n * .9, -ext, ext)
     
     y = sign(x1^2 + x2^2 - .6)
     
     #generate noise 10%
     noise1 = runif(n * .1, -ext, ext)
     noise2 = runif(n * .1, -ext, ext)
     
     noisey = -1 * sign(noise1^2 + noise2^2 - .6)
     
     rbind(data.frame(x1=x1,x2=x2,y=y), data.frame(x1=noise1,x2=noise2, y=noisey))
     
}

non.data.generate.out = function(n = 1000, ext = 1, slope, intercept){ 
     # Generate the points.
     x1 = runif(n, -ext, ext)
     x2 = runif(n, -ext, ext)

     #Assign the dependent values.
     y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
     
     # Return the values.
     return(data.frame(x1,x2,y))
}


nonlin <- function(samples=1000, nobs=10, drawPlot=T, calcPla=F)
{
     errs <- numeric(0)
     eout <- numeric(0)

     for (i in 1:samples){
          d <- non.data.generate(nobs)

          if (drawPlot){
               #draw the target function f
               plot(d$x1, d$x2, col=as.factor(d$y))
          }
          
          x <- cbind(1,d[,c(1,2)])
          y <- d$y
          m <- 0; b<-0;
          
          w <- pinv(as.matrix(x)) %*% as.matrix(y)
          
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
     }

     
     print(paste("ein mean:",mean(errs)))
}

nonlin2 <- function(samples=1000, nobs=10, drawPlot=T, calcPla=F)
{
     errs <- numeric(0)
     eout <- numeric(0)
     x1w <- numeric(0)
     x2w <- numeric(0)
     x3w <- numeric(0)
     x4w <- numeric(0)
     x5w <- numeric(0)
     x6w <- numeric(0)
     
     for (i in 1:samples){
          d <- non.data.generate(nobs)
          
          if (drawPlot){
               #draw the target function f
               plot(d$x1, d$x2, col=as.factor(d$y))
          }
          
          x <- cbind(1, e$x1, e$x2, e$x1*e$x2, e$x1^2, e$x2^2)
          y <- d$y
          m <- 0; b<-0;
          
          w <- pinv(as.matrix(x)) %*% as.matrix(y)

          x1w[i] <- w[1,1]
          x2w[i] <- w[2,1]
          x3w[i] <- w[3,1]
          x4w[i] <- w[4,1]
          x5w[i] <- w[5,1]
          x6w[i] <- w[6,1]
          
          
          ##relcaluate y
          tempy = apply(x, 1, 
                        FUN=function(e){
                             sign(1*w[1,1] + 
                                   e[2]*w[2,1] + 
                                   e[3]*w[3,1] +
                                   e[4]*w[4,1] +
                                   e[5]*w[5,1] +
                                   e[6]*w[6,1])
                             })
          errs[i] <- sum(tempy != y)/nobs
          
          ##draw the selected hypothesis (g)
          if (drawPlot){
               m <- -1 * w[2,1]/w[3,1]
               b <- -1 * w[1,1]/w[3,1]
               abline(b=m, a=b, col="green", lwd=4)
               Sys.sleep(.5)
          }
          
          print(paste("ein:",errs[i]))
     }
     
     print(c(mean(x1w), mean(x2w), mean(x3w), mean(x4w), mean(x5w), mean(x6w)))
     
     print(paste("ein mean:",mean(errs)))
}


nonlin3 <- function(samples=1000, nobs=10)
{
     x1w <- numeric(0)
     x2w <- numeric(0)
     x3w <- numeric(0)
     x4w <- numeric(0)
     x5w <- numeric(0)
     x6w <- numeric(0)
     
     for (i in 1:samples){
          d <- non.data.generate(nobs)
          x <- data.frame(x1=d$x1, x2=d$x2, x3=d$x1*d$x2, x4=d$x1^2, x5=d$x2^2, y=d$y)
          
          fit <- lm(y~x1+x2+x3+x4+x5, data=x)

          x1w[i] <- summary(fit)$coef[1,1]
          x2w[i] <- summary(fit)$coef[2,1]
          x3w[i] <- summary(fit)$coef[3,1]
          x4w[i] <- summary(fit)$coef[4,1]
          x5w[i] <- summary(fit)$coef[5,1]
          x6w[i] <- summary(fit)$coef[6,1]
          
          print(summary(fit))
          
     }
     print(c(mean(x1w), mean(x2w), mean(x3w), mean(x4w), mean(x5w), mean(x6w)))
}

calcError <- function(nobs=1000, sample=10000)
{
     err <- numeric(0)
     
     for (i in 1:sample){
          #generate 1000 points
          d <- non.data.generate(nobs)
          y <- d$y
          calcy <- sign(-1 - .05*d$x1
                        +.08*d$x2 
                        + 0.13*d$x1*d$x2 
                        + 1.5*d$x1^2
                        + 1.5*d$x2^2
          )
          
          err[i] = sum(calcy != y)/nobs
     }
     
     mean(err)
     
     
     
}
