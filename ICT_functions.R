######################################################Functions for ICT data pre-processing######################################################

#Transformation of x-y coordinates for ICT data
transform_xy <- function(min_x, max_x, min_y, max_y, x, y, data)
{newx <- (x - min_x)/(max_x-min_x) * 2
newy <- (y - min_y)/(max_y-min_y) * 1.5
newdata <- as.data.frame(cbind(data, newx, newy))
newx <- as.numeric(newdata$newx)
newy <- as.numeric(newdata$newy)
return(newdata)
}

#Function to get previous x-y coordinates and calculate movements at specific time points
movements <- function(x, y, t, data) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  prevX <- lag(x,1)
  nextX <- lead(x,1)
  prevY <- lag(y,1)
  nextY <- lead(y,1)
  movedX <- abs(x - prevX)
  movedY <- abs(y - prevY)
  movedXY = sqrt((prevX - x)^2 + (prevY - y)^2)
  rt_sample1 <- lag(t, 2)
  rt_sample2 <- lag(t, 11)
  coords <- cbind(prevX, nextX, prevY, nextY, movedX, movedY, movedXY, rt_sample1, rt_sample2)
  coords <- as.data.frame(coords)
  data <- cbind(data, coords)
  return(data)
}
#Function for removing duplicate items 
rm_d <- function(data) {
  x <- unique(data[duplicated(data$eval_jpg), ])
  r <- as.numeric(rownames(x))
  data <- subset(data, !(rownames(data) %in% r))
  return(data)
}

#Transformation of x coordinates for EET data
transform_x <- function(min, max, x, data)
{newx <- (x - min)/(max-min)
newx <- newx * 100
newdata <- as.data.frame(cbind(data, newx))
newx <- as.numeric(newdata$newx)
return(newdata)
}

#Function for obtaining mean ratings from EET data
eval <- function (x) {
  c(X.S.o= mean(x$xcoord[x$selection==1 & x$novelty==1]), 
    X.S.n= mean(x$xcoord[x$selection==1 & x$novelty==2]),
    X.N.o= mean(x$xcoord[x$selection==2 & x$novelty==1]),
    X.N.n= mean(x$xcoord[x$selection==2 & x$novelty==2]),
    X.S= mean(x$xcoord[x$selection==1]),
    X.N= mean(x$xcoord[x$selection==2]),
    X.o= mean(x$xcoord[x$novelty==1]),
    X.n= mean(x$xcoord[x$novelty==2]),
    X= mean(x$xcoord),
    Group = mean(x$group))}

#Function for obtaining mean GoRTs from go test blocks

go_RTs <- function(x) {
  c(GoRT.U.Pre = as.numeric(mean(x$GoRT[x$block=="go_test" & x$target_healthiness==2])),
    GoRT.U.Post = as.numeric(mean(x$GoRT[x$block=="go_test_end" & x$target_healthiness==2])),
    GoRT.H.Pre = as.numeric(mean(x$GoRT[x$block=="go_test" & x$target_healthiness==1])),
    GoRT.H.Post = as.numeric(mean(x$GoRT[x$block=="go_test_end" & x$target_healthiness==1])),

    GoRT.SU.Pre = as.numeric(mean(x$GoRT[x$block=="go_test" & x$target_healthiness==2 & x$selection==1])),
    GoRT.SU.Post = as.numeric(mean(x$GoRT[x$block=="go_test_end" & x$target_healthiness==2 & x$selection==1])),
    GoRT.NU.Pre = as.numeric(mean(x$GoRT[x$block=="go_test" & x$target_healthiness==2 & x$selection==2])),
    GoRT.NU.Post = as.numeric(mean(x$GoRT[x$block=="go_test_end" & x$target_healthiness==2 & x$selection==2])),
    Group = head(x$group, n=1)
  )
}
