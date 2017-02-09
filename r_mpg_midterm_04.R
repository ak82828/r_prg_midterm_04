#Create a function: my.sort.asc()
my.sort.asc <- function(input_vec){
  for(i in 1:(length(input_vec) - 1) ){
    for(j in (i + 1):length(input_vec) ){
      if(input_vec[i] > input_vec[j]){
        temp_i <-  input_vec[i]
        input_vec[i] <-  input_vec[j]
        input_vec[j] <-  temp_i
      }
    }
  }
  return(input_vec)
}

#Create input
set.seed(87)    #©T©wÀH¾÷¼Æ¦r
my_seq <- round(runif(10) * 100)

#Function all
my.sort.asc(my_seq)
sort(my_seq)



#Create a function¡Gmy.sd()
my.sd <- function(input_vec){
  x_bar <-mean(input_vec)
  n_minus_one <- length(input_vec) - 1
  summation <-  0
  for(x_i in input_vec){
    summation <- summation + (x_i - x_bar)^2
  }
  return(sqrt(summation))
}

#Create an input
set.seed(87)
my_seq <-  round(runif(10) * 100)

#Function all
my.sd(my_seq)
sd(my_seq)


#Create a function¡GBMI calculator()
bmi.calculator <- function(w,h){
  h <-  h / 100
  return(w / h^2)
}

#create in put
heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)

#Function call
bmi_data <- mapply(FUN = bmi.calculator, w = weights, h = heights)
bmi_data <- cbind(heights_and_weights,bmi_data)
View(bmi_data)