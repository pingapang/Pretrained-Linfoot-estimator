## A set of functions needed to estimate the linfoot informational correlation on a data set

linfoot <- function(data){
  
require(tidyverse)
require(reticulate)
require(tensorflow)
require(keras)


# compute statistics
stats <- makFeatures(data = data)
stats <- c(stats, fnn = FNN(data, k = round(nrow(data)^(3/5))))


# compute image data
image <- makImage(data) 
image <- as.vector(image) 

# put data in a dataframe
image <- matrix(image, 1, 2500) %>% as.data.frame() %>% tibble()
s <- matrix(stats, 1, 56) %>% as.data.frame() %>% tibble()
colnames(s) <- names(stats)
inputfile <- bind_cols(s, image)

# write data to a file so that python script can read it in
write_csv(inputfile, "input.csv")

# make predictions based on pretrained CNN model
py_run_file("linfoot.py")


}

# function that computes FNN estimator
FNN <- function(data, k){
  if(missing(data)){ stop("data needs to be specified") }
  if(missing(k)){ stop("smoothing parameter needs to be specified") }
  data <- data.frame(data)
  x <- data[,1]
  y <- data[,2]
  I <- mutinfo(x, y, k=k, direct=TRUE)
  # Make sure that I is between 0 and Inf
  if(!is.na(I)){
    if(I < 0){ I <- 0 }
    else { I <- I }
  }
  L <- sqrt(1-exp(-2*I))     # Linfoot correlation
  return(L)
}

# compute sample statistics (handcrafted features)
makFeatures <- function(nbin = 50, data){
  require(FNN)
  require(e1071)  # to compute skewness
  data <- data.frame(data)
  x = data[,1]
  y = data[,2]
  pearson = cor(x, y)
  xx <- (x-min(x)) /(max(x)-min(x))
  yy <- (y-min(y)) /(max(y)-min(y))
  x3 <- cut(xx, 
            breaks = c(seq(0, 1, l = round(nbin/3 + 1))), 
            include.lowest = T ,
            labels = FALSE)
  d <- data.frame(x = x3,y = yy)
  features <- d %>% 
    group_by(x) %>% 
    summarise(mean = mean(y), 
              var = var(y),
              skewness = skewness(y), 
              n = n()) %>% 
    mutate(var = ifelse(n > 2, var, 0), 
           skewness = ifelse(n > 2, skewness, 0))
  leftout <- setdiff(1:round(nbin/3 + 1), features$x)
  features <- bind_rows(
    tibble(x = leftout, mean = .5, var = 0, skewness = 0, n = 0L), 
    features)
  features <- features %>% 
    pivot_longer(cols = -x, names_to = "feature", values_to = "value") %>% 
    arrange(x) %>% 
    dplyr::filter(feature != "n") # dont include n as feature
  features <- features %>% 
    unite(col = "name", c("feature", "x"), sep = "_")
  features = bind_rows(features, data.frame(name = "pearson", value = pearson))
  names = features$name
  features = features$value
  names(features) <- names
  featurenames <<- names
  return(features)
}

# compute the scatterplot data
makImage <- function(data, nbin = 50, enlargePoints = FALSE) {
  data <- data.frame(data)
  x = data[,1]
  y = data[,2]
  #standardization to 0-1 range
  xx <- (x-min(x)) /(max(x)-min(x))
  yy <- (y-min(y)) /(max(y)-min(y))
  x3 <- cut(xx, 
            breaks = c(seq(0, 1, l = (nbin + 1))), 
            include.lowest = T ,
            labels = FALSE)
  y3 <- cut(yy, 
            breaks = c(seq(0, 1, l = (nbin + 1))), 
            include.lowest = T, 
            labels = FALSE)
  d <- data.frame(x = x3,y = y3)
  # from formula notation for flat contingency tables:
  # "The left and right hand side of formula specify the column and row variables, respectively, of the flat contingency table to be created. "
  dd <- as.data.frame(ftable(y ~ x, data = d))
  ## this goes wrong in original script: x and y are factors and the factor labels do not
  # match the factor level numbers
  # levels(dd$y)
  dd <- mutate(dd, x = as.character(x), y = as.character(y))
  dd <- mutate(dd, x = as.numeric(x), y = as.numeric(y))
  #standardisation
  dd$Freq<- dd$Freq/sum(dd$Freq)
  u <- dd %>% pivot_wider(names_from = y, values_from = Freq) %>% 
    column_to_rownames("x") %>% as.matrix()
  u <- matrix(0, nbin, nbin )
  for (ii in 1:nrow(dd)) {
    u[dd$x[ii], dd$y[ii]] <- dd$Freq[ii]
  }
  return(u)
}

# # simulate a dataset
# library(MASS)
# library(reticulate)
# require(tidyverse)
# correlation <- 0.4
# data <- mvrnorm(100, mu = c(0, 0), 
#                 Sigma = matrix(c(1, correlation, correlation, 1), 2, 2))
# 
# 
# # specify the environment you want to use, should have tensorflow etc installed
# use_condaenv("/opt/miniconda3/envs/tensorflow")
# 
# # compute linfoot estimate
# linfoot(data)

