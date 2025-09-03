
linfoot_features <- function(data){
  
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

names(image) <- paste0("V", seq(1:2500))

inputrow <- c(stats, image)
return(inputrow)
}


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

# bootstrap from a dataset
library(MASS)
require(tidyverse)
library(mets) # contains twin data on bmi

data(twinbmi)

data_original <- twinbmi %>% 
  pivot_wider(names_from = num, values_from = bmi) %>% 
  mutate(zyg = factor(zyg, levels = c("MZ", "DZ"))) %>% 
  filter(!is.na(`1`) & !is.na(`2`)) %>% 
  dplyr::select(`1`: `2`, zyg)

mz_original <- data_original %>% 
  filter(zyg == "MZ")
dz_original <- data_original %>% 
  filter(zyg == "DZ")

npairs_mz = nrow(mz_original)
npairs_dz = nrow(dz_original)
n_samples = 1000
inputfile = matrix(NA, n_samples, (56+2500))

# For MZ twins
for (i in 1:n_samples){
  data <- mz_original %>% 
    slice_sample(n = npairs_mz, replace = T)
  inputfile[i, ] <- linfoot_features(data) 
}
colnames(inputfile) <- names(linfoot_features(data))
write_csv(inputfile %>% data.frame(), file = "input.csv")
out <- py_run_file("linfoot.py")
predictions <- out$predictions

hist(predictions)
quantile(predictions, c(0.025, 0.975))
# > quantile(predictions, c(0.025, 0.975))
# 2.5%     97.5% 
#   0.7026978 0.7855175 

# For DZ twins
for (i in 1:n_samples){
  data <- dz_original %>% 
    slice_sample(n = npairs_dz, replace = T)
  inputfile[i, ] <- linfoot_features(data) 
}
colnames(inputfile) <- names(linfoot_features(data))
write_csv(inputfile %>% data.frame(), file = "input.csv")
out <- py_run_file("linfoot.py")
predictions <- out$predictions

hist(predictions)
quantile(predictions, c(0.025, 0.975))
# > quantile(predictions, c(0.025, 0.975))
# 2.5%     97.5% 
#   0.4572802 0.5432711 

