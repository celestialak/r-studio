makeCacheVector <- function(x = numeric()) {
  mean <- NULL  # initialize the mean to NULL
  
  # Function to set the value of the vector
  set <- function(y) {
    x <<- y
    mean <<- NULL  # reset the mean since the vector has changed
  }
  
  # Function to get the value of the vector
  get <- function() {
    x
  }
  
  # Function to set the cached mean
  setMean <- function(meanValue) {
    mean <<- meanValue
  }
  
  # Function to get the cached mean
  getMean <- function() {
    mean
  }
  
  # Return a list of the functions
  list(set = set, get = get, setMean = setMean, getMean = getMean)
}

cacheMean <- function(x, ...) {
  # Get the cached mean if it exists
  mean <- x$getMean()
  
  # If mean is not NULL, return the cached mean
  if (!is.null(mean)) {
    message("Getting cached mean")
    return(mean)
  }
  
  # Otherwise, compute the mean
  data <- x$get()
  mean <- mean(data, ...)
  
  # Cache the computed mean
  x$setMean(mean)
  
  # Return the mean
  mean
}



# Create a special vector
specialVector <- makeCacheVector(c(1, 2, 3, 4, 5))

# Compute the mean and cache it
meanValue <- cacheMean(specialVector)
print(meanValue)  # Outputs: 3

# Retrieve the cached mean without recomputing
cachedMeanValue <- cacheMean(specialVector)
print(cachedMeanValue)  # Outputs: 3 (with message "Getting cached mean")
