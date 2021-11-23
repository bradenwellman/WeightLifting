# Packages that we always need
library(tidyverse)

# Load in the data and make sure it is working properly
lifting <- read.csv('Lifting.csv', header = TRUE)
head(lifting)

# ---------------------------------------------------------- Data Cleaning -----------------------------------------------------------------

# Fix our Days
table(lifting$Muscle)

# Loop through dataset
for (i in 1:nrow(lifting)) {
  
  # If the muscle equals Bicep
  if (lifting$Muscle[i] == 'Bicep') {
    
    # Change it to "Biceps
    lifting$Muscle[i] <- 'Biceps'
  }
}

# Check to make sure it worked (Yay)
table(lifting$Muscle)

# -------------------------------------------------------- Feature Engineering ----------------------------------------------------------------

# Create day column and a temporary column to keep track
lifting$Day <- 0
lifting$temp <- ''

# Do the first row
lifting$Day[1] <- 1
lifting$temp[1] <- 'push'

# First for Day (Loop through the dataset)
for (i in 1:nrow(lifting)) {
  
  # If it is the last row, break out of the loop
  if (i == nrow(lifting)) {
    break
  }
  
  # Determine if it is push day
  if (lifting$Muscle[i + 1] == "Chest" | lifting$Muscle[i + 1] == "Triceps") {
    lifting$temp[i + 1] <- 'push'
  }
  
  # Determine if it is pull day
  if (lifting$Muscle[i + 1] == "Biceps" | lifting$Muscle[i + 1] == "Back") {
    lifting$temp[i + 1] <- 'pull'
  }
  
  # Determine if it is leg day
  if (lifting$Muscle[i + 1] == "Legs" | lifting$Muscle[i + 1] == "Shoulders") {
    lifting$temp[i + 1] <- 'legs'
  }
  
  # If the muscle group is not the same as the day before, augment the day
  if (lifting$temp[i + 1] != lifting$temp[i]) {
    lifting$Day[i + 1] <- lifting$Day[i] + 1
    
    # Else the day stays the same
  } else {
    lifting$Day[i + 1] <- lifting$Day[i]
  }
}

# Remove the temporary column
lifting %>%
  select(-temp) -> lifting

# Make sure it worked
lifting %>%
  select(Muscle, Day) %>%
  head(n = 244)

# Let's do the same thing for Set
length(unique(lifting$Exercise))

# Do the first row
lifting$Set <- 0
lifting$Set[1] <- 1
head(lifting)

# Loop through the entire dataset
for (i in 1:nrow(lifting)) {
  
  # If it's the last row break
  if (i == nrow(lifting)) {
    break
  }
  
  # If the next value is same as the current value, augment the set
  if (lifting$Exercise[i + 1] == lifting$Exercise[i]) {
    lifting$Set[i + 1] <- lifting$Set[i] + 1
  }
  
  # Otherwise it is a new set
  else {
    lifting$Set[i + 1] <- 1
  }
}

# Check it worked and how my sets usually go
head(lifting, n = 25)
table(lifting$Set)

# Finally let's do it for different exercise same muscle 
# Logic is as follows if (same day & same Muscle & new exercise, lift = new)

# Create the new "newLift" column
lifting$NewLift <- 1

# Loop through the dataset
for (i in 1:nrow(lifting)) {
  
  # Last row of the dataset break
  if(i == nrow(lifting)) {
    break
  }
  
  
  
  ###### SAME DAY --------------------------------------------------
  
  
  # If the day is the same
  if(lifting$Day[i + 1] == lifting$Day[i]) {
    
    # Do 2 for whether or not muscle is the same (i.e control for other)
    if(lifting$Muscle[i + 1] == lifting$Muscle[i]) {
      
      # If the exercise is not the same
      if(lifting$Exercise[i + 1] != lifting$Muscle) {
        
        # Augment the New Lift category
        lifting$Newlift[i + 1] <- lifting$Muscle[i] + 1
      }
      
      
    }
    
    # If it is a *different Muscle*
    else {
      
      use modulo
      
      
    }
    
    
    
  }
  
# Different Day
  
  else {
    lifting$NewLift[i] <- 1
  }
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------
