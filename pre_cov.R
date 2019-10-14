# Individual Case

time <- 25
y <- numeric(time)
x <- numeric(time)

count <- 0

for(i in 1:time){
  count <- count + 1
  
  if(i == 1){
    
    y[count] <- rnorm(1, mean = 0.5, sd = 0.5)
    x[count] <- 0
    
  }else{
    
    
    # y up or down with autoregression
    
    updownsame <- sample(c('up', 'down', 'same'), 1)
    
    if(updownsame == 'up'){
      
      y[count] <- y[count - 1] + 0.25
      
    }else if(updownsame == 'down'){
      
      y[count] <- y[count - 1] - 0.25
      
    }else{
      
      y[count] <- y[count - 1]
      
    }
    
    # x is a function of y
    
    if(y[count] <= -1.00){
      
      x_prob <- 0
      
    }else if(y[count] >= 1.5){
      
      x_prob <- 0.25
      
    }else{
      
      x_prob <- 0.10004*y[count] + 0.09994
      
      
    }
    
    x[count] <- rbinom(1, 1, x_prob)
    
  }
  
}

# Full Sample

people <- 600
time <- 100
df <- matrix(, nrow = people*time, ncol = 4)

count <- 0

for(j in 1:people){
  
  
  
  for(i in 1:time){
    count <- count + 1
    
    if(i == 1){
      
      df[count, 1] <- j
      df[count, 2] <- i
      df[count, 3] <- rnorm(1, mean = 0.5, sd = 0.5)
      df[count, 4] <- 0
      
    }else{
      
      df[count, 1] <- j
      df[count, 2] <- i
      
      # y up or down with autoregression
      
      updownsame <- sample(c('up', 'down', 'same'), 1)
      
      if(updownsame == 'up'){
        
        df[count, 3] <- df[count - 1, 3] + 0.25
        
      }else if(updownsame == 'down'){
        
        df[count, 3] <- df[count - 1, 3] - 0.25
        
      }else{
        
        df[count, 3] <- df[count - 1, 3]
        
      }
      
      # x is a function of y
      
      if(df[count, 3] <= -1.00){
        
        x_prob <- 0
        
      }else if(df[count, 3] >= 1.5){
        
        x_prob <- 0.25
        
      }else{
        
        x_prob <- 0.10004*df[count, 3] + 0.09994
        
      }
      
      df[count, 4] <- rbinom(1, 1, x_prob)
      
    }
    
  }
  
  
  
}

df <- data.frame(df)
names(df) <- c('id', 'time', 'happy', 'met_friend')

library(tidyverse)

happy10_sample <- df %>%
  filter(time < 11)

friend_count <- happy10_sample %>%
  as_tibble %>% 
  filter(time > 4) %>%
  group_by(id) %>%
  summarise(
    friend_count = sum(met_friend)
  )

friend_count <- friend_count %>%
  mutate(friend_event = case_when(
    friend_count == 0 ~ 0,
    friend_count != 0 ~ 1
  ))

# Merge back into y10 df

happy10_sample <- left_join(happy10_sample, friend_count)

# Filter down to what's needed for regression

happy10_filter <- happy10_sample %>%
  select(id, time, happy, friend_event) %>%
  filter(time == 1 | time == 10)

library(reshape2)

happy10_wide <- reshape(happy10_filter, idvar = 'id', timevar = 'time', direction = 'wide')

# The x columns are synonymous, so I can remove one 

happy10_wide <- happy10_wide[, c('id', 'happy.10', 'happy.1', 'friend_event.1')]
names(happy10_wide) <- c('id', 'happy_post', 'happy_pre', 'met_friend')

summary(lm(happy_post ~ happy_pre + met_friend,
           data = happy10_wide))$coefficients

#####

df_create <- function(time1){
  library(reshape2)
  library(tidyverse)
  time2 <- time1 - 5
  
  y_sample <- df %>%
    filter(time <= time1)
  
  friend_count <- y_sample %>%
    filter(time >= time2) %>%
    group_by(id) %>%
    summarise(
      friend_count = sum(met_friend)
    )
  
  friend_count <- friend_count %>%
    mutate(friend_event = case_when(
      friend_count == 0 ~ 0,
      friend_count != 0 ~ 1
    ))
  
  y_sample <- left_join(y_sample, friend_count)
  y_filter <- y_sample %>%
    select(id, time, happy, friend_event) %>%
    filter(time == 1 | time == time1)
  
  y_wide <- reshape(y_filter, idvar = 'id', timevar = 'time', direction = 'wide')
  
  yname <- paste('happy.', time1, sep = '')
  
  y_wide <- y_wide[, c('id', yname, 'happy.1', 'friend_event.1')]
  names(y_wide) <- c('id', 'happy_post', 'happy_pre', 'met_friend')
  
  summary(lm(happy_post ~ happy_pre + met_friend,
             data = y_wide))$coefficients
  
}

#####

# On y_wide, make happy_post NA for both met_friend = 1 and 0
# Impute both and see if that works?

# Impute the counterfactual for both groups
# What if they _had_ met a friend
# What if they _had not_ met a friend

y_sample <- df %>%
  filter(time <= time1)

friend_count <- y_sample %>%
  filter(time >= time2) %>%
  group_by(id) %>%
  summarise(
    friend_count = sum(met_friend)
  )

friend_count <- friend_count %>%
  mutate(friend_event = case_when(
    friend_count == 0 ~ 0,
    friend_count != 0 ~ 1
  ))

y_sample <- left_join(y_sample, friend_count)
y_filter <- y_sample %>%
  select(id, time, happy, friend_event) %>%
  filter(time == 1 | time == time1)

y_wide <- reshape(y_filter, idvar = 'id', timevar = 'time', direction = 'wide')

yname <- paste('happy.', time1, sep = '')

y_wide <- y_wide[, c('id', yname, 'happy.1', 'friend_event.1')]
names(y_wide) <- c('id', 'happy_post', 'happy_pre', 'met_friend')

y_wide_missing_0 <- y_wide
y_wide_missing_1 <- y_wide

y_wide_missing_0$happy_post[y_wide$met_friend == 0] <- NA
y_wide_missing_1$happy_post[y_wide$met_friend == 1] <- NA

#####

ini_0 <- mice(y_wide_missing_0, maxit = 0)
ini_1 <- mice(y_wide_missing_1, maxit = 0)

#####

y_wide_missing_1$happy_post[is.na(y_wide_missing_1$happy_post)] <- ini_1$imp$happy_post %>% 
  rowMeans() %>% 
  as.vector()

y_wide_missing_0$happy_post[is.na(y_wide_missing_0$happy_post)] <- ini_0$imp$happy_post %>% 
  rowMeans() %>% 
  as.vector()

summary(lm(happy_post ~ happy_pre + met_friend,
           data = y_wide))$coefficients

summary(lm(happy_post ~ happy_pre + met_friend,
           data = y_wide_missing_1))$coefficients

summary(lm(happy_post ~ happy_pre + met_friend,
           data = y_wide_missing_0))$coefficients


