library(tidyverse)
library(gganimate)
library(gifski)
library(keras)


gen_data <- function(week){
  week1 <- read_csv(paste0("tracking_week_",week,".csv"))
  
  
  plays <- read_csv("plays.csv")
  
  tackles <- read_csv('tackles.csv')
  

  play_title <- plays
  
  
  tackle_title <- tackles %>% 
    
    mutate(t_val = case_when(
      tackle == 1 ~ 1,
      assist == 1 ~ 0.5,
      T ~ 0
    )) %>%
    mutate(event = 'tackle') %>%
    dplyr::select(gameId,playId,nflId,event,t_val)

  
  football <- week1 %>%
    group_by(gameId,playId) %>%
    filter(displayName == 'football') %>%
    dplyr::select(frameId,x,y,s,a)
  
  poss_tm <- plays %>%
    dplyr::select(gameId,playId,possessionTeam)
  
  df <- week1 %>%
    filter(displayName != 'football') %>%
    left_join(football, by = c('gameId','playId','frameId'),suffix = c('_p','_f')) %>%
    left_join(poss_tm, by = c('gameId','playId')) %>%
    mutate(poss_tm = ifelse(club == possessionTeam,1,0)) %>%
    left_join(tackle_title, by = c('gameId','playId','nflId','event')) %>%
    mutate(t_val = ifelse(is.na(t_val),0,t_val)) %>%
    rowwise() %>%
    mutate(ball_dist = sqrt((x_f-x_p)^2 + (y_f-y_p)^2)) %>%
    mutate(playId = paste(gameId,playId,sep='_')) %>%
    dplyr::select(playId,nflId,frameId,s_p,a_p,s_f,a_f,ball_dist,poss_tm,t_val)
  df
}


week1 <- read_csv("tracking_week_1.csv")


plays <- read_csv("plays.csv")

tackles <- read_csv('tackles.csv')

playswk1 <- plays %>%
  filter(gameId %in% week1$gameId)

tackleswk1 <- tackles%>%
  filter(gameId %in% week1$gameId)

games <- unique(week1$gameId)


game_num <- 2022091103

play <- unique(filter(week1,gameId == game_num)$playId)

play_int <- 2462

play_num <- play_int
angle_adjust <- function(angle) {
  ((abs(360-angle)+90) %% 360)/(360/(2*pi))
}

# 2018091605 tyreek td gameid
# 3549 tyreek td playid


# qb_data <- week1 %>%
#   #filter(position == "QB") %>%
#   select(x,y,o,frameId,gameId,playId)

length(unique(week1$playId))

test1 <- week1 %>%
  #filter(gameId == game_num) %>%
  #filter(playId == play_num) %>%
  rowwise() %>%
  mutate(o_rad = angle_adjust(o)) %>%
  mutate(dir_rad = angle_adjust(dir)) #%>%
# mutate(qb_left = case_when(
#   position == "QB" ~ angle_adjust(o)+15/(360/(2*pi)),
#   T ~ NA_real_
# )) %>%
# mutate(qb_right =case_when(
#   position == "QB" ~ angle_adjust(o)-15/(360/(2*pi)),
#   T ~ NA_real_
# )) %>%
# left_join(qb_data, by = c("gameId","playId","frameId"),suffix = c("",".QB")) %>%
# mutate(dist = sqrt((`x.QB` - x)^2 + (`y.QB` - y) ^2)) %>%
# mutate(angle = case_when(
#   playDirection == "left" ~ atan2((y-`y.QB`),abs(`x.QB` - x)),
#   T ~ atan2((y-`y.QB`),abs(x - `x.QB`))
# )
# ) %>%
# mutate(angle_adjusted = case_when(
#   angle < 0 ~ (2*pi) + angle,
#   T ~ angle
# ))
#mutate(angle_adjusted_plot = -)
# mutate(o_qb_rad = case_when(
#   playDirection == "left" ~ pi - angle_adjust(o.QB),
#   T ~ angle_adjust(o.QB)
# )) %>%
#mutate(o_qb_rad = angle_adjust(o.QB)) %>%
# mutate(angle_diff = case_when(
#   playDirection == "left" ~ ( o_qb_rad-(2*pi-angle)) %% 2*pi,
#   T ~ (o_qb_rad - angle) %% 2*pi)) %>%
# mutate(angle_diff2 = case_when(
#   angle_diff > pi ~ 2*pi - angle_diff,
#   T ~ angle_diff
# )) %>%
# mutate(angle_diff2 = case_when(
#   playDirection == "left" ~ pi - angle_diff2,
#   T ~ angle_diff2
# ))

# ggplot(filter(test1,position=="QB"), aes(x=frameId, y = o_rad )) +
#   geom_point() +
#   geom_path(data = filter(test1,position=="WR"), aes(color = displayName, y = angle_diff2)) +
#   geom_vline(data = filter(test2, event == "pass_forward"), aes(xintercept = frameId)) 
# test2 <- test1 %>%
#   mutate(funval = angle_diff2) %>% #dgamma(dist,shape,rate) * (angle_diff)) %>%
#   group_by(nflId) %>%
#   arrange(frameId) %>%
#   mutate(aggval = (funval + .8*lag(funval,n=5) + .6*lag(funval,n=10)+.4*lag(funval,n=15) + .2*(lag(funval,n=20))))

play_title <- plays# %>% 
  #filter(gameId == unique(test1$gameId)) %>%
  #filter(playId == unique(test1$playId))

tackle_title <- tackles %>% 
  #filter(gameId == unique(test1$gameId)) %>%
  #filter(playId == unique(test1$playId)) %>%
  mutate(t_val = case_when(
    tackle == 1 ~ 1,
    assist == 1 ~ 0.5,
    T ~ 0
  )) %>%
  mutate(event = 'tackle') %>%
  dplyr::select(gameId,playId,nflId,event,t_val)

# test2 <- test1 %>%
#   filter(nflId %in% c(play_title$ballCarrierId,tackle_title$nflId))



football <- test1 %>%
  group_by(gameId,playId) %>%
  filter(displayName == 'football') %>%
  dplyr::select(frameId,x,y,s,a)

poss_tm <- plays %>%
  dplyr::select(gameId,playId,possessionTeam)

df <- test1 %>%
  filter(displayName != 'football') %>%
  left_join(football, by = c('gameId','playId','frameId'),suffix = c('_p','_f')) %>%
  left_join(poss_tm, by = c('gameId','playId')) %>%
  mutate(poss_tm = ifelse(club == possessionTeam,1,0)) %>%
  left_join(tackle_title, by = c('gameId','playId','nflId','event')) %>%
  mutate(t_val = ifelse(is.na(t_val),0,t_val)) %>%
  rowwise() %>%
  mutate(ball_dist = sqrt((x_f-x_p)^2 + (y_f-y_p)^2)) %>%
  mutate(playId = paste(gameId,playId,sep='_')) %>%
  #filter(nflId %in% c(play_title$ballCarrierId,tackle_title$nflId)) %>%
  dplyr::select(playId,nflId,frameId,s_p,a_p,s_f,a_f,ball_dist,poss_tm,t_val)


df_examine <- df %>%
  group_by(playId,frameId) %>%
  summarise(num = n())



# 3. Generating tensors with padding
generate_tensors <- function(data, max_timesteps) {
  unique_plays <- length(unique(data$playId))
  unique_nflIds <- 22 # as specified
  features <- 6
  
  # Initialize 4D tensor for predictors with zeros
  predictors_4d <- array(0, dim = c(unique_plays, max_timesteps, unique_nflIds, features))

  # Initialize 3D tensor for t_val with zeros
  t_val_3d <- array(0, dim = c(unique_plays, max_timesteps, unique_nflIds))
  
  play_idx <- 0
  for (play in unique(data$playId)) {

    play_idx <- play_idx + 1
    play_data <- data[data$playId == play, ]
    #print(play_data)
    #print(unique(play_data$nflId))
    player_field <- 1
    for (nfl in unique(play_data$nflId)) { # assuming nflId is a sequence from 1 to 22
      #print(nfl)
      nfl_data <- play_data[play_data$nflId == nfl, ]
      
      #print(nfl_data)
      # Add this nfl's data into the predictor tensor
      predictors_4d[play_idx, 1:nrow(nfl_data), player_field, ] <- as.matrix(nfl_data[, 4:9])
      
      # Add t_val into the target tensor
      t_val_3d[play_idx, 1:nrow(nfl_data), player_field] <- nfl_data$t_val
      
      player_field <- player_field + 1
    }
  }
  
  return(list(predictors = predictors_4d, target = t_val_3d))
}

max_timesteps <- max(df$frameId)
max_timesteps <- 150
train_tensors <- generate_tensors(df, max_timesteps)

dim(train_tensors$predictors)

summary(train_tensors$target[,,])


df1 <- gen_data('2')

max(df1$frameId)
valid_tensors <- generate_tensors(df1, max_timesteps)

# Now you can use:
# train_tensors$predictors for training predictors
# train_tensors$target for training targets
# And similar for validation/test sets

# ... [Model building, training, and prediction remains the same]

# Function to build the model
build_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    # Flattening nflId and features dimensions
    layer_reshape(target_shape = c(input_shape[2], input_shape[3] * input_shape[4]),
                  input_shape = input_shape[2:4]) %>%
    layer_lstm(units = 50, return_sequences = TRUE) %>%
    layer_lstm(units = 22, return_sequences = TRUE) 
  
  
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = "mse"
  )
  
  return(model)
}

# Function to train the model
train_model <- function(model, train_tensors, valid_tensors, epochs = 10) {
  history <- model %>% fit(
    x = train_tensors$predictors,
    y = train_tensors$target,
    validation_data = list(valid_tensors$predictors, valid_tensors$target),
    epochs = epochs,
    batch_size = 32
  )
  return(history)
}

# Function to evaluate the model
evaluate_model <- function(model, valid_tensors) {
  mse <- model %>% evaluate(valid_tensors$predictors, valid_tensors$target)
  return(mse)
}

# Actual workflow
dim(valid_tensors$predictors)
input_shape <- dim(train_tensors$predictors)
model <- build_model(input_shape)
history <- train_model(model, train_tensors, valid_tensors, epochs = 10)
mse <- evaluate_model(model, valid_tensors)

print(paste("Validation MSE:", mse))




# Function to make predictions
predict_on_validation <- function(model, valid_tensors) {
  predicted_values <- model %>% predict(valid_tensors$predictors)
  return(predicted_values)
}

predicted_t_val <- predict_on_validation(model, valid_tensors)

# Reshape the predicted values to match original dataframe structure
predicted_t_val <- array_reshape(predicted_t_val, dim = c(nrow(df1) * max_timesteps * 22,1))
dim(predicted_t_val)
# Convert it to a dataframe for easier handling and further analysis
predicted_df <- as.data.frame(predicted_t_val)


length(unique(df1$playId)) * 22 * 150


4666200/1311904
ggplot(players) +
  geom_path(aes(x = frameId, y = ball_dist, color = as.factor(nflId), group = nflId))

p1 <- ggplot(test2, aes(x=x, y = y,color = club)) +
  geom_rect(aes(xmin=0,xmax=120,ymin=53.3,ymax=Inf),fill= "white",color="transparent") +
  geom_rect(aes(xmin=0,xmax=120,ymax=0,ymin=-Inf),fill= "white",color = "transparent") +
  geom_rect(aes(xmin=-Inf,xmax=0,ymin=-Inf,ymax=Inf),fill= "white",color="transparent") +
  geom_rect(aes(xmin=120,xmax=Inf,ymin=-Inf,ymax=Inf),fill= "white",color = "transparent") +
  geom_rect(aes(xmin=0,xmax=10,ymin=0,ymax=53.3),fill= "#c4b59f",color="transparent") +
  geom_rect(aes(xmin=110,xmax=120,ymin=0,ymax=53.3),fill= "#c4b59f",color="transparent") +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100,110),
                     labels = c("G",10,20,30,40,50,40,30,20,10,"G")) +
  #coord_cartesian(xlim = c(min(test1$x),max(test1$x)),ylim = c(-5,55)) +
  ggsci::scale_color_jama()+
  geom_point(size = 2) +
  theme(
    panel.background = element_rect(fill = "#94b59b"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  labs(title = play_title$playDescription)

p1
p2 <- p1 + transition_time(frameId)
animate(p2,nframes = 1*max(test1$frameId),width = 600,duration = as.numeric(max(test1$time)- min(test1$time)))
anim_save("bdb.gif")




week1 %>%
  group_by(gameId,playId,nflId) %>%
  summarise(n()) %>%
  summary()
