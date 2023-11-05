library(ggplot2)
library(gridExtra)

run_sim <- function(scenario = FALSE) {
  # Set basic vars
  splits <- 1
  hours <- 2*splits
  days <- 24*hours
  month <- 30*days
  t2 <- 12*hours
  multiplier <- 2^(1/t2)
  
  # Random numbers to use
  does_drink <- rbinom(month, 1, 0.1)
  volumes <- rnorm(month,240,25)
  
  # Set up bottle dataframe
  bottle <- data.frame(
    time = 1:month,
    volume = c(1000, rep(NA, month-1)),
    colonies = c(1000, rep(NA, month-1)),
    conc = c(1, rep(NA, month-1)),
    step = c("initialize", rep(NA, month-1))
  )
  
  # Set up drink counter
  drunk <- data.frame(
    volume = c(0, rep(NA, month-1)),
    colonies = c(0, rep(NA, month-1))
  )
  
  # Define function to refill bottle
  refill <- function(df, time) {
    vol_refilled <- 1000 - df[time-1,]$volume
    last_col <- df[time-1,]$colonies
    df[time,] <- data.frame(
      time = time,
      volume = 1000,
      colonies = last_col + vol_refilled,
      conc = (last_col + vol_refilled) / 1000,
      step = "refill"
    )
    return(df)
  }
  
  # Define function for drinking
  drink <- function(df, df2, time) {
    last_vol <- df[time-1,]$volume
    drink_vol <- min(volumes[time], last_vol)
    new_vol <- last_vol - drink_vol
    volume_ratio <- new_vol / last_vol
    
    last_col <- df[time-1,]$colonies
    new_col <- last_col * volume_ratio
    colony_diff <- last_col - new_col
    
    df[time,] <- data.frame(
      time = time,
      volume = new_vol,
      colonies = new_col,
      conc = new_col / new_vol,
      step = "drink"
    )
    
    df2[time,] <- data.frame(
      volume = drink_vol,
      colonies = colony_diff
    )
    
    return(list(df,df2))
  }
  
  # Define function for bacterial spread
  spread <- function(df, time) {
    new_col <- df[time-1,]$colonies * multiplier
    new_vol <- df[time-1,]$volume
    
    df[time,] <- data.frame(
      time = time,
      volume = new_vol,
      colonies = new_col,
      conc = new_col / new_vol,
      step = "spread"
    )
    
    return(df)
  }
  
  # Define function to pick the next function to run
  pick_step <- function(bottle, drunk, time) {
    if (scenario == "continuous") {
      condition <- bottle[time-1,]$step == "drink"
    } else {
      condition <- bottle[time-1,]$volume == 0
    }
    if (condition) {
      bottle <- refill(bottle, time)
    } else {
      if (does_drink[time] == 1) {
        df_list <- drink(bottle, drunk, time)
        bottle <- df_list[[1]]
        drunk <- df_list[[2]]
      } else {
        bottle <- spread(bottle,time)
      }
    }
    return(list(bottle, drunk))
  }
  
  for (i in 2:nrow(bottle)) {
    df_list <- pick_step(bottle, drunk, i)
    bottle <- df_list[[1]]
    drunk <- df_list[[2]]
  }
  
  print(sum(drunk$colonies, na.rm=TRUE))
  
}

no_of_sims <- 100

t1 <- as.numeric(rep(NA,no_of_sims))
for (repetition in 1:no_of_sims) {
  t1[repetition] <- run_sim("continuous")
}


t2_summary <- as.numeric(rep(NA,no_of_sims))
for (repetition in 1:no_of_sims) {
  t2_summary[repetition] <- run_sim()
}


t1_df <- as.data.frame(t1)
t1_plot <- ggplot(t1_df, aes(x=t1)) +
  geom_density() + labs(
    title = paste("Refilling continuously - ", no_of_sims, " iterations", sep=""),
    x = "Total bacteria drunk",
    y = "Density"
  ) + xlim(0,1000000)

z1 <- as.matrix(summary(t1))
z1 <- tableGrob(z1)
grid.arrange(t1_plot, z1, heights=c(2.5,1))




t2_df <- as.data.frame(t2_summary)
t2_plot <- ggplot(t2_df, aes(x=t2_summary)) +
  geom_density() + labs(
    title = paste("Refilling when empty - ", no_of_sims, " iterations", sep=""),
    x = "Total bacteria drunk",
    y = "Density"
  )

z2 <- as.matrix(summary(t2_summary))
z2 <- tableGrob(z2)
grid.arrange(t2_plot, z2, heights=c(2.5,1))

