Simulating bacterial growth in water bottle
================
Zheer Kejlberg Al-Mashhadi
2023-11-05

Load dependencies

``` r
library(ggplot2)
library(gridExtra)
```

Set some basic variables

``` r
# Set basic vars
splits <- 1
hours <- 2*splits # two updates of bacterial number per hour
days <- 24*hours
month <- 30*days
t2 <- 12*hours # doubling time of bacterial load
multiplier <- 2^(1/t2)
no_of_sims <- 20
```

Define all the functions

``` r
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
drink <- function(df, df2, time, volumes) {
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
pick_step <- function(bottle, drunk, time, does_drink, scenario, volumes) {
  if (scenario == "continuous") {
    condition <- bottle[time-1,]$step == "drink"
  } else {
    condition <- bottle[time-1,]$volume == 0
  }
  if (condition) {
    bottle <- refill(bottle, time)
  } else {
    if (does_drink[time] == 1) {
      df_list <- drink(bottle, drunk, time, volumes)
      bottle <- df_list[[1]]
      drunk <- df_list[[2]]
    } else {
      bottle <- spread(bottle,time)
    }
  }
  return(list(bottle, drunk))
}

# Define the function to run a full simulation
run_sim <- function(scenario = FALSE) {
  
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
  
  for (i in 2:nrow(bottle)) {
    df_list <- pick_step(bottle, drunk, i, does_drink, scenario, volumes)
    bottle <- df_list[[1]]
    drunk <- df_list[[2]]
  }
  
  print(sum(drunk$colonies, na.rm=TRUE))
  
}
```

Run the simulations

``` r
# Run the simulation with continuous refilling
t1 <- as.numeric(rep(NA,no_of_sims))
for (repetition in 1:no_of_sims) {
  t1[repetition] <- run_sim("continuous")
}
```

    ## [1] 588092.4
    ## [1] 1043426
    ## [1] 333402.1
    ## [1] 314135.4
    ## [1] 508355.9
    ## [1] 2622388
    ## [1] 1132464
    ## [1] 445508.5
    ## [1] 357371.1
    ## [1] 1001393
    ## [1] 3977928
    ## [1] 336702.7
    ## [1] 655542.4
    ## [1] 915481.3
    ## [1] 990948.2
    ## [1] 298273.4
    ## [1] 773240.5
    ## [1] 781327.9
    ## [1] 495350.9
    ## [1] 790518.3

``` r
# Run the simulation with refilling whenever empty
t2_summary <- as.numeric(rep(NA,no_of_sims))
for (repetition in 1:no_of_sims) {
  t2_summary[repetition] <- run_sim()
}
```

    ## [1] 76231.69
    ## [1] 66945.13
    ## [1] 85469.91
    ## [1] 81538.38
    ## [1] 74653.32
    ## [1] 70122.61
    ## [1] 63139.04
    ## [1] 66498.84
    ## [1] 63577.14
    ## [1] 88217.38
    ## [1] 67575.25
    ## [1] 67814.63
    ## [1] 60375.77
    ## [1] 73089.88
    ## [1] 63747.15
    ## [1] 68465.92
    ## [1] 70357.91
    ## [1] 66686.37
    ## [1] 65621.27
    ## [1] 81490.62

Create plots of distributions of total bacterial intake

``` r
# Create density plots
t1_df <- as.data.frame(t1)
t1_plot <- ggplot(t1_df, aes(x=t1)) +
  geom_density() + labs(
    title = paste("Refilling continuously - ", no_of_sims, " iterations", sep=""),
    x = "Total bacteria drunk",
    y = "Density"
  ) + xlim(min(t1_df),max(t1_df))

z1 <- as.matrix(summary(t1))
z1 <- tableGrob(z1)
grid.arrange(t1_plot, z1, heights=c(2.5,1))
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
t2_df <- as.data.frame(t2_summary)
t2_plot <- ggplot(t2_df, aes(x=t2_summary)) +
  geom_density() + labs(
    title = paste("Refilling when empty - ", no_of_sims, " iterations", sep=""),
    x = "Total bacteria drunk",
    y = "Density"
  ) + xlim(min(t2_df),max(t2_df))

z2 <- as.matrix(summary(t2_summary))
z2 <- tableGrob(z2)
grid.arrange(t2_plot, z2, heights=c(2.5,1))
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->
