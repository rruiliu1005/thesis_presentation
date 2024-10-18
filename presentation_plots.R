library(dplyr)
library(tseries)
library(tidyr)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(GGally)
library(copula)
library(mvtnorm)
setwd("/Users/ruiliu/Desktop/research/Presentation")

data = read.csv('/Users/ruiliu/Desktop/research/data/USDT_perp_futures_0902_daily.csv')

data$openTime = as.POSIXct(data$openTime/1000, origin="1970-01-01")
data$openTime = as.Date(data$openTime)

startdates = data %>%
  group_by(inst) %>%
  summarise(min_time = min(openTime))

startdates$start_date_rank = rank(startdates$min_time)

trading_vol = data %>%
  group_by(inst)%>%
  summarise(average_volume = mean(QuoteAssetVolume))
trading_vol$trading_vol_rank = rank(-trading_vol$average_volume) 
summary = inner_join(startdates, trading_vol, by = 'inst')
summary$total_rank = (summary$start_date_rank*0.5 + summary$trading_vol_rank*0.5)
inst_to_keep = summary[order(summary$total_rank, decreasing = FALSE), ][1:20,]$inst
tmp = data[,"inst"]
data = data[tmp %in% inst_to_keep, ]
#inst_to_keep = c('BTCUSDT', 'ETHUSDT', 'BNBUSDT', 'XRPUSDT', 'ADAUSDT', 'DOGEUSDT', 'SOLUSDT', 'XLMUSDT', 'MATICUSDT', 'TRXUSDT', 
#                 'LTCUSDT', 'DOTUSDT', 'AVAXUSDT', 'NEARUSDT', 'FILUSDT', 'ATOMUSDT', 'DASHUSDT', 'LINKUSDT', 'UNIUSDT', 'XMRUSDT')

data = data[, c("openTime", "inst", "Close")]
data = data %>% arrange(inst, openTime) 
data = unique(data)
daily_returns = data %>%
  group_by(inst) %>%
  mutate(daily_return = (Close / lag(Close) - 1))
daily_returns['log_returns'] = log(daily_returns['daily_return']+1)
daily_returns = na.omit(daily_returns)
tmp = daily_returns %>%  group_by(inst) %>% summarise(max_openTime = max(openTime))
tmp = min(tmp$max_openTime)
tmp1 = daily_returns %>%  group_by(inst) %>% summarise(min_openTime = min(openTime))
tmp1 = max(tmp1$min_openTime)
daily_returns = daily_returns[(daily_returns$openTime >= tmp1)&(daily_returns$openTime <= tmp), ]
# daily_returns <- daily_returns[,c('openTime', 'inst', 'daily_return')] %>%
#     pivot_wider(
#         names_from = inst,
#         values_from = c(daily_return)
#     )
p <- ggplot(daily_returns, aes(x = openTime, y = daily_return)) +
  geom_line(color = "#3466A5") +  
  facet_wrap(~ inst, ncol = 5, nrow = 4, scales = "free_x") +  
  labs(x = "Date", y = "Daily Returns") +
  theme_minimal() +  
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA), 
    plot.background = element_rect(fill = "#FCF9ED", color = NA), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA), 
    strip.text = element_text(color = "#2E8044"), 
    axis.text = element_text(color = "#2E8044", size = 15), 
    axis.title = element_text(color = "#2E8044"),
    plot.title = element_text(color = "#2E8044")
  )  +
  ylim(-0.3, 0.3)
ggsave("daily_returns_plot.jpeg", plot = p, width = 20, height = 10, dpi = 300)


###########################################################################################
df <- daily_returns[,c('openTime', 'inst', 'daily_return')] %>%
  pivot_wider(
    names_from = inst,
    values_from = c(daily_return)
  )
library(GGally)
library(ggplot2)
library(dplyr)

# Example data and transformation
df <- daily_returns[, c('openTime', 'inst', 'daily_return')] %>%
  pivot_wider(
    names_from = inst,
    values_from = c(daily_return)
  )

# Create the ggpairs plot
p <- ggpairs(
  df[, c("DOGEUSDT", "BNBUSDT", "MATICUSDT", "LTCUSDT")], 
  title = "", 
  upper = NULL,  # Removes the upper triangle
  lower = list(continuous = wrap("points", alpha = 0.6, size = 1.5)), 
  diag = list(continuous = wrap("densityDiag", fill = "skyblue"))
) + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),  # Background color
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.text.y.left = element_blank(),  # Remove text on left y-axis
    axis.ticks.y.left = element_blank(), # Remove ticks on left y-axis
    axis.text.x.bottom = element_blank(), # Remove text on bottom x-axis
    axis.ticks.x.bottom = element_blank(), # Remove ticks on bottom x-axis
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")
  )

# Display the plot
ggsave("pairwise_plot.jpeg", plot = p, width = 20, height = 10, dpi = 300)

###########################################################################################

unif_df = read.csv('/Users/ruiliu/Desktop/research/data/unif_df.csv')
unif_norm_df = data.frame(apply(unif_df[,1:(ncol(unif_df)-11)], 1:2, qnorm, mean = 0, sd = 1))
p1 = ggpairs(
  unif_norm_df,
  upper = list(continuous = wrap("points", size = 0.5)),
  lower = list(continuous = wrap("points", size = 0.5)))+  
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.text.y.left = element_blank(), 
    axis.ticks.y.left = element_blank(),
    axis.text.x.bottom = element_blank(), 
    axis.ticks.x.bottom = element_blank(), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")
  )

ggsave("data_normal_plots.png", plot = p1, width = 15, height = 10)




###########################################################################################
spearmans_rho_transform = function(rho_s){
  rho_c = 2*sin(pi/6*rho_s)
  return(rho_c)
}

correlation_mat_spearman = cor(unif_df[,1:(ncol(unif_df)-11)], method = 'spearman')
correlation_mat_copula = apply(correlation_mat_spearman, 1:2, spearmans_rho_transform)
simulated_gaussian = MASS::mvrnorm(n = 10000, mu = rep(0, nrow(correlation_mat_copula)), Sigma = correlation_mat_copula)

p2 = ggpairs(
  simulated_gaussian,
  upper = list(continuous = wrap("points", size = 0.5)),
  lower = list(continuous = wrap("points", size = 0.5)))+  
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),  # Background color
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.text.y.left = element_blank(), 
    axis.ticks.y.left = element_blank(), 
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("simulated_normal_plots.png", plot = p2, width = 15, height = 10)

##########################################################################
# Load necessary libraries
n = 10000
kendall_tau = 0.7 

gaussian_rho = sin(pi/2*kendall_tau)
mean_vector <- c(0, 0)
cov_matrix <- matrix(c(1, kendall_tau, kendall_tau, 1), nrow = 2) 

data <- MASS::mvrnorm(n = n, mu = mean_vector, Sigma = cov_matrix)
df <- data.frame(x = data[, 1], y = data[, 2])

p = ggplot(df, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA),
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),  
    plot.title = element_text(color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("gaussian_copula_contour.png", plot = p, width = 8, height = 8)

theta = 1/(1-kendall_tau)
gumbel_copula <- gumbelCopula(param = theta, dim = 2)  
u <- rCopula(n, gumbel_copula)
data_gumbel <- qnorm(u) 
df_gumbel <- data.frame(x = data_gumbel[, 1], y = data_gumbel[, 2])


p <- ggplot(df_gumbel, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA), 
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("gumbel_copula_contour.png", plot = p, width = 10, height = 10)


# Reflected Gumbel Copula

gumbel_copula <- gumbelCopula(param = 3, dim = 2)  
u <- rCopula(n, gumbel_copula)
data_gumbel <- qnorm(1-u) 
df_gumbel <- data.frame(x = data_gumbel[, 1], y = data_gumbel[, 2])


p <- ggplot(df_gumbel, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA), 
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("rgumbel_copula_contour.png", plot = p, width = 10, height = 10)


theta = 2*kendall_tau/(1-kendall_tau)
clayton_copula <- claytonCopula(param = theta, dim = 2)  
u <- rCopula(n, clayton_copula)
data_clayton <- qnorm(u) 
df_clayton <- data.frame(x = data_clayton[, 1], y = data_clayton[, 2])


p <- ggplot(df_clayton, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA), 
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("clayton_copula_contour.png", plot = p, width = 10, height = 10)


theta <- -3*kendall_tau/(log(1-kendall_tau))

frank_copula <- frankCopula(param = theta, dim = 2)  
u <- rCopula(n, frank_copula)
data_frank <- qnorm(u) 
df_frank <- data.frame(x = data_frank[, 1], y = data_frank[, 2])

p <- ggplot(df_frank, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA),
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggsave("frank_copula_contour.png", plot = p, width = 10, height = 12)


rho <- sin(pi/2*kendall_tau)
df_t <- 1

t_copula <- tCopula(param = rho, dim = 2, df = df_t)

u <- rCopula(n, t_copula)
data_t <- qnorm(u) 
df_t <- data.frame(x = data_t[, 1], y = data_t[, 2])

p <- ggplot(df_t, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 2) +
  scale_fill_gradient(low = "#FFF8F9", high = "black") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FFF8F9", color = NA), 
    plot.background = element_rect(fill = "#FFF8F9", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FFF8F9", color = NA),
    strip.text = element_text(color = "black"),  
    axis.text.y.left = element_blank(), 
    axis.ticks.y.left = element_blank(),
    axis.text.x.bottom = element_blank(), 
    axis.ticks.x.bottom = element_blank(), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")
  )

# Save the plot
ggsave("t_copula_contour.png", plot = p, width = 10, height = 10)

############################################################
plot_list = list()

for (i in 1:length(best_distributions)) {
  
  hist_data = hist(AR_results[[i]]$std_residuals, plot = FALSE, breaks = 100)
  
  df_hist = data.frame(x = hist_data$mids, y = hist_data$density)
  
  df_line = data.frame(x = x, y = plot_data[[i]])
  
  p = ggplot() +
    geom_histogram(aes(x = AR_results[[i]]$std_residuals, y = ..density..), 
                   bins = 100, fill = "lightgray", color = "black") +
    geom_line(data = df_line, aes(x = x, y = y), color = "#3466A5", size = 0.5) +
    labs(title = paste(names(AR_results)[i], ": ", best_distributions[[i]]),
         x = "Standardized Residuals",
         y = "Density") +
    xlim(-10, 10) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#FCF9ED", color = NA),
      plot.background = element_rect(fill = "#FCF9ED", color = NA),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#FCF9ED", color = NA),
      strip.text = element_text(color = "black"),  
      axis.text = element_blank(),  
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
  plot_list[[i]] = p
}

p = grid.arrange(grobs = plot_list, ncol = 2, nrow = 10)

ggsave("fitted_distributions.jpg", plot = p, width = 10, height = 10)


###################################################
ylim_range <- range(c(upper_tail_dependence_mse_gaussian_90, upper_tail_dependence_mse_clayton_90, upper_tail_dependence_mse_gumbel_90, upper_tail_dependence_mse_rgumbel_90))
dates_90 <- as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
plot(dates_90, upper_tail_dependence_mse_gaussian_90, type = "l", ylim = ylim_range, 
     ylab = "Log Likelihood Value", xlab = "Date", col = "#3466A5", 
     cex.lab = 0.7,  
     cex.axis = 0.6, 
     mgp = c(2, 0.5, 0))  
lines(dates_90, upper_tail_dependence_mse_clayton_90, col = "#AA3A39")
lines(dates_90, upper_tail_dependence_mse_gumbel_90, col = "#FF983B")
lines(dates_90, upper_tail_dependence_mse_rgumbel_90, col = "#00C4D4")
legend("topleft", legend = c("Gaussian", 
                             "Clayton", 
                             "Gumbel", 
                             "Reflected Gumbel"),
       col = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4"), lty = 1, cex = 0.5)
title("Upper Tail Dependence Squared Error (rolling window = 90)")

ylim_range <- range(c(lower_tail_dependence_mse_gaussian_90, lower_tail_dependence_mse_clayton_90, lower_tail_dependence_mse_gumbel_90, lower_tail_dependence_mse_rgumbel_90))
dates_90 <- as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
plot(dates_90, lower_tail_dependence_mse_gaussian_90, type = "l", ylim = ylim_range, 
     ylab = "Log Likelihood Value", xlab = "Date", col = "#3466A5", 
     cex.lab = 0.7,  
     cex.axis = 0.6, 
     mgp = c(2, 0.5, 0))  
lines(dates_90, lower_tail_dependence_mse_clayton_90, col = "#AA3A39")
lines(dates_90, lower_tail_dependence_mse_gumbel_90, col = "#FF983B")
lines(dates_90, lower_tail_dependence_mse_rgumbel_90, col = "#00C4D4")
legend("topleft", legend = c("Gaussian", 
                             "Clayton", 
                             "Gumbel", 
                             "Reflected Gumbel"),
       col = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4"), lty = 1, cex = 0.5)
title("Lower Tail Dependence Squared Error (rolling window = 90)")


ylim_range <- range(c(upper_tail_dependence_mse_gaussian_180, upper_tail_dependence_mse_clayton_180, upper_tail_dependence_mse_gumbel_180, upper_tail_dependence_mse_rgumbel_180))
dates_180 <- as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
plot(dates_180, upper_tail_dependence_mse_gaussian_180, type = "l", ylim = ylim_range, 
     ylab = "Log Likelihood Value", xlab = "Date", col = "#3466A5", 
     cex.lab = 0.7,  
     cex.axis = 0.6, 
     mgp = c(2, 0.5, 0))  
lines(dates_180, upper_tail_dependence_mse_clayton_180, col = "#AA3A39")
lines(dates_180, upper_tail_dependence_mse_gumbel_180, col = "#FF983B")
lines(dates_180, upper_tail_dependence_mse_rgumbel_180, col = "#00C4D4")
legend("topleft", legend = c("Gaussian", 
                             "Clayton", 
                             "Gumbel", 
                             "Reflected Gumbel"),
       col = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4"), lty = 1, cex = 0.5)
title("Upper Tail Dependence Squared Error (rolling window = 180)")
###################################################


load("/Users/ruiliu/Desktop/research/results/result_gaussian_90_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gaussian_180_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gaussian_360_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gaussian_720_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_clayton_90_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_clayton_180_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_clayton_360_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_clayton_720_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gumbel_90_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gumbel_180_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_gumbel_360_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_rgumbel_90_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_rgumbel_180_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_rgumbel_360_days.RData")

plot_list = list()
second_element <- result_gaussian_90_days[[2]]
matrix_data <- data.frame(do.call(rbind, second_element))
matrix_data <- 2/pi*asin(matrix_data)
colnames(matrix_data) = colnames(unif_df)[1:20]
matrix_data$Date = as.Date(unif_df[91:nrow(df), 21], format = "%Y-%m-%d")
plot_list[[1]] = ggplot(data = matrix_data, aes(x = Date, y = ETHUSDT))+
  geom_line(color = "#B7A137", size = 2)  +
  labs(title = "Ethereum - Gaussian", 
       x = "Date",
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )

second_element <- result_gumbel_90_days[[2]]
matrix_data <- data.frame(do.call(rbind, second_element))
matrix_data <- 1 - 1/matrix_data
colnames(matrix_data) = colnames(unif_df)[1:20]
matrix_data$Date = as.Date(unif_df[91:nrow(df), 21], format = "%Y-%m-%d")
plot_list[[2]] = ggplot(data = matrix_data, aes(x = Date, y = ETHUSDT))+
  geom_line(color = "#B7A137", size = 2)  +
  labs(title = "Ethereum - Gumbel", 
       x = "Date",
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )

second_element <- result_rgumbel_90_days[[2]]
matrix_data <- data.frame(do.call(rbind, second_element))
matrix_data <- 1 - 1/matrix_data
colnames(matrix_data) = colnames(unif_df)[1:20]
matrix_data$Date = as.Date(unif_df[91:nrow(df), 21], format = "%Y-%m-%d")
plot_list[[3]] = ggplot(data = matrix_data, aes(x = Date, y = ETHUSDT))+
  geom_line(color = "#B7A137", size = 2)  +
  labs(title = "Ethereum - Reflected Gumbel", 
       x = "Date",
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )
second_element <- result_clayton_90_days[[2]]
matrix_data <- data.frame(do.call(rbind, second_element))
matrix_data <- matrix_data/(matrix_data+2)
colnames(matrix_data) = colnames(unif_df)[1:20]
matrix_data$Date = as.Date(unif_df[91:nrow(df), 21], format = "%Y-%m-%d")
plot_list[[4]] = ggplot(data = matrix_data, aes(x = Date, y = ETHUSDT))+
  geom_line(color = "#B7A137", size = 2)  +
  labs(title = "Ethereum - Clayton", 
       x = "Date",
       y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )

ggsave("dynamic_dependence_gaussian_90.jpg", plot = plot_list[[1]], width = 15, height = 5)
ggsave("dynamic_dependence_gumbel_90.jpg", plot = plot_list[[2]], width = 15, height = 5)
ggsave("dynamic_dependence_rgumbel_90.jpg", plot = plot_list[[3]], width = 15, height = 5)
ggsave("dynamic_dependence_clayton_90.jpg", plot = plot_list[[4]], width = 15, height = 5)

###################################################
period = 90 
tail_dependence_mse_total_gaussian = upper_tail_dependence_mse_gaussian_90 + lower_tail_dependence_mse_gaussian_90
tail_dependence_mse_total_clayton = upper_tail_dependence_mse_clayton_90 + lower_tail_dependence_mse_clayton_90
tail_dependence_mse_total_gumbel = upper_tail_dependence_mse_gumbel_90 + lower_tail_dependence_mse_gumbel_90
tail_dependence_mse_total_rgumbel = upper_tail_dependence_mse_rgumbel_90 + lower_tail_dependence_mse_rgumbel_90
best_model_90 = c()
best_tau_90 = c()
for (i in 1:(nrow(unif_df) - period)) {
  choices = c(tail_dependence_mse_total_gaussian[i], tail_dependence_mse_total_clayton[i], tail_dependence_mse_total_gumbel[i], tail_dependence_mse_total_rgumbel[i])
  best = which.min(choices)
  if (best == 1){
    best_model_90[i] = 'Gaussian'
    best_tau_90[[i]] = 2/pi*asin(result_gaussian_90_days$estimates[[i]])
  }
  else if (best == 2){
    best_model_90[i] = 'Clayton'
    best_tau_90[[i]] = result_clayton_90_days$estimates[[i]]/(result_clayton_90_days$estimates[[i]]+2)
  }
  else if (best == 3){
    best_model_90[i] = "Gumbel"
    best_tau_90[[i]] = 1-1/result_gumbel_90_days$estimates[[i]]
  }
  else if (best == 4){
    best_model_90[i] = "Reflected Gumbel"
    best_tau_90[[i]] = -(1-1/result_gumbel_90_days$estimates[[i]])
  }  
  if(best_tau_90[[i]][1] < 0){
    best_tau_90[[i]] = -1*best_tau_90[[i]]
  }
} 
matrix_data <- do.call(rbind, best_tau_90)
df <- data.frame(
  dates = rep(dates_90, times = 20), 
  value = as.vector(matrix_data),   
  series = rep(colnames(unif_df)[1:20], each = length(dates_90)), 
  best_model = rep(best_model_90, times = 20)  
)

plot_list <- list() 

remove_outliers <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  x[x >= qnt[1] & x <= qnt[2]]  
}


df_i <- df %>% filter(series == "MATICUSDT")

df_returns_filtered <- daily_returns %>%
  filter(inst == "MATICUSDT" & openTime %in% dates_90)

filtered_squared_error <- remove_outliers(df_i$value)
filtered_daily_return <- remove_outliers(df_returns_filtered$daily_return)

ymin <- min(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1
ymax <- max(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1

p <- ggplot(df_i, aes(x = dates)) +  
  geom_line(aes(y = value, color = best_model, group = 1), size = 1.5) +  
  
  geom_line(data = df_returns_filtered, aes(x = openTime, y = daily_return, color = "Daily Returns"), size = 1) +
  
  scale_color_manual(values = c("Gaussian" = "#3466A5", 
                                "Clayton" = "#AA3A39", 
                                "Gumbel" = "#FF983B", 
                                "Reflected Gumbel" = "#B7A137", 
                                "Daily Returns" = "grey")) +
  
  ylim(ymin, ymax) +
  labs(title = "MATIC  (rolling window = 90 days)", x = element_blank(), y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = c(0.9,0.5),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

ggsave(
  filename = "copula_switching_90_days_tau_MATIC.jpeg", 
  plot = p, 
  width = 16, height = 6, units = "in", dpi = 300
)

# Display the best model count
table(best_model_90)

period = 180 
tail_dependence_mse_total_gaussian = upper_tail_dependence_mse_gaussian_180 + lower_tail_dependence_mse_gaussian_180
tail_dependence_mse_total_clayton = upper_tail_dependence_mse_clayton_180 + lower_tail_dependence_mse_clayton_180
tail_dependence_mse_total_gumbel = upper_tail_dependence_mse_gumbel_180 + lower_tail_dependence_mse_gumbel_180
tail_dependence_mse_total_rgumbel = upper_tail_dependence_mse_rgumbel_180 + lower_tail_dependence_mse_rgumbel_180
best_model_180 = c()
best_tau_180 = c()
for (i in 1:(nrow(unif_df) - period)) {
  choices = c(tail_dependence_mse_total_gaussian[i], tail_dependence_mse_total_clayton[i], tail_dependence_mse_total_gumbel[i], tail_dependence_mse_total_rgumbel[i])
  best = which.min(choices)
  if (best == 1){
    best_model_180[i] = 'Gaussian'
    best_tau_180[[i]] = 2/pi*asin(result_gaussian_180_days$estimates[[i]])
  }
  else if (best == 2){
    best_model_180[i] = 'Clayton'
    best_tau_180[[i]] = result_clayton_180_days$estimates[[i]]/(result_clayton_180_days$estimates[[i]]+2)
  }
  else if (best == 3){
    best_model_180[i] = "Gumbel"
    best_tau_180[[i]] = 1-1/result_gumbel_180_days$estimates[[i]]
  }
  else if (best == 4){
    best_model_180[i] = "Reflected Gumbel"
    best_tau_180[[i]] = -(1-1/result_gumbel_180_days$estimates[[i]])
  }  
  if(best_tau_180[[i]][1] < 0){
    best_tau_180[[i]] = -1*best_tau_180[[i]]
  }
} 
matrix_data <- do.call(rbind, best_tau_180)
df <- data.frame(
  dates = rep(dates_180, times = 20), 
  value = as.vector(matrix_data),   
  series = rep(colnames(unif_df)[1:20], each = length(dates_180)), 
  best_model = rep(best_model_180, times = 20)  
)

plot_list <- list() 

remove_outliers <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  x[x >= qnt[1] & x <= qnt[2]]  
}


df_i <- df %>% filter(series == "MATICUSDT")
  
df_returns_filtered <- daily_returns %>%
    filter(inst == "MATICUSDT" & openTime %in% dates_180)
  
filtered_squared_error <- remove_outliers(df_i$value)
filtered_daily_return <- remove_outliers(df_returns_filtered$daily_return)
  
ymin <- min(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1
ymax <- max(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1
  
p <- ggplot(df_i, aes(x = dates)) +  
geom_line(aes(y = value, color = best_model, group = 1), size = 1.5) +  
    
geom_line(data = df_returns_filtered, aes(x = openTime, y = daily_return, color = "Daily Returns"), size = 1) +
    
scale_color_manual(values = c("Gaussian" = "#3466A5", 
                                  "Clayton" = "#AA3A39", 
                                  "Gumbel" = "#FF983B", 
                                  "Reflected Gumbel" = "#B7A137", 
                                  "Daily Returns" = "grey")) +

ylim(ymin, ymax) +
labs(title = "MATIC  (rolling window = 180 days)", x = element_blank(), y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = c(0.9,0.5),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

ggsave(
  filename = "copula_switching_180_days_tau_MATIC.jpeg", 
  plot = p, 
  width = 16, height = 6, units = "in", dpi = 300
)

# Display the best model count
table(best_model_180)


period = 360 
tail_dependence_mse_total_gaussian = upper_tail_dependence_mse_gaussian_360 + lower_tail_dependence_mse_gaussian_360
tail_dependence_mse_total_clayton = upper_tail_dependence_mse_clayton_360 + lower_tail_dependence_mse_clayton_360
tail_dependence_mse_total_gumbel = upper_tail_dependence_mse_gumbel_360 + lower_tail_dependence_mse_gumbel_360
tail_dependence_mse_total_rgumbel = upper_tail_dependence_mse_rgumbel_360 + lower_tail_dependence_mse_rgumbel_360
best_model_360 = c()
best_tau_360 = c()
for (i in 1:(nrow(unif_df) - period)) {
  choices = c(tail_dependence_mse_total_gaussian[i], tail_dependence_mse_total_clayton[i], tail_dependence_mse_total_gumbel[i], tail_dependence_mse_total_rgumbel[i])
  best = which.min(choices)
  if (best == 1){
    best_model_360[i] = 'Gaussian'
    best_tau_360[[i]] = 2/pi*asin(result_gaussian_360_days$estimates[[i]])
  }
  else if (best == 2){
    best_model_360[i] = 'Clayton'
    best_tau_360[[i]] = result_clayton_360_days$estimates[[i]]/(result_clayton_360_days$estimates[[i]]+2)
  }
  else if (best == 3){
    best_model_360[i] = "Gumbel"
    best_tau_360[[i]] = 1-1/result_gumbel_360_days$estimates[[i]]
  }
  else if (best == 4){
    best_model_360[i] = "Reflected Gumbel"
    best_tau_360[[i]] = -(1-1/result_gumbel_360_days$estimates[[i]])
  }  
  if(best_tau_360[[i]][1] < 0){
    best_tau_360[[i]] = -1*best_tau_360[[i]]
  }
} 
matrix_data <- do.call(rbind, best_tau_360)
df <- data.frame(
  dates = rep(dates_360, times = 20), 
  value = as.vector(matrix_data),   
  series = rep(colnames(unif_df)[1:20], each = length(dates_360)), 
  best_model = rep(best_model_360, times = 20)  
)

plot_list <- list() 

remove_outliers <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  x[x >= qnt[1] & x <= qnt[2]]  
}


df_i <- df %>% filter(series == "MATICUSDT")

df_returns_filtered <- daily_returns %>%
  filter(inst == "MATICUSDT" & openTime %in% dates_360)

filtered_squared_error <- remove_outliers(df_i$value)
filtered_daily_return <- remove_outliers(df_returns_filtered$daily_return)

ymin <- min(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1
ymax <- max(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1

p <- ggplot(df_i, aes(x = dates)) +  
  geom_line(aes(y = value, color = best_model, group = 1), size = 1.5) +  
  
  geom_line(data = df_returns_filtered, aes(x = openTime, y = daily_return, color = "Daily Returns"), size = 1) +
  
  scale_color_manual(values = c("Gaussian" = "#3466A5", 
                                "Clayton" = "#AA3A39", 
                                "Gumbel" = "#FF983B", 
                                "Reflected Gumbel" = "#B7A137", 
                                "Daily Returns" = "grey")) +
  
  ylim(ymin, ymax) +
  labs(title = " (rolling window = 360 days)", x = element_blank(), y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = c(0.9,0.5),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

ggsave(
  filename = "copula_switching_360_days_tau_MATIC.jpeg", 
  plot = p, 
  width = 16, height = 6, units = "in", dpi = 300
)

# Display the best model count
table(best_model_360)

period = 720 
tail_dependence_mse_total_gaussian = upper_tail_dependence_mse_gaussian_720 + lower_tail_dependence_mse_gaussian_720
tail_dependence_mse_total_clayton = upper_tail_dependence_mse_clayton_720 + lower_tail_dependence_mse_clayton_720
tail_dependence_mse_total_gumbel = upper_tail_dependence_mse_gumbel_720 + lower_tail_dependence_mse_gumbel_720
tail_dependence_mse_total_rgumbel = upper_tail_dependence_mse_rgumbel_720 + lower_tail_dependence_mse_rgumbel_720
best_model_720 = c()
best_tau_720 = c()
for (i in 1:(nrow(unif_df) - period)) {
  choices = c(tail_dependence_mse_total_gaussian[i], tail_dependence_mse_total_clayton[i], tail_dependence_mse_total_gumbel[i], tail_dependence_mse_total_rgumbel[i])
  best = which.min(choices)
  if (best == 1){
    best_model_720[i] = 'Gaussian'
    best_tau_720[[i]] = 2/pi*asin(result_gaussian_720_days$estimates[[i]])
  }
  else if (best == 2){
    best_model_720[i] = 'Clayton'
    best_tau_720[[i]] = result_clayton_720_days$estimates[[i]]/(result_clayton_720_days$estimates[[i]]+2)
  }
  else if (best == 3){
    best_model_720[i] = "Gumbel"
    best_tau_720[[i]] = 1-1/result_gumbel_720_days$estimates[[i]]
  }
  else if (best == 4){
    best_model_720[i] = "Reflected Gumbel"
    best_tau_720[[i]] = -(1-1/result_gumbel_720_days$estimates[[i]])
  }  
  if(best_tau_720[[i]][1] < 0){
    best_tau_720[[i]] = -1*best_tau_720[[i]]
  }
} 
matrix_data <- do.call(rbind, best_tau_720)
df <- data.frame(
  dates = rep(dates_720, times = 20), 
  value = as.vector(matrix_data),   
  series = rep(colnames(unif_df)[1:20], each = length(dates_720)), 
  best_model = rep(best_model_720, times = 20)  
)

plot_list <- list() 

remove_outliers <- function(x, lower_quantile = 0.01, upper_quantile = 0.99) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  x[x >= qnt[1] & x <= qnt[2]]  
}


df_i <- df %>% filter(series == "MATICUSDT")

df_returns_filtered <- daily_returns %>%
  filter(inst == "MATICUSDT" & openTime %in% dates_720)

filtered_squared_error <- remove_outliers(df_i$value)
filtered_daily_return <- remove_outliers(df_returns_filtered$daily_return)

ymin <- min(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1
ymax <- max(filtered_squared_error, filtered_daily_return, na.rm = TRUE) * 1.1

p <- ggplot(df_i, aes(x = dates)) +  
  geom_line(aes(y = value, color = best_model, group = 1), size = 1.5) +  
  
  geom_line(data = df_returns_filtered, aes(x = openTime, y = daily_return, color = "Daily Returns"), size = 1) +
  
  scale_color_manual(values = c("Gaussian" = "#3466A5", 
                                "Clayton" = "#AA3A39", 
                                "Gumbel" = "#FF983B", 
                                "Reflected Gumbel" = "#B7A137", 
                                "Daily Returns" = "grey")) +
  
  ylim(ymin, ymax) +
  labs(title = "MATIC (rolling window = 720 days)", x = element_blank(), y = "Kendall's Tau") +
  theme_minimal() +
  theme(
    legend.position = c(0.9,0.5),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

ggsave(
  filename = "copula_switching_720_days_tau_MATIC.jpeg", 
  plot = p, 
  width = 16, height = 6, units = "in", dpi = 300
)

# Display the best model count
table(best_model_720)

####################################################################################################
df_upper <- data.frame(
  Date = rep(dates_90, 4),
  Squared_Error = c(upper_tail_dependence_mse_gaussian_90, 
                    upper_tail_dependence_mse_clayton_90, 
                    upper_tail_dependence_mse_gumbel_90, 
                    upper_tail_dependence_mse_rgumbel_90),
  Copula = rep(c("Gaussian", "Clayton", "Gumbel", "Reflected Gumbel"), each = length(dates_90))
)

df_lower <- data.frame(
  Date = rep(dates_90, 4),
  Squared_Error = c(lower_tail_dependence_mse_gaussian_90, 
                    lower_tail_dependence_mse_clayton_90, 
                    lower_tail_dependence_mse_gumbel_90, 
                    lower_tail_dependence_mse_rgumbel_90),
  Copula = rep(c("Gaussian", "Clayton", "Gumbel", "Reflected Gumbel"), each = length(dates_90))
)

p = ggplot(df_upper, aes(x = Date, y = Squared_Error, color = Copula)) +
  geom_line() +
  labs(title = "Upper Tail Dependence Squared Error (rolling window = 90)", 
       x = "Date", 
       y = "Squared Error") +
  scale_color_manual(values = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 25),
    legend.title = element_blank(), 
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )
ggsave(
  filename = "upper_dependence_error_90days.jpeg", 
  plot = p, 
  width = 16, height = 8, units = "in", dpi = 300
)

p = ggplot(df_lower, aes(x = Date, y = Squared_Error, color = Copula)) +
  geom_line() +
  labs(title = "Lower Tail Dependence Squared Error (rolling window = 90)", 
       x = "Date", 
       y = "Squared Error") +
  scale_color_manual(values = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 25),
    legend.title = element_blank(), 
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )

ggsave(
  filename = "lower_dependence_error_90days.jpeg", 
  plot = p, 
  width = 16, height = 8, units = "in", dpi = 300
)


df_upper <- data.frame(
  Date = rep(dates_180, 4),
  Squared_Error = c(upper_tail_dependence_mse_gaussian_180, 
                    upper_tail_dependence_mse_clayton_180, 
                    upper_tail_dependence_mse_gumbel_180, 
                    upper_tail_dependence_mse_rgumbel_180),
  Copula = rep(c("Gaussian", "Clayton", "Gumbel", "Reflected Gumbel"), each = length(dates_180))
)

df_lower <- data.frame(
  Date = rep(dates_180, 4),
  Squared_Error = c(lower_tail_dependence_mse_gaussian_180, 
                    lower_tail_dependence_mse_clayton_180, 
                    lower_tail_dependence_mse_gumbel_180, 
                    lower_tail_dependence_mse_rgumbel_180),
  Copula = rep(c("Gaussian", "Clayton", "Gumbel", "Reflected Gumbel"), each = length(dates_180))
)

p = ggplot(df_upper, aes(x = Date, y = Squared_Error, color = Copula)) +
  geom_line() +
  labs(title = "Upper Tail Dependence Squared Error (rolling window = 180)", 
       x = "Date", 
       y = "Squared Error") +
  scale_color_manual(values = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4"), 
                     guide = guide_legend(override.aes = list(size = 4))) +
  theme_minimal(base_size = 12) + theme(
    legend.position = "top",
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20)
  )+ guides(color = guide_legend(override.aes = list(size = 4))) 

ggsave(
  filename = "upper_dependence_error_180days.jpeg", 
  plot = p, 
  width = 16, height = 8, units = "in", dpi = 300
)

p = ggplot(df_lower, aes(x = Date, y = Squared_Error, color = Copula)) +
  geom_line() +
  labs(title = "Lower Tail Dependence Squared Error (rolling window = 180)", 
       x = "Date", 
       y = "Squared Error") +
  scale_color_manual(values = c("#3466A5", "#AA3A39", "#FF983B", "#00C4D4")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 25),
    legend.title = element_blank(), 
    panel.background = element_rect(fill = "#FCF9ED", color = NA),
    plot.background = element_rect(fill = "#FCF9ED", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#FCF9ED", color = NA),
    strip.text = element_text(color = "black"),  
    axis.title = element_text(color = "black", size = 30),  
    plot.title = element_text(color = "black", size = 30),
    axis.text  = element_text(color = "black", size = 20),  
  )

ggsave(
  filename = "lower_dependence_error_180days.jpeg", 
  plot = p, 
  width = 16, height = 8, units = "in", dpi = 300
)
