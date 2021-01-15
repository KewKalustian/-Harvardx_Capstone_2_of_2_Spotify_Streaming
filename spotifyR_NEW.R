
Sys.setlocale("LC_TIME", "English")

################################################################################
# Important! For reading Umlauts:

# RStudio >> "Preferences" and select "Code". 
# From there select the "Saving" tab from the top and select "UTF-8"
################################################################################

# Cleaning the environment

remove(list = ls(all = T)); gc(T,T,T)

# ################ #
# Loading packages #
# ################ #

# Generating a neat function to load all desired packages (x).

load_packages <- function(x){
  
  # Object which contains the desired packages (x) as long as they are not already 
  # installed. 
  
  inst.packages <- x[!x %in% installed.packages()]
  
  # Generating a for-loop.
  
  for (i in inst.packages) {install.packages(i, dependencies = T)}
  
  sapply(x, require, character = T)}

# Finally, using the just generated function to load and/or install the 
# following desired packages.

desired.packages <- c(# Tidy coding paradigm
  "tidyverse", "magrittr", 
  # Data import & Web-Scraping
  "readr", "rvest",
  # Data frames 
  "tibble", "tsibble",
  # Data wrangling
  "lubridate", 
  # Graphics
  "ggExtra", "ggrepel", "scales", 
  # Machine Learning + extra Random Forest and kNN 
  # algorithms
  "caret", "randomForest", "ranger", "FNN",
  # Forecasting
  "forecast", "fpp2", 
  # Time series related tests / accuracy-metrics
  "urca", "FuzzyR", 
  # Reporting
  "knitr", "kableExtra") 

load_packages(desired.packages) 

# ############################## # 
# Retrieving Data | Web-Scraping #
# ############################## # 

# To run these code chunks please un-comment it (takes to almost one hour to run
# this web-scraping procedure). For skipping this, my already retrieved data will
# will downloaded from my GitHub repo.

# # First, we have to store the permanent Spotify-link.
# 
# url <- "https://spotifycharts.com/regional/de/daily/"
# 
# # Here we specify the entire streaming period (i.e., the sequence of 546 days).
# 
# streaming_period <- seq(as.Date("2019/01/01"), as.Date("2020/06/29"),
#                         by = "day")
# 
# # Next, we write a generic function that combines or, respectively, concatenates 
# # the URLs (for the entire period) by taking the permanent link from above and a 
# # blank argument (x) as another argument to which the URL should refer 
# # (i.e., our streaming_period).
# 
# gathering_urls <- function(x){paste0(url, x)}
# 
# # Using the just created function to apply it on the streaming period to finally
# # get those 546 URLs.
# 
# all_urls  <- gathering_urls(streaming_period)
# 
# # Everything looks fine thus far. Hence, we create now a function that fills
# # the desired column names with the information we are going to retrieve from 
# # those 546 URLs (i.e., chart position, song/track title, artist, stream counts, 
# # and dates).
# 
# spotifyR_scrapeR <- function(x) {page <- x
# 
# # Retrieving the 200 chart positions of each day.
# 
# chart_position <- page %>% 
#   read_html() %>% 
#   html_nodes(".chart-table-position") %>% 
#   html_text()
# 
# # Retrieving the 200 song/track titles of each day.
# 
# title <- page %>% 
#   read_html() %>% 
#   html_nodes("strong") %>% 
#   html_text()
# 
# # Retrieving the 200 artist names of each day.
# 
# artist <- page %>% 
#   read_html() %>% 
#   html_nodes(".chart-table-track span") %>% 
#   html_text()
# 
# # Retrieving the 200 stream counts of each day.
# 
# stream_count <- page %>% 
#   read_html() %>% 
#   html_nodes("td.chart-table-streams") %>% 
#   html_text()
# 
# # Retrieving the dates of for each day of the period. 
# 
# date <- page %>% 
#   read_html() %>% 
#   html_nodes(".responsive-select~ .responsive-select+ 
#                     .responsive-select .responsive-select-value") %>%
#   html_text()
# 
# # Putting these chunks together in a table of the class. 
# 
# tab <- data.frame(chart_position, title, artist, stream_count, date)
# 
# return(tab)}
# 
# # As the amount of data that should be retrieved is not that small, we can 
# # expect that this process will take some time. To know how long this process
# # will last, we calculate the difference between the process initialization and 
# # its end.
# 
# init_time <- Sys.time()
# 
# # The actual process of web scraping: Applying the spotifyR_scrapeR-function
# # to the object of that definitive URLs for each list element. That is, the just 
# # created spotifyR_scrapeR-function retrieves from each URL the desired 
# # information.
# 
# spotifyR <- map_df(definitive_urls, spotifyR_scrapeR) 
# 
# # End time of the retrieving-process.
# 
# end_time <- Sys.time()
# 
# # Difference (i.e., processing time to retrieve the desired information).
# 
# process_time <- end_time - init_time
# print(process_time)
# 
# # Exporting and saving the retrieved datatable as .csv-file.
# write_csv(spotifyR, "spotifyR_charts.csv")


# For rerunning the code:
spotifyR <- read_csv("https://bit.ly/3kNXCuC")

# ############# #
# Data Cleaning #
# ############# #

spotifyRR <- spotifyR %>% 
  
  # Group-wise count of distinct songs/tracks (How many times occurs
  # a distinct song/track during the whole period? A maximum of 546 times
  # is possible as the period lasts 546 days).
  
  add_count(title) %>% 
  mutate(chart_position = as.integer(chart_position),
         
         # gsub: Replacing of a matching string pattern by a 
         # replacement string (i.e., we simply omit the string "by" 
         # and the whitespace before the artist names).
         
         artist = sub("by\\s", "", artist),
         
         # Adding songIDs is useful since there could be songs/tracks 
         # with the same title but from different artists.
         
         songID = group_indices(., title, artist),
         
         # Converting/coercing stream_count as integer-class
         
         stream_count = as.integer(stream_count),
         
         date = as.Date(date, "%m/%d/%Y"),
         
         # lubridate::wday: Setting days and specifying the week 
         # start on Monday based on ISO conventions (i.e., week 
         # starts on Monday = 1)
         
         weekday = wday(date, label = T, week_start = 1)) 

# ###################### #
# Plotting Stream counts #
# ###################### #


Streams <- spotifyRR %>%
  filter(date <= as.Date("2020-03-10"))

Streams_Pan <- spotifyRR %>%
  filter(date > as.Date("2020-03-10"))

cols <- c(# Colorblind friendly blue
  "Pre-Pandemic (01.01.19—10.03.20)" = "#045a8d",
  # Colorblind friendly red
  "Pandemic (11.03.20—29.06.20)" = "#bd0026")

# Calculating the binwidth following the formula of Freedman & Diaconis (1981) 
# for the upcoming histogram (see Hyndman 1995)

binw <- 2 * IQR(Streams$stream_count) / nrow(Streams)^(1/3)  

binw_Pan <- 2 * IQR(Streams_Pan$stream_count) / nrow(Streams_Pan)^(1/3) 

# Plotting the histograms in question.

ggplot() +
  
  # Frequency Histogram   
  geom_histogram(data = Streams, aes(stream_count, 
                                     color = "Pre-Pandemic (01.01.19—10.03.20)"), 
                 binwidth  = binw, alpha = 0.5) +
  
  geom_histogram(data = Streams_Pan, aes(stream_count,
                                         color = "Pandemic (11.03.20—29.06.20)"), 
                 binwidth  = binw_Pan, alpha = 0.75) +
  
  # Customizing the labels and break points on the x-axis
  scale_x_continuous(breaks = c(min(spotifyRR$stream_count), 1e5, 2e5, 25e4, 5e5, 
                                1e6, 15e5, max(spotifyRR$stream_count)),
                     labels = c("37568", "100K", "200K", "250K", "500K", "1M", 
                                "1.5M", "c.1.97M"))+
  
  # Label numbers with SI prefixes 
  scale_y_continuous(labels = label_number_si(accurarcy = NULL))+
  
  # For visual purposes (i.e., avoiding that high frequencies and a heavy tail 
  # of high counts dominate the plot), we use here a square root-scaling of 
  # the axes (not of the values). That is, it is of ample importance to note
  # that we do not transform the scale statistics of our data: They are still
  # the same.
  coord_trans(x = "sqrt", y = "sqrt")+ 
  
  # Customizing the labels on both axes    
  labs(x = "\nStream Counts of Songs", y = "Frequency\n")+
  
  # Legend colors and shape of the color fields
  scale_color_manual(name = "Legend:", values =cols) +
  scale_fill_manual(name = "Legend:", values = cols) +
  
  guides(color = guide_legend(override.aes = list(shape = 22, size = 3))) +
  
  # Layout  
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "grey90"),
        plot.margin = unit(c(.66, .33, .66, .66), "cm"))


# ############################################################################ #
# EDA_H: Stream counts during the Pandemic are higher than during Pre-Pandemic #
# ############################################################################ #

# Subsetting and summarizing the data into a pre-pandemic set  

spotifyR_pre <- spotifyRR %>%
  filter(date >= as.Date("2019-03-11") & 
           date <= as.Date("2019-06-29")) %>%
  group_by(weekday) %>%
  # Using the median instead of mean because of
  # skewed overall distribution (see histogram)
  summarize(Mdn_Streams = median(stream_count))

# Subsetting and summarizing the data into a pandemic set  

spotifyR_pandemic <- spotifyRR %>%
  filter(date >= as.Date("2020-03-11") & 
           date <= as.Date("2020-06-29")) %>%
  group_by(weekday) %>%
  # Using the median instead of mean because of
  # skewed overall distribution (see histogram)
  summarize(Mdn_Streams = median(stream_count))

# ################## #
# t-Test assumptions #
# ################## #
# 
# Checking, whether the differences of the stream counts between the two 
# periods in question are approximately normally distributed.

# Assigning the differences to a new object. We calculate the difference 
# because if the difference between those two sets is approximately normally 
# distributed, so are the sets.

dif <- spotifyR_pandemic$Mdn_Streams - spotifyR_pre$Mdn_Streams

# Testing for normality of the differences
# Null hypothesis: No differences are detectable (i.e., data is normally 
# distributed). That is, we "want" here a p-value greater than 0.05 at a 
# confidence level of 95%.

normality <- shapiro.test(dif)

print(normality)

# Testing variance homogeneity:
# Null hypothesis: Variances of both periods are equal. That is, we "want" 
# here a p-value greater than 0.05 at a confidence level of 95% (see above).

var_homo <- var.test(spotifyR_pandemic$Mdn_Streams, spotifyR_pre$Mdn_Streams)
print(var_homo)
# ###### #
# t-Test #
# ###### #
# Performing the t-Test

# Null hypothesis: No significant difference is detectable. That is, we "want" 
# here a p-value equal to or lower than 0.05 at a confidence level of 95% since 
# we hypothesized in H1 that a difference between both periods is observable in 
# that way that the average of streaming counts during the pandemic is higher 
# (i.e., "greater") than during the pre-pandemic period. 

t <- t.test(spotifyR_pandemic$Mdn_Streams, spotifyR_pre$Mdn_Streams, 
            alernative = "greater", paired = T, conf.level = .95)
print(t)

# Calculating the effect size r after Person (see second equation) by using the 
# t-value (see first equation) and the degrees of freedom. 

eff_size_r <- abs(sqrt(t$statistic^2 / (t$statistic^2 + t$parameter)))

print(eff_size_r)

# Central tendencies and dispersion for the (pre-)pandemic periods:

# Pre-Pandemic
mean(spotifyR_pre$Mdn_Streams); sd(spotifyR_pre$Mdn_Streams)
median(spotifyR_pre$Mdn_Streams)

# Pandemic
mean(spotifyR_pandemic$Mdn_Streams); sd(spotifyR_pandemic$Mdn_Streams)
median(spotifyR_pandemic$Mdn_Streams)
# ################################################# #
# EDA_H1: # Bringing everything together | Plotting #
# ################################################# #
# Creating a color-object
cols_1 <- c(# Colorblind friendly blue
  "Pre-Pandemic (11.03.19-29.06.19)" = "#045a8d",
  # Colorblind friendly red
  "Pandemic (11.03.20-29.06.20)" = "#bd0026")

ggplot(data = spotifyR_pre) +
  
  # Connected Scatter plot
  geom_point(aes(weekday, Mdn_Streams, 
                 color = "Pre-Pandemic (11.03.19-29.06.19)")) +
  
  geom_line(aes(weekday, Mdn_Streams, group = 1,
                color = "Pre-Pandemic (11.03.19-29.06.19)"),
            size = .75, show.legend = F) +
  
  geom_point(data = spotifyR_pandemic, aes(weekday, Mdn_Streams, 
                                           color = "Pandemic (11.03.20-29.06.20)")) +
  
  geom_line(data = spotifyR_pandemic, aes(weekday, Mdn_Streams, group = 1,
                                          color = "Pandemic (11.03.20-29.06.20)"), 
            size = .75, show.legend = F) +
  
  # Color and Legend
  scale_color_manual(name = "Legend:", values = cols_1) +
  scale_fill_manual(name = "Legend:", values = cols_1) +
  
  # Customizing the labels on both axes
  labs(x = "\nWeekdays", y = "Stream Counts per Weekday\n(Median)") +
  
  # Label numbers with SI prefixes
  scale_y_continuous(labels = label_number_si(accuracy = NULL)) +
  
  # Customizing the legend
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +
  
  # Layout
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "#bdbdbd"),
        plot.margin = unit(c(.66,.33,.66,.33), "cm"))
# ########################### #
# Towards a Predictions Model #
# ########################### #

# Dates and median stream counts 

spotifyRRR <- spotifyRR %>% 
  group_by(date) %>%
  summarize(MdnStreams = median(stream_count)) 

# Plotting
spotifyRRR %>% 
  
  ggplot(aes(date, MdnStreams))+
  
  # Connected Scatter plot
  geom_point(size = .5)+
  geom_line()+
  
  # Annotating
  geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), color = "#252A52",
             size = 0.2, lty ="dashed")+
  
  geom_label(aes(x = as.Date("2019-07-01"), y = 175e3, label = "Pre-Pandemic"), 
             color = "#045a8d", size = 3)+
  
  geom_label(aes(x = as.Date("2020-05-02"), y = 175e3, 
                 label = "Pandemic\ncontact restrictions"), color= "#bd0026", 
             size = 3)+
  
# Detour: Extracting the date of the max stream count in December
# 
# which.max(spotifyRRR$MdnStreams)
# [1] 358
#
# spotifyRRR$date[358]
# [1] "2019-12-24"
#
# spotifyRRR$MdnStreams[358]
# [1] 263718.5
# 
# Row number 358 (i.e., Christmas) consists the maximal stream counts per day 
# with 263718.5 streaming counts (median).

annotate(geom = "point", x = as.Date("2019-12-24"), y = 263718.5 , size = 5, 
         shape = c(22,23), fill = "darkgoldenrod2", alpha = .4)+
  
  geom_label(aes(x = as.Date("2019-11-19"), y = 262e3, 
                 label = "More than 263K\nStreams (Median)\non Christmas"), 
             color = "darkgoldenrod2", size = 3)+
  # Annotating  
  annotate("rect", xmin = as.Date("2019-01-01"), xmax = as.Date("2020-03-11"),
           ymin = -Inf, ymax = Inf, alpha = .08, fill = "#045a8d")+
  annotate("rect", xmin = as.Date("2020-03-11"), xmax = as.Date("2020-06-29"),
           ymin = -Inf, ymax = Inf, alpha = .08, fill = "#bd0026")+
  
  # Setting breaks on the x-axis  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  
  # Label numbers with SI prefixes
  scale_y_continuous(labels = label_number_si(accuracy = NULL))+
  
  # Customizing the labels on both axes
  labs(x = "\nStream Date", y = "Stream Counts per Day\n(Median)\n")+ 
  
  # Layout
  theme_bw(base_size = 14)+
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .33, .33), "cm"))
# ############## #
# Stationarizing #
# ############## #

# Converting into time-series data

# 1. From a data.frame into a tibble
spotifyR_tib <- tibble(Date = as.Date(spotifyRRR$date),
                       Actual_Stream_Counts = spotifyRRR$MdnStreams)

# 2. From a tibble into a tsibble
spotifyR_tsib <- as_tsibble(spotifyR_tib)

# 3. From a tsibble into a daily time-series, starting on the January 1st, 2019
spotifyR_TS <- as.ts(spotifyR_tsib, frequency = 365, start = c(2019,1))


# Data splitting #


# The end-argument in the window-function indicates the 10. March (day 70 of the 
# year 2020)

spotifyR_TS_orig <- window(spotifyR_TS, end = c(2020, 70)) 

# We estimate the median stream counts of 111 days (that is the analyzed
# period or horizon of the COVID-19-pandemic of interest); start and end days
# are included.

horizon <- 111


# Benchmarking #


# Seasonal-naïve predicitons
benchmark <- snaive(spotifyR_TS_orig, h = horizon)

# Bringing the original data and the estimates together
spotifyR_tib_naïve <- spotifyR_tib %>% 
  mutate(SN_Estimates = c(rep(NA, length(spotifyR_TS_orig)), benchmark$mean)) %>% 
  tail(., 111)

# Calculating the benchmark accuracy
accuracy(spotifyR_tib_naïve$SN_Estimates, 
         spotifyR_tib_naïve$Actual_Stream_Counts)

# Plotting
ggplot(data = spotifyR_tib_naïve) +
  
  # Connected scatter plot
  geom_point(aes(Date, Actual_Stream_Counts),color = "black", size = .75) +
  geom_line(aes(Date, Actual_Stream_Counts),color = "black", lty = "dashed") +
  
  geom_point(aes(Date, SN_Estimates),color = "deeppink", size = .75) +
  geom_line(aes(Date, SN_Estimates),color = "deeppink", lty = "solid") +
  
  # Layout/Background
  annotate("rect", xmin = as.Date("2020-03-11"), xmax = as.Date("2020-06-29"), 
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#ef8a62") +
  
  # Limiting the range of the x-axis to the pandemic period
  scale_x_date(limits = c(as.Date("2020-03-11"), as.Date("2020-06-29")), 
               date_breaks = "7 day", date_labels = "%d. %b %y") +
  
  # Label numbers with SI prefixes
  scale_y_continuous(labels = label_number_si(accuracy = NULL)) +
  
  # Customizing the labels on both axes  
  labs(x = "\nStream Date", y = "Stream Counts per Day\n(Median)\n") +
  
  # Layout
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "#bdbdbd"),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

# Stationarity #

# Is the data stationary? If not: How many times does the data need to be 
# differenced?

ndif <- ndiffs(spotifyR_TS_orig)
print(ndif)

# Apparently, we have to difference our data only once. This appears reasonable
# based on the inspection of the connected scatter plot from above.

# For doing so, we log-transform the data, then we difference the data 
# (i.e., stabilizing the variance and the means; also known as de-trending and 
# de-seasonalizing)

spotifyR_TS_trans <- diff(log(spotifyR_TS_orig), differences = ndif)

# How many lags (i.e., repeating frequency pattern) does the period consist?
# 
lag_selection <- vars::VARselect(spotifyR_TS_trans)

print(lag_selection$selection)

# Lag of 7 seems reasonable since we can see in the connected scatter plot a 
# weekly pattern: 4-5 spikes per month (= 4-5 pattern of 7 days)  

lags <- 7

# ADF: Null hypo: Data has a unit-root. That is, we want reject the null 
# (i.e., p < .05)

spotifyR_TS_trans %>%
  # we choose "none" since we already have transformed the data
  ur.df(lags = lags, type = "none") %>%
  summary()

# Plotting

spotifyR_TS_trans %>%
  
  # Here we can use the autoplot-function since it provides everything we need. 
  # Otherwise, we had to convert (and manipulate) the time series-object to a 
  # data.frame-format. This would be, however, unnecessary additional work.
  
  autoplot() + 
  geom_point(size = .5) +
  
  # Annotating 
  geom_label(aes(x = decimal_date(as.Date(c("2019-07-01"))), y = 0.5, 
                 label = "Pre-Pandemic"), color = "#045a8d",
             size = 3) +
  
  # Layout/Background
  annotate("rect", xmin = 2019, xmax = decimal_date(as.Date(c("2020-03-10"))),
           ymin = -Inf, ymax = Inf, alpha = .08, fill = "#045a8d") +
  
  # Customizing the labels on both axes
  labs(x = "\nStream Period", 
       y = "Stream Counts per Day\n(log-transformed and differenced)\n") +
  
  # Layout
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

# Embedding #

spotifyR_TS_trans_embed <- embed(spotifyR_TS_trans,lags + 1) 



# ############### #
# Train/Test Sets #
# ############### #

# Regressand (aka: target, label, dependent variable)

y_train <-  spotifyR_TS_trans_embed[, 1] 

# Regressor (aka: input, independent variable)

X_train <- spotifyR_TS_trans_embed[, -1] 

# The actual test set with those suggested lags 

X_valid <- spotifyR_TS_trans_embed[nrow(spotifyR_TS_trans_embed), c(1:lags)] 

# Now we assign the actual final dataset that we want to estimate with our 
# models. In our case: The time series between March 11th, 2020 (day 71 of the year 
# 2020) and June 29th, 2020 (day 181 of the year 2020); this is the examined period 
# of the COVID-19 pandemic.        

y_test <- window(spotifyR_TS, start = c(2020, 71), end = c(2020, 181)) 

# ################# #
# Cross—validations #
# ################# #

# 10-fold cross-validation
rf_train_control <- trainControl(method = "repeatedcv", number = 10, 
                                 repeats = 5)

# We define the number of predictors in the mtry-object. This is the number 
# of randomly chosen splits at each tree. According to the suggestion of 
# Breiman and Cutler (2018) we should divide the predictors by 3 for 
# regression-approaches.

rf_tune_grid <- expand.grid(mtry = col(X_train/3), 
                            # using extra trees as random split points instead 
                            # of bootstrapped samples. Each tree uses the whole 
                            # training data.
                            splitrule = "extratrees", 
                            # tree complexity: default 5 for regressions
                            min.node.size = 5)

set.seed(61, sample.kind = "Rounding")

rf_kfold_cv <- train(data.frame(X_train), y_train, 
                     # faster Random Forest implementation 
                     method = "ranger", 
                     trControl = rf_train_control, tuneGrid = rf_tune_grid)

print(rf_kfold_cv$bestTune[,1])
################################################################################

# 10-fold cross-validation
knn_train_control <- trainControl(method = "repeatedcv", number = 10, 
                                  repeats = 5)

# Determining the optimal number of neighbors of data points with which the next 
# data point is estimated based on the resulting outcome values of the 
# nearest neighbors on average. Note, that the "knn" method of the caret-package
# provides here "regression" error metrics. The "best" k value provides here the
# lowest RMSE value.

knn_tune_grid <- expand.grid(k = c(1:25))

set.seed(61, sample.kind = "Rounding")

knn_kfold_cv <- train(data.frame(X_train), y_train, method = "knn", 
                      trControl = knn_train_control, tuneGrid = knn_tune_grid)
print(knn_kfold_cv$bestTune[,1])

# ########### #
# PREDICTIONS #
# ########### #

# Now, we save each estimate in a blank object (like a container that gets 
# filled) 

rf_estimates <- numeric(horizon)

set.seed(1, sample.kind = "Rounding")

# For-loop: Random Forest Training

for (i in 1:horizon) {
  
  set.seed(1857, sample.kind = "Rounding")
  
  rf_mod <- randomForest(X_train, y_train, mtry = rf_kfold_cv$bestTune[,1], 
                         nodesize = 5, replace = T, splitrule = "extratrees")
  
  # Here we fill the mentioned container with the predicted values
  
  rf_estimates[i] <- predict(rf_mod, X_valid)
  
  # The training data is here constantly updated to account for the time distance 
  # corresponding to the respective data point of the horizon.
  
  y_train <- y_train[-1] 
  
  X_train <- X_train[-nrow(X_train),] }

################################################################################

# Setting back the training objects. Ensuring a fresh start.

y_train <- spotifyR_TS_trans_embed[, 1] 
X_train <- spotifyR_TS_trans_embed[, -1] 

# Same procedure as above, however, adjusted to the kNN-algorithm 

knn_estimates <- numeric(horizon)

# For-loop: kNN Training

set.seed(1, sample.kind = "Rounding")

for (j in 1:horizon) {
  
  set.seed(1857, sample.kind = "Rounding")
  
  knn_estimates[j] <- knn.reg(train = data.frame(X_train), test = X_valid,
                              y = y_train, k = knn_kfold_cv$bestTune[,1])$pred 
  
  y_train <- y_train[-1] 
  
  X_train <- X_train[-nrow(X_train),] }

# ############## #
# Retransforming #
# ############## #

# Retransforming the estimates by taking the "anti-logs" (i.e., computing 
# exponents for every estimate).

rf_exponents_of_estimates <- exp(cumsum(rf_estimates))
knn_exponents_of_estimates <- exp(cumsum(knn_estimates))

# Extracting the last observation from the time-series.

last_observation <- as.vector(tail(spotifyR_TS_orig, 1))

# Getting the final values by retransforming them.

rf_retransformed_estimates <- last_observation * rf_exponents_of_estimates
knn_retransformed_estimates <- last_observation * knn_exponents_of_estimates

# Converting them into time series-format

rf_y_pred <- ts(rf_retransformed_estimates, start = c(2020, 71), 
                frequency = 365)
knn_y_pred <- ts(knn_retransformed_estimates, start = c(2020, 71), 
                 frequency = 365)
# ####### #
# Results #
# ####### #

# Bringing the original data and the estimates together

spotifyR_tib_star <- spotifyR_tib %>% 
  mutate(RF_Estimates = c(rep(NA, length(spotifyR_TS_orig)), rf_y_pred),
         kNN_Estimates = c(rep(NA, length(spotifyR_TS_orig)), knn_y_pred),
         SN_Estimates = c(rep(NA, length(spotifyR_TS_orig)), benchmark$mean))

# Extracting only the pandemic-period

test <-  tail(spotifyR_tib_star, horizon)

# Calculating the error metrics

fuzzyr.accuracy(test$RF_Estimates, test$Actual_Stream_Counts, test$SN_Estimates)

fuzzyr.accuracy(test$kNN_Estimates, test$Actual_Stream_Counts,test$SN_Estimates)

# Residuals
resids <- test$Actual_Stream_Counts - test$RF_Estimates
shapiro.test(resids)

# Since the residuals are normally distributed according to the Shapiro-Wilk-
# Test (W = 0.98609, p = 0.307), we can calculate the prediction intervals for 
# each point by multiplying the standard deviation of the residuals with the 
# z-score of the 0.025th and, respectively, the 0.975th percentile point.

SD_star <- sd(resids)

spotifyR_tib_star %<>% 
  mutate(RF_E_lo.95 = RF_Estimates  - (1.96 * SD_star),
         RF_E_hi.95 = RF_Estimates  + (1.96 * SD_star))

# Final Plot
# Creating a color-object

cols <- c("Actual Stream Counts (Median)" = "black",
          # Colorblind Green
          "Random Forest Predictions\nwithin 95% PI" = "#009E73",
          # Colorblind Orange
          "kNN Predictions" = "#E69F00",
          # Colorblind Pink
          "Seasonal-naïve Predictions" = "#CC79A7")

spotifyR_tib_star %>%
  filter(Date >= as.Date("2020-03-11")) %>%
  ggplot(aes(x = Date)) +
  
  # Connected scatter plot  
  geom_point(aes(y = Actual_Stream_Counts, 
                 color = "Actual Stream Counts (Median)"), size = .75) +
  geom_line(aes(y = Actual_Stream_Counts,
                color = "Actual Stream Counts (Median)"), lty = "dashed") +
  
  geom_point(aes(y = RF_Estimates,
                 color = "Random Forest Predictions\nwithin 95% PI"), 
             size = .75) +
  geom_line(aes(y = RF_Estimates,
                color = "Random Forest Predictions\nwithin 95% PI" ),
            lty = "solid") +
  
  geom_point(aes(y = kNN_Estimates, color = "kNN Predictions"), size = .75) +
  geom_line(aes(y = kNN_Estimates, color = "kNN Predictions"),lty = "solid") +
  
  geom_point(aes(y = SN_Estimates, color = "Seasonal-naïve Predictions"),
             size = .75) +
  geom_line(aes(y = SN_Estimates,color = "Seasonal-naïve Predictions"),
            lty = "solid") +
  
  # Prediction Interval  
  geom_ribbon(aes(y = RF_Estimates, ymin = RF_E_lo.95, ymax = RF_E_hi.95),
              fill = "#009E73", alpha = 0.2) +
  
  geom_vline(xintercept = as.numeric(as.Date("2020-03-11")),
             color = "#0072B2", size = 0.2, lty = "dashed") +
  geom_label(aes(x = as.Date("2020-05-03"), y = 13e4,
                 label = "Test Dataset\n(Pandemic Period)"  ),  color = "#ef8a62", 
             size = 6) +
  annotate("rect",  xmin = as.Date("2020-03-11"), xmax = as.Date("2020-06-29"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#ef8a62") +
  
  # Setting breaks on the x-axis
  scale_x_date(limits = c(as.Date("2020-03-11"), as.Date("2020-06-29")),
               date_breaks = "7 day", date_labels = "%d. %B %y") +
  
  # Label numbers with SI prefixes
  scale_y_continuous(labels = label_number_si(accuracy = NULL)) +
  
  # Legend colors
  scale_colour_manual(name = "Legend:", values = cols) +
  scale_fill_manual(name = "Legend:", values = cols) +
  
  # Customizing the labels on both axes
  labs(x = "\nDate of Streams", y = "Stream Counts per Day\n(Median)\n") +
  
  # Layout
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -1),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "#bdbdbd"),
        plot.margin = unit(c(.66, .33, .66, .33), "cm")) 

# ### #
# End #
# ### #

sessioninfo::session_info()