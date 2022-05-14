



#14.388 ML Topics-5 HW
#"Tom Harris et al."
#date: "May 2022"

if (!require(pacman))
  install.packages("pacman")
library(pacman)
pacman::p_load(
  forecast,
  ggplot2,
  dplyr,
  tidyr,
  stargazer,
  data.table,
  tableone,
  lattice,
  pwr,
  rcompanion,
  scales,
  plm,
  readxl,
  MatchIt,
  lfe,
  Synth,
  gsynth,
  panelView,
  CausalImpact,
  knitr,
  SCtools,
  tidyverse,
  remotes
)
remotes::install_github("kwuthrich/scinference")
library(scinference)

#### Data entry and cleaning ####

data(basque)
# saveRDS(basque, file = "basque_data.rds") #saving data to use on Kaggle

#Using the Basque dataset, we'll estimate the economic impact of terrorist conflict in the Basque country, an autonomous community in Spain, with the help of data from 17 other regions as well. The data can be found [here](https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/basque). Let's look at some facts about the data and the experimental design.

## Data Description:
#Dataset contains information from year 1955 - 1997
#Information about 18 Spanish regions is available
#One of which is average for the whole country of Spain (we'll remove that)
#The treatment year is considered to be year 1975
#The treatment region is "Basque Country (Pais Vasco)"
#The economic impact measurement variable is GDP per capita (in thousands)


#Using the Basque dataset, we'll estimate the economic impact of terrorist conflict in the
#Basque country, an autonomous community in Spain, with the help of data from 17 other regions
#as well.
#The data can be found [here](https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/basque).

unused <-
  c(
    "sec.agriculture",
    "sec.energy" ,
    "sec.industry" ,
    "sec.construction" ,
    "sec.services.venta" ,
    "sec.services.nonventa",
    "school.illit",
    "school.prim",
    "school.med",
    "school.high",
    "school.post.high",
    "popdens"
  )
basq_clean <- basque[, !(names(basque) %in% unused)]
basq_clean <- basq_clean %>%
  mutate(
    post = ifelse(year > 1975, 1, 0),
    treat = ifelse(regionname == "Basque Country (Pais Vasco)", 1, 0),
    regionname = as.factor(regionname)
  ) %>%
  filter(regionno != 1)

#### First Difference Estimate ####

#One approach to estimating the impact of terrorism in 1975 is a simple pre-post analysis:

basq_fdid <- basq_clean %>%
  filter(treat == 1)

ggplot(basq_fdid, aes(x = year, y = gdpcap)) +
  geom_line(color = "#F8766D") + theme_classic() +
  geom_vline(xintercept = 1975,
             color = "steelblue",
             linetype = "dashed") +
  labs(
    title = "GDP trend over years for Basque",
    y = "GDP per capita",
    x = "Years",
    color = "Region"
  ) +
  annotate(
    "text",
    x = 1970,
    y = 9,
    label = "Pre-period",
    size  = 5,
    color = "steelblue"
  ) +
  annotate(
    "text",
    x = 1980,
    y = 9,
    label = "Post-period",
    size  = 5,
    color = "steelblue"
  )

# Calculating first differences
f_did <- lm(data = basq_fdid, gdpcap ~ post)
stargazer(f_did, type = "text")


#The coefficient of post indicator suggests that there is an increase of GDP per
#capita by ~2,500$ from pre to post the terrorist conflict.


####  Difference in differences (DD): ####

#For this analysis, the control region was identified by spotting for the region
#that had the lowest variation in % difference of GDP across years between each
#region and Basque country.
#In this case, Cataluna region was recognized to be the best control region. Let's look at
#the GDP trend for test and control regions below:

#Picking the closest control group based on gdp
pre <- basq_clean %>%
  filter(post == 0) %>%
  left_join(dplyr::select(basq_clean[basq_clean$post == 0 &
                                       basq_clean$treat == 1,],
                          gdpcap, year),
            by = c("year" = 'year')) %>%
  mutate(perc_diff = (gdpcap.y - gdpcap.x) / gdpcap.y) %>%
  group_by(regionname) %>%
  summarise(gdp_var = abs(var(perc_diff))) %>%
  arrange(gdp_var)

# Validating assumption
did_data <- basq_clean %>%
  filter(regionname %in% c("Basque Country (Pais Vasco)", "Cataluna"))
ggplot(did_data, aes(x = year, y = gdpcap, group = regionname)) +
  geom_line(aes(color = regionname)) +
  theme_classic() +
  geom_vline(xintercept = 1975,
             color = "steelblue",
             linetype = "dashed") +
  labs(
    title = "GDP trend over years for different regions",
    y = "GDP per capita",
    x = "Years",
    color = "Region"
  ) +
  scale_color_manual(
    labels = c("Basque (treated)", "Cataluna (control)"),
    values = c("#00BFC4", "#F8766D")
  )

#GDP trend for Cataluna region goes hand in hand with Basque's GDP with an
#exception for a few years in the pre-period.

# Difference in differences
did <- lm(data = did_data, gdpcap ~ treat * post)
stargazer(did, type = "text")

#Looking at the estimate of the interaction variable suggests that the GDP per cap in
#Basque country reduced by 0.855 units because of the terrorist intervention that
#happened.

#### Causal Impact ####

#This is a methodology developed by Google.. The official documentation can be found [here]
#(https://google.github.io/CausalImpact/CausalImpact.html).

#The motivation to use Causal Impact methodology is that the Difference in
#differences is limited in the following ways:

#DD is traditionally based on a static regression model that assumes independent and
#identically distributed data despite the fact that the design has a temporal component
# Most DD analyses only consider two time points: before and after the intervention.
#In practice, we also have to consider the manner in which an effect evolves over time,
#especially its onset and decay structure

#The idea here is to use the trend in the control group to forecast the trend in the
#treated group which would be the trend if the treatment had not happened. Then the
#actual causal estimate would be the difference in the actual trend vs the counter-
#factual trend of the treated group that we predicted. Causal Impact uses Bayesian
#structural time-series models to explain the temporal evolution of an observed outcome.
#Causal Impact methodology is very close to the Synthetic control
#methodology we are going to see next.

#Control region in this case is considered to be Cataluna again. With the treated
#and control region's GDP in place, let's feed them to the Causal Impact function in
#R and look at the results.

# Causal Impact
basq_CI <- basq_clean %>%
  filter(regionname %in% c("Basque Country (Pais Vasco)", "Cataluna")) %>%
  mutate(date = as.Date(paste0(year, "-01", "-01"), format = "%Y-%m-%d")) %>%
  dplyr::select(date, regionname, gdpcap) %>%
  spread(regionname, gdpcap)
names(basq_CI) <- c("date", "Basque", "another")
pre.period <- as.Date(c("1955-01-01", "1975-01-01"))
post.period <- as.Date(c("1976-01-01", "1997-01-01"))
impact <- CausalImpact(basq_CI, pre.period, post.period)
summary(impact)

#The Absolute effect is the difference in GDP per capita between what the actual GDP
#was after the treatment and what the GDP would have been if the treatment had not
#occurred. From the results, we can see the absolute effect gives us a value of -0.76
#which means that the GDP per capita reduced by 0.76 units i.e. 8.8% because of the
#terrorist conflict that happened in Basque country. This is almost equal to what we
#saw using Difference in Differences method.

#### Synthetic Control: ####

#Synthetic control is a technique which is very similar to Causal Impact in

#The difference between Synthetic Control and Causal Impact is that
#Synthetic Control uses only pre-treatment variables for matching while
#Causal Impact uses the full pre and post-treatment time series of predictor
#variables for matching. Let's look at a plot for GDP trend in all the 17 other
#regions in the data.

basq_synth <- basq_clean %>%
  rename(Y = gdpcap) %>%
  mutate(regionname = as.character(regionname))
ggplot(basq_synth, aes(x = year, y = Y, group = regionno)) +
  geom_line(aes(color = as.factor(treat), size = as.factor(treat))) +
  geom_vline(xintercept = 1975,
             linetype = "dashed",
             color = "steelblue") + theme_classic() +
  labs(
    title = "GDP trend over years for all regions",
    y = "GDP per capita",
    x = "Years",
    color = "Treatment group"
  ) +
  scale_color_manual(labels = c("Control", "Treated"),
                     values = c("#F8766D", "#00BFC4")) +
  scale_size_manual(values = c(0.5, 1.5), guide = 'none')



#As seen from the plot above, all the control regions have a similar upward
#trend in GDP as Basque country's in the pre-period.

#The implementation of synthetic control on this problem statement has already
#been given in the official documentation of the package found
#[here](https://cran.r-project.org/web/packages/Synth/Synth.pdf).

# synth
sink(nullfile())

dataprep.out <-
  dataprep(
    foo = basque
    ,
    predictors = c(
      "school.illit",
      "school.prim",
      "school.med",
      "school.high",
      "school.post.high"
      ,
      "invest"
    )
    ,
    predictors.op = c("mean")
    ,
    dependent     = c("gdpcap")
    ,
    unit.variable = c("regionno")
    ,
    time.variable = c("year")
    ,
    special.predictors = list(
      list("gdpcap", 1960:1975, c("mean")),
      list("sec.agriculture", seq(1961, 1975, 2), c("mean")),
      list("sec.energy", seq(1961, 1975, 2), c("mean")),
      list("sec.industry", seq(1961, 1975, 2), c("mean")),
      list("sec.construction", seq(1961, 1975, 2), c("mean")),
      list("sec.services.venta", seq(1961, 1975, 2), c("mean")),
      list("sec.services.nonventa", seq(1961, 1975, 2), c("mean")),
      list("popdens", seq(1961, 1975, 2), c("mean"))
    )
    ,
    treatment.identifier  = 17
    ,
    controls.identifier   = c(2:16, 18)
    ,
    time.predictors.prior = c(1964:1975)
    ,
    time.optimize.ssr     = c(1960:1975)
    ,
    unit.names.variable   = c("regionname")
    ,
    time.plot            = c(1955:1997)
  )
sink()

synth.out = synth(dataprep.out)


# The output from synth opt can be flexibly combined with the output from dataprep to
# compute other quantities of interest for example, the period by period discrepancies
# between the treated unit and its synthetic control unit can be computed:

gaps <-
  dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps

gaps_2 <- as.data.frame(gaps)
gaps_2 <- rownames_to_column(gaps_2, var = "Year")
gaps_post <- subset(gaps_2, gaps_2$Year > 1975)


#We can now plot this:
# Two native plotting functions.
# Path.plot() plots the synthetic against the actual treated unit data.
path.plot(
  dataprep.res = dataprep.out,
  synth.res = synth.out,
  Xlab = "Year",
  Ylab = "GDP Per Capita"
)
abline(v = 1975, lty = 2, col = "steelblue")
title("Actual vs Synthetic GDP for Basque")
# Gaps.plot() shows the deviation between the synthetic and the actual over time.
gaps.plot(
  dataprep.res = dataprep.out,
  synth.res = synth.out,
  Xlab = "Year",
  Ylab = "Diff in GDP Per Capita"
)
abline(v = 1975, lty = 2, col = "red")



# What is the Effect size?
#Simple average of differences between treatment and (synthetic) control post 1975:
simple_mean_effect <- mean(gaps_post$`17`)
simple_mean_effect

#Elsewhere, we've seen it's typical to calculate effect size from root-mean-squared differences
#Here this include the pre data (which we don't quite understand!)
effect_size_all <-
  -sqrt(mean((
    dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  ) ^ 2))
effect_size_all

#So, here we've done the same but just for the post differences.
effect_size_post <- -sqrt(mean((gaps_post$`17`) ^ 2))
effect_size_post



labels <- c("DiD", "Causal Impact", "Synthetic Control")
values <- c(-0.85, -0.76, simple_mean_effect)
values_df <- data.frame(labels, values)
names(values_df) <- c("Method", "Change in GDP per captia")
kable(values_df)

#What makes up our synthetic control?
#Here, we see c.85% weight on Catalonia and 15% weight on Madrid.
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)
print(synth.tables)


#### Synthetic control: Permutation test ####

#We can conduct inference via placebo tests which help inform us of the significance of
#the above effect size.
# These tests involve applying the synthetic control method after reassigning the
# intervention in the data to units and periods where the intervention did not occur


## run the generate.placebos command to reassign treatment status
## to each unit listed as control, one at a time, and generate their
## synthetic versions. Sigf.ipop = 2 for faster computing time.
## Increase to the default of 5 for better estimates.
tdf <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2)

## Plot the gaps in outcome values over time of each unit --
## treated and placebos -- to their synthetic controls
p <-
  plot_placebos(
    tdf,
    discard.extreme = T,
    mspe.limit = 20,
    xlab = 'Year',
    ylab = 'Gap in real per-capita GDP (000s of US$1986)',
    title = 'Intervention and Placebo Effects'
  )
p




# Test how extreme was the observed treatment effect given the placebos:
ratio <- mspe.test(tdf)
ratio$p.val

1 - (14 / 17)
#This is simply calculating: 1 - (14/17)
#i.e. in how many of the other regions did we observe treatment effect sizes larger
#than what we observed for the Basque Country

#Plotting mspe's for each region
mspe.plot(tdf, discard.extreme = F)

#Doing placebo test using another method:

# inference via placebo tests


# These tests involve applying the synthetic control method after reassigning the intervention in the
# data to units and periods where the intervention did not occur

store <- matrix(NA, length(1955:1997), 17)
colnames(store) <- unique(basque$regionname)[-1]

# run placebo test
for (iter in 2:18)
{
  dataprep.out <-
    dataprep(
      foo = basque,
      predictors = c(
        "school.illit" ,
        "school.prim" ,
        "school.med" ,
        "school.high" ,
        "school.post.high" ,
        "invest"
      ) ,
      predictors.op = "mean" ,
      time.predictors.prior = 1964:1969 ,
      special.predictors = list(
        list("gdpcap" , 1960:1969 , "mean"),
        list("sec.agriculture" ,      seq(1961, 1969, 2), "mean"),
        list("sec.energy" ,           seq(1961, 1969, 2), "mean"),
        list("sec.industry" ,         seq(1961, 1969, 2), "mean"),
        list("sec.construction" ,     seq(1961, 1969, 2), "mean"),
        list("sec.services.venta" ,   seq(1961, 1969, 2), "mean"),
        list("sec.services.nonventa" , seq(1961, 1969, 2), "mean"),
        list("popdens", 1969, "mean")
      ),
      dependent = "gdpcap",
      unit.variable = "regionno",
      unit.names.variable = "regionname",
      time.variable = "year",
      treatment.identifier = iter,
      controls.identifier = c(2:18)[-iter + 1],
      time.optimize.ssr = 1960:1969,
      time.plot = 1955:1997
    )


  dataprep.out$X1["school.high", ] <-
    dataprep.out$X1["school.high", ] + dataprep.out$X1["school.post.high", ]
  dataprep.out$X1 <-
    as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1) == "school.post.high"), ])
  dataprep.out$X0["school.high", ] <-
    dataprep.out$X0["school.high", ] + dataprep.out$X0["school.post.high", ]
  dataprep.out$X0 <-
    dataprep.out$X0[-which(rownames(dataprep.out$X0) == "school.post.high"), ]

  lowest  <- which(rownames(dataprep.out$X0) == "school.illit")
  highest <- which(rownames(dataprep.out$X0) == "school.high")

  dataprep.out$X1[lowest:highest, ] <-
    (100 * dataprep.out$X1[lowest:highest, ]) / sum(dataprep.out$X1[lowest:highest, ])
  dataprep.out$X0[lowest:highest, ] <-
    100 * scale(dataprep.out$X0[lowest:highest, ],
                center = FALSE,
                scale = colSums(dataprep.out$X0[lowest:highest, ]))

  # run synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # store gaps
  store[, iter - 1] <-
    dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# now do figure
data <- store
rownames(data) <- 1955:1997

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1955:1997
gap.end.pre  <- which(rownames(data) == "1969")

#  MSPE Pre-Treatment
mse        <- apply(data[gap.start:gap.end.pre, ] ^ 2, 2, mean)
basque.mse <- as.numeric(mse[16])


# Exclude states with 5 times higher MSPE than basque
data <- data[, mse < 100 * basque.mse]
Cex.set <- .75

# Plot
plot(
  years,
  data[gap.start:gap.end, which(colnames(data) == "Basque Country (Pais Vasco)")],
  ylim = c(-2, 2),
  xlab = "year",
  xlim = c(1955, 1997),
  ylab = "gap in real per-capita GDP (1986 USD, thousand)",
  type = "l",
  lwd = 2,
  col = "black",
  xaxs = "i",
  yaxs = "i"
)

# Add lines for control states
for (i in 1:ncol(data)) {
  lines(years, data[gap.start:gap.end, i], col = "gray")
}

## Add Basque Line
lines(years, data[gap.start:gap.end, which(colnames(data) == "Basque Country (Pais Vasco)")], lwd =
        2, col = "black")

# Add grid
abline(v = 1975, lty = "dotted", lwd = 2)
abline(h = 0, lty = "dashed", lwd = 2)
legend(
  "bottomright",
  legend = c("Basque country", "control regions"),
  lty = c(1, 1),
  col = c("black", "gray"),
  lwd = c(2, 1),
  cex = .8
)
arrows(1969, -1.5, 1972, -1.5, col = "black", length = .1)
text(1964, -1.5, "Terrorism Onset", cex = Cex.set)
abline(v = 1955)
abline(v = 1997)
abline(h = -2)
abline(h = 2)


##### Implementing scinference package: ####

#See here for GitHub link: https://github.com/kwuthrich/scinference

#GitHub notes for implementation:

#' Inference estimation_methods for synthetic control
#'
#' The function scinference implements the inference methods for synthetic controls proposed by Chernozhukov et al. (2020a,b).
#' The methods apply to a canonical synthetic control setup with 1 treated unit and J control units.
#' The treated unit is untreated for the first T0 periods and treated for the remaining T1=T-T0 periods.
#'
#' Y1 outcome data for treated unit (T x 1 vector)
#' Y0 outcome data for control units (T x J matrix)
#' T1 number of post-treatment periods
#' T0 number of pre-treatment periods
#' inference_method inference method; conformal inference ("conformal",default), t-test ("ttest")
#' alpha significance level; default alpha = 0.1
#' ci logical, indicating whether pointwise confidence intervals are reported; default ci=FALSE}
#' theta0 null hypothesis for treatment effect trajectory (T1 x 1 vector or scalar if constant effects null); default theta0}=0
#' estimation_method estimation method; difference-in-differences ("did"), synthetic control ("sc", default), constrained lasso ("classo"). Note that constrained lasso is not implemented for the t-test.
#' permutation_method permutation method; moving block permutations ("mb", default), iid permutations ("iid")
#' ci_grid grid for the confidence interval
#' n_perm number of permutation (relevant for iid permutations); default = 5000
#' lsei_type option for lsei (package limSolve) used for sc; default = 1
#' K K>1 number of cross-fits for t-test; default = 2
#' conformal inference: p-value for testing the null that theta=theta0 and pointwise CI (lower bounds and upper bounds) if ci=TRUE; t-test: ATT estimate, standard error, and confidence interval

#### Getting data into the required shape: ####

unused <-
  c(
    "sec.agriculture",
    "sec.energy" ,
    "sec.industry" ,
    "sec.construction" ,
    "sec.services.venta" ,
    "sec.services.nonventa",
    "school.illit",
    "school.prim",
    "school.med",
    "school.high",
    "school.post.high",
    "popdens"
  )
basq_clean <- basque[, !(names(basque) %in% unused)]
basq_clean <- basq_clean %>%
  mutate(
    post = ifelse(year > 1975, 1, 0),
    treat = ifelse(regionname == "Basque Country (Pais Vasco)", 1, 0),
    regionname = as.factor(regionname)
  ) %>%
  filter(regionno != 1)



basq_CI <- basq_clean %>%
  mutate(date = as.Date(paste0(year, "-01", "-01"), format = "%Y-%m-%d")) %>%
  dplyr::select(date, regionname, gdpcap) %>%
  spread(regionname, gdpcap)


Y1data <- basq_clean %>%
  mutate(date = as.Date(paste0(year, "-01", "-01"), format = "%Y-%m-%d")) %>%
  filter(regionname %in% c("Basque Country (Pais Vasco)")) %>%
  dplyr::select(date, regionname, gdpcap) %>%
  spread(regionname, gdpcap)

Y0data <- dplyr::select(basq_CI, c(-1, -5))
Y1data <- dplyr::select(Y1data, c(-1))

#Defining T1, T0, Y1, Y0 as per package:
T1 <- 22
T0 <- 21
Y1 <- data.matrix(Y1data)
Y0 <- data.matrix(Y0data)

#Checking correct length of time periods
length(Y1)
nrow(Y0) #looks good


#### Running prep code from scinference GitHub: ####

# Moving block permutations
movingblock <-
  function(Y1,
           Y0,
           T1,
           T0,
           theta0,
           estimation_method,
           lsei_type) {
    T01 <- T0 + T1

    Y1_0 <- Y1
    Y1_0[(T0 + 1):T01] <- Y1[(T0 + 1):T01] - theta0

    if (estimation_method == "classo") {
      u.hat <- classo(Y1_0, Y0)$u.hat
    }
    if (estimation_method == "sc") {
      u.hat <- sc(Y1_0, Y0, lsei_type)$u.hat
    }
    if (estimation_method == "did") {
      u.hat <- did(Y1_0, Y0)$u.hat
    }
    sub.size  <- T1
    u.hat.c   <- c(u.hat, u.hat)
    S.vec     <- matrix(NA, T01, 1)
    for (s in 1:(T01)) {
      S.vec[s, 1]  <- sum(abs(u.hat.c[s:(s + sub.size - 1)]))
    }
    p <- mean(S.vec >= S.vec[T0 + 1])
    return(p)
  }

# All/iid permutations (use random subsample of size n_perm all permutations)
iid <-
  function(Y1,
           Y0,
           T1,
           T0,
           theta0,
           estimation_method,
           n_perm,
           lsei_type) {
    T01 <- T0 + T1

    Y1_0 <- Y1
    Y1_0[(T0 + 1):T01] <- Y1[(T0 + 1):T01] - theta0

    if (estimation_method == "classo") {
      u.hat <- classo(Y1_0, Y0)$u.hat
    }
    if (estimation_method == "sc") {
      u.hat <- sc(Y1_0, Y0, lsei_type)$u.hat
    }
    if (estimation_method == "did") {
      u.hat <- did(Y1_0, Y0)$u.hat
    }
    post.ind  <- ((T0 + 1):T01)
    pre.ind   <- (1:T0)
    S.vec     <- matrix(NA, n_perm, 1)

    Sq <- sum(abs(u.hat[post.ind]))
    for (r in 1:n_perm) {
      u.hat.p     <- u.hat[sample(1:T01, replace = F)]
      S.vec[r, 1]  <- sum(abs(u.hat.p[post.ind]))
    }
    p <- 1 / (n_perm + 1) * (1 + sum(S.vec >= Sq))
    return(p)
  }

# Confidence interval via test inversion
confidence_interval <-
  function(Y1,
           Y0,
           T1,
           T0,
           estimation_method,
           alpha,
           ci_grid,
           lsei_type) {
    lb <- ub <- rep(NA, T1)
    for (t in 1:T1) {
      indices   <- c(1:T0, T0 + t)
      Y1_temp   <- Y1[indices]
      Y0_temp   <- Y0[indices, ]

      ps_temp <- rep(NA, length(ci_grid))
      for (ind in 1:length(ci_grid)) {
        Y1_0_temp <- Y1_temp
        Y1_0_temp[(T0 + 1)] <- Y1_temp[(T0 + 1)] - ci_grid[ind]
        if (estimation_method == "classo") {
          u_hat <- classo(Y1_0_temp, Y0_temp)$u.hat
        }
        if (estimation_method == "sc") {
          u_hat <- sc(Y1_0_temp, Y0_temp, lsei_type)$u.hat
        }
        if (estimation_method == "did") {
          u_hat <- did(Y1_0_temp, Y0_temp)$u.hat
        }
        ps_temp[ind] <- mean(abs(u_hat) >= abs(u_hat[(T0 + 1)]))
      }
      ci_temp <- ci_grid[ps_temp > alpha]
      lb[t]     <- min(ci_temp, na.rm = TRUE)
      ub[t]     <- max(ci_temp, na.rm = TRUE)
    }

    return(list(lb = lb, ub = ub))
  }

# difference-in-differences
did <- function(Y1, Y0) {
  u.hat <- Y1 - mean(Y1 - rowMeans(Y0)) - rowMeans(Y0)
  return(list(u.hat = u.hat))
}

# synthetic control
sc <- function(Y1, Y0, lsei_type) {
  J <- dim(Y0)[2]
  e <- matrix(1, 1, J)
  f <- 1
  g <- diag(x = 1, J, J)
  h <- matrix(0, J, 1)
  w.hat <-
    limSolve::lsei(
      A = Y0,
      B = Y1,
      E = e,
      F = f,
      G = g,
      H = h,
      type = lsei_type
    )$X
  u.hat <- Y1 - Y0 %*% w.hat
  return(list(u.hat = u.hat, w.hat = w.hat))
}

# constrained lasso
classo <- function(Y1, Y0) {
  J         <- dim(Y0)[2]
  w         <- CVXR::Variable((J + 1))
  objective <- CVXR::Minimize(mean((Y1 - cbind(1, Y0) %*% w) ^ 2))
  prob      <-
    CVXR::Problem(objective, constraints = list(sum(abs(w[2:(J + 1)])) <= 1))
  result    <- CVXR::solve(prob)
  w.hat     <- result$getValue(w)
  u.hat     <- Y1 - cbind(1, Y0) %*% w.hat
  return(list(u.hat = u.hat, w.hat = w.hat))
}


scinference <-
  function(Y1,
           Y0,
           T1,
           T0,
           inference_method = "conformal",
           alpha = 0.1,
           ci = FALSE,
           theta0 = 0,
           estimation_method = "sc",
           permutation_method = "mb",
           ci_grid = NULL,
           n_perm = 5000,
           lsei_type = 1,
           K = 2) {
    # preliminaries
    if (length(Y1) != (T0 + T1))
      stop("length of Y1 needs to be equal to T")
    if (dim(Y0)[1] != (T0 + T1))
      stop("number of rows in Y0 needs to be equal to T")
    if (!(inference_method %in% c("conformal", "ttest")))
      stop("The selected inference method is not available")

    if (inference_method == "conformal") {
      if (!(estimation_method %in% c("did", "sc", "classo")))
        stop("The selected estimation method is not implemented for conformal inference")
      if (!(permutation_method %in% c("iid", "mb")))
        stop("The selected class of permutations is not available")
      if (length(theta0) != 1) {
        if (length(theta0) != T1) {
          stop("length of theta0 should be T1")
        }
      }

      # p-value for overall null hypothesis

      if (permutation_method == "mb") {
        p_val <-
          movingblock(
            Y1 = Y1,
            Y0 = Y0,
            T1 = T1,
            T0 = T0,
            theta0 = theta0,
            estimation_method = estimation_method,
            lsei_type = lsei_type
          )
      }
      if (permutation_method == "iid") {
        p_val <-
          iid(
            Y1 = Y1,
            Y0 = Y0,
            T1 = T1,
            T0 = T0,
            theta0 = theta0,
            estimation_method = estimation_method,
            n_perm = n_perm,
            lsei_type = lsei_type
          )
      }

      # pointwise confidence intervals

      if (ci == TRUE) {
        if (is.null(ci_grid)) {
          stop("no grid specified for confidence interval")
        }
        obj <-
          confidence_interval(
            Y1 = Y1,
            Y0 = Y0,
            T1 = T1,
            T0 = T0,
            estimation_method = estimation_method,
            alpha = alpha,
            ci_grid = ci_grid,
            lsei_type = lsei_type
          )
        ub <- obj$ub
        lb <- obj$lb
      } else {
        lb <- NA
        ub <- NA
      }
      return(list(p_val = p_val, lb = lb, ub = ub))

    }

    if (inference_method == "ttest") {
      if (!(estimation_method %in% c("did", "sc")))
        stop("The selected estimation method is not implemented for the t-test")
      if (K == 1)
        stop("K must be strictly than 1")  # This is a typo in the package, it should read "strictly greater"
      if (estimation_method == "did") {
        obj <- did.cf(Y1, Y0, T1, T0, K)
      }
      if (estimation_method == "sc") {
        obj <- sc.cf(Y1, Y0, T1, T0, K, lsei_type)
      }

      att <- obj$tau.hat
      se  <- obj$se.hat
      lb  <- att - qt(1 - alpha / 2, df = K - 1) * se
      ub  <- att + qt(1 - alpha / 2, df = K - 1) * se

      return(list(
        att = att,
        se = se,
        lb = lb,
        ub = ub
      ))

    }

  }


# t-test

sc.cf <- function(Y1, Y0, T1, T0, K, lsei_type) {
  T01   <- T0 + T1
  r     <- min(floor(T0 / K), T1)

  Y1.pre  <- Y1[1:T0]
  Y0.pre  <- Y0[1:T0, ]
  Y1.post <- Y1[(T0 + 1):T01]
  Y0.post <- Y0[(T0 + 1):T01, ]

  tau.mat <- matrix(NA, K, 1)
  for (k in 1:K) {
    Hk            <- (T0 - (r * K)) + seq((k - 1) * r + 1, k * r, 1)
    w.Hk          <-
      sc(Y1.pre[-Hk], Y0.pre[-Hk, ], lsei_type = lsei_type)$w.hat
    tau.mat[k, 1]  <-
      mean(Y1.post - Y0.post %*% w.Hk) - mean(Y1.pre[Hk] - Y0.pre[Hk, ] %*% w.Hk)
  }

  tau.hat <- mean(tau.mat)
  se.hat  <- sqrt(1 + ((K * r) / T1)) * sd(tau.mat) / sqrt(K)
  t.hat   <- tau.hat / se.hat # this has a t_{K-1} distribution

  return(list(
    t.hat = t.hat,
    tau.hat = tau.hat,
    se.hat = se.hat
  ))
}

did.cf <- function(Y1, Y0, T1, T0, K) {
  T01   <- T0 + T1
  r     <- min(floor(T0 / K), T1)

  Y1.pre  <- Y1[1:T0]
  Y0.pre  <- Y0[1:T0, ]
  Y1.post <- Y1[(T0 + 1):T01]
  Y0.post <- Y0[(T0 + 1):T01, ]

  tau.mat <- matrix(NA, K, 1)
  for (k in 1:K) {
    Hk            <- (T0 - (r * K)) + seq((k - 1) * r + 1, k * r, 1)
    tau.mat[k, 1]  <-
      mean(Y1.post - rowMeans(Y0.post)) - mean(Y1.pre[Hk] - rowMeans(Y0.pre[Hk, ]))
  }

  tau.hat <- mean(tau.mat)
  se.hat  <- sqrt(1 + ((K * r) / T1)) * sd(tau.mat) / sqrt(K)
  t.hat   <- tau.hat / se.hat # this has a t_{K-1} distribution

  return(list(
    t.hat = t.hat,
    tau.hat = tau.hat,
    se.hat = se.hat
  ))
}


#### Using scinference on our data: ####


# Here, we implement the permutation test from scinference.
# It permutes the estimated gaps between the Basque Country and the
# synthetic Basque Country in the pre-treatment and post-treatment periods.
# It relies on the long time series of outcome data (43 periods), which is
# also necessary for synthetic control. In the iid case, if we had just 4
# periods, there would only be 4!/(2! * 2!) = 6 permutations of the pre and
# post status for the periods. But with 43 periods (22 treatment), there are
# 10^12 i.i.d. permutations. But considering every i.i.d. permutation would
# overstate our confidence in the treatment effect because post period outcomes
# are likely to be similar to one another due to autocorrelation. So we use
# the moving block method: simply consider each of the 43 possible start dates
# for treatment, and designate the 22 following periods as treated (including
# the first 22-x periods if the placebo treatment starts at time x > 22).

# There's a challenge in comparing a placebo that includes two disconnected
# segments (i.e. 1-9 and 31-43) to the true treatment that occurs from time
# 21 to 43 because the true treatment is still more likely to be extreme due
# to autocorrelation, but this issue is much less severe than in the i.i.d
# case.


scinference(
  Y1,
  Y0,
  T1,
  T0,
  inference_method = "conformal",
  #using conformal method
  alpha = 0.1,
  ci = FALSE,
  theta0 = 0,
  estimation_method = "sc",
  #using synthetic control method
  permutation_method = "mb",
  #using moving block method (default)
  ci_grid = NULL,
  # n_perm = 5000,
  lsei_type = 1,
  K = 2
)

# Here, we get a pvalue of 0.3

scinference(
  Y1,
  Y0,
  T1,
  T0,
  inference_method = "ttest",
  #using ttest method
  alpha = 0.1,
  ci = FALSE,
  theta0 = 0,
  estimation_method = "sc",
  #using synthetic control method
  permutation_method = "mb",
  #using moving block method (default)
  ci_grid = NULL,
  # n_perm = 5000,
  lsei_type = 1,
  K = 2
)

#Here, we get an ATT effect of -0.633 with a se of 0.248
#Therefore, the lb of the confidence interval is -2.2
#ub is 0.933

(0.9355857--0.6330696) / 0.2484506

#This is because the t stat here is 6.31 which comes from alpha/2 and a DOF of 1
