
library(here)
library(tidyverse)
library(rio)
library(brms)
library(tseries)
library(tidyr)
library(stats)
library(lavaan)
library(sjPlot)
library(performance)
library(lmtest)
library(texreg)
library(effects)
library(robustlmm)
library(semfindr)
library(MVN)



d <- rio::import(
  here::here(
    "data", "raw", "psicometria_ema.xlsx"
  )
)

foo <- d[, 3]


# Rosemberg 10-20 --------------------------------------------------------------
rosenberg_items <- d[, c(10:16, 18:20)]
colnames(rosenberg_items) <- paste0("ros_", 1:10)

# DASS-21 ----------------------------------------------------------------------
dass21_items <- d[, c(21:42)]
dass21_items <- dass21_items[, c(1:15, 17:22)]
colnames(dass21_items) <- paste0("dass_", 1:21)

# SCS --------------------------------------------------------------------------
# Self-compassion Scale
scs_items <- d[, c(43:69)]
scs_items <- scs_items[, c(1:19, 21:27)]
colnames(scs_items) <- paste0("scs_", 1:26)

# RSCS -------------------------------------------------------------------------
# Relational Self-Compassion Scale
rscs_items <- d[, c(70:86)]
rscs_items <- rscs_items[, c(1:3, 5:17)]
colnames(rscs_items) <- paste0("rscs_", 1:16)

# neuroticism ------------------------------------------------------------------
# NEO-FFI60
neuro_items <- d[, c(87:99)]
neuro_items <- neuro_items[, c(1:6, 8:13)]
colnames(neuro_items) <- paste0("neuro_", 1:12)

# DERS -------------------------------------------------------------------------
# Difficulties in Emotion Regulation Scale (DERS)
ders_items <- d[, c(100:136)]
ders_items <- ders_items[, c(1:32, 34:37)]
colnames(ders_items) <- paste0("ders_", 1:36)

# EGO --------------------------------------------------------------------------
# Non Attachment To Ego Scale
ego_items <- d[, c(137:144)]
ego_items <- ego_items[, c(1:2, 4:8)]
colnames(ego_items) <- paste0("ego_", 1:7)

# SCL90 ------------------------------------------------------------------------
# Symptom Checklist-90 (SCL90)
scl90_items <- d[, c(145:235)]
scl90_items <- scl90_items[, c(1:87, 89:91)]
colnames(scl90_items) <- paste0("scl90_", 1:90)

# Merge dfs --------------------------------------------------------------------

df <- cbind(
  rosenberg_items, dass21_items, scs_items, rscs_items, neuro_items, 
  ders_items, ego_items, scl90_items
)
df$id <- d[, 3]



# --------------------------

mood <- rio::import("mpath_ema.xlsx")
quest <- rio::import("quest_ema_self_com.xlsx")

# bysubj_mood <- mood |> 
#   group_by(id) |> 
#   summarize(
#     mood_var = var(positive_affect)
#   )
# 
# plot(density(bysubj_mood$mood_var, na.rm=T))


df_wide <- mood %>% 
  pivot_wider(names_from = time_window, values_from = positive_affect)

# df_wide <- df_wide[!duplicated(df_wide$id), ]

# df_wide <- df_wide[complete.cases(df_wide), ] 
# df_autocorr <- df_wide %>% 
#   group_by(id) %>% 
#   summarize(acorr = acf(as.numeric(unlist(cur_data())), lag.max = 1, plot = FALSE)$acf[2])
# 
# hist(df_autocorr$acorr)
# unique(quest$dass_21)


# Select DASS21 items
dass21_df <- quest |> 
  select(all_of(starts_with("dass_")))

neuro_df <- quest |> 
  select(all_of(starts_with("neuro_")))

quest$neuro <- rowSums(neuro_df)

# Recode responses
dass21_df <- dass21_df %>%
  mutate_all(
    ~ recode(
      .,
      "Non mi è mai accaduto"                 = 0L,
      "Mi è capitato qualche volta"           = 1L,
      "Mi è capitato con una certa frequenza" = 2L,
      "Mi è capitato quasi sempre"            = 3L
    )
  )

# DASS-21 Depression: 3, 5, 10, 13, 16, 17, 21 
quest$dass_d <- dass21_df$dass_3 + dass21_df$dass_5 + dass21_df$dass_10 + 
  dass21_df$dass_13 + dass21_df$dass_16 + dass21_df$dass_17 + dass21_df$dass_21 

# Stress 1, 6, 8, 11, 12, 14, 18 
quest$dass_s <- dass21_df$dass_1 + dass21_df$dass_6 + dass21_df$dass_8 + 
  dass21_df$dass_11 + dass21_df$dass_12 + dass21_df$dass_14 + dass21_df$dass_18 

# Anxiety 2, 4, 7, 9, 15, 19, 20 
quest$dass_a <- dass21_df$dass_2 + dass21_df$dass_4 + dass21_df$dass_7 + 
  dass21_df$dass_9 + dass21_df$dass_15 + dass21_df$dass_19 + dass21_df$dass_20 


new_quest_df <- data.frame(
  id = quest$id,
  dass_d = quest$dass_d,
  dass_a = quest$dass_a,
  dass_s = quest$dass_s,
  neuro = quest$neuro
)

# Remove duplicated rows
new_quest_df <- new_quest_df[!duplicated(new_quest_df$id), ]

# dd <- inner_join(df_autocorr, new_quest_df, by = "id")
# dd <- dd[complete.cases(dd), ] 
# 
# plot(dd$dass_d, dd$acorr)
# 
# plot(density(dd$acorr))
# 
# 
# fm <- lm(acorr ~ dass_d, dd)
# summary(fm)
# 
# mod <- brm(
#   acorr ~ dass_d,
#   family = student(),
#   data = dd,
#   backend = "cmdstanr"
# )
# 
# summary(mod)

mood$sex <- stringr::str_sub(mood$id, -1, -1)
mood$is_female <- ifelse(mood$sex == "f", 1, 0)

dd <- left_join(mood, new_quest_df, by = "id")

# Check gender effect by comparing the means of the two groups -----------------
bysubj_df <- dd |> 
  group_by(id, sex) |> 
  summarize(
    pos_aff = mean(positive_affect)
  )

t.test(pos_aff ~ sex, bysubj_df)

# Complete pooling -------------------------------------------------------------
# Regression analysis without distinguishing between and withing effects

fm0 <- lm(positive_affect ~ time_window, data = dd)
summary(fm0)

# Multiple R-squared:  0.003415


# No pooling -------------------------------------------------------------------

fm1 <- lm(positive_affect ~ time_window + +as.factor(id), data = dd)
summary(fm1)

# Multiple R-squared:  0.8668

lrtest(fm0, fm1)

# Partial Pooling: random intercept --------------------------------------------

fm2 <- lmer(
  positive_affect ~ time_window + (1 | id), 
  data = dd)

# see group coefficients
model_coefs <- coef(fm2)$id %>% 
  rename(Intercept = `(Intercept)`, Slope = "time_window") %>% 
  rownames_to_column("id")

# see coefficients
model_coefs |> 
  head()


df_rani <- left_join(dd, model_coefs, by = "id")

model_coef_plot <- df_rani |> 
  ggplot(
  mapping = aes(
    x = time_window,
    y = positive_affect
    # colour = id
  )
) +
  geom_point(na.rm = T, alpha = 0.01) +
  geom_abline(
    aes(
      intercept = Intercept,
      slope = Slope, alpha = 0.01
      # colour = id
    ),
    size = .5
  ) 
model_coef_plot



# Partial Pooling: Random Intercepts and Slopes --------------------------------

fm3 <- lmer(
  positive_affect ~ time_window + (1 + time_window | id), 
  data = dd,
  control = lmerControl(optimizer ="Nelder_Mead"))

# see group coefficients
model_coefs <- coef(fm3)$id %>% 
  rename(Intercept = `(Intercept)`, Slope = "time_window") %>% 
  rownames_to_column("id")

# see coefficients
model_coefs |> 
  head()


df_rans <- left_join(dd, model_coefs, by = "id")


model_coef_plot <- df_rans |> 
  ggplot(
    mapping = aes(
      x = time_window,
      y = positive_affect
      # colour = id
    )
  ) +
  geom_point(na.rm = T, alpha = 0.01) +
  geom_abline(
    aes(
      intercept = Intercept,
      slope = Slope, alpha = 0.01
      # colour = id
    ),
    size = .5
  ) 
model_coef_plot


# Model comparison -------------------------------------------------------------

lrtest(fm2, fm3)


# Checking assumptions ---------------------------------------------------------

# Crucially, we care about:
# Linearity
# Normality of the residuals
# Homogeneity of residual variance (homoscedasticity)
# No autoccorelation and no multicolinearity

plot_model(fm3, type='diag') # you can ajust type (see package info: ?plot_model)


dd = mutate(
  dd,
  prediction = fitted(fm3),
  resid = positive_affect - prediction
) 

# Normality
qqnorm(resid(fm3))
qqline(resid(fm3), col = "maroon4")


# Linearity
plot(fm2)

scatter.smooth(fitted(fm3), resid(fm3))
abline(h = 0, col = "tomato2")

smoothplot <- function(data, xvar, yvar, jitter = .2) {
  ggplot(data, aes({{ xvar }}, {{ yvar }})) +
    geom_point(
      position = position_jitter(width = jitter),
      alpha = .5
    ) +
    stat_smooth(method = "loess", formula = y ~ x)
}

smoothplot(dd, prediction, resid)

# Homogeneity of Variance
scatter.smooth(fitted(fm3), sqrt(abs(resid(fm3))))

smoothplot(dd, time_window, resid)

ggplot(dd, aes(time_window, resid)) + 
  stat_summary() + labs(y = 'Mean Squared Error')

smoothplot(dd, id, resid)

ggplot(dd, aes(id, resid)) + 
  stat_summary() + labs(y = 'Mean Squared Error')

# Random effects

rfx = data.frame(ranef(fm2))
hist(rfx$condval)

fm3r <- rlmer(
  positive_affect ~ time_window + (1 + time_window | id), 
  data = dd,
  control = lmerControl(optimizer ="Nelder_Mead")
)

plot_model(fm3r, type='diag') # you can ajust type (see package info: ?plot_model)


# R2 ---------------------------------------------------------------------------

# In un modello a effetti misti lineare (LMM) stimato usando la funzione `lmer` 
# in R, i valori di R-quadrato marginale e condizionale sono due misure 
# comunemente utilizzate di bontà di adattamento che valutano la quantità di 
# varianza spiegata dagli effetti fissi e casuali nel modello.

# Il R-quadrato marginale (R²m) è definito come la proporzione di varianza 
# spiegata dagli effetti fissi nel modello, ignorando gli effetti casuali. 
# Viene calcolato come:
  
# ```
# R²m = (SS_fissi - SS_residuali) / (SS_fissi + SS_casuali + SS_residuali)
# ```

# dove `SS_fissi` è la somma dei quadrati degli effetti fissi, `SS_casuali` è 
# la somma dei quadrati degli effetti casuali e `SS_residuali` è la somma dei 
# quadrati dei residui. Il R-quadrato marginale varia da 0 a 1, con valori più 
#elevati che indicano un adattamento migliore degli effetti fissi ai dati.

# Il R-quadrato condizionale (R²c) è definito come la proporzione di varianza 
# spiegata sia dagli effetti fissi che casuali nel modello. Viene calcolato 
# come:
  
# ```
# R²c = (SS_fissi + SS_casuali - SS_residuali) / 
#       (SS_fissi + SS_casuali + SS_residuali)
# ```

# dove `SS_fissi`, `SS_casuali` e `SS_residuali` sono definiti come sopra. Il 
# R-quadrato condizionale varia da 0 a 1, con valori più elevati che indicano 
# un adattamento migliore dell'intero modello (effetti fissi e casuali) ai dati.

# La differenza principale tra i valori di R-quadrato marginale e condizionale 
# è che il R-quadrato marginale considera solo gli effetti fissi nel modello, 
# mentre il R-quadrato condizionale tiene conto sia degli effetti fissi che 
# casuali. Di conseguenza, il R-quadrato condizionale è generalmente considerato 
# una misura più completa di bontà di adattamento per i LMM, poiché cattura il 
# contributo sia degli effetti fissi che casuali alla varianza spiegata nella 
# variabile di risposta.

r2_nakagawa(fm3) 

# Gender effects ---------------------------------------------------------------

dd1 <- dd %>% drop_na(dass_d)

fm3a <- lmer(
  positive_affect ~ time_window + (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

fm4a <- lmer(
  positive_affect ~ time_window * dass_d + (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

lrtest(fm3a, fm4a)

r2_nakagawa(fm4a) 

fm4b <- lmer(
  positive_affect ~ time_window + dass_d + (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

lrtest(fm4a, fm4b)

fm5 <- lmer(
  positive_affect ~ time_window + dass_d + dass_s + (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

lrtest(fm4b, fm5)

fm6 <- lmer(
  positive_affect ~ time_window + dass_d + dass_s + dass_a + 
    (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

lrtest(fm5, fm6)

screenreg(list(fm5, fm6))

icc(fm5)


r2_nakagawa(fm5)

car::Anova(fm5)

fm7 <- lmer(
  positive_affect ~ dass_s + sex + time_window + dass_d + dass_a +
    (1 + time_window | id), 
  data = dd1,
  control = lmerControl(optimizer ="Nelder_Mead")
)

lrtest(fm7, fm5)

plot_model(fm7, type = "pred", terms = c("time_window"))
plot_model(fm7, type = "pred", terms = c("dass_d"))
plot_model(fm7, type = "pred", terms = c("dass_s"))
plot_model(fm7, type = "pred", terms = c("dass_a"))

summary(fm7)


# LGM --------------------------------------------------------------------------

mood <- rio::import("mpath_ema.xlsx")
head(mood)

df_wide <- mood %>% 
  pivot_wider(names_from = time_window, values_from = positive_affect)

colnames(df_wide) <- c("id", "mood1", "mood2", "mood3", "mood4", "mood5")
df_wide <- df_wide[!duplicated(df_wide$id), ]




mydf <- left_join(df_wide, new_quest_df, by = "id")


# Multivariate outliers --------------------------------------------------------

# https://cran.r-project.org/web/packages/Routliers/Routliers.pdf
# foo <- mydf[complete.cases(mydf), ]

# SOC <- rowMeans(foo[,c("mood1", "mood2", "mood3", "mood4", "mood5")])
# HSC <- rowMeans(foo[,c("dass_d", "dass_a", "dass_s", "neuro")])
# res <- outliers_mahalanobis(x = cbind(SOC,HSC))
# plot_outliers_mahalanobis(res, x = cbind(SOC,HSC))

# foo$id <- NULL
# result <- mvn(data = foo[24:259, ], mvnTest = "hz", 
#               multivariateOutlierMethod = "quan",
#               showOutliers = TRUE, showNewData = TRUE)
# 
# ### New data without multivariate outliers
# mydf_clean <- result$newData

# https://www.r-bloggers.com/2019/01/a-new-way-to-handle-multivariate-outliers/
library(MASS)

foo <- df_wide
foo <- foo[complete.cases(foo), ]
foo$id <- NULL
output75 <- cov.mcd(foo, quantile.used = nrow(foo)*.75)
mhmcd75 <- mahalanobis(foo, output75$center, output75$cov)
cutoff <- (qchisq(p = 1 - 0.01, df = ncol(foo)))
names_outlier_MCD75 <- which(mhmcd75 > cutoff)

mydf <- left_join(df_wide[-names_outlier_MCD75, ], new_quest_df, by = "id")


# ------------------------------------------------------------------------------



lg_mood_model <- '
  # latent variable definitions
      #intercept (note intercept is a reserved term)
      eta_1 =~ 1*mood1
      eta_1 =~ 1*mood2
      eta_1 =~ 1*mood3
      eta_1 =~ 1*mood4
      eta_1 =~ 1*mood5

      #linear slope 
      eta_2 =~ 0*mood1
      eta_2 =~ 1*mood2
      eta_2 =~ 2*mood3
      eta_2 =~ 3*mood4
      eta_2 =~ 4*mood5

  # factor variances
      eta_1 ~~ eta_1
      eta_2 ~~ eta_2

  # covariances among factors 
      eta_1 ~~ eta_2

  # factor means 
      eta_1 ~ 1
      eta_2 ~ 1

  # manifest means (fixed at zero)
      mood1 ~ 0*1
      mood2 ~ 0*1
      mood3 ~ 0*1
      mood4 ~ 0*1
      mood5 ~ 0*1

  # Time invariant covaraite
      # regression of time-invariant covariate on intercept and slope factors
      eta_1 ~ dass_d + dass_s + neuro
      eta_2 ~ dass_d + dass_s + neuro

  # variance of dass_d covariates
      dass_d ~~ dass_d 
      #dass_a ~~ dass_a
      dass_s ~~ dass_s
      neuro ~~ neuro

  # covariance of dass_d covariates
      #dass_d ~~ dass_a 
      dass_d ~~ dass_s
      #dass_a ~~ dass_s
      #neuro ~~ dass_a
      neuro ~~ dass_s
      neuro ~~ dass_d

  # means of dass_d covariates (freely estimated)
      dass_d ~ 1
      # dass_a ~ 1
      dass_s ~ 1
      neuro ~ 1

' #end of model definition


# estimating the model using sem() function
lg_mood_model_fit <- sem(lg_mood_model,
  data = mydf,
  meanstructure = TRUE,
  estimator = "MLR",
  missing = "fiml"
)

# https://cran.r-project.org/web/packages/semfindr/vignettes/selecting_cases.html
# rerun_out <- lavaan_rerun(lg_mood_model_fit,
#                           case_id = mydf$id,
#                           resid_md_top = 10)

summary(lg_mood_model_fit, fit.measures = TRUE)

mood_predicted <- as.data.frame(
  cbind(mydf$id, lavPredict(lg_mood_model_fit))
)

#naming columns
names(mood_predicted) <- c("id", "eta_1", "eta_2")

head(mood_predicted)
mood_predicted$eta_1 <- as.numeric(mood_predicted$eta_1)
mood_predicted$eta_2 <- as.numeric(mood_predicted$eta_2)

#calculating implied manifest scores
mood_predicted$mood_1 <- 1 * mood_predicted$eta_1 + 0 * mood_predicted$eta_2
mood_predicted$mood_2 <- 1 * mood_predicted$eta_1 + 1 * mood_predicted$eta_2
mood_predicted$mood_3 <- 1 * mood_predicted$eta_1 + 2 * mood_predicted$eta_2
mood_predicted$mood_4 <- 1 * mood_predicted$eta_1 + 3 * mood_predicted$eta_2
mood_predicted$mood_5 <- 1 * mood_predicted$eta_1 + 4 * mood_predicted$eta_2


# reshaping wide to long
temp <- mood_predicted |> 
  dplyr::select("id",  "mood_1", "mood_2", "mood_3", "mood_4", "mood_5")

mood_predicted_long <- temp |> 
  pivot_longer(!id, names_to = "time", values_to = "mood")

ggplot(
  data = mood_predicted_long, # data set
  aes(x = time, y = mood, group = id)
) + 
  geom_line(alpha = 0.35) 



# eof ------------  





?library("lcsm")

x_var_list <- c("mood1", "mood2", "mood3", "mood4", "mood5")

plot_trajectories(
  data = df_wide,
  id_var = "id",
  var_list = x_var_list,
  xlab = "Time", ylab = "Value",
  connect_missing = FALSE,
  title_n = TRUE
)

plot_trajectories(
  data = df_wide,
  id_var = "id", 
  var_list = x_var_list,
  xlab = "Time", ylab = "Value",
  connect_missing = TRUE, 
  random_sample_frac = 0.15, 
  title_n = TRUE) +
  facet_wrap(~id)

uni_lavaan_results <- fit_uni_lcsm(
  data = df_wide, 
  var = x_var_list,
  model = list(alpha_constant = TRUE, 
               beta = TRUE, 
               phi = TRUE)
)

extract_fit(uni_lavaan_results) |>
  print()

extract_param(uni_lavaan_results) |>
  print()


uni_lavaan_syntax <- fit_uni_lcsm(
  data = df_wide,
  var = x_var_list,
  model = list(
    alpha_constant = TRUE,
    beta = TRUE,
    phi = TRUE
  ),
  return_lavaan_syntax = TRUE
)

plot_lcsm(
  lavaan_object = uni_lavaan_results,
  lavaan_syntax = uni_lavaan_syntax,
  edge.label.cex = .9,
  # lcsm_colours = TRUE,
  lcsm = "univariate"
)

fs = lavPredict(uni_lavaan_results)
head(fs) |>
  print()


lx_fs = fs[, 1:5]
head(lx_fs) |>
  print()

dim(lx_fs)

dd <- as_tibble(lx_fs)
dim(dd)

dd$id <- 1:dim(lx_fs)[1]
head(dd) |>
  print()

vlist = paste0("lx", 1:5)
vlist |>
  print()

plot_trajectories(
  data = dd,
  id_var = "id",
  var_list = vlist,
  xlab = "Time", ylab = "Value",
  connect_missing = FALSE,
  # random_sample_frac = 0.018,
  title_n = TRUE
)
