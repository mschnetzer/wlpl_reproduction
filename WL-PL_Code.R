librarian::shelf(tidyverse, haven, readxl, WDI, countrycode)
# Graphs and tables
librarian::shelf(patchwork, texreg, Hmisc, maps)
# Stats
librarian::shelf(MASS, lmtest, sandwich, DescTools, ggvenn, rigr)
select <- dplyr::select



# LOAD AND MANIPULATE DATA ----

# Filter outlier estimate for Italy by Oyvat/Öztunali
rawdata <- read_xlsx("WL-PL_Database.xlsx") |> 
  filter(!(studyid %in% c(30) & country_region=="Italy"))

# Create and edit variables
data <- rawdata |> mutate(
  es_num = ifelse(wshare_pshare == "Profit Share", as.numeric(es)*-1, as.numeric(es)),
  es_dom = ifelse(wshare_pshare == "Profit Share", as.numeric(es_domestic)*-1, as.numeric(es_domestic)),
  C_num = ifelse(wshare_pshare == "Profit Share", as.numeric(C)*-1, as.numeric(C)),
  I_num = ifelse(wshare_pshare == "Profit Share", as.numeric(I)*-1, as.numeric(I)),
  dom_demand = ifelse(is.na(as.numeric(es_domestic)), C_num + I_num, es_dom),
  precision_obs = sqrt(n_obs),
  n_obs_inv = 1/n_obs,
  precision_obs_inv = 1/precision_obs,
  se = as.numeric(se),
  se_dom = as.numeric(se_dom),
  yaverage = round(yaverage, 0),
  d_published = as.numeric(!journal == "."),
  d_insig_tot = `Insignificant_results_0_total(C_I_or_NX)`,
  d_insig_dom = `Insignificant_results_0_domestic(C_or_I)`,
  d_quarterly = as.numeric(Yearly_Quarterly == "Quarterly"),
  d_earlyperiod = as.numeric(yaverage < 1990),
  d_simultaneous = as.numeric(method_e == "Simultaneous"),
  d_endogeneity = as.numeric(!estimationstrat %in% c("OLS","GLS","WLS","PLS")),
  d_capacity = as.numeric(dep_v == "Capacity Utilisation"),
  d_oecd = as.numeric(country_region %in% c("Austria","Belgium","Australia","Canada","Chile","Czech Republic","Denmark","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Italy","Japan","South Korea","Luxembourg","Mexico","Netherlands","Norway","Poland","Portugal","Slovakia","Spain","Sweden","Switzerland","Turkey","United Kingdom","USA","OECD","EU-28","Euro Area","Euro Area (EU12)","Northern Euro-Countries","Southern Euro-Countries")),
  d_wealth = as.numeric(d_wealth_effects == 1),
  d_ineq = as.numeric(d_personal_inequality == 1),
  d_fin = as.numeric(d_debtandcredit == 1),
  d_gov = as.numeric(d_gov_spending == 1),
  d_I_profits = as.numeric(d_I_profit_share == 1 | d_I_profit_rate == 1),
  d_I_interest = as.numeric(d_I_interest_rate == 1),
  d_e_profits = as.numeric(d_net_exports_profit_share == 1 |
                             d_net_exports_profit_rate == 1 | d_e_profit_share == 1 |
                             d_e_profit_rate == 1),
  d_e_demand = as.numeric(d_net_exports_foreign_demand == 1 | d_e_foreign_demand == 1),
  d_e_ULC = as.numeric(d_e_ULC == 1),
  d_e_exchangerate = as.numeric(d_net_exports_exchange_rate == 1 | 
                                  d_e_exchangerate == 1))


# DESCRIPTIVE TABLE ----

rawtab <- read_xlsx("WL-PL_Variables.xlsx") |>
  filter(!variable %in% c("es_num", "dom_demand")) |> 
  add_row(variable = "estimate", name = "Effect size", description = "Marginal effect between the functional income distribution and aggregate demand", .before = 1)

descstat <- data |> 
  select(any_of(rawtab$variable), es_num, dom_demand, d_insig_tot, d_insig_dom) |> 
  pivot_longer(names_to = "type", values_to = "estimate", 
               cols = c(es_num, dom_demand)) |> 
  drop_na() |> 
  mutate(d_insig = case_when(type == "es_num" & d_insig_tot == 1 ~ 1,
                             type == "dom_demand" & d_insig_dom == 1 ~ 1,
                             TRUE ~ 0)) |>  
  select(-c(d_insig_tot, d_insig_dom)) |> 
  summarise(across(everything(), c(mean = mean, sd = sd)), .by = type) |> 
  data.table::transpose(keep.names = "variable", make.names = 1) |> 
  separate(variable, into = c("variable", "measure"), "_(?=[^_]+$)") |> 
  pivot_wider(names_from = measure, values_from = c(es_num, dom_demand)) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

# No controls in exports X for domestic specification!
splittab <- rawtab |> left_join(descstat) |>  
  select(name, description, starts_with("es"), starts_with("dom")) |> 
  mutate(across(starts_with("dom"), 
                ~ifelse(str_detect(name, "(in X)"), NA_integer_, .)))

# Save table as HTML
library(htmlTable)
library(kableExtra)
splittab |> select(-name) |> 
  addHtmlTableStyle(align = "lrrrr") |> 
  htmlTable(cgroup = c("", "Total demand", "Domestic demand"),
            n.cgroup = c(1,2,2),
            header = c("Description", "Mean", "S.D.", "Mean", "S.D."),
            rnames = c(splittab$name),
            rgroup = c("Dependent variable", "Publication characteristics", "Estimation strategy", "Meta-regression controls for time and space", "Studies’ controls in investment (I) or net export (X) functions", "Other controls"), 
            n.rgroup = c(1, 2, 6, 2, 6, 4), 
            ctable = T,
            caption = "Variable definitions and descriptive statistics") |> 
  save_kable("desctable.html")


# META REGRESSION ANALYSIS ----

# Function for meta regression
metareg <- function(type, test, addtit = "", addfn = "_inv") {
  res <- list()
  
  depvar <- ifelse(type == "tot", "es_num", "dom_demand")
  precvar <- ifelse(test == "fat", "precision_obs_inv", "n_obs_inv")
  weightvar <- ifelse(test == "fat", "n_obs", "n_obs")
  insigvar <- ifelse(type == "tot", "d_insig_tot", "d_insig_dom")
  modname <- ifelse(type == "tot", "Total demand", "Domestic demand")
  specname <- ifelse(test == "fat", "FAT-PET", "PEESE")
  
  pubbias <- as.formula(get(paste(depvar)) ~ get(paste(precvar)))
  pubspec <- as.formula(get(paste(depvar)) ~ get(paste(precvar)) + 
                          d_published + get(paste(insigvar)))
  methspec <- update(pubspec, ~ . + d_endogeneity + d_simultaneous + d_meanmarginal + 
                       d_quarterly + d_capacity + d_real_wages)
  timespec <- update(methspec, ~ . + d_earlyperiod + d_oecd)
  funspec <- if (type == "tot") {
    update(timespec, ~ .  + d_I_profits + 
             d_I_interest + d_e_demand + d_e_profits +
             d_e_ULC + d_e_exchangerate)
  } else {
    update(timespec, ~ .  + d_I_profits + d_I_interest)
  }
  fullspec <- update(funspec, ~ . + d_gov + d_fin + d_ineq + d_wealth)
  
  for (spec in c("pubbias","pubspec","methspec","timespec","funspec","fullspec")) {
    
    res[[paste("mra",type,spec,test, sep =".")]] <- 
      lm(get(paste(spec)), data = data, weights = get(paste(weightvar)))
    res[[paste("rse",type,spec,test, sep =".")]] <- 
      coeftest(res[[paste("mra",type,spec,test, sep =".")]],
               function(x) vcovHC(x, method="arellano", type="HC1",cluster="studyid"))
    
  }
  
  htmlreg(list(res[[1]], res[[3]], res[[5]], res[[7]], res[[9]], res[[11]]),
          caption = paste0("Regression results for ", tolower(modname), 
                           " (", specname, ")", addtit),
          label = paste0("tab:reg",type,"_",test,addfn),
          caption.above = TRUE,
          stars = c(0.01, 0.05, 0.1),
          custom.header = list("(1)"=1,"(2)"=2,"(3)"=3,"(4)"=4,"(5)"=5,"(6)"=6),
          custom.model.names = c("Pub. bias","Pub. char.","Est. strat.","Time/Space","Controls in I/X", "Oth. controls"),
          custom.coef.map = list("(Intercept)"="Constant",
                                 "get(paste(precvar))"="Precision",
                                 "d_published"="Published",
                                 "get(paste(insigvar))"="Insignificant estimate",
                                 "d_endogeneity"="Tackling endogeneity",
                                 "d_simultaneous"="Simultaneous estimation",
                                 "d_meanmarginal"="Mean marginal effect",
                                 "d_quarterly"="Quarterly data",
                                 "d_capacity"="Capacity utilization",
                                 "d_real_wages"="Real wages",
                                 "d_earlyperiod"="Early observation period",
                                 "d_oecd"="OECD country",
                                 "d_I_profits"="Profits in I",
                                 "d_I_interest"="Interest rate in I",
                                 "d_e_demand"="Demand in X",
                                 "d_e_profits"="Profits in X",
                                 "d_e_ULC"="Unit labor costs in X",
                                 "d_e_exchangerate"="Exchange rate in X",
                                 "d_gov"="Government spending",
                                 "d_fin"="Debt and credit",
                                 "d_ineq"="Personal inequality",
                                 "d_wealth"="Wealth effects"),
          override.se = list(res[[2]][,2], res[[4]][,2], res[[6]][,2], 
                             res[[8]][,2], res[[10]][,2], res[[12]][,2]),
          override.pvalues = list(res[[2]][,4], res[[4]][,4], res[[6]][,4], 
                                  res[[8]][,4], res[[10]][,4], res[[12]][,4]),
          digits = 3,
          fontsize = "footnotesize",
          no.margin = TRUE,
          dcolumn = TRUE,
          use.packages = FALSE,
          file = paste0("models_",type,"_",test,addfn,".html"))
  
}

metareg(type = "tot", test = "peese")
metareg(type = "tot", test = "fat")
metareg(type = "dom", test = "peese")
metareg(type = "dom", test = "fat")


# BEST PRACTICE MEANS ----

### Domestic (no best practice for simultaneity, early period, OECD sample)
bpm_dom_alt <- regress("mean", dom_demand ~ precision_obs_inv + d_published + d_insig_dom + d_endogeneity + d_meanmarginal + d_quarterly + d_capacity + d_real_wages + d_I_profits + d_I_interest + d_gov + d_fin + d_ineq + d_wealth, data = data, weights = data$n_obs)
testdat_dom_alt <- c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1)
lincom(bpm_dom_alt, testdat_dom_alt, robustSE = TRUE)

### Total (no best practice for simultaneity, early period, OECD sample)
bpm_tot_alt <- regress("mean", es_num ~ precision_obs_inv + d_published + d_insig_tot + d_endogeneity + d_meanmarginal + d_quarterly + d_capacity + d_real_wages + d_I_profits + d_I_interest + d_e_profits + d_e_ULC + d_e_exchangerate + d_gov + d_fin + d_ineq + d_wealth, data = data, weights = data$n_obs)
testdat_tot_alt <- c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
lincom(bpm_tot_alt, testdat_tot_alt, robustSE = TRUE)


# NON-LINEAR TESTS ----

robdata <- data |>
  mutate(se = precision_obs_inv,
         var = n_obs_inv)

# Create subsamples
# Top 10% of estimates for total and domestic
data.t10 <- robdata |> filter(!is.na(es_num)) |> 
  slice_max(n_obs, prop = 0.1)

# Exclude large study (id=12) with high n_obs for domestic: only 9 of 360 observations in top 10%
data.d10 <- robdata |> filter(!is.na(dom_demand)) |> 
  slice_max(n_obs, n = 9)

# Subset with standard errors
subse <- data |> filter(!is.na(se)) |> 
  mutate(var = se^2)

# WAAP (Ioannidis et al. 2017)
waap <- data |> filter(!is.na(se)) |> 
  mutate(ape = abs(es_num)/2.8,
         ind = se < ape) |> 
  filter(ind == TRUE) |> 
  mutate(var = se^2,
         weight = 1/var)

# Sample for Andrews/Kasy approach: https://maxkasy.github.io/home/metastudy/
subse |> select(es_num, se) |> 
  write.table(file = "mra_kasy.csv", sep = ",", row.names = F, col.names = F)

# Publication bias function
pubbias <- function(type, test, addtit = "", addfn = "_inv") {
  res <- list()
  
  depvar <- ifelse(type == "tot", "es_num", "dom_demand")
  precvar <- ifelse(test == "fat", "se", "var")
  weightvar <- ifelse(test == "fat", "n_obs", "n_obs")
  subdat <- ifelse(type == "tot", "data.t10", "data.d10")
  modname <- ifelse(type == "tot", "Total demand", "Domestic demand")
  specname <- ifelse(test == "fat", "FAT-PET", "PEESE")
  
  model <- as.formula(get(paste(depvar)) ~ get(paste(precvar)))
  
  # Full sample
  colname <- c("Full sample")
  res[[paste0("pb.",type,".fullsample.",test)]] <- 
    lm(model, data = robdata, weights = get(paste(weightvar))) %>%
    coeftest(., function(x) vcovHC(x, method="arellano",type="HC1",cluster="studyid"))
  
  res[[paste0("pb.",type,".fullsample.",test,".desc")]] <- 
    data |> filter(!is.na(get(paste(depvar)))) |> 
    summarise(mean = round(mean(get(paste(depvar)), na.rm = T),3),
              studies = length(unique(studyid)),
              count = n())
  
  # Subsample Top 10%
  colname <- c(colname, "Top 10%")
  res[[paste0("pb.",type,".top10.",test)]] <- 
    lm(model, data = get(paste(subdat)), weights = get(paste(weightvar))) %>%
    coeftest(., function(x) vcovHC(x, method="arellano",type="HC1",cluster="studyid"))
  
  res[[paste0("pb.",type,".top10.",test,".desc")]] <- 
    get(paste(subdat)) |> 
    summarise(mean = round(mean(get(paste(depvar)), na.rm = T),3),
              studies = length(unique(studyid)),
              count = n())
  
  if (type == "tot") {
    # Subsample standard errors
    colname <- c(colname, "Std.err.")
    res[[paste0("pb.",type,".subse.",test)]] <- 
      lm(model, data = subse, weights = var) %>%
      coeftest(., function(x) vcovHC(x, method="arellano",type="HC1",cluster="studyid"))
    
    res[[paste0("pb.",type,".subse.",test,".desc")]] <- 
      subse |>
      summarise(mean = round(mean(get(paste(depvar)), na.rm = T),3),
                studies = length(unique(studyid)),
                count = n())
    
    # WAAP
    colname <- c(colname, "WAAP")
    res[[paste0("pb.",type,".waap.",test)]] <- 
      lm(model, data = waap, weights = weight) %>%
      coeftest(., function(x) vcovHC(x, method="arellano",type="HC1",cluster="studyid"))
    
    res[[paste0("pb.",type,".waap.",test,".desc")]] <- 
      waap |>
      summarise(mean = round(mean(get(paste(depvar)), na.rm = T),3),
                studies = length(unique(studyid)),
                count = n())
  }
  
  elements <- length(res)
  colhead <- list()
  for(i in 1:(elements/2)) {
    colhead[[paste0("(", i, ")")]] <- i
  }
  
  
  htmlreg(res[seq(1, elements, 2)],
          caption = paste0("Publication bias for ",tolower(modname), " (",specname,")", addtit),
          caption.above = TRUE,
          label = paste0("tab:pubbiassg_",type,"_",test,addfn), 
          custom.header = colhead,
          custom.model.names = colname,
          custom.coef.names = c("&beta;<sub>0</sub> [mean beyond bias]","&beta;<sub>1</sub> [publication bias]"),
          stars = c(0.01, 0.05, 0.1),
          custom.gof.rows = 
            list("Sample mean" = c(res[seq(2, elements, 2)] |> map_dbl(1) |> as.numeric()),
                 "Observations" = c(res[seq(2, elements, 2)] |> map_dbl(3) |> as.numeric()),
                 "Studies" = c(res[seq(2, elements, 2)] |> map_dbl(2) |> as.numeric())),
          fontsize = "scriptsize",
          digits = 3,
          no.margin = TRUE,
          dcolumn = TRUE,
          use.packages = FALSE,
          file = paste0("pubbias_",type,"_",test,addfn,".html"))
}

pubbias(type = "tot", test = "fat")
pubbias(type = "dom", test = "fat")


# Domestic Top 10%: keep top 9 obs and draw 27 obs (36 obs = 10%) randomly from study 12 and replicate regression
# ATTENTION: HAS TO BE ADDED MANUALLY TO THE OUTPUT TABLE
sampreg <- function(test) {
  precvar <- ifelse(test == "fat", "precision_obs_inv", "n_obs_inv")
  dfsample <- data.d10 |> slice_max(n_obs, n = 9) |>
    bind_rows(data |> filter(studyid == 12) |> slice_sample(n = 27))
  fit <- lm(dom_demand ~ get(paste(precvar)), weights = n_obs, data = dfsample)
  c(coef(fit)[1], coef(fit)[2], mean=mean(dfsample$dom_demand), studies=length(unique(dfsample$studyid)), observations=nrow(dfsample))
}
set.seed(0)
z.fat <- t(replicate(1000, sampreg(test = "fat"))) |> as.data.frame()
z.fat |> summarise(across(everything(), .fns = list(Mean = mean, SD = sd)))
t.test(z.fat$`(Intercept)`, conf.level = 0.99)
z.peese <- t(replicate(1000, sampreg(test = "peese"))) |> as.data.frame()
z.peese |> summarise(across(everything(), .fns = list(Mean = mean, SD = sd)))
t.test(z.peese$`(Intercept)`, conf.level = 0.99)


# ROBUSTNESS ----

### Exclude Stockhammer/Stehrer (studyid = 12) ----
data.tmp <- data
data <- data |> filter(!studyid == 12)

metareg(type = "dom", test = "peese", 
        addtit = " w/o Stockhammer/Stehrer (2011)", addfn = "_wo_id12_inv")
metareg(type = "dom", test = "fat", 
        addtit = " w/o Stockhammer/Stehrer (2011)", addfn = "_wo_id12_inv")

pubbias(type = "dom", test = "peese", addtit = " w/o Stockhammer/Stehrer (2011)", 
        addfn = "_stst")
pubbias(type = "dom", test = "fat", addtit = " w/o Stockhammer/Stehrer (2011)",
        addfn = "_stst")

data <- data.tmp
rm(data.tmp)

### Exclude Stockhammer/Reddy/Rabinovich (studyid = 31) ----
data.tmp <- data
data <- data |> filter(!studyid == 31)

metareg(type = "dom", test = "peese", 
        addtit = " w/o Stockhammer/ea. (2018)", addfn = "_wo_id31_inv")
metareg(type = "dom", test = "fat", 
        addtit = " w/o Stockhammer/ea. (2018)", addfn = "_wo_id31_inv")

pubbias(type = "dom", test = "peese", addtit = " w/o Stockhammer/ea. (2018)", 
        addfn = "_wo_id31")
pubbias(type = "dom", test = "fat", addtit = " w/o Stockhammer/ea. (2018)",
        addfn = "_wo_id31")

data <- data.tmp
rm(data.tmp)


# MULTILEVEL ESTIMATION ----

library(lme4)
library(performance)

fit0.tot <- lmer(es_num ~ 1 + (1 | studyid), data = data, REML = F)
fit1.tot <- lmer(es_num ~ precision_obs_inv + (1 | studyid), data = data, REML = F)
fit0.dom <- lmer(dom_demand ~ 1 + (1 | studyid), data = data, REML = F)
fit1.dom <- lmer(dom_demand ~ precision_obs_inv + (1 | studyid), data = data, REML = F)

htmlreg(list(fit0.tot,fit1.tot,fit0.dom,fit1.dom), file = "multilevel_inv.html",
        caption.above = TRUE, reorder.gof = c(2,3,4,5,6,7,1), include.loglik= F,
        digits = 3, stars = c(0.01, 0.05, 0.1),
        caption = "Linear mixed effects model", 
        custom.header = list("Total demand" = 1:2, "Domestic demand" = 3:4),
        custom.model.names = paste0("(",1:4,")"),
        custom.coef.map = list("(Intercept)"="Constant",
                               "precision_obs_inv"="Precision"),
        custom.gof.names = c("AIC","BIC","Num. estimates", "Num. studies","Between-group var.", "Within-group var."),
        custom.gof.rows = 
          list("ICC" = c(round(performance::icc(fit0.tot)[1], 3),
                         round(performance::icc(fit1.tot)[1], 3),
                         round(performance::icc(fit0.dom)[1], 3),
                         round(performance::icc(fit1.dom)[1], 3))),
        no.margin = TRUE,
        dcolumn = TRUE)


# PLOTS ----

# Update theme

theme_jes <- function() {
  theme_minimal() %+replace%
  theme(axis.title = element_text(size=10, family = "Roboto Condensed"),
        text = element_text(family = "Roboto Condensed"),
        axis.text = element_text(size=8, family = "Roboto Condensed",
                                 color = "gray30"),
        plot.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2)) 
  }


### Wage share in the OECD ----
library(rdbnomics)
library(ggrepel)
library(gghighlight)
wsdata <- rdb("AMECO", "ALCD2", 
              dimensions=list(geo = c("deu","d-w","ita","fra","jpn","can","usa","gbr"))) |> 
  select(Country,original_period,value) |>  drop_na() |> 
  mutate(year = as.Date(original_period, "%Y")) |> 
  filter(original_period <= 2020 & !(Country =="West Germany" & original_period == 1991)) |> 
  mutate(Country = recode(Country, "West Germany" = "Germany"))


wsdata |> 
  ggplot(aes(x=year, y=value, group = Country)) +
  geom_line(linewidth = 0.5) +
  gghighlight(use_direct_label = F, unhighlighted_params = list(linewidth = 0.3)) +
  facet_wrap(~Country) +
  scale_y_continuous(limits = c(55,80), breaks = seq(50,80,10),
                     labels = scales::percent_format(scale = 1)) +
  labs(y = "Wage share as % of GDP at factor cost", x = "") +
  theme_jes() +
  theme(strip.text = element_text(size = 9, hjust = 0.5, margin = margin(b=2, unit="pt")),
        panel.spacing.x = unit(0.8, "lines"))

ggsave("wageshare_facets.png", dpi=320, width = 6.5, height = 3.5, bg = "white")
ggsave("figure1.pdf", width = 6.5*3, height = 3.5*3, unit = "cm", device = cairo_pdf)


### Histogram of estimates ----
data |> select(es_num, dom_demand) |>  
  pivot_longer(everything(), names_to = "type", values_to = "value") |> drop_na() |> 
  mutate(type = recode_factor(type, es_num = "Total demand", dom_demand = "Domestic demand")) |> 
  ggplot(aes(x = value, fill = type)) +
  geom_histogram(binwidth = 0.05, position = 'identity') +
  scale_fill_manual(values = c("gray40", "gray70"),
                    name = NULL, guide = guide_legend(reverse = F, keywidth = 0.5)) +
  scale_x_continuous(breaks = seq(-2,3,1)) +
  geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 0.25) +
  geom_hline(aes(yintercept=0), linewidth = 0.1) +
  facet_wrap(~type, nrow = 2, scales = "free_y") +
  labs(x = "Estimate",
       y = "Frequency") +
  theme_jes() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.3),
        strip.text = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.7, "line"))

ggsave("histogram_facet.png", dpi=320, width = 6, height = 4, bg = "white")
ggsave("figure4.pdf", width = 6*2.5, height = 4*2.5, unit = "cm", device = cairo_pdf)


### Funnel plots (total & domestic) ----
mean.tot <- data |> summarise(mean(es_num, na.rm=T)) |> pull()
mean.dom <- data |> summarise(mean(dom_demand, na.rm=T)) |> pull()

ggplot(data) + 
  geom_point(aes(x = es_num, y = precision_obs), alpha = 0.4, 
             stroke = 0.15, size = 2) + 
  geom_vline(xintercept = 0, color = "black", linewidth = 0.1, linetype = "9f") + 
  geom_vline(xintercept = mean.tot, color = "black", linewidth = 0.1) + 
  scale_y_continuous(limits = c(0,NA)) +
  annotate("text", y = 40, x = mean.tot - 0.05, angle = 90, size = 2.5,
           label = paste0("Mean: ",round(mean.tot, 3)), family = "Roboto Condensed") +
  labs(x = "Estimate", y = "Square root of observations") +
  theme_jes()

ggsave("funnel_total.png", dpi=320, width = 6.5, height = 3.5, bg = "white")
ggsave("figure5.pdf", width = 6.5*2.5, height = 3.5*2.5, unit = "cm", device = cairo_pdf)


ggplot(data) + 
  geom_point(aes(x = dom_demand, y = precision_obs), alpha = 0.4,
             stroke = 0.25, size = 2) + 
  geom_vline(xintercept = 0, color = "black", linewidth = 0.1, linetype = "9f") + 
  geom_vline(xintercept = mean.dom, color = "black", linewidth = 0.1) + 
  scale_y_continuous(limits = c(0,NA)) +
  annotate("text", y= 40, x = mean.dom + 0.05, angle = 270, size = 2.5,
           label = paste0("Mean: ", round(mean.dom, 3)), family = "Roboto Condensed") +
  labs(x = "Estimate", y = "Square root of observations") +
  theme_jes()

ggsave("funnel_domestic.png", dpi=320, width = 6.5, height = 3.5, bg = "white")
ggsave("figure6.pdf", width = 6.5*2.5, height = 3.5*2.5, unit = "cm", device = cairo_pdf)

### Time ----
time <- data |> 
  ggplot() + 
  geom_count(aes(x = ystart, y = ydifference), alpha = 0.4) +
  scale_size_area(max_size = 15) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  labs(x = "Initial year of observation", y = "Observation period in years") +
  theme_jes() +
  theme(legend.position = "none")

ggsave("time.png", dpi = 320, width = 4, height = 4, bg = "white")

period <- data |>
  arrange(author1) |> mutate(appid = as.numeric(as.factor(paste0(author1,studyid)))) |> 
  filter(!studyid == 31) |> # Study 31 starts in 1855 (include manually further below)
  summarise(start = min(ystart), ende = max(yend), .by = appid) |>   
  pivot_longer(start:ende, names_to = "label", values_to = "year") |> 
  ggplot(aes(x = year, y = reorder(appid, desc(year)))) +
  geom_line(aes(group = appid)) + 
  geom_point(color = "black", size = 1) +
  geom_text(aes(x = 2023.5, label = paste0("[",appid,"]")), family = "Roboto Condensed", 
            size = 2.5, hjust = 1) +
  geom_segment(aes(x=1935, xend=2010, y=33, yend=33), linewidth = 0.4, 
               arrow = arrow(ends = "first", type = "closed", length = unit(0.12,"cm")), ) +
  annotate("text", x=2023.5, y=33, label="[26]", size=2.5, family="Roboto Condensed", hjust = 1) +
  annotate("text", x=1936, y=31.8, label="1855", size=3, family="Roboto Condensed") +
  geom_point(aes(x=2010, y=33), color= "gray10", size = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, 20), limits = c(1933,2023.5), expand = c(0,0)) +
  scale_y_discrete(expand = c(0.05,0.05)) +
  labs(x = "Year", y = NULL) +
  theme_jes() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("period.png", dpi = 320, width = 4, height = 4, bg = "white")

period + time
ggsave("figure2.pdf", width = 8*3, height = 4*3, unit = "cm", device = cairo_pdf)

### Geography ----
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
setdiff(data$country_region, world$geounit)

est.num <- data |> mutate(country_region = case_when(
  country_region == "USA" ~ "United States of America",
  country_region %in% c("China Coastal", "China-Macao", "Hong Kong") ~ "China",
  TRUE ~ country_region)) |> 
  count(country_region)

# Merge data
welt <- left_join(world, est.num, by = c("geounit"="country_region"))
welt <- welt |> mutate(num.group = cut(n, breaks = c(0,1,5,10,20,100),
                                       include.lowest = F,
                                       labels = c("1","2—5","6—10","11—20",">20")))

welt <- st_transform(welt, crs = 3857)

welt |> filter(geounit != "Antarctica") |> 
  ggplot(aes(fill = num.group)) + 
  geom_sf(color = "grey60", linewidth = 0.07) +
  scale_fill_manual(values = c("gray90","gray75","gray60","gray30","black"),
                    name="Number of\n estimates", na.value = "white") +
  theme_jes() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(l=-.5, unit="cm"),
        legend.position = "inside",
        legend.position.inside = c(0.15,0.2),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.35,"cm"),
        legend.box.background = element_rect(fill = "white", linewidth = 0.2)) +
  labs(x = NULL, y = NULL)

ggsave("map.png", dpi = 320, width = 7, height = 4, bg = "white") 
ggsave("figure3.pdf", width = 7*2.5, height = 4*2.5, unit = "cm", device = cairo_pdf)
