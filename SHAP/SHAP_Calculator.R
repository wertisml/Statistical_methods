rm(list=ls(all=TRUE))

library(data.table)
library(tidyverse)
library(tidymodels)
library(DALEX)
library(iBreakDown)
library(doParallel)

setwd("~/Statistical_methods/SHAP/Data")

#==============================================================================#
# Input data
#==============================================================================#

Data <- fread("Example_SHAP_Data.csv")

#==============================================================================#
# Clean Data
#==============================================================================#

# If you need to filter out parts of data, do that here

# Remove the NA rows
Data <- Data[complete.cases(Data),]

#==============================================================================#
# Split data for training
#==============================================================================#

set.seed(24)
split <- initial_split(Data,
                       prop = 0.8, 
                       strata = Outcome)

Train <- training(split)
Test <- testing(split)

#==============================================================================#
# SHAP
#==============================================================================#

SHAP_Calculation <- function(variable_names, Outcome, Model_type, SHAP_sample_size, Interval_col){
  
  # Set-up parallel
  n.cores <- parallel::detectCores() - 1
  
  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  #create and register cluster
  my.cluster <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl = my.cluster)
  
  # Create SHAP-ID
  Train <- Train %>%
    arrange(Interval_col) %>%
    mutate(ID_SHAP = 1:nrow(.)) %>%
    as.data.frame()
  
  Test <- data.frame(Test)

  Select_predictor <- variable_names
  
  explain_Model <- DALEX::explain(Model,
                                  data = Train[, Select_predictor],
                                  y = Train[, Outcome],
                                  label = Model_type)
  
  Hoge <- c()
  
  Select_ID_SHAP <- sample(Train$ID_SHAP, size = SHAP_sample_size)
  
  Hoge <- foreach(iii = Select_ID_SHAP, .combine = "rbind") %do% {
    SHAP_model <- shap(explain_Model, subset(Train, ID_SHAP == iii), B = 5)
    
    Kari <- SHAP_model %>% 
      data.table()
    
    Kari[, ID := iii, ]
    Kari
    
  }
  
  #==============================================================================#
  # Summarise SHAP
  #==============================================================================#
  
  SHAP_data <- Hoge %>% 
    group_by(ID, variable_name) %>%
    summarize(Variable_value = first(variable_value),
              Contribution = mean(contribution)) %>%
    mutate(Variable_value = as.numeric(Variable_value),
           Contribution = as.numeric(Contribution)) %>%
    group_by(variable_name) %>%
    mutate(Variable_value_scale = scale(Variable_value)) %>%
    data.table() 
  
  SHAP <- SHAP_data %>%
    group_by(variable_name) %>%
    mutate(mean_value = mean(abs(Contribution))) %>%
    filter(mean_value > 0)
  
  unregister()
  
  return(SHAP)
}

# File must be .rds
# Read in rds file
Model <- readRDS("GAM_rfe.rds")

system.time(
Shap <- SHAP_Calculation(variable_names = c(names(Train)), # list of variables used to create your model
                         Outcome = "Outcome", # Name of the outcome variable we are looking at
                         Model_type = "XGB", # What type of ML model is Model?
                         Interval_col = "Date", # The column with the information on the frequency of data (ex: Date)
                         SHAP_sample_size = 100) # This is the number of points from Train that you want the SHAP value calculated for, the larger the number, the slower the calculation
)                   
        
#==============================================================================#
# Plot SHAP
#==============================================================================#

Plot_data <- Shap[ , , ]

myfuns <- list(Low = min, High = max)
ls_val <- unlist(lapply(myfuns, function(f) f(Plot_data$Contribution)))

SHAP_Plot <- ggplot(data = Plot_data) +
  coord_flip() +
  ggforce::geom_sina(aes(x = fct_reorder(variable_name, mean_value), y = Contribution, color = Variable_value_scale),
                     method = "counts", alpha = 3) + 
  scale_color_gradient(low = "#FFCC33", high = "#6600CC") +
  geom_text(data = unique(Plot_data[, c("variable_name", "mean_value")]),
            aes(x = variable_name, y=-Inf, label = round(mean_value,3)),
            size = 3, alpha = 0.7,
            hjust = -0.2,
            fontface = "bold",
            check_overlap = TRUE) +
  theme_bw() +
  ylim(1.45*(ls_val)) +
  scale_color_gradient(low="#FFCC33", high="#6600CC",
                       breaks=ls_val, 
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), # remove axis line
        legend.position="bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title.x= element_text(size = 10)) +
  labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value")                 

SHAP_Plot

#==============================================================================#
# Take a photo of the plot
#==============================================================================#

pdf(file = paste0("SHAP_Plot", ".pdf"), width = 6, height = 6)

SHAP_Plot

dev.off()
