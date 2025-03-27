
#### Libraries ####
library(tidyverse)
library(psych)
library(corrplot)
library(naniar)
library(patchwork)

# Load the data
clean_train <- read_rds("clean_train")

#### Overview: Visualizing All Features ####

## Plot numerical distributions

# VB Plot
clean_train %>%
  select(-id) %>%
  select_if(is.numeric)%>%
  na.omit() %>%
  pivot_longer(cols = everything(),names_to = "feature",values_to = "value") %>%
  ggplot(aes(x = feature,y = value)) +
  geom_violin(fill = "grey",alpha = 0.5)+
  geom_boxplot()+
  facet_wrap(~feature,scales = "free")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "VB Plot of Numerical Features",
    x = "",
    y = "Value")+
  theme(
    title = element_text(size = 16,face = "bold"),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 10,face = "italic"),
    axis.text.y = element_blank())

## Corr Plot
clean_train %>%
  select_if(is.numeric) %>%
  select(-id) %>%
  na.omit() %>%
  cor() %>%
  corrplot(
    method = "color",
    order = "hclust",
    addrect = 4,
    tl.col = "grey30",
    col = colorRampPalette(c("white", "gray85", "black"))(200),
    addCoef.col = "red4",
    tl.srt = 40
  )
# Add title with adjustments
title("Correlation Matrix of Features", cex.main = 2.5, line = 2.5)

# Bar Plot for Categorical Features
clean_train %>%
  select_if(is.factor) %>%
  na.omit() %>%
  pivot_longer(cols = everything(),names_to = "feature",values_to = "value")%>%
  ggplot(aes(x = fct_rev(fct_infreq(value)),fill = feature))+
  geom_bar()+
  scale_fill_brewer(palette="Spectral")+
  facet_wrap(~feature,scales = "free")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "Distribution of Categorical Features",
    x = "",
    y ="Count",
    fill = "Feature")+
  theme(
    title = element_text(size = 15,face = "bold"),
    strip.text = element_text(size = 13,face = "italic"),
    axis.title = element_text(size = 13)
  )

# Viz the missing data 
vis_miss(clean_train,sort_miss = TRUE)
  

#### Hypothesis Testing ####
# Sig level 0.05 *

# Remove the missing data 
hypothesis_data <- na.omit(clean_train)

# Habitability_score ~ categorical features

# Check : Is distribution of habitability_score differ between property_type
kruskal.test(habitability_score ~ property_type,data = hypothesis_data) # p-value < 2.2e-16

# Determine which level differ 
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$property_type)

# Function to viz the stat diff for habitability_score ~ one categorical feature
hypothesis_plots_nc <- function(df,col1,levels,title){
  
  # Filter based on column and levels
  data <- df %>%
    filter(df[[col1]] %in% c(levels))
  
  # Perform the Wilcox
  wilcox <- wilcox.test(habitability_score ~ data[[col1]],data = data)
  p_value <- round(wilcox$p.value,4)
  # ECDF plot
 p1 <- ggplot(data = data ,aes(x = habitability_score,color = data[[col1]]))+
   stat_ecdf(linewidth = 1)+
   theme_minimal()+
   scale_color_viridis_d(option = "plasma")+
   labs(
     title = "ECDF Plot",
     y = "ECDF",
     color = "Property Type")+
   theme(
     title = element_text(face = "italic")
   )
   
 # VB Plot
 p2 <- ggplot(data = data ,aes(x = habitability_score,y = data[[col1]],fill = data[[col1]]))+
   geom_boxplot()+
   geom_violin(fill = "grey50",alpha = 0.1)+
   theme_minimal()+
   scale_fill_viridis_d(option = "plasma")+
   labs(
     title = "VB Plot",
     y = "",
     fill = ""
     )+
   theme(
     title = element_text(face = "italic")
   )
 
 # Density plot
 p3 <- ggplot(data = data ,aes(x = habitability_score,fill = data[[col1]]))+
   geom_density(alpha = 0.5)+
   theme_minimal()+
   scale_fill_viridis_d(option = "plasma")+
   labs(
     title = "Density Plot",
     y = "Density",
     fill = "Property Type"
   )+
   theme(
     title = element_text(face = "italic")
   )
 
  # Combine the ECDF VF and Density plot
 sig_plot <- (p1|p2)/p3 + 
   plot_annotation(
     title = title,
     subtitle = paste0(" Wilcox Test p ~ ",p_value,"")) &
   theme(
     title = element_text(size = 10,face = "bold"),
     plot.subtitle = element_text(face = "italic"))
 
 
 return(
   sig_plot
 )
}

# Plot ~Property_type
hypothesis_plots_nc(hypothesis_data,col1 = "property_type",levels = c("apartment","bungalow"),title = "Differences betweem Apartment and Bungalow")
hypothesis_plots_nc(hypothesis_data,col1 = "property_type",levels = c("container_home","bungalow"),title = "Differences betweem container_home and bungalow")
hypothesis_plots_nc(hypothesis_data,col1 = "property_type",levels = c("duplex","apartment"),title =  "Differences betweem duplex and apartment")
hypothesis_plots_nc(hypothesis_data,col1 = "property_type",levels = c("single_family_home","container_home"),title =  "Differences betweem Single family home and Container home")

# Plot ~ Furnishing
kruskal.test(habitability_score ~ furnishing,data = hypothesis_data) #  p-value < 2.2e-16
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$furnishing)

hypothesis_plots_nc(hypothesis_data,col1 = "furnishing",levels = c("fully_furnished","unfurnished"),title = "Differences between Fully furnished and Unfurnished home")

# Plot ~ Crime Rate
kruskal.test(habitability_score ~ crime_rate,data = hypothesis_data) # p-value < 2.2e-16
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$crime_rate)

hypothesis_plots_nc(hypothesis_data,col1 = "crime_rate",levels = c("well_above_average","well_below_average"),title = "Differences between Habitability score by Crime rate")

# Plot ~ Dust and Noise
kruskal.test(habitability_score ~ dust_and_noise,data = hypothesis_data) # p-value < 2.2e-16
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$dust_and_noise)

hypothesis_plots_nc(hypothesis_data,col1 = "dust_and_noise",levels = c("low","high"),title = "Differences betweem Habitability score by Dust and Noise")

# Plot ~ Power Backup
kruskal.test(habitability_score ~ power_backup,data = hypothesis_data)
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$power_backup)

hypothesis_plots_nc(hypothesis_data,col1 = "power_backup",levels = c("not_mentioned","no"),title = "Habitability score by Power backup ")
hypothesis_plots_nc(hypothesis_data,col1 = "power_backup",levels = c("yes","no"),title = "Habitability score by Power backup")

# Plot ~ Water Supply
kruskal.test(habitability_score ~ water_supply,data = hypothesis_data)
pairwise.wilcox.test(hypothesis_data$habitability_score,hypothesis_data$water_supply)

hypothesis_plots_nc(hypothesis_data,col1 = "water_supply",levels = c("once_in_two_days","all_time"),title = "Habitability score by Water_supply")
hypothesis_plots_nc(hypothesis_data,col1 = "water_supply",levels = c("once_in_a_day_evening","once_in_a_day_morning"),title = "Habitability score by Water_supply")
hypothesis_plots_nc(hypothesis_data,col1 = "water_supply",levels = c("all_time","once_in_two_days"),title = "Habitability score by Water_supply")

# Habitability_score ~ numerical features
cor.test(x = hypothesis_data$property_area ,y = hypothesis_data$habitability_score ,method = "spearman") # rho 0.1294569  p-value < 2.2e-16
cor.test(x = hypothesis_data$property_area,y = hypothesis_data$number_of_windows,method = "spearman") # rho 0.4720676    p-value < 2.2e-16
cor.test(x = hypothesis_data$number_of_doors,y = hypothesis_data$number_of_windows,method = "spearman") # rho 0.2680299  p-value < 2.2e-16

# Function to viz the stat diff for habitability_score ~ one num feature
hypothesis_plots_nn <- function(df, col1,title){
  # Remove NA values
  df <- na.omit(df)  
  
  # Compute cor tests
  spearman <- cor.test(x = df[[col1]],y = df$habitability_score,method = "spearman")
  rho <- round(spearman$estimate,4)
  p_value <- round(spearman$p.value,4)
  
  # Jitter Plot
  n1 <- ggplot(data = df,aes(x = df[[col1]],y = habitability_score))+
    geom_jitter(aes(colour = habitability_score))+
    geom_smooth()+
    scale_colour_viridis_c(option = "plasma")+
    theme_minimal()+
    labs(
      title = "Jitter plot",
      x = col1,
      y = "Habitability",
      fill = ""
    )+
    theme(
      title = element_text(face = "italic"),
      legend.position = "none"
    )
  
    # Hex plot
  n2 <- ggplot(data = df, aes(x = df[[col1]], y = habitability_score)) +
    geom_hex()+
    scale_fill_viridis_c(option = "plasma")+
    theme_minimal()+
    labs(
      title = "Hex Plot",
      x = col1,
      y = "Habitability",
      fill = " Count"
    )+
    theme(
      title = element_text(face = "italic")
    )
    
  # Density plot
  n3 <- ggplot(data = df,aes(x = df[[col1]],y = habitability_score))+
    stat_density_2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE ) +
    scale_fill_viridis_c(option = "plasma")+
  theme_minimal()+
    labs(
      title = "Density Plot",
      x = col1,
      y = "Habitability",
      fill = ""
    )+
    theme(
      title = element_text(face = "italic"),
      legend.position = "none"
    )
  
  # Combine the plots
  final_plot <- (n3 + n1)/n2 +
    plot_annotation(
      title = title,
      subtitle = paste0("Spearman's rank correlation rho =",rho,", p ~",p_value,""))& 
    theme(
      plot.title = element_text(size = 18, face = "bold"),  
      plot.subtitle = element_text(size = 14)              
    )
  # Return 
  return(final_plot)
}

# Plot Habitability ~ air_quality_index
hypothesis_plots_nn(df = hypothesis_data,col1 = "air_quality_index",title = "Habitability Score and Air Quality")

# Plot Habitability ~ neighborhood_review
hypothesis_plots_nn(df = hypothesis_data,col1 = "neighborhood_review",title = "Habitability Score and Neighborhood Review")

# Plot Habitability ~ traffic_density_score
hypothesis_plots_nn(df = hypothesis_data,col1 = "traffic_density_score",title = "Habitability Score and Traffic Density")

# Plot Habitability ~ frequency_of_powercuts
hypothesis_plots_nn(df = hypothesis_data,col1 = "frequency_of_powercuts",title = "Habitability Score and Frequency of powercuts")

# Plot Habitability ~ number_of_windows
hypothesis_plots_nn(df = hypothesis_data,col1 = "number_of_windows",title = "Habitability Score and Number of windows")


