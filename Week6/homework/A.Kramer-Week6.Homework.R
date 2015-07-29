## ------------------------------------
## Aleksey Kramer
## Week 6 homework
## ------------------------------------

library(igraph)
library(logging)

setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_350\\Week6\\homework')

if(interactive()){
  
  # Load Facebook edge list
  facebook_edges <- read.csv("facebook_edge_list.csv", stringsAsFactors=FALSE)
  facebook_edges <- facebook_edges[complete.cases(facebook_edges),]
  facebook_degree_list <- table(c(facebook_edges$Source))
  facebook_mean_degree <- mean(facebook_degree_list)
  
  # Generate histogram
  facebook_hist <- hist(facebook_degree_list, breaks=70, freq=FALSE)
  
  # Fit power law
  pow_fit <- power.law.fit(facebook_degree_list)
  
  # Test if Poisson distribution
  # Determine length and mean of poisson random poisson distribution
  n <- length(facebook_edges$Source)
  m_mean <- facebook_mean_degree
  
  # Create sample poisson distribution
  poisson_dist <- rpois(n, m_mean)
  hist(poisson_dist)
  
  # Performing Kolmogorov-Smirnoff test to determine if facebook_edges$Source is
  # a type of Poisson distribution
  kt <- ks.test(facebook_edges$Source, poisson_dist)
  
}




