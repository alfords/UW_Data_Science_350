## ------------------------------------
## Aleksey Kramer
## Week 6 homework
## ------------------------------------

require(igraph)
require(logging)

# setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_350\\Week6\\homework')
setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week6/homework')

if(interactive()){
  # Setup Test Logger
  basicConfig()
  addHandler(writeToFile, file="HW-WEEK6-LOG.txt", level='DEBUG')
  
  # Load Facebook edge list
  loginfo("Read facebook_edgee_list.csv")
  facebook_edges <- read.csv("facebook_edge_list.csv", stringsAsFactors=FALSE)
  
  loginfo("Filter complete cases")
  facebook_edges <- facebook_edges[complete.cases(facebook_edges),]
  
  loginfo("Generate facebook degree list based on Source variable")
  facebook_degree_list <- table(c(facebook_edges$Source))
  
  loginfo("Store a mean of facebook degree list")
  facebook_mean_degree <- mean(facebook_degree_list)
  
  # Generate histogram
  loginfo("Generating histogram of facebook degree list")
  facebook_hist <- hist(facebook_degree_list, breaks=70, freq=FALSE)
  
  # Fit power law
  loginfo("Fit power law")
  pow_fit <- power.law.fit(facebook_degree_list)
  
  # Test if Poisson distribution
  # Determine length and mean of poisson random poisson distribution
  loginfo("Determine the length needed to produce poisson distribution")
  n <- length(facebook_edges$Source)
  
  loginfo("Get the mean required to produce poisson distibution")
  m_mean <- facebook_mean_degree
  
  # Create sample poisson distribution
  loginfo("Generating and graphing poison distibution")
  poisson_dist <- rpois(n, m_mean)
  hist(poisson_dist)
  
  # Performing Kolmogorov-Smirnoff test to determine if facebook_edges$Source is
  # a type of Poisson distribution
  loginfo("Running Kolmogorov-Smirnov test of facebook_edge_list and poisson_dist")
  kt <- ks.test(facebook_edges$Source, poisson_dist)
}




