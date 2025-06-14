## CALCULATE DIVERSITY METRICS FOR BRITAIN AND IRELAND: HECTAD LEVEL ##

## I. Set the working directory  -----
setwd("D:/1_ExOrChiST-data/ExOrChiST-GDrive folder/Plant_Atlas_Trends-WORK WITH THESE")

## II. Load the libraries  -----
library(terra)
library(raster)
library(sf)
library(tidyverse)
library(phyloregion)
library(picante)
library(betapart)
library(CommEcol)


## III. Calculate taxonomic diversity for each time period selected -----

# Time Period 1: pre 1970 (1930-1969)
# Time Period 2: 1987 - 1999
# Time Period 3: 2000 - 2009
# Time Period 4: 2010 - 2019

## ----- BRITAIN ----- ##

# a. Load time period shapefiles

gb_dt_1 <- st_read("./Spatial_Data_trends/GB_NEW/gb_pre_1970.shp")
gb_dt_2 <- st_read("./Spatial_Data_trends/GB_NEW/gb_1987_99.shp")
gb_dt_3 <- st_read("./Spatial_Data_trends/GB_NEW/gb_2000_09.shp") 
gb_dt_4 <- st_read("./Spatial_Data_trends/GB_NEW/gb_2010_19.shp") 


# b. Assign zeros to NA values

gb_dt_1[is.na(gb_dt_1)] <- 0
gb_dt_2[is.na(gb_dt_2)] <- 0
gb_dt_3[is.na(gb_dt_3)] <- 0
gb_dt_4[is.na(gb_dt_4)] <- 0


# c. Remove column 'sum' from dataframes

gb_dt_1 <- gb_dt_1 %>% dplyr::select(-sum)
gb_dt_2 <- gb_dt_2 %>% dplyr::select(-sum)
gb_dt_3 <- gb_dt_3 %>% dplyr::select(-sum)
gb_dt_4 <- gb_dt_4 %>% dplyr::select(-sum)


## d. Assign species names and codes for each time-period

time_periods <- c("t1", "t2", "t3", "t4") # Define the time periods
abrs <- list() # create an empty list to store the abbreviations for each

# Loop through each time period
for (t in time_periods) {
  abrs[[t]] <- readxl::read_excel(paste0('./species_abrs/species_list_GB_', t, '.xlsx')) %>%
    mutate(sp_name = str_replace_all(sp_name, '[- ]', '_')) %>%   # Replace both '-' and ' ' with '_'
    arrange(sp_name)
}

abrs1 <- abrs$t1
abrs2 <- abrs$t2
abrs3 <- abrs$t3
abrs4 <- abrs$t4


# Replace species codes with proper species names

clnms_1 <- gb_dt_1 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% abrs1$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             abrs1) %>% 
  add_row(name = 1,
          sp_code = 'GridRef',
          sp_name = 'GridRef') %>% 
  add_row(name = 55,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)

clnms_1
colnames(gb_dt_1) <- clnms_1$sp_name ## match names with codes in the colnames


clnms_2 <- gb_dt_2 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% abrs2$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             abrs2) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 55,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)

clnms_2

colnames(gb_dt_2) <- clnms_2$sp_name ## match names with codes in the colnames


clnms_3 <- gb_dt_3 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% abrs3$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             abrs3) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 55,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)

clnms_3
colnames(gb_dt_3) <- clnms_3$sp_name ## match names with codes in the colnames


clnms_4 <- gb_dt_4 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% abrs4$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             abrs4) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 55,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)

clnms_4
colnames(gb_dt_4) <- clnms_4$sp_name ## match names with codes in the colnames


## e. Estimate Species Richness, CWE and their 1%, 2.5% and 5% -----

# 1. Create the sparse matrices for Corrected Weighted Endemism (CWE)

comm_1 <-  gb_dt_1 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

comm_2 <-  gb_dt_2 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

comm_3 <-  gb_dt_3 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

comm_4 <-  gb_dt_4 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()


# 2. Define the datasets for each time period
time_periods <- c("t1", "t2", "t3", "t4")
GB_datasets <- list(gb_dt_1, gb_dt_2, gb_dt_3, gb_dt_4)

# 3. Loop through each dataset and calculate metrics
GB_data <- lapply(seq_along(GB_datasets), function(i) {
  
  dataset <- GB_datasets[[i]] # Get the dataset and time period name
  period <- time_periods[i]

  SR <- dataset %>% # Calculate Species Richness (SR)
    st_drop_geometry() %>%
    dplyr::select(Anacamptis_morio:Spiranthes_spiralis) %>%
    rowSums()
  
  dataset$SR <- SR
  
  dataset <- dataset %>% # Calculate Hotspots for SR
    dplyr::mutate(
      SR_T1 = hotspots(SR, prob = 1),
      SR_T2_5 = hotspots(SR, prob = 2.5),
      SR_T5 = hotspots(SR, prob = 5)
    )
  
  # Calculate Corrected Weighted Endemism (CWE) and its hotspots
  dataset <- dataset %>%
    dplyr::mutate(
      CWE = weighted_endemism(dataset %>%
                                st_drop_geometry() %>%
                                dplyr::select(-GridRef) %>%
                                dense2sparse()),
      CWE_T1 = hotspots(CWE, prob = 1),
      CWE_T2_5 = hotspots(CWE, prob = 2.5),
      CWE_T5 = hotspots(CWE, prob = 5)
    )
  
  # Assign the dataset to its original variable name
  assign(paste0("gb_dt_", i), dataset, envir = .GlobalEnv)
  
  return(dataset)
})

# 4. Name the dataset list assign each dataset to a name for easier handling

names(GB_data) <- paste0("gb_dt_", 1:4)

GB_t1 <- GB_data$gb_dt_1
GB_t2 <- GB_data$gb_dt_2
GB_t3 <- GB_data$gb_dt_3
GB_t4 <- GB_data$gb_dt_4


# 5. Create new shapefiles including all calculated metrics

# Define the datasets and file names for each time period
datasets <- list(GB_t1, GB_t2, GB_t3, GB_t4)
time_periods <- c("t1", "t2", "t3", "t4")
output_path <- './Hotspot_Analysis/Shapefiles_SR_CWE/'

# Loop through each dataset and write the shapefile
lapply(seq_along(datasets), function(i) {
  dataset <- datasets[[i]]
  period <- time_periods[i]
  
  # Clean and select relevant columns
  cleaned_data <- dataset %>%
    dplyr::select(GridRef,
                  contains('SR'),
                  contains('CWE'),
                  geometry) %>%
    janitor::clean_names()
  
  # Write the shapefile
  st_write(cleaned_data, paste0(output_path, "GB_", period, "_metrics.shp"))
})




##============================================================================##
##
##
## --------- IRELAND ---------- ##

## Calculate taxonomic diversity for each time period selected -----

# Time Period 1: pre 1970 (1930-1969)
# Time Period 2: 1987 - 1999
# Time Period 3: 2000 - 2009
# Time Period 4: 2010 - 2019


# a. Load time period shapefiles

ir_dt_1 <- st_read("./Spatial_Data_trends/IR_NEW/ir_pre_1970.shp")
ir_dt_2 <- st_read("./Spatial_Data_trends/IR_NEW/ir_1987_99.shp")
ir_dt_3 <- st_read("./Spatial_Data_trends/IR_NEW/ir_2000_09.shp") 
ir_dt_4 <- st_read("./Spatial_Data_trends/IR_NEW/ir_2010_19.shp") 


# b. Assign zeros to NA values

ir_dt_1[is.na(ir_dt_1)] <- 0
ir_dt_2[is.na(ir_dt_2)] <- 0
ir_dt_3[is.na(ir_dt_3)] <- 0
ir_dt_4[is.na(ir_dt_4)] <- 0

# c. Remove column 'sum' from dataframes

ir_dt_1 <- ir_dt_1 %>% dplyr::select(-sum)
ir_dt_2 <- ir_dt_2 %>% dplyr::select(-sum)
ir_dt_3 <- ir_dt_3 %>% dplyr::select(-sum)
ir_dt_4 <- ir_dt_4 %>% dplyr::select(-sum)


## d. Load the species and abbreviations for each time-period  

time_periods <- c("t1", "t2", "t3", "t4")
ir_abrs <- list()

# Loop through each time period
for (t in time_periods) {
  ir_abrs[[t]] <- readxl::read_excel(paste0('./species_abrs/species_list_IR_', t, '.xlsx')) %>%
    mutate(sp_name = str_replace_all(sp_name, '[- ]', '_')) %>%   # Replace both '-' and ' ' with '_'
    arrange(sp_name)
}

ir_abrs1 <- ir_abrs$t1
ir_abrs2 <- ir_abrs$t2
ir_abrs3 <- ir_abrs$t3
ir_abrs4 <- ir_abrs$t4


# Replace species codes with proper species names

ir_clnms_1 <- ir_dt_1 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% ir_abrs1$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             ir_abrs1) %>% 
  add_row(name = 1,
          sp_code = 'GridRef',
          sp_name = 'GridRef') %>% 
  add_row(name = 33,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)
ir_clnms_1

colnames(ir_dt_1) <- ir_clnms_1$sp_name


ir_clnms_2 <- ir_dt_2 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% ir_abrs2$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             ir_abrs2) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 33,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)
ir_clnms_2

colnames(ir_dt_2) <- ir_clnms_2$sp_name


ir_clnms_3 <- ir_dt_3 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% ir_abrs3$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             ir_abrs3) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 33,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)
ir_clnms_3

colnames(ir_dt_3) <- ir_clnms_3$sp_name


ir_clnms_4 <- ir_dt_4 %>% 
  colnames() %>% 
  enframe() %>% 
  dplyr::filter(value %in% ir_abrs4$sp_code) %>% 
  dplyr::rename(sp_code = value) %>% 
  inner_join(., 
             ir_abrs4) %>% 
  add_row(
    name = 1,
    sp_code = 'GridRef',
    sp_name = 'GridRef') %>% 
  add_row(name = 33,
          sp_code = 'geometry',
          sp_name = 'geometry') %>% 
  arrange(name)
ir_clnms_4

colnames(ir_dt_4) <- ir_clnms_4$sp_name


## e. Estimate Species Richness, CWE and the 1%, 2.5% and 5% -----

# Create the sparse matrices for Corrected Weighted Endemism

ir_comm_1 <-  ir_dt_1 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

ir_comm_2 <-  ir_dt_2 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

ir_comm_3 <-  ir_dt_3 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

ir_comm_4 <-  ir_dt_4 %>% 
  st_drop_geometry() %>% 
  dplyr::select(-GridRef) %>% 
  dense2sparse()

# Calculate the metrics

# 1. Define the datasets for each time period
time_periods <- c("t1", "t2", "t3", "t4")
IR_datasets <- list(ir_dt_1, ir_dt_2, ir_dt_3, ir_dt_4)

# 2. Loop through each dataset and perform the calculations
IR_data <- lapply(seq_along(IR_datasets), function(i) {
  
  # Get the dataset and time period name
  dataset <- IR_datasets[[i]]
  period <- time_periods[i]
  
  # Calculate Species Richness (SR)
  SR <- dataset %>%
    st_drop_geometry() %>%
    dplyr::select(Anacamptis_morio:Spiranthes_spiralis) %>%
    rowSums()
  
  dataset$SR <- SR
  
  # Calculate Hotspots for SR
  dataset <- dataset %>%
    dplyr::mutate(
      SR_T1 = hotspots(SR, prob = 1),
      SR_T2_5 = hotspots(SR, prob = 2.5),
      SR_T5 = hotspots(SR, prob = 5)
    )
  
  # Calculate Corrected Weighted Endemism (CWE) and its hotspots
  dataset <- dataset %>%
    dplyr::mutate(
      CWE = weighted_endemism(dataset %>%
                                st_drop_geometry() %>%
                                dplyr::select(-GridRef) %>%
                                dense2sparse()),
      CWE_T1 = hotspots(CWE, prob = 1),
      CWE_T2_5 = hotspots(CWE, prob = 2.5),
      CWE_T5 = hotspots(CWE, prob = 5)
    )
  
  # Assign the dataset to its original variable name
  assign(paste0("ir_dt_", i), dataset, envir = .GlobalEnv)
  
  return(dataset)
})

# 3. Name the dataset list assign each dataset to a name for easier handling

names(IR_data) <- paste0("ir_dt_", 1:4)

IR_t1 <- IR_data$ir_dt_1
IR_t2 <- IR_data$ir_dt_2
IR_t3 <- IR_data$ir_dt_3
IR_t4 <- IR_data$ir_dt_4



# 4. Create new shapefiles including all calculated metrics

# Define the datasets and file names for each time period
datasets <- list(IR_t1, IR_t2, IR_t3, IR_t4)
time_periods <- c("t1", "t2", "t3", "t4")
output_path <- './Hotspot_Analysis/Shapefiles_SR_CWE/'

# Loop through each dataset and write the shapefile
lapply(seq_along(datasets), function(i) {
  dataset <- datasets[[i]]
  period <- time_periods[i]
  
  # Clean and select relevant columns
  cleaned_data <- dataset %>%
    dplyr::select(GridRef,
                  contains('SR'),
                  contains('CWE'),
                  geometry) %>%
    janitor::clean_names()
  
  # Write the shapefile
  st_write(cleaned_data, paste0(output_path, "IR_", period, "_metrics.shp"))
})



##============================================================================##

