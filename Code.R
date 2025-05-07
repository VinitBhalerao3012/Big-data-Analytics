# Title - Processing and Analysing a Vehicle Sales dataset using R programming language.
#Author - Vinit Sachin Bhalerao(30121974)
#Date - 29/03/2024
#Description - The "Vehicle Sales and Market Trends Dataset" is a comprehensive collection of information on car sales transactions. This dataset includes information such as the year, make, model, trim, body type, gearbox type, VIN (Vehicle Identification Number), state of registration, condition rating, odometer reading, exterior and interior colors, seller information, Manheim Market Report (MMR) values, selling prices, and sale dates and I'm doing several tasks connected to evaluation 1.
#Input - I'm utilising the Vehicle Sales and Market Trends dataset in CSV format


                                                #Code

# We are utilising the "External" code package, which is not included with base R, therefore I have to install and load the tidyverse package before I can use it.

#Installation
install.packages("tidyverse")

#Loading
library(tidyverse)


#Importing an external data file (which is always in CSV format) into R using two pathways.
# 1.Relative path- It can be used if your CSV file is located in your R project folder (For example, in the "Input" sub folder).

data_relative <- read_csv("Input/car_prices.csv")

#2.Absolute path - If your CSV file is outside of your project folder, you can use it.

data_absolute <- read_csv("/Users/vinitbhalerao/Desktop/Assessment 1/Code/DataAssess1/Input/car_prices.csv")

# Data Transformation using dplyr package
#Using pipe operator( %>% -older)-supplying the next function's input with the previous function's output.

#columns functions
#Select()

  #Choose all column
  data_absolute %>%
  select(everything())

  # Choosing a specific column by name
  data_absolute %>%
  select(year,body)

  #choose each column between year and body
  data_absolute %>%
  select(year:body)

  #choose every column excluding those that go from year to body
  data_absolute %>%
  select(!year:body)

  #Use "-" before the column name if you wish to exclude a specific column.
  data_absolute %>%
  select(-year, -body)

  #choose each column between year or body
  data_absolute %>%
  select(year|body)

  #Choose just the character column
  data_absolute %>%
  select(where(is.character))

  #Select only numeric column
  data_absolute %>%
  select(where(is.numeric))

  #Select the columns whose names begin with letter "y"
  data_absolute %>%
  select(starts_with("y"))

  #Select the columns whose names ends with letter "e"
  data_absolute %>%
  select(ends_with("e"))

  #Select the columns whose names contains with letter "i"
  data_absolute %>%
  select(contains("i"))


#Renaming columns

  #rename a single column
  data_absolute %>%
   rename(MODEL = model)
  #rename a multiple columns
  data_absolute %>%
   rename(MODEL = model , BODY = body)


#Relocate or move some important column to the front

  data_absolute %>%
    relocate(color,condition)

#To add fresh columns that are computed using the current ones.

  KM_reading <- data_absolute %>%
    mutate(
      odometer_km = odometer * 1.60934 # 1 mile = 1.60934 km
    )
  view(KM_reading)

  #By default, newly created columns are on the right side; if you want to add a new column on the left side, then use ".before" argument

  data_absolute %>%
  mutate(
    odometer_km = odometer * 1.60934, # 1 mile = 1.60934 km
    .before = 1  # we can use position instead of column name
    )
  

  # Use ".after" argument if we want to add new column after some columns
  data_absolute %>%
    mutate(
      odometer_km = odometer * 1.60934, # 1 mile = 1.60934 km
      .after = year # we can use column name instead of position
     )
 


# To delete newly created column
  data_absolute %>%
    select(-c(2))
  

# Row functions

  #filter - which modifies which rows, in the same order, are present.
  # < - lessthan
   data_absolute %>%
    filter(sellingprice < 10000)

  # > - Greaterthan
   data_absolute %>%
    filter(sellingprice > 100000)

# <= lessthan equal to
     data_absolute %>%
      filter(year <= 2013)

    # >= graterthan equal to
     data_absolute %>%
      filter(condition >= 20)


    # We can also use logical operator
     data_absolute %>%
      filter(year >= 2014 & odometer <= 10000)


    # If we want only 2014 cars data then use "==" instead of "="
    data_absolute %>%
      filter(year == 2014)


    # arrange - Based on the value of the column, it modifies the row's order.
    data_absolute %>%
      arrange(year,model,trim)

    

    # Desc() can be utilized within Arrange to rearrange the data frame.(However, it will only function on a single column—not several.)
     data_absolute %>%
      arrange(desc(year))

  # distinct - You receive unique data within the dataset.
  data_absolute %>%
      distinct(year,model)
    

    #We can use in order to retain additional columns while searching for unique rows.Choose to
    # keep_all = True.
    data_absolute %>%
      distinct(year,model, .keep_all = TRUE)

    # For an accurate count of the number of occurrences, use count with sort = TRUE, e.g selling car's per year.
    data_absolute %>%
      count(year, sort = TRUE) %>%
      arrange(year)

   


    #New pipe operator |> (To insert this, make sure"Use native pipe operator" is selected in Tool-Option(Golbal options-code -editing window)

  data_absolute|>
    filter(sellingprice < 10000) |>
    mutate(odometer_km = odometer * 1.60934, .before = year) |>
    select(odometer_km:body) |>
    arrange(desc(year))


# group_by - divide data set into group

  data_absolute |>
    group_by(year=2015, make='kia')

# summarize - By using this we can calculate single summary statistic.

  data_absolute |>
    group_by(year) |>
    summarise(
      Avg_car_run_per_year=mean(odometer, na.rm = TRUE)

    )
# ungrouping - use to remove grouping from data frame

  data_absolute |>
    ungroup()

# .by - use to group with in the function.

  data_absolute |>
    summarise(
      Avg_car_run_per_year=mean(odometer, na.rm = TRUE),
      .by = year
       )
# Identify and correct errors in the data
  #Checking and Managing missing values

  missing_values <- sum(is.na(data_absolute))
  View(missing_values)

  Missing_values_percolumn <- colSums(is.na(data_absolute)) # Examine each column in detail for any missing data.
    view(Missing_values_percolumn)

    #Removing missing values from data set
    Remove_missing_values <- na.omit(data_absolute)
    View(Remove_missing_values)

    # Amount of removable missing values displayed as per column
    Remove_count_percolumn <- colSums(is.na(Remove_missing_values))
    view(Remove_count_percolumn)

    # Add some values(Mean, median or other suitable Value)  if we don't want to remove them.
    data_absolute$sellingprice[is.na(data_absolute$sellingprice)] <- mean(data_absolute$sellingprice, na.rm = TRUE)
    missing_value <- sum(is.na(data_absolute$sellingprice))
    View(missing_value)

# Identify and remove duplicate entries

    duplicate_entries <- data_absolute[duplicated(data_absolute) , ]
    view(duplicate_entries) # There are no duplicate records

    # If we found duplicate records then remove them using distinct()
    Remove_duplicate_entries <- distinct(data_absolute)

# Identify and handle outliers for selling price 

  #calculate Z-score(calculate Mean and standard deviation)
    Mean_sellingprice <- mean(data_absolute$sellingprice)
    SD_sellingprice <- sd(data_absolute$sellingprice)

      data_absolute$Z_score <- (data_absolute$sellingprice - Mean_sellingprice ) / SD_sellingprice

      threshold <- 3  # set threshold to identify outliers

      Outliers <- data_absolute[abs(data_absolute$Z_score) > threshold, ] # Identify outliers

      # Remove or Handle outliers
      data_absolute_clean <- data_absolute[abs(data_absolute$Z_score) <= threshold, ]

      #summary statistics both prior to and following
      summary(data_absolute$sellingprice)
      summary(data_absolute_clean$sellingprice)

      # Visualization before and after outliers removal
      boxplot(data_absolute$sellingprice, main="Selling price prior to removing outliers")
      boxplot(data_absolute_clean$sellingprice, main="Selling price after removing outliers")

    
#Communicating the structure of data_absolute (car_prices) dataset
      
      # Number of obsevations
      Total_No_Observation <- nrow(data_absolute)
      cat("Number of Observations:", Total_No_Observation, "\n\n") # Printing on the console by using "cat()"concatenation.
      
      # Number of variables 
      Total_No_Variables <- ncol(data_absolute)
      cat("Number of Variables:", Total_No_Variables, "\n\n")
      
      # Names of variables
      Variable_Names <- names(data_absolute)
      cat("Variable Names:", paste(Variable_Names, collapse = ", "), "\n\n")
      
      # Type of variables
      Variables_Types <- sapply(data_absolute , class)
      cat("Variable Types:", paste(Variables_Types, collapse = ", "), "\n\n")
      
      
      
# Summary statistics 
      
      # Select numeric variables
      numeric_variables <- c("year", "condition", "odometer", "mmr", "sellingprice")
      
      # Calculate mean for each numeric variable
      mean_values <- sapply(data_absolute[numeric_variables], mean, na.rm = TRUE)
      #Every numerical variable is iterated over in the "for" loop, and the mean value is printed.
      for (i in seq_along(numeric_variables)) { 
        cat("Mean of", numeric_variables[i], ":", mean_values[i], "\n")
      }
      
      # Calculate maximum for each numeric variable
      Maximum_values <- sapply(data_absolute[numeric_variables], max, na.rm= TRUE)
      for (i in seq_along(numeric_variables)) { 
        cat("Maximum val of", numeric_variables[i], ":", Maximum_values[i], "\n")
      }
      
      # Calculate minimum for each numeric variable
      Minimum_values <- sapply(data_absolute[numeric_variables], min, na.rm= TRUE)
      for (i in seq_along(numeric_variables)) { 
        cat("Minimum val of", numeric_variables[i], ":", Minimum_values[i], "\n")
      }
      
      # Calculate median for each numeric variable
      Median_values <- sapply(data_absolute[numeric_variables], median, na.rm= TRUE)
      for (i in seq_along(numeric_variables)) { 
        cat("Median of", numeric_variables[i], ":", Median_values[i], "\n")
      }
      
      #Calculate mode for each numeric variable 
      # We have to write one function 
        calculate_mode <- function(x) {
        tbl <- table(x)
        modes_values <- as.numeric(names(tbl)[tbl == max(tbl)])
        if (length(modes_values) > 1) {
          return("Multiple modes")
        } else {
          return(modes_values)
        }
      }
      mode_values <- sapply(data_absolute[numeric_variables], calculate_mode) # Calculating mode values
      for (i in seq_along(numeric_variables)) {
        cat("Mode of", numeric_variables[i], ":", mode_values[i], "\n")
      }
      
      
# Dispersion of numeric data
      
      # Variance <- sapply(data_absolute[numeric_variables], var, na.rm = TRUE)
      # for(i in seq_along(numeric_variables)){
      #   cat("Variance of" , numeric_variables[i], ":" , Variance[i], "\n")
      # } try to find out this way but it give me error so i have to check numeric variables are exist or not
      
      # Check if numeric variables exist in the dataset
      missing_variables <- setdiff(numeric_variables, names(data_absolute))
      if (length(missing_variables) > 0) {
        cat("The following variables are missing from the dataset:", paste(missing_variables, collapse = ", "), "\n")
      } else {
        # Calculate variance for each numeric variable
        variance_values <- sapply(data_absolute[numeric_variables], function(x) var(x, na.rm = TRUE))
        for (i in seq_along(numeric_variables)) {
          cat("Variance of", numeric_variables[i], ":", variance_values[i], "\n")
        }
      }
      
      
      # Calculate standard deviation for each numeric variables
      SD_values <- sapply(data_absolute[numeric_variables], sd, na.rm = TRUE)
      for (i in seq_along(numeric_variables)) {
        cat("Standard Deviation of", numeric_variables[i], ":", SD_values[i], "\n")
      }
      
      # Calculate Range for each numeric variables 
      Range_variables <- sapply(data_absolute[numeric_variables], function(x) max(x, na.rm = TRUE) - min(x , na.rm = TRUE))
      for (i in seq_along(numeric_variables)) {
        cat("Range of", numeric_variables[i], ":", Range_variables[i], "\n")
      }
      
      # Calculate Interquartile range (IQR) for each variable
      IQR_values <- sapply(data_absolute[numeric_variables], IQR, na.rm = TRUE)
      for (i in seq_along(numeric_variables)) {
        cat("IQR of", numeric_variables[i], ":", IQR_values[i], "\n")
      }
      
      
      
      
      # Calculate frequency count of categorical data
      # We have to check each column for categorical data
      # Select columns to check for categorical data
      columns_to_check <- c("year", "make", "model", "trim", "body", "transmission", 
                            "vin", "state", "condition", "odometer", "color", 
                            "interior", "seller", "mmr", "sellingprice", "saledate")
     for (column in columns_to_check) {
        unique_values <- unique(data_absolute[[column]])
        num_unique <- length(unique_values)
        if (num_unique <= 10) {
          cat(column, "appears to contain categorical data with", num_unique, "unique values:", unique_values, "\n")
        } else {
          cat(column, "does not appear to contain categorical data with", num_unique, "unique values\n")
        }
     }
      #Calculate frequency count of transmission 
      Transmission_freq_count <-  table(data_absolute$transmission)
      print(Transmission_freq_count)
      
# Visualising the distribution and patterns of data.
      
      # Histogram for single variable (Histograms are univariate plots)
      ggplot(data_absolute_clean, aes(x = sellingprice)) +
        geom_histogram(fill = "skyblue", color = "black") +
        labs(title = "Distribution of Selling Price")
      
      # Scatter plot for two variables 
      ggplot(data_absolute_clean, aes(x = odometer, y = sellingprice)) +
        geom_point() +
        labs(title = "Scatterplot of Odometer vs. Selling Price")
      
      # Density plot for single variable 
      ggplot(data_absolute_clean, aes(x = sellingprice)) +
        geom_density(fill = "orange", color = "black") +
        labs(title = "Density Plot of Sellingprice Reading")
      
      # Density plot for two-variable (2D density plot) 
      ggplot(data_absolute_clean, aes(x = odometer, y = sellingprice)) +
        geom_density_2d(color = "black") +
        labs(
          title = "Density Plot of Odometer vs. Selling Price",
          x = "Odometer",
          y = "Selling Price"
        ) +
        theme_minimal()
      
      
      # box plot for two variables 
      ggplot(data_absolute_clean, aes(x = transmission, y = sellingprice)) +
        geom_boxplot(fill = "white", color = "black") +
        labs(title = "Box Plot of Selling Price by Transmission Type")