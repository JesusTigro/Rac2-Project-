## SIMULATION OF RESURGENCE AS CHOICE IN CONTEXT (RaC2)
## Author: Rubén Jesús Jiménez Ríos
## SCRIPT DESIGNED TO SIMULATE/ANALYZE DATA FROM RESURGENCE EXPERIMENTS 

# Load libraries
library(tidyverse)
library(openxlsx)
library(nloptr)

####################################################################
###### CREATE FUNCTION THAT SIMULATES RaC2 ########################
##################################################################
#PARAMETERS LIST
## valores_obj = List of the overall reinforcement rate for the Target Behavior
## valores_alt = List of the overall reinforcement rate for the Alternative Behavior 
## datos = The DF where the ON and OFF values will be obtained
## lambda = Modulates how quickly "c" increases with the reinforcement rate
## k = Asymptotic baseline response rates
## a = Slope of the relationship between arousal and value
## dm = level of the asymptotic value of the bias term
## b = bias for an alternative

simulate_experiment <- function(valores_obj, valores_alt, datos, lambda, k, a, dm, b) {
  
  #######################################################
  #CALCULATE THE VALUE OF THE TARGET RESPONSE
  ######################################################
  
  #session : the session to analyze in each iteration
  #RX: The rate of reinforcers for the session time
  #valor: The final output. It is the value of the session to be analyzed
  
  # Create the empty dataframe. It will be filled with the following function #
  
  data_obj <- data.frame(sesion = integer(0), 
                         RX = numeric(0), 
                         valor = numeric(0), 
                         id = character(0),
                         stringsAsFactors = FALSE)
  
  # Function to add an RX value to the dataframe and update the session
  RX_objetivo <- function(value, lambda, sesion_actual) {
    if (sesion_actual %in% data_obj$sesion) {
      data_obj <<- data.frame(sesion = integer(0), 
                              RX = numeric(0), 
                              valor = numeric(0),
                              id = character(0),
                              stringsAsFactors = FALSE)
    }
    
    data_obj <<- data_obj %>% 
      add_row(sesion = ifelse(nrow(data_obj) == 0, 1, max(data_obj$sesion) + 1), 
              RX = value,
              id = "RO")
    data_calculada <- calcular_columnas(data_obj, lambda, sesion_actual)
    V_t <- sum(data_calculada$w_x_times_RX)
    data_obj[["valor"]][nrow(data_obj)] <<- V_t
  }
  
  # Function to calculate the values of tx, 1/tx^c, 
  #wx(temporal weighting) and session value (wx * Rx), 
  #and the parameters lambda and analyzed session
  calcular_columnas <- function(data_obj, lambda, sesion_actual) {
    c_valor <- (lambda * mean(data_obj$RX)) + 1
    data_obj <- data_obj %>%
      mutate(t_x = sesion_actual - sesion + 1,
             one_over_t_x_c = 1 / (t_x ^ c_valor),
             w_x = one_over_t_x_c / sum(one_over_t_x_c),
             w_x_times_RX = w_x * RX)
    return(data_obj)
  }
  
  
  # Iterates over the values and calls the RX_alternativa function with the value and current session
  for (i in 1:length(valores_obj)) {
    sesion_actual <- i
    RX_objetivo(valores_obj[i], lambda, sesion_actual)
  }
  
  
  #########################################################
  #CALCULATE THE VALUE OF THE ALTERNATIVE RESPONSE
  ########################################################
  
  # Create the empty dataframe
  data_alt <- data.frame(sesion = integer(0), 
                         RX = numeric(0), 
                         valor = numeric(0),
                         id = character(0),
                         stringsAsFactors = FALSE)
  
  # Function to add an RX value to the dataframe and update the session
  RX_alternativa <- function(value, lambda, sesion_actual) {
    if (sesion_actual %in% data_alt$sesion) {
      data_alt <<- data.frame(sesion = integer(0), 
                              RX = numeric(0), 
                              valor = numeric(0),
                              id = character(0),
                              stringsAsFactors = FALSE)
    }
    
    data_alt <<- data_alt %>% 
      add_row(sesion = ifelse(nrow(data_alt) == 0, 1, 
                              max(data_alt$sesion) + 1), 
              RX = value,
              id = "RA")
    
    data_calculada <- calcular_columnas(data_alt, lambda, sesion_actual)
    V_t <- sum(data_calculada$w_x_times_RX)
    data_alt[["valor"]][nrow(data_alt)] <<- V_t
  }
  
  # Function to calculate the values of tx, 1/tx^c, 
  #wx(temporal weighting) and session value (wx * Rx), 
  #and the parameters lambda and analyzed session
  calcular_columnas <- function(data_alt, lambda, sesion_actual) {
    c_valor <- (lambda * mean(data_alt$RX)) + 1
    data_alt <- data_alt %>%
      mutate(t_x = sesion_actual - sesion + 1,
             one_over_t_x_c = 1 / (t_x ^ c_valor),
             w_x = one_over_t_x_c / sum(one_over_t_x_c),
             w_x_times_RX = w_x * RX)
    return(data_alt)
  }
  
  
  # Iterates over the values and calls the RX_alternativa function with the value and current session
  for (i in 1:length(valores_alt)) {
    sesion_actual <- i
    RX_alternativa(valores_alt[i], lambda, sesion_actual)
  }
  
  #################################################################
  # CALCULATE THE PROBABILITY OF THE TARGET AND ALTERNATIVE RESPONSE
  #################################################################
  
  # Merge the two dataframes of the Target response and Alternative response
  data_valor <- rbind(data_alt, data_obj)
  
  Data_sum <- data_valor %>%
    group_by(sesion, id) %>% # Groups the data by session and type of response
    summarise(valor_sum = sum(valor)) %>% # Sums the "value" for each response and session
    spread(id, valor_sum) %>% # Converts the value of the responses into columns
    mutate(total = RO + RA) %>% # Creates a new column "total" that contains the sum of the responses
    gather(id, valor_sum, -sesion, -total) # Converts the columns into rows
  
  # Merge the results with the original dataframe
  data_valor <- data_valor %>%
    left_join(Data_sum, by = c("sesion", "id"))
  
  # Apply the formula a/(a+b) for each data of the "valor" column in function of the columns "id" and "sesion"
  data_valor <- data_valor %>%
    mutate(probabilidad = valor / total) %>%
    select(-valor_sum, -total)
  
  
  ################################################
  # CALCULATE THE RESPONSE RATE OF THE TARGET AND ALTERNATIVE RESPONSE
  ################################################
  # Merge the values of the alternative and target behavior
  datos_tasa <- rbind(data_alt, data_obj)
  
  # Get the value of "A"
  datos_tasa <- datos_tasa %>%
    select(-RX) %>% # Removes the column 'Rx', as it is not necessary
    spread(key = id, value = valor) %>% # Reorganizes the data transforming "id" into new columns and "valor" is distributed in these columns
    mutate(A = a * (RA + RO))%>% # Creates the column "A" with the results of a*(RA+RO)
    mutate(condition = datos$condition) %>% # Loads a condition phase
    # Loads the ON and OFF data for the alternative and target behavior
    mutate( on_Tb = datos$on_Tb ) %>%
    mutate(off_Tb = datos$off_Tb) %>%
    mutate(on_Ab = datos$on_Ab ) %>%
    mutate(off_Ab = datos$off_Ab)
  
  
  # Convert ON and OFF values to numeric
  datos_tasa$on_Tb <- as.numeric(datos_tasa$on_Tb)
  datos_tasa$off_Tb <- as.numeric(datos_tasa$off_Tb)
  datos_tasa$on_Ab <- as.numeric(datos_tasa$on_Ab)
  datos_tasa$off_Ab <- as.numeric(datos_tasa$off_Ab)
  
  # Set the values of d1 and d0
  datos_tasa <- datos_tasa %>%
    mutate(d1_obj = ifelse(condition == 2, dm * (1 - exp(1)^-on_Tb), NA),
           d0_obj = ifelse(condition == 3, dm * (1 - exp(1)^-off_Tb), NA),
           d1_alt = ifelse(condition == 3, dm * (1 - exp(1)^-on_Ab), NA),
           d0_alt = ifelse(condition == 2, dm * (1 - exp(1)^-off_Ab), NA))
  
  
  # Creates the target and alternative response rate of phase 1
  datos_tasa <- datos_tasa %>%
    mutate( tasa_respuesta_obj = (k*RO)/(RO + (RA/b)+ (1/A)),
            tasa_respuesta_alt = (k * (RA/b))/(RO + (RA/b) + (1/A)))
  
  # Create the target response rate in ON and OFF phases
  datos_tasa <- datos_tasa %>%
    mutate(tasa_respuesta_obj_on = ifelse(condition == 2, (k * RO) / (RO + (d1_obj * RA) + (1 / A)), NA),
           tasa_respuesta_obj_off = ifelse(condition == 3, ((k * RO) / d0_obj) / ((RO / d0_obj) + (RA / d0_obj) + (1 / A)), NA))
  
  
  # Create the target response rate in ON and OFF phases
  datos_tasa <- datos_tasa %>%
    mutate(tasa_respuesta_alt_off = ifelse(condition == 2, ((k*RA)/d0_alt)/((RO/d0_alt)+(RA/d0_alt)+ (1/A)), NA),
           tasa_respuesta_alt_on = ifelse(condition == 3, ((k*d1_alt)*RA)/(RO + (d1_alt * RA) + (1 / A)), NA))
  
  # Build the final DF to be plotted
  datos_tasa <- datos_tasa %>%
    mutate(tasa_respuesta_objetivo = case_when(
      condition == 1 ~ datos_tasa$tasa_respuesta_obj,
      condition == 2 ~ datos_tasa$tasa_respuesta_obj_on,
      condition == 3 ~ datos_tasa$tasa_respuesta_obj_off,
      TRUE ~ NA
    ))
  datos_tasa <- datos_tasa %>%
    mutate(tasa_respuesta_alternativa = case_when(
      condition == 1 ~ datos_tasa$tasa_respuesta_alt,
      condition == 2 ~ datos_tasa$tasa_respuesta_alt_off,
      condition == 3 ~ datos_tasa$tasa_respuesta_alt_on,
      TRUE ~ NA
    ))
  
  
}  

######################################################################
############ Space to load your data #################################
#####################################################################

#Load your database. Your DF should contain 7 necessary columns:
#a) Sessions, b) General reinforcement rate of B. Target,
#c) General reinforcement rate of T. Alternative, 
#d) ON List for T. Objective, e) OFF List for B.Target 
#f) ON List for T. Alternative, g) OFF List for B. Alternative 

#datos <- read_excel("Thesis/Tasks/Resurgence Experiment/example.xlsx")
#At the moment, it only accepts one subject (or group) per analysis
#datos <- datos %>%
# filter(Group == 1)

##For purposes of illustrating how the model works, a DF is built from 
##scratch, but it is possible to load DF with the commented lines behind it.
on_Tb <- c(NA,NA,NA,NA,NA,1, 2, 3, 4, 5, NA,NA,NA,NA,NA)
off_Tb <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 1, 2, 3, 4, 5)
on_Ab <- c(NA,NA,NA,NA,NA, NA,NA,NA,NA,NA, 1, 2, 3, 4, 5)
off_Ab <- c(NA,NA,NA,NA,NA, 1, 2, 3, 4, 5, NA,NA,NA,NA,NA)
condition <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3 ,3)
datos <- data.frame(on_Tb,off_Tb, on_Ab, off_Ab, condition)
######################################################################
######### CALCULATE DIFFERENCES BETWEEN THE PREDICTED AND OBTAINED ###
#####################################################################

#Load the first two parameters of the simulate_experiment function
valores_obj <- c(1800,1800,1800,1800,1800,0,0,0,0,0,0,0,0,0,0)
valores_alt <- c(0,0,0,0,0,1800,1800,1800,1800,1800, 0,0,0,0,0)
params <- c(0.001082181, 126.046457,0.0000467983, 1.9667317, 1)

#Load the response rates obtained empirically
s1 <- c(94.44, 116.44, 120.84, 122.06, 116.92, 50.14, 15.6, 12.62, 11.1,
        9.84, 39.16, 29.8, 26.52, 19.44, 23.22)
s2 <- c(22.72, 7.4, 5.48, 4.66, 5.48, 78.78, 119.46, 119.52, 120.08, 120.64,
        68.98, 39.24, 34.86, 24.82, 26.7)
sesion <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
condition <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
#Create a DF to use within the next function
datos_tasa_real <- cbind(s1, s2, sesion, condition)
datos_tasa_real <- as.data.frame(datos_tasa_real)

######################################################################
#Function to calculate the error between the predicted and obtained
#####################################################################
calculate_error <- function(params, valores_obj, valores_alt, datos_tasa_real) {
  # Extract the parameters from the 'params' vector
  lambda <- params[1]
  k <- params [2]
  a <- params[3]
  dm <- params[4]
  b <- params[5]
  
  # Print the parameters (useful for debugging)
  print(params)
  
  # Check if parameters are negative. If so, return Inf
  if(lambda < 0 || k < 0 || a < 0 || dm < 0 || b < 0){
    return(Inf)
  }
  
  # Attempt to simulate the experiment with the current parameters
  datos_tasa_simulated <- tryCatch({
    simulate_experiment(valores_obj, valores_alt, datos, lambda, k, a, dm, b)
  }, error = function(e) {
    # If the simulation fails, print an error message and return NULL
    print("Error in simulate_experiment")
    return(NULL)
  })
  
  # If the simulation failed and returned NULL, return Inf
  if(is.null(datos_tasa_simulated)){
    return(Inf)
  }
  
  # Calculate the sum of the absolute differences between the simulated and real data
  error_obj <- sum(abs(datos_tasa_simulated$tasa_respuesta_obj_on - datos_tasa_real$s1), na.rm = TRUE)
  error_alt <- sum(abs(datos_tasa_simulated$tasa_respuesta_alt_off - datos_tasa_real$s2), na.rm = TRUE)
  
  # Add up the errors of both columns to get a total error value
  total_error <- error_obj + error_alt
  
  # If the total error is NaN or Inf, replace it with Inf
  if (is.nan(total_error) || is.infinite(total_error)) {
    total_error <- Inf
  }
  
  # Print the total error (useful for debugging)
  print(total_error)
  
  # Return the total error
  return(total_error)
}

######################################################################
############ ESTIMATE THE PARAMETERS OF THE MODEL ####################
#####################################################################

# Define initial values for the parameters lambda, k, a, dm, and b.
initial_params <- c(0.001082181, 126.046457, 0.0000467983, 1.9667317, 1)

# Define lower bounds for the parameters. 
# These bounds are used by the optimization algorithm to ensure that parameters stay within an acceptable range.
lower_bounds <- c(0, 0, 0, 0, 0.1)

# Define upper bounds for the parameters. 
# Like the lower bounds, these bounds are used by the optimization algorithm.
upper_bounds <- c(1, Inf, 1, Inf, Inf) # Inf means infinity in R

# Define options for the optimization algorithm, including the specific algorithm to use (NLOPT_LN_NELDERMEAD), 
# the relative tolerance for algorithm convergence (xtol_rel = 1.0e-8), and the maximum number of evaluations of the objective function (maxeval = 10000).
opts <- list("algorithm" = "NLOPT_LN_NELDERMEAD", 
             "xtol_rel" = 1.0e-8,
             "maxeval" = 2000)

# Use the 'nloptr' function to minimize the error function 'calculate_error' using the parameters and bounds defined above.
# The arguments 'target_values', 'alternative_values' and 'real_rate_data' are additional data that is passed to the error function.
optim_result <- nloptr(x0=initial_params, eval_f=calculate_error, lb=lower_bounds, 
                       ub=upper_bounds, opts=opts,
                       valores_obj = valores_obj, 
                       valores_alt = valores_alt, 
                       datos_tasa_real = datos_tasa_real)

# Print the optimized parameters found by 'nloptr'.
print(optim_result$solution)

# Print the status of the optimization. A value of 0 generally indicates success in optimization.
print(optim_result$status)

# Print the minimum value of the error function reached during the optimization.
print(optim_result$objective)

############################################################################
### Uses the function with the estimated parameters.#######################
###########################################################################
datos_tasa <- simulate_experiment(valores_obj, valores_alt, datos,
                                  )

####################################################
############# Model Quality Estimation #############
####################################################

# New columns are created in the 'datos_tasa' data frame. These are the log-transformed versions of the
# target and alternative response rates.
datos_tasa <- datos_tasa %>%
  mutate(log_R_Rate_target_pre = log(datos_tasa$tasa_respuesta_objetivo),
         log_R_Rate_alternative_pre =
           ifelse(condition == 2 | condition == 3,log(datos_tasa$tasa_respuesta_alternativa), NA))

# Similar log-transformations are done for the 'datos_tasa_real' data frame.
datos_tasa_real <- datos_tasa_real %>%
  mutate(log_R_Rate_target_obt = log(datos_tasa_real$s1),
         log_R_Rate_alternative_obt = 
           ifelse(condition == 2 | condition == 3, log(datos_tasa_real$s2),NA))


# A new data frame 'fit_model' is created by column-binding the log-transformed response rates from the 
# 'datos_tasa' and 'datos_tasa_real' data frames.
fit_model <- cbind(datos_tasa$log_R_Rate_target_pre, datos_tasa$log_R_Rate_alternative_pre,
                   datos_tasa_real$log_R_Rate_target_obt, datos_tasa_real$log_R_Rate_alternative_obt)

# The resulting matrix is converted to a data frame.
fit_model <- as.data.frame(fit_model)

# The column names of the 'fit_model' data frame are updated.
colnames(fit_model) <- c("Log_RRate_target_pred", "Log_RRate_alternative_pred",
                         "Log_RRate_target_obt", "Log_RRate_alternative_obt")


# The actual and predicted log-transformed target response rates are assigned to 'y_real' and 'y_pred' variables, respectively.
y_real_tar <- fit_model$Log_RRate_target_obt
y_pred_tar <-fit_model$Log_RRate_target_pred
y_real_alt <- fit_model$Log_RRate_alternative_obt
y_pred_alt <- fit_model$Log_RRate_alternative_pred

# The R^2 value, which measures the goodness of fit of the model, is calculated by squaring the correlation 
# between the actual and predicted response rates.
r2_obj <- cor(y_real_tar, y_pred_tar)^2
r2_alt <- cor(y_real_alt, y_pred_alt, use = "complete.obs")^2
# The calculated R^2 value is printed to the console.
print(r2_obj)
print(r2_alt)


### Plot the obtained and predicted results for the target and alternative behavior

ggplot(data = datos_tasa, mapping = aes(x = sesion)) +
  geom_line(aes(y = datos_tasa$tasa_respuesta_objetivo, color = "Target Rate"), 
            size = 1, alpha = 1) +
  geom_line(aes(y = datos_tasa$tasa_respuesta_alternativa, color = "Alternative Rate"), 
            size = 1, alpha = 1) +
  geom_line(data = datos_tasa_real, mapping = aes(y = datos_tasa_real$s1, color = "Control TR_rate"), 
            size = 1, alpha = 1) +
  geom_line(data = datos_tasa_real, mapping = aes(y=datos_tasa_real$s2, color = "Control ALT_rate"), 
            size = 1, alpha = 1) +
  scale_color_manual(values = c('black', 'red', 'pink', 'green'),
                     breaks = c('Target Rate', 'Alternative Rate', 'Control TR_rate', 'Control ALT_rate'),
                     name = "Response:",
                     labels = c('Target Rate', 'Alternative Rate', 'Control TR_rate', 'Control ALT_rate')) +
  labs(title = "Predicted and real target and alternative response rate",
       x = "Sessions",
       y = "Responses per minute (log scale)",
       color = "Response:") +
  theme_classic() +
  scale_y_log10()
