#install.packages("apollo")

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Set directory 
#setwd("Proivde your data file location")

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="NestedLogit",
  modelDescr ="Two-level NL model with Car and Air in a single nest",
  indivID    ="PID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("intcity.csv",header=TRUE)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car               = 0,
              asc_air               = 0,
              asc_rail              = 0,
              b_IVTT                = 0,
              b_OVTT                = 0,
              b_COST                = 0,
              b_FREQ                = 0, 
              ThetaParameter             = 0.01)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car")

### Read in starting values for at least some parameters from existing model output file
#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"Apollo_example_3",overwriteFixed=FALSE)

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[['car']]  = asc_car  + b_IVTT*IVTT_c +  b_OVTT * OVTT_c + b_COST * COST_c + 0
  V[['air']]  = asc_air  + b_IVTT*IVTT_a +  b_OVTT * OVTT_a + b_COST * COST_a + b_FREQ * FREQ_a
  V[['rail']] = asc_rail + b_IVTT*IVTT_r +  b_OVTT * OVTT_r + b_COST * COST_r + b_FREQ * FREQ_r
  
  ### Specify nests for NL model
  nlNests      = list(root=1, Nest=ThetaParameter)
  
  ### Specify tree structure for NL model: Car and air 
  nlStructure= list()
  nlStructure[["root"]]   = c("rail","Nest")
  nlStructure[["Nest"]]     = c("car","air")
  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(car=1, air=2, rail=3),
    avail        = list(car=1, air=1, rail=1),
    choiceVar    = CHALT,
    V            = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

#apollo_saveOutput(model)
