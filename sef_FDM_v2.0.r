
#entireScript <- function() {
  
  ###START FUELBED DYNAMICS MODEL####
  
  #Version 2.0 (Derviced from version 17e, the most recent version withmodel 
  #documentation
  
  
  
  #>>>>>>>>>>>>>>>>>>>          HOW WOULD YOU LIKE TO RUN THE FUELBED DYNAMICS MODEL?
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #>>>>>>>>>>>>>>>>>>>          COMPUTING PARAMETERS...
  
  #Do you want to install required R packages on this machine?
  #Yes --- TRUE
  #No ---- FALSE
  INSTALL_PACKAGES <- FALSE
  
  #Enable the graphics card as a processor for cellular automata sub-models?
  #This will only work on machines with a Linux OS
  #Yes --- TRUE
  #No ---- FALSE
  USE_GPU <- FALSE
  
  #Would you like to replicate this run?
  #If so use the same seed number for subsequent runs
  #SEED is the starting point for psuedo random number generator
  SEED <- 999
  
  #Select a run ID, this should be a number, ideally unique that will help track this
  #run. Output files are tagged with this ID number.
  RUN <- 59
  
  #Reporting interval, how often (in model years) should output maps be produced?
  #I.e., once every ... years.
  #Must be less than model run time (YEARS object)
  Interval <- 2
  
  #What is your working directory. I.e. where are your input files coming from?
  input_path <- "C:/Users/jcronan/Documents/GitHub/EglinAirForceBase"     
  
  #What is your output directory. I.e., here do you want maps and status reports to 
  #go?
  output_path <- "C:/usfs_sef_outputs_FDM/"
  
  #>>>>>>>>>>>>>>>>>>>          DISTURBANCE AND MODEL RUN TIME PARAMETERS...
  
  #Select pre-packaged or manually entered forest management and wildfire regime 
  #parameters + model run time (in years)
  #1 ---  Actual paramater values derived from forest operations and wildfire data 
  #       for the period 2000-2015 and 50 year run
  #2 ---  Testing generates small areas of presribed fire and wildfire annually 
  #       and 2 yearr un.
  #3 ---  Manually enter disturbance parameters. Enter parameters below on 
  #       lines 99-123.
  disturbance_regime <- 3
  
  #Disturbance and time parameters
  if(disturbance_regime == 1)
    {
    #Number of years the model should run for.
    YEARS <- 50
  
    #Acres thinned annually.
    THINNING <- 5000
    
    #Acres of herbicide application annually
    HERBICIDE <- 5000
    
    #Acres prescribed burned annually
    #RX_FIRE <- 100000
    
    #Natural fire rotation in years for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    NATURAL_FIRE_ROTATION <- c(54.38,457.39)
    
    #Mean fire size in acres for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    MEAN_FIRE_SIZE <- c(103.65,5.23)        
    
    #Standard deviation of mean fire size for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    STAND_DEV_FIRE_SIZE <- c(361.12,13.98)
  } else 
  {if(disturbance_regime == 2)
    {
    #Number of years the model should run for.
    YEARS <- 2
    
    #Acres thinned annually.
    THINNING <- 0
    
    #Acres of herbicide application annually
    HERBICIDE <- 0
    
    #Acres prescribed burned annually
    #RX_FIRE <- 1000
    
    #Natural fire rotation in years for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    NATURAL_FIRE_ROTATION <- c(10554.38, 10457.39)
    
    #Mean fire size in acres for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    MEAN_FIRE_SIZE <- c(103.65, 5.23)        
    
    #Standard deviation of mean fire size for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    STAND_DEV_FIRE_SIZE <- c(361.12, 13.98)
  } else
  {
    #Number of years the model should run for.
    YEARS <- 2
    
    #Acres thinned annually.
    THINNING <- 1000
    
    #Acres of herbicide application annually
    HERBICIDE <- 1000
    
    #Acres prescribed burned annually
    RX_FIRE <- 2500
    
    #Natural fire rotation in years for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    NATURAL_FIRE_ROTATION <- c(1054.38,10457.39)
    
    #Mean fire size in acres for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    MEAN_FIRE_SIZE <- c(103.65,5.23)    
    
    #Standard deviation of mean fire size for:
    #Element 1 -- Eglin Air Force Base
    #Element 2 -- Surrounding 10-km buffer landscape
    STAND_DEV_FIRE_SIZE <- c(361.12, 13.98)
  }}
  
  #>>>>>>>>>>>>>>>>>>>          FINISHED
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #>>>>>>>>>>>>>>>>>>>          HARD-CODED PARAMETERS -- THESE SHOULD NOT BE CHANGED
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #>>>>>>>>>>>>>>>>>>>          COMPUTING PARAMTERS...
  
  #Number of iterations for the following nested loops:
  #Loop 2 (var. = b)    -- management actions
  #Loop 3 (var. = cc)   -- blocks (management actions)
  #Loop 4 (var. = d)    -- expansions (management actions)
  #Loop 9 (var. = f)    -- blocks (wildfire)
  #Loop 10 (var. = g)   -- expansions (unsuppressed wildfire)
  #Loop 11 (var. = h)   -- expansions (wildfire with block and burn suppression)
  r.max <- 1000#7
  
  #StarTing stand numbers for...
  #-----------------------------------------------------------------------------------
  #Treatments:
  treat.stand <- 4000000
  
  #Fires:
  fire.stand <- 8000000
  #-----------------------------------------------------------------------------------
  
  #>>>>>>>>>>>>>>>>>>>          ASCII MAP FILE METADATA...
  
  #The number of acres per pixel.
  MapRes <- 0.22239
  
  #Number of rows and columns in ascii map files.
  rows <- 1771
  cols <- 3491
  
  #Number of rows with metadata for each ascii map file.
  fh.adj <- 6  #fuelbed map (f.map)
  sh.adj <- 6  #stand map (s.map)
  bh.adj <- 6  #burn unit map (b.map)
  lh.adj <- 6  #coordinate map (l.map)
  
  #>>>>>>>>>>>>>>>>>>>          FOREST MANAGEMENT PARAMTERS...
  
  #Vector of burn unit numbers (corresponds with b.map) that are within Eglin but 
  #unmanaged.
  Unmanaged.Unit <- 9999
  #Vector of burn unit numbers (corresponds with b.map) that are within the buffer 
  #perimeter.
  Buffer.Unit <- 8888
  #Vector of burn unit numbers (corresponds with b.map) that are outside of the 
  #Eglin perimeter.
  NoData.Unit <- -9999999
  
  #Number of start/ignition points. Select the proportion of available cells within a 
  #treatment unit to locate seed cells (i.e. start point locations that are fed into 
  #the cellilar automata sub-model.
  #Element 1 -- Thinning
  #Element 1 -- Herbicide Application
  #Element 1 -- Prescribed Fire
  seed.cells <- c(0.50, 0.50, 0.10)
  
  #>>>>>>>>>>>>>>>>>>>          FUELBED PARAMTERS...
  
  #Vector of fuelbed numbers (corresponds with f.map) with a fixed age at zero.
  Fixed.Age <- c(-9999, 1061401, 1069000, 1071401, 5079000, 5089000, 5099000, 6000000)
  
  #Open Water fuelbed
  Open.Water <- 6000000
  
  #Vector of fuelbed numbers (corresponds with f.map) that are non-burnable.
  Non.Flammable <- c(-9999, 5089000, 5099000, 6000000)
  #Key to fuelbeds in two objects above
  #-9999    No Data
  #1061401  Shrub swamp
  #1069000  Cleared wetland
  #1071401  Herbaceous marsh
  #5079000  Rangeland
  #5089000  Agriculture
  #5099000  Developed
  #6000000  Open water
  
  #>>>>>>>>>>>>>>>>>>>          FIRE BEHAVIOR PARAMTERS...
  
  #Do not map wildfires below this value (in acres). Purpose is to reduce model run 
  #time by excluding small fires that do not impact vegetation at the landscape scale.
  fire.cut <- 10
  
  #Ceilings for forest management and wildfire disturbances
  #-----------------------------------------------------------------------------
  #Maximum annual area burned in wildfires
  #Element 1 -- Eglin Air Force Base
  #Element 2 -- 10-km buffer landscape
  Truncate.AAB <- c(50000, 25000)
  
  #Maximum fire size of wildfires 
  #Element 1 -- Eglin Air Force Base
  #Element 2 -- 10-km buffer landscape
  Truncate.Area <- c(12000, 6000)
  
  #Maximum number of fires and treatments in a given year
  #Element 1 -- Eglin Air Force Base
  #Element 2 -- 10-km buffer landscape
  #Element 3 -- Thnning Treatment (Corresponds with THINNING)
  #Element 4 -- Herbicide Application (Corresponds with HERBICIDE)
  #Element 5 -- Prescribed Fire (Corresponds with RX_FIRE)
  Truncate.Number <- c(400, 800, 50, 50, 500)
  #-----------------------------------------------------------------------------
  
  #These two parameters will cause flammability of fuels to slowly equilibrate
  #as annual area burned increases. For these values (c.shape = 1.5 and 
  #s.scale = 0.1) equlibration begins when area burned for a fire in the unmanaged
  #unit (management unit = 9999) or buffer zone (management unit = 8888)
  #almost immediately as fire size grows and all but unburnable fuels equilibrate to  
  #1 by the time fire size equals 1100 acres (5000 pixels).
  #When wildfires are burned by the block and burn method flammability of fuels is 
  #based on probability. The meaning of the scale.factor and dist.curve are flipped 
  #and corresponding values are randomly selected from each dataset
  c.shape <- 1.5
  c.scale <- 0.1
  
  #Values to guide stochastic generation of treatments. Order is thinning, herbicide, and
  #prescribed fire.
  minSize <- c(5, 20, 1)#minimum treated stand size within a treatment unit
  #Shape parameters are used to inform the beta distribution function that determines
  #The percentage of a treatment unit to be effected for each treatment.
  shape1 <- c(30, 30, 10)#shape 1 parameter
  shape2 <- c(5,5, 2.5)#shape 2 parameter
  
  #Flame extinction variable in wildfire loop (loop 10). After cell has burned
  #for the specified number of expansion it burns out.
  burn.out <- 3
  
  #Size threshold where fires are primarily wind driven.
  #Testing, 10,000 acres was my original limit. It seems to high.
  windThresholdSize <- 1000#acres
  
  #>>>>>>>>>>>>>>>>>>>          WIND DATA PARAMTERS...
  
  #Describe probability of wind coming from a given direction.
  #0 = North
  #1 = Northeast
  #2 = East
  #3 = Southeast
  #4 = South
  #5 = SouthWest
  #6 = West
  #7 = Northwest
  windProbs <- c(0.1,0.025,0.01,0.01,0.025,0.05,0.16,0.62)
  
  #>>>>>>>>>>>>>>>>>>>          FINISHED
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
  ####################################################################################
  ####################################################################################
  #STEP 01: Administrative Information
  
  # Reads mutable parameters from AWS user data
  try(host_sim_params <- read.table("host_sim_params.csv"), silent=TRUE)
  host_sim_params <- read.csv("host_sim_params.csv")
  
  if (exists("host_sim_params") && "run_id" %in% colnames(host_sim_params)) {
    # from AWS user data
    run <- as.character(host_sim_params$run_id)
  } else if (exists("RUN")) {
    # manual
    run <- RUN
  } else {
    stop("No run id present.")
  }
  
  if (exists("host_sim_params") && "seed" %in% colnames(host_sim_params)) {
    # from AWS user data
    set.seed(host_sim_params$seed)
  } else if (exists("SEED")) {
    # manual
    set.seed(SEED)
  } else {
    set.seed(NULL)
    seed <- runif(1)*2e9
    cat("Random seed used: ", seed, file=paste(run, "_dump.txt"))
    set.seed(seed)  # replace seed with manual seed if desired
  }
  
  if (exists("host_sim_params") && "rx_fire" %in% colnames(host_sim_params)) {
    # from AWS user data
    RX_FIRE <- host_sim_params$rx_fire
  }
  
  if (exists("host_sim_params") && "use_gpu" %in% colnames(host_sim_params)) {
    # from AWS user data
    USE_GPU <- host_sim_params$use_gpu
  } else if (exists("USE_GPU")) {
    # manual, no need to do anything
  } else {
    USE_GPU <- FALSE
  }
  
  # TODO bernease: Pull INSTALL_PACKAGES variable from host_sim_params, similar to use_gpu
  
  if (exists("host_sim_params") && "input_path" %in% colnames(host_sim_params)) {
    # from AWS user data
    input_path <- as.character(host_sim_params$input_path)
  }
  
  if (exists("host_sim_params") && "output_path" %in% colnames(host_sim_params)) {
    # from AWS user data
    output_path <- as.character(host_sim_params$output_path)
  }
  
  #Set working directory
  setwd(input_path)
  
  #>>>>>>>>>>>>>>>>>>>          R PACKAGES...
  
  #Manage packages.
  if(INSTALL_PACKAGES == TRUE)
  {
    #Install packages
    install.packages("Hmisc", repos="http://cran.fhcrc.org/")
    install.packages("GenKern", repos="http://cran.fhcrc.org/")
    install.packages("SDMTools", repos="http://cran.fhcrc.org/")
    install.packages("gtools", repos="http://cran.fhcrc.org/")
    #Open libraries
    library(Hmisc) #for summarize()
    library(GenKern)#for nearest()
    library(SDMTools)
    library(gtools)  #for combinations()ge
    library(utils)#for Rprof()
    if(USE_GPU == T)
    {
      #Install GPU package
      install.packages("gmatrix", repos="http://cran.fhcrc.org/")
      #Open GPU library
      library(gmatrix)#GPU package, will only work on a Linux machine
    } else
    {
      #nothing
    }
  } else
  {
    #Open libraries
    library(Hmisc) #for summarize()
    library(GenKern)#for nearest()
    library(SDMTools)
    library(gtools)  #for combinations()ge
    library(utils)#for Rprof()
    if(USE_GPU == T)
    {
      #Open GPU library
      library(gmatrix)#GPU package, will only work on a Linux machine
    } else
    {
      #nothing
    }
  }
  
  ####################################################################################
  ####################################################################################
  #STEP 02: Operational Parameters
  
  #Stop model run if map production interval is greater than the number of model years.
  if (Interval > YEARS) {
    cat("Interval too high. Make interval less than year variable.")
    stop("Interval too high. Make interval less than year variable.")
  }
  
  #Average annual area treated for thinning, herbicide, and prescribed fire.
  #Read in third meanTAP parameter from file
  if (exists("RX_FIRE")) {
    # manual
    meanTAP <- c(THINNING, HERBICIDE, RX_FIRE)
  } else if (exists("host_sim_params") && "rxfire" %in% host_sim_params) {
    # from AWS
    meanTAP <- c(THINNING, HERBICIDE, host_sim_params$rxfire)
  } else {
    stop("No rxfire parameter found.")
  }
  
  #Convert area in acres to 30 m pixels
  meanTAP <- round(meanTAP/MapRes,0)
  
  ####################################################################################
  ####################################################################################
  #STEP 03: Import Spatial Database (Raster Subset)
  f.map <- matrix(scan(paste("inputs/sef_fmap_v2_",rows,"x",cols,".txt",
                             sep = ""),skip = fh.adj),ncol=cols,byrow=T)#16
  
  s.map <- matrix(scan(paste("inputs/sef_smap_092715_",rows,"x",cols,".txt",
                             sep = ""),skip = sh.adj),ncol=cols,byrow=T)#17
  
  b.map <- matrix(scan(paste("inputs/sef_bmap_",rows,"x",cols,".txt",
                             sep = ""),skip = bh.adj),ncol=cols,byrow=T)#18
  
  l.map <- matrix(scan(paste("inputs/sef_lmap_",rows,"x",cols,".txt",
                             sep = ""),skip = lh.adj),ncol=cols,byrow=T)#20
  
  ####################################################################################
  ####################################################################################
  #STEP 04: Import Spatial Database (Pseudo-vector Subset)
  Stand.List <- read.table(paste(
    "inputs/sef_StandList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  Stand.List <- as.vector(Stand.List[,2], mode = "numeric")#20
  Stand.List <- Stand.List[-1]
  
  Fuelbed.List <- read.table(paste(
    "inputs/sef_FuelbedList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  Fuelbed.List <- as.vector(Fuelbed.List[,2], mode = "numeric")#21
  Fuelbed.List <- Fuelbed.List[-1]
  
  Coord.List <- read.table(paste(
    "inputs/sef_CoordList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  Coord.List <- as.vector(Coord.List[,2], mode = "numeric")#21
  Coord.List <- Coord.List[-1]
  
  Age.List <- read.table(paste(
    "inputs/sef_AgeList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  Age.List <- as.vector(Age.List[,2], mode = "numeric")#22
  Age.List <- Age.List[-1]
  
  Area.List <- read.table(paste(
    "inputs/sef_AreaList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  Area.List <- as.vector(Area.List[,2], mode = "numeric")#23
  Area.List <- Area.List[-1]
  
  mfri.List <- read.table(paste(
    "inputs/sef_mfriList_v2_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  mfri.List <- as.vector(mfri.List[,2], mode = "numeric")#23
  mfri.List <- mfri.List[-1]
  
  mfri.Matrix <- read.table(paste(
    "inputs/sef_mfriMatrix_v2_",rows,"x",cols,".txt",
    sep = ""), header=T, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  #Remove no Data Unit
  mm1 <- mfri.Matrix[,-1]
  #Force into a matrix (opens as a data.frame with mode = integer; these characteristics will crash FDM)
  mm2 <- data.matrix(mm1)
  #Data mode is still integer and there are additional attributes
  #Convert to vector (removes attributes from data.frame and coverts integer to numeric)
  mm3 <- as.vector(mm2, mode = 'numeric')
  #Restore rows and cols.
  mm4 <- matrix(data = mm3, nrow = length(mm1[,1]), ncol = length(mm1[1,]))
  mfri.Matrix <- mm4
  
  T1E.List <- read.table(paste(
    "inputs/sef_T1EList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  T1E.List <- as.vector(T1E.List[,2], mode = "numeric")#24
  T1E.List <- T1E.List[-1]
  
  T2E.List <- read.table(paste(
    "inputs/sef_T2EList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  T2E.List <- as.vector(T2E.List[,2], mode = "numeric")#24
  T2E.List <- T2E.List[-1]
  
  D1E.List <- read.table(paste(
    "inputs/sef_D1EList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  D1E.List <- as.vector(D1E.List[,2], mode = "numeric")#25
  D1E.List <- D1E.List[-1]
  
  D2E.List <- read.table(paste(
    "inputs/sef_D2EList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  D2E.List <- as.vector(D2E.List[,2], mode = "numeric")#25
  D2E.List <- D2E.List[-1]
  
  MU.List <- read.table(paste(
    "inputs/sef_MUList_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  MU.List <- as.vector(MU.List[,2], mode = "numeric")#25
  MU.List <- MU.List[-1]
  
  TSLFxUnits <- read.table(paste(
    "inputs/sef_TSLF.List_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  TSLFxUnits <- TSLFxUnits[,-1]
  
  mfri_lower.List <- read.table(paste(
    "inputs/sef_less_fire_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  mfri_lower.List <- mfri_lower.List[,-1]
  
  mfri_upper.List <- read.table(paste(
    "inputs/sef_more_fire_",rows,"x",cols,".txt",
    sep = ""), header=TRUE, 
    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  mfri_upper.List <- mfri_upper.List[,-1]
  
  #nslt.List <- read.table(paste(
  #  "inputs/sef_NSLTList_",rows,"x",cols,".txt",
  #  sep = ""), header=TRUE, 
  #  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  #nslt.List <- nslt.List[,-1]
  tslt.List <- rep(1,length(Stand.List))
  tslt.Stands <- Stand.List
  tslt.Fuelbeds <- Fuelbed.List
  tslt.List <- tslt.List[Fuelbed.List == 4016204]
  tslt.Stands <- Stand.List[Fuelbed.List == 4016204]
  tslt.Fuelbeds <- Fuelbed.List[Fuelbed.List == 4016204]
  #Last line removes the first integer from the .List objects which contain the NoDate
  #area and will not match up with any date in f.path, resulting in an error.
  
  ####################################################################################
  ####################################################################################
  #STEP 05: Import Conditional Database
  f.path <- read.table("inputs/sef_lut_pathways_succession.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  t.path <- read.table("inputs/sef_lut_pathways_treatmentsuccession.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  f.treatments <- read.table("inputs/sef_lut_menu_treatment.csv", header=TRUE, 
                             sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
  
  f.disturbances <- read.table("inputs/sef_lut_menu_disturbance.csv", header=TRUE, 
                               sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
  
  t.post <- read.table("inputs/sef_lut_pathways_silviculture.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  d.post <- read.table("inputs/sef_lut_pathways_fire.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  pda <- read.table("inputs/sef_lut_thresholds_disturbances.csv", header=TRUE, 
                    sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  f.probability <- read.table("inputs/sef_lut_prob_burning.csv", header=TRUE, 
                              sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
  
  f.wind <- read.table("inputs/sef_lut_prob_wind.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  b.unit <- read.table("inputs/sef_lut_burn_units.txt", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  b.unit <- data.frame(unit = b.unit[,2], area_ac = b.unit[,3], thin= b.unit[,4], 
                       herb = b.unit[,5], fire = b.unit[,6])#remove col 1 and rename cols 2-6.
  
  b.block <- read.table("inputs/sef_lut_pathways_burnBlocks.csv", header=T, 
                        sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  b.block <- b.block[-1,]#remove first row -- no data unit.
  
  b.thresh <- read.table("inputs/sef_lut_threshold_mgmtOptions.csv", header=T, 
                         sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  f.start <- read.table("inputs/sef_lut_pathways_fireStart.csv", header=T, 
                        sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  f.start <- f.start[-1,]#remove first row -- no data unit.
  ####################################################################################
  ####################################################################################
  #STEP 06: Simplify input data.
  ttxm <- t.post[,-1]#removes the fuelbed column for t.post.
  dtxm <- d.post[,-1]#removes the fuelbed column from d.post.
  pdxm <- pda[,-1]#removes the fuelbed column from pda.
  
  ####################################################################################
  ####################################################################################
  #STEP 07: Generate secondary data.
  fblo <- 1:length(t.post$fuelbed)#fuelbed location.
  fbls <- t.post$fuelbed#master list of fuelbeds.
  
  trls <- 1:length(f.treatments$TreatmentName)#Creates a numeric code for treatments.
  
  #Temporary stand numbers
  #The first will be used to seed disturbance and the second will be used in the fire
  #loop to progressively track expansion of the fire front (allows for fire to burn
  #out).
  tesn <- -1#temporary stand number.
  tesn_t <- -1#used for treatment loop
  s.map[s.map == -9999] <- NoData.Unit
  
  eglin.area <- length(b.map[!b.map %in% c(Buffer.Unit,-9999)])
  #pixels within Eglin's perimeter.
  buffer.area <- length(b.map[b.map == Buffer.Unit & !f.map != Open.Water])
  #pixels within the buffer landscape, excluding pixels with open water fuelbeds.
  
  #Determine Area of BANSA units, these must be burned every year and will determine
  #the amount of area burned in CCA units.
  #assigns thresholds for CCA and BANSA management threshold which will be
  #based on the relative areas of each management level.
  BANSA.Area <- sum(Area.List[MU.List %in% b.block$BurnBlock[b.block$BANSA == 2]])
  
  #FF.e = Fire Frequency for Eglin
  FF.e <- ((eglin.area*MapRes)/(NATURAL_FIRE_ROTATION[1]*MEAN_FIRE_SIZE[1]))
  
  #FF.b = Fire Frequency for buffer
  FF.b <- ((buffer.area*MapRes)/(NATURAL_FIRE_ROTATION[2]*MEAN_FIRE_SIZE[2]))
  
  #mean of log transformed mean fire size
  Mu.e <- 2*log(MEAN_FIRE_SIZE[1]) - 0.5*(log(STAND_DEV_FIRE_SIZE[1]^2 + MEAN_FIRE_SIZE[1]^2))
  
  #mean of log transformed mean fire size
  Mu.b <- 2*log(MEAN_FIRE_SIZE[2]) - 0.5*(log(STAND_DEV_FIRE_SIZE[2]^2 + MEAN_FIRE_SIZE[2]^2))
  
  #variance of log transformed fire sizes
  Sigma.e <- sqrt(log(STAND_DEV_FIRE_SIZE[1]^2 + MEAN_FIRE_SIZE[1]^2) - 2*log(MEAN_FIRE_SIZE[1]))
  
  #variance of log transformed fire sizes
  Sigma.b <- sqrt(log(STAND_DEV_FIRE_SIZE[2]^2 + MEAN_FIRE_SIZE[2]^2) - 2*log(MEAN_FIRE_SIZE[2]))
  
  #Pixels to search (y coordinates, difference from flame front)
  search.set <- matrix(data = 0,48,2)             #Cellular automata input
  search.set[,1] <- c(-1,-1,0,1,1,1,0,-1,-2,-2,-2,-1,0,1,2,2,2,2,2,1,0,-1,-2,-2,
                      -3,-3,-3,-3,-2,-1,0,1,2,3,3,3,3,3,3,3,2,1,0,-1,-2,-3,-3,-3)   
  #Pixels to search (x coordinates, difference from flame front)
  search.set[,2] <- c(0,rows,rows,rows,0,-rows,-rows,-rows,0,rows,(rows*2),(rows*2),(rows*2),(rows*2),(rows*2),rows,0,-rows,(-rows*2),
                       (-rows*2),(-rows*2),(-rows*2),(-rows*2),-rows, 
                       0,rows,(rows*2),(rows*3),(rows*3),(rows*3),(rows*3),(rows*3),(rows*3),(rows*3),
                       (rows*2),rows,0,-rows,(-rows*2),(-rows*3),(-rows*3),(-rows*3),(-rows*3),
                       (-rows*3), (-rows*3),(-rows*3),(-rows*2), -rows)
  distance.coefficient <- c(rep(1,8), rep(0.002,16), rep(0.001,24))
  dcl <- length(distance.coefficient)#for wildfires
  dcl_t <- 8#for treatments
  
  #Vectors show number of pixels in each concentric ring of pixels around a burnign pixel.
  wind.set1 <- rep(seq(1,8,1),3)
  wind.set2 <- rep(seq(1,16,1),3)
  wind.set3 <- rep(seq(1,24,1),3)
  
  #Isolate each ring of pixels around burning pixel.
  wind_1a <- f.wind$NorthWind[1:8]
  wind_2a <- f.wind$NorthWind[9:24]
  wind_3a <- f.wind$NorthWind[25:48]
  
  #Code wind directions.
  #0 = North
  #1 = Northeast
  #2 = East
  #3 = Southeast
  #4 = South
  #5 = SouthWest
  #6 = West
  #7 = Northwest
  windDirs <- c(0,1,2,3,4,5,6,7)
  
  #Map.History <- list()                          #Tracking Database Template.
  #Tracking Database Template. 
  tdn <- vector(mode = "numeric", length = 0)    #Records wildfire data
  tdy <- vector(mode = "numeric", length = 0)    #Records wildfire data
  tdc <- vector(mode = "numeric", length = 0)    #Records wildfire data
  tda <- vector(mode = "numeric", length = 0)    #Records wildfire data
  tdt <- vector(mode = "numeric", length = 0)    #Records wildfire data
  
  #Combine fuelbed shift lists. Makes selecting between them more efficient (i.e. no if/then
  #statments needed to select an object)
  D.List <- cbind(T1E.List, T2E.List, D1E.List, D2E.List)
  
  #This will be used to log run times for disturbance loops.
  #e.summary <- data.frame()
  
  ####################################################################################
  ####################################################################################
  #STEP 08: Generate functions.
  #This function (grabbed from the r-help site) is the same as the sample()
  #function except if the length of x is one it will just use that number rather than
  #sample from 1:x.
  resample <- function(x, size, ...)
    if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
  #This function provides the location of all pixels surrounding pixels in stand[sn]
  sn.seeker <- function(x,y) 
  {
    v1 <- s.map
    v2 <- tesn
    v3 <- which(v1 %in% v2)
    matrix(data = v3, nrow = length(x), ncol = length(v3), byrow = T) + x + y
  }
  
  find_actively_burning_cells_in_smap <- function()
  {
    v1 <- s.map
    v2 <- tesn
    return (which(v1 %in% v2))
    
  }
  
  find_neighbors <- function(x, radius, row_cnt = rows, col_cnt = cols)
  {
    # Unit tests: shift(seq(1,20), 0, 0, row_cnt=5) = seq(1,20) 
    #             shift(seq(1,20), -1, 0, row_cnt=5) = c(1,2,3,4,5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
    #             shift(seq(1,20), 0, 1, row_cnt=5) = c(2,3,4,5,5,7,8,9,10,10,12,13,14,15,15,17,18,19,20,20)
    #             shift(seq(1,20), -3, 2, row_cnt=5) = c(1,2,3,11,12,6,7,8,16,17,11,12,13,14,15,16,17,18,19,20)
    params <- expand.grid(x, seq(-radius,radius,1), seq(-radius,radius,1))
    params <- as.matrix(params[!(params[,2]==0 & params[,3]==0),])
    x_row = ifelse(params[,1] %% row_cnt == 0, row_cnt, params[,1] %% row_cnt)  # row number
    x_col = ifelse(params[,1] %% row_cnt == 0, params[,1] %/% row_cnt, params[,1] %/% row_cnt + 1)  # column number
    if (USE_GPU) {
      g_params <- as.gmatrix(params)
      g_x_row <- as.gmatrix(x_row)
      g_x_col <- as.gmatrix(x_col)
      result <- ifelse(g_x_row + g_params[,2] > 0 & g_x_row + g_params[,2] <= row_cnt & g_x_col + g_params[,3] > 0 & g_x_col + g_params[,3] <= col_cnt,
                       g_params[,1] + g_params[,3]*row_cnt + g_params[,2], 0)
      result <- h(result)
      result <- as.vector(result)
    } else {
    result <- ifelse(x_row + params[,2] > 0 & x_row + params[,2] <= row_cnt & x_col + params[,3] > 0 & x_col + params[,3] <= col_cnt,
                              params[,1] + params[,3]*row_cnt + params[,2], 0)
    }
    
    return (result)
  }
  
  #Object that will translate find_neighbors() output into matrix with each row listing
  #concentric rings of neighbor cells for a single burning cell.
  translate_to_matrix <- c(24, 30, 31, 32, 25, 19, 18, 17, 23, 29, 36, 37, 38, 39, 40, 33, 26, 
                           20, 13, 12, 11, 10, 9, 16, 22, 28, 35, 42, 43, 44, 45, 46, 47, 48, 
                           41, 34, 27, 21, 14, 7, 6, 5, 4, 3, 2, 1, 8, 15)
  
  #This function shaves off pixels locations generated by the seeker function
  #that have "spilled out" from the top or bottom of s.map.
  sn.limit <- function(x,y) 
  {
    m1 <- l.map[rows,which(array(s.map %in% tesn, dim =c(rows,cols)),arr.ind=TRUE)[,2]]
    m1a <- matrix(data = m1, nrow = length(x), ncol = length(m1), byrow = T) + x
    
    m2 <- l.map[1,which(array(s.map %in% tesn, dim = c(rows, cols)), arr.ind=TRUE)[,2]]
    m2a <- matrix(data = m2, nrow = length(x), ncol = length(m2), byrow = T) + x
    
    m3a <- matrix(data =  rep(y >= 0, length(m2)), nrow = length(x), ncol = length(m2), byrow = F)
    
    ifelse(m3a == T, m1a, m2a)
  }
  
  ####################################################################################
  ####################################################################################
  #STEP 09: Fire Regime Simulation for Eglin (excludes buffer zone).
  
  #Since the buffer zone fire regime is determined by the eglin fire regime just use
  #the Eglin fire regime to drive flammability of fuels.
  
  sim.years <- 10000#1000 year run.
  annual.ff <- vector(length = sim.years, mode = 'numeric')
  annual.ab <- vector(length = sim.years, mode = 'numeric')
  fire.area <- vector(length = sim.years, mode = 'numeric')
  
  for(i in 1:sim.years)
  {
    p.fino <- round(rpois(1,FF.e),0)
    p.fiar <- round(rlnorm(p.fino,Mu.e,Sigma.e),0)
    p.fiar <- p.fiar[p.fiar > 0]
    p.fino <- length(p.fiar)
    annual.ff[i] <- p.fino
    annual.ab[i] <- sum(p.fiar)
    fire.area <- c(fire.area, p.fiar)
  }
  
  max.fire <- min(max(fire.area),Truncate.Area[1])
  area.dist <- seq(length = 3000, from = 1, to = max.fire/MapRes)
  
  dist.curve <- mapply(function(y) 1-exp(-1*((y/c.scale)^c.shape)), 
                       seq(length = 3000, from = (1/max.fire), to = 1))
  scale.factor <- mapply(function(x) exp(-1*((x/c.scale)^c.shape)), 
                         seq(length = 3000, from = (1/max.fire), to = 1))
  
  #asymptote of curve on x axis (where 0 represents 0 pixels 
  #burned and 1 represents anything greater than value in windThresholdSize)
  #is roughly at this value
  asymptote <- 0.5
  
  #Specify relationship between the ratio of current-to-max fire size and
  #weibull shape/scale parameter (1-6; a value of 1 will generate mostly
  #low windspeeds while a value of 6 will generate average windspeeds of ~12 mph)
  fire.ratio <- round(seq(length = 1001, from = 0, to = 1),3)
  assoc.wsp <- mapply(function(y) 1-exp(-1*((y/asymptote)^3)), 
                      seq(length = 1001, from = 0, to = 1))
  assoc.wsp <- (assoc.wsp*5)+1
  
  ####################################################################################
  ####################################################################################
  #STEP 10: Create vectors that list which fuelbeds are eligible for treatment.
  
  #Determine available fuelbeds:
  #For thinning treatments
  avfb_thin <- fbls[ttxm[,4] == 2]
  
  #For herbicide treatments
  avfb_herb <- fbls[ttxm[,5] == 2]
  
  #For prescribed fire treatments
  avfb_fire <- fbls[ttxm[,6] == 2]
  
  ####################################################################################
  ####################################################################################
  #STEP 11: CREATE DETAILED LIST OF ACREAGES FOR EACH MANAGEMENT OPTION
  
  #Fill in percent values so perc_cats adds up to one for each level within each treatment
  #type
  b.thresh$perc_level[b.thresh$t_code == 3 & b.thresh$level == 1] <- BANSA.Area/meanTAP[3] 
  b.thresh$perc_level[b.thresh$t_code == 3 & b.thresh$level == 2] <- 1-BANSA.Area/meanTAP[3] 
  b.thresh$perc_mgmt[b.thresh$t_code == 3] <-  b.thresh$perc_level[b.thresh$t_code == 3] * 
    b.thresh$perc_mgmt[b.thresh$t_code == 3]
  b.thresh$perc_cats[b.thresh$t_code == 3] <-  b.thresh$perc_mgmt[b.thresh$t_code == 3] * 
    b.thresh$perc_cats[b.thresh$t_code == 3]
  
  #Now convert percentages to pixels
  for(i in 1:length(meanTAP))
  {
    b.thresh$perc_level[b.thresh$t_code == i] <- 
      b.thresh$perc_level[b.thresh$t_code == i] * meanTAP[i]
    b.thresh$perc_mgmt[b.thresh$t_code == i] <- 
      b.thresh$perc_mgmt[b.thresh$t_code == i] * meanTAP[i]
    b.thresh$perc_cats[b.thresh$t_code == i] <- 
      b.thresh$perc_cats[b.thresh$t_code == i] * meanTAP[i]
  }
  
  #Now condense tables to remove combinations of levels, management options, and
  #mfri categories that do not apply
  b.thresh <- b.thresh[b.thresh$perc_cats > 0,]
  
  ####################################################################################
  ####################################################################################
  #STEP 12: DEFAULT VALUES
  
  #Set loop values to default (i.e. zero).
  a <- 0
  b <- 0
  cc <- 0
  d <- 0
  e <- 0
  f <- 0
  g <- 0
  
  ####################################################################################
  ####################################################################################
  #STEP 13: RUN MODEL LOOP
  
  #LOOP 111111111111111111111111111111111111111111111111111111111111111111111111111111
  #Loop 1 (by year). This loop encases the entire expression that maps regimes.
  for(a in 1:YEARS)#a <- 1
  { #1.0.0 ---------------------------------------------------------------------------
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    #The number of treatments per year is drawn from the normal distribution.
  
    #Null value for t.code. It will be re-assigned a value in the treatment loop (loop 2)
    t.code <- 4
    
    #Record cumulative burn units
    c.bun <- vector()
    
    #Burn units excluding c.bun
    t.bun <- b.unit$unit
    
    #Stores management options that ran out of space
    end.rc <- vector()
    
    #Objects to record annual area for thinning, herbicide, and prescribed fire treatments.
    meanTAA <- c(0, 0, 0)#records area treated (+ untreated area)
    meanUAA <- c(0, 0, 0)#records untreated area
    
    #Objects to record treated and untreated area within each management type.
    b.treated <- rep(0, length(b.thresh[,10]))
    b.untreated <- rep(0, length(b.thresh[,10]))
    
    #UPDATE PRIORITY FOR EACH MANAGEMENT UNIT
    
    #Determine priority of management units for thinning treatments
    #Lists percentage of unit with eligible fuelbeds
    pri.thin <- mapply(function(y)
    {
      sum(Area.List[Age.List >= D.List[,1] & 
                      Fuelbed.List %in% avfb_thin & 
                      MU.List == b.unit[y,1]])/b.unit[y,2]
    }, 1:length(b.unit$unit))
    #Incorporate hard rules for eligibility specified in management unit table (access through
    #ArcMap; file buun_map_9.raster).
    pri.thin[b.unit$thin == 1] <- 0#set to zero if management unit is not eligible for treatment
    
    #Determine priority of management units for herbicide treatments
    #Lists percentage of unit with eligible fuelbeds
    pri.herb <- mapply(function(y)
    {
      sum(Area.List[Age.List >= D.List[,2] & 
                      Fuelbed.List %in% avfb_herb & 
                      MU.List == b.unit[y,1]])/b.unit[y,2]
    }, 1:length(b.unit$unit))
    #Incorporate hard rules for eligibility specified in management unit table (access through
    #ArcMap; file buun_map_9.raster).
    pri.herb[b.unit$herb == 1] <- 0#set to zero if management unit is not eligible for treatment
    
    #Determine priority of management units for prescribed fire treatments
    #Lists percentage of unit with eligible fuelbeds
    pri.fire <- mapply(function(y)
    {
      sum(Area.List[Age.List >= D.List[,3] & 
                      Fuelbed.List %in% avfb_fire & 
                      MU.List == b.unit[y,1]])/b.unit[y,2]
    }, 1:length(b.unit$unit))
    #Incorporate hard rules for eligibility specified in management unit table (access through
    #ArcMap; file buun_map_9.raster).
    pri.fire[b.unit$fire == 1] <- 0#set to zero if management unit is not eligible for treatment
    
    
    pri <- data.frame(thin = pri.thin, herb = pri.herb, fire = pri.fire)
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
   
    #The number of fires per year is drawn from the Poisson Distribution (Wimberly 2002)
    #FF = Annual Fire Frequency. This is calculated in step 7.
    fino.e <- min(rpois(1,FF.e),Truncate.Number[1])
    #For the buffer zone the number of fires is predicted by the number of fires at
    #Eglin so the severity of the wildfire season is synchronized.
    fino.b <- min(round((max(1,fino.e)*(FF.b/FF.e)),0),Truncate.Number[2])
    
    #For area within Eglin
    if(fino.e==0)
    {
      fiar.e <- vector(mode = "numeric", length = 0)
    } else
    {
      #Fire area code. Uses the log normal distribution to assign fire areas to each fire.
      fiar.e <- rlnorm(fino.e,Mu.e,Sigma.e)
      
      #This step restricts the maximum fire size to that specified by the user (11,978 
      #acres as of 11-Aug-2015). This step will on average reduce the fire cycle by ?? years.
      fiar.e[fiar.e > Truncate.Area[1]] <- Truncate.Area[1]
      
      #Just in case the total area to be burned is larger than the burnable landscape, this 
      #step scales the total burnable to a ceiling that represent the largest area burned
      #at Eglin by wildfires (Truncate.AAB = 50,000 acres). All fires
      #are scaled evenly. This step had ?? effect on fire size over a 100,000
      #year period. No year produced a burned area over ?????? ha for the 1,619,226 ha
      #map area
      if(sum(fiar.e) > Truncate.AAB[1])
      {
        fiar.e <- mapply(function(x) (x/sum(fiar.e)) * min(sum(fiar.e),Truncate.AAB[1]),
                         fiar.e) 
        #SIMPLER VERSION
        #fiar.e <- mapply(function(x) (x/sum(fiar.e)) * Truncate.AAB[1],
        #                 fiar.e) 
      } else 
      {
        fiar.e <- fiar.e
      }
      
      #All above units are in acres, divide by MapRes to convert units into pixels.
      fiar.e <- fiar.e/MapRes
      
      #Round values.
      fiar.e <- round(fiar.e,0)
      
      fiar.e <- fiar.e[fiar.e > (fire.cut/MapRes)]#removes any fire areas set to zero.
      fino.e <- length(fiar.e)#adjusts number of fires in case any were removed above.
    }
  
    #For area within buffer zone.
    if(fino.b==0)
    {
      fiar.b <- vector(mode = "numeric", length = 0)
    } else
    {
      #Fire area code. Uses the log normal distribution to assign fire areas to each fire.
      fiar.b <- rlnorm(fino.b,Mu.b,Sigma.b)
      
      #This step restricts the maximum fire size to that specified by the user (11,978 
      #acres as of 11-Aug-2015). This step will on average reduce the fire cycle by ?? years.
      fiar.b[fiar.b > Truncate.Area[2]] <- Truncate.Area[2]
      
      #Just in case the total area to be burned is larger than the burnable landscape, this 
      #step scales the total burnable to a ceiling that represent the largest area burned
      #at Eglin by wildfires (Truncate.AAB = 50,000 acres). All fires
      #are scaled evenly. This step had ?? effect on fire size over a 100,000
      #year period. No year produced a burned area over ?????? ha for the 1,619,226 ha
      #map area
      if(sum(fiar.b) > Truncate.AAB[2])
      {
        fiar.b <- mapply(function(x) (x/sum(fiar.b)) * Truncate.AAB[2], fiar.b) 
      } else 
      {
        fiar.b <- fiar.b
      }
      
      #All above units are in acres, multiply by MapRes to convert units into pixels.
      fiar.b <- fiar.b/MapRes
      
      #Round values.
      fiar.b <- round(fiar.b,0)
      
      fiar.b <- fiar.b[fiar.b > (fire.cut/MapRes)]#removes any fire areas set to zero.
      fino.b <- length(fiar.b)#adjusts number of fires in case any were removed above.
    }
  
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
    
    #Update the age list. Age of fuelbed number is kept at zero.
    Age.List[!Fuelbed.List %in% Fixed.Age] <- Age.List[!Fuelbed.List %in% Fixed.Age] + min(a-1,1)
    
    #Update the mfri matrix. Age of fuelbed number is kept at zero.
    mfri.Matrix <- cbind(mfri.Matrix, rep(0,length(mfri.Matrix[,1])))
    mfri.Matrix <- mfri.Matrix[,-1]
    
    #Update the time-since-last-treatment list
    tslt.List <- tslt.List + min(a-1,1)
    
    #Order tslt objects by stand number
    tslt.List <- tslt.List[order(tslt.Stands)]
    tslt.Fuelbeds <- tslt.Fuelbeds[order(tslt.Stands)]
    tslt.Stands <- sort(tslt.Stands)
    
    #Common disturbance file lists areas of disturbances
    diar <- c(fiar.e, fiar.b)
    tda <- c(tda,diar)
    
    #Parallel object lists disturbance codes associated with distrubance areas
    dico <- c(rep(1,length(fiar.e)),rep(2,length(fiar.b)))
    tdc <- c(tdc,dico)
    
    #Parallel object assigns unique disturbance numbers to each disturbance.
    diun <- vector(mode = "numeric", length = 0)
    if(length(dico) > 0)
      diun <- ((length(tdn)+1):(length(tdn) + length(dico)))
    tdn <- c(tdn,diun)
    
    #Parallel object keeps track of the year associated with the disturbance.
    diyr <- rep(a,length(dico))
    tdy <- c(tdy,diyr)
    
    #This object tracks new stand numbers that have been mapped on s.map by 
    #treatment[b] in loop 3[cc].
    loopB.new_stand <- vector(mode = "numeric", length = 0)
    #This object tracks the treatment type in loop 3[cc].
    loopB.treat_type <- vector(mode = "numeric", length = 0)
    #This object tracks management units associated with new stand numbers 
    #recorded in loop 3[cc].
    loopB.new_mgmtUnit <- vector(mode = "numeric", length = 0)
    #This object tracks the area of new stands recorded in loop 3[cc].
    loopB.new_area <- vector(mode = "numeric", length = 0)
    
    #This object tracks old stand numbers that are being overwritten by 
    #loop 3[cc].
    loopB.old_stand <- vector(mode = "numeric", length = 0)
    
    #This object tracks new stands that have been mapped on s.map for disturbance[e].
    loopE.NewStand <- vector(mode = "numeric", length = 0)
    #This object tracks the area of new stands.
    loopE.Area <- vector(mode = "numeric", length = 0)
    #This object tracks management units associated with new stand numbers 
    #recorded in loop 9[f].
    loopE.ReplacedStand <- vector(mode = "numeric", length = 0)
    #This object tracks management unit numbers associated with each stand.
    loopE.E_no <- vector(mode = "numeric", length = 0)
    loopE.F_no <- vector(mode = "numeric", length = 0)
    loopE.G_no <- vector(mode = "numeric", length = 0)
    
    #This object tracks weather the fire affecting the stand was a surface fire (1)
    #or crown fire (2).
    loopE.fireType <- vector(mode = "numeric", length = 0)
    
    #This object lists starting stand numbers in year[a] before the mapping loops
    #(loop 2, treatments in this case) run. This is used to keep treatments from 
    #overwriting a stand that was created within the same mapping loop. NOTE, this 
    #object is updated between loops 2 and 5 so that a disturbance (loop 5) can 
    #overwrite a treatment (loop 2) created within the same year so long as the new 
    #fuelbed created by the treatment is susceptible to the disturbance trying to 
    #overwrite it.
    loopA.snO <- sort(unique(as.vector(s.map[s.map != NoData.Unit])))
    #ERROR: CONSIDER USING COORDINATES BECAUSE IN THIS FORM SUBSEQUENT DISTURBANCE CANNOT
    #IMPACT ANY PART OF A STAND THAT HAS BEEN EVEN MINIMALLY AFFECTED BY A PREVIOUS DISTURBANCE.
    #ALSO, YOU NEED SOME WAY OF KEEPING TREATMENTS OUT OF UNITS THAT HAVE ALREADY BEEN TREATED
  
  if(sum(meanTAP) <= 0)
    {#1.1.1-----------------------------------------------------------------------------
    tbsa <- 0
    Treatment.Area <- 0
    PrctTrmt.Mapped <- 0
    } else #1.1.1------------------------------------------------------------------------ 
    { #1.1.2-----------------------------------------------------------------------------
      #LOOP 222222222222222222222222222222222222222222222222222222222222222222222222222222
      #Loop 2 (by treatments within year[a]). This loop runs all treatments for year[a].
      for (b in 1:r.max)#b <- 1
      { #2.0.0 ---------------------------------------------------------------------------
        
        #Set breaks to a default value
        #The breaks is used to terminate a disturbance when certain conditions are not being met
        #Loops will breaks when this object != 400.
        breaks <- 400
        
        #Governs loop
        end <- 1# switches to 2 if there is no remaining area available for the last management option
        
        #Lists treatment code for current treatment
        t.code <- ifelse(length(which((meanTAA + meanUAA) < meanTAP)) == 0,
                         4,min(which((meanTAA + meanUAA) < meanTAP)))
        
        if(t.code == 4)
        { #2.1.1-----------------------------------------------------------------------------
          break
        } else #2.1.1------------------------------------------------------------------------
  { #2.1.2-----------------------------------------------------------------------------
        
    #Tracks expansions
    d.d <- vector(length = 1, mode = 'numeric')
          
          #Updated here in case any original stands have been completely overwritten
          loopA.snO <- sort(unique(as.vector(s.map[!s.map %in% c(NoData.Unit, loopB.new_stand)])))
          
          #This object tracks new stand numbers that have been mapped on s.map by 
          #treatment[b] in iteration[cc].
          loopC.new_stand <- vector(mode = "numeric", length = 0)
          #This object tracks the treatment type in loop 3[cc].
          loopC.treat_type <- vector(mode = "numeric", length = 0)
          #This object tracks management units associated with new stand numbers 
          #recorded in loop 3[cc].
          loopC.new_mgmtUnit <- vector(mode = "numeric", length = 0)
          #This object tracks the area of new stands recorded in loop 3[cc].
          loopC.new_area <- vector(mode = "numeric", length = 0)
          
          #This object tracks old stand numbers that are being overwritten by 
          #loop 3[cc].
          loopC.old_stand <- vector(mode = "numeric", length = 0)
          
          #Pre-run Loop 3 number (used in tracking devices).
          cc <- 0
          
          #These objects record data for each mapping iteration.
          Treatment.Area <- vector(mode = "numeric", length = 1)
          PrctTrmt.Mapped <- vector(mode = "numeric", length = 1)
          
          #Record pre-run data (cc iteration = zero).
          Treatment.Area[cc+1] <- 0
          PrctTrmt.Mapped[cc+1] <- 0
          
          #Determine available fuelbeds.
          avfb <- fbls[ttxm[,t.code + 3] == 2]
          
    #Find eligible stands with eligible fuelbeds
    elst <- sort(unique(s.map[!b.map %in% c(NoData.Unit, Buffer.Unit, 
                                            Unmanaged.Unit) & 
                                      f.map %in% avfb & s.map %in% loopA.snO]))
          
    #Determine how management options will impact selection of burn unit selected based on area
    #treated for current treatment type
    row.code <- ifelse(length(which((b.treated + b.untreated) >= b.thresh$perc_cats)) == 0, 
                       1, (max(which((b.treated + b.untreated) >= b.thresh$perc_cats)) + 1))
    
    if(t.code %in% c(1,2))
    {
      #Determine the burn unit for treatment
      #Only consider management units where over 50% of the unit is available for treatment and
      #units are within the prescribed management option.
      #Units available based on these criteria are assigned a probability of assignment based 
      #on the percent of unit available for treatment with increasing probability of selection
      #as percentage of unit available increases.
      bun <- resample(b.unit$unit[pri[,t.code] > 0.50 & 
                                    b.block[,2 + b.thresh$m_code[row.code]] == 2 & b.unit$unit %in% t.bun], size = 1, 
                      prob = pri[,t.code][pri[,t.code] > 0.50 & 
                                            b.block[,2 + b.thresh$m_code[row.code]] == 2 & b.unit$unit %in% t.bun])
    } else
    {
      #Determine the burn unit for prescribed fire treatment
      #Only consider management units where TSLF is in the specified range and units are within
      #the prescribed management option.
      bun <- resample(b.unit$unit[TSLFxUnits %in% seq(b.thresh$mfriCats_min[row.code], 
                                                      b.thresh$mfriCats_max[row.code], 1) &  
                                    b.block[,2 + b.thresh$m_code[row.code]] == 2 & b.unit$unit %in% t.bun], size = 1)
    }
    
    #If no areas are available for specific management option then move to next one.
    if(length(bun) == 1)
    {
      #Status quo, no change
      row.code <- row.code#unit available, do not change row code.
    } else
    {
      #No available area for this treatment and management option, are there any remaining management options?
      if(row.code < length(b.thresh[,1]))
      {
        #Yes, move to next one and continue with disturbance loop.
        
        #Switch value of end to return to top of loop 2
        end <- 2
        
        #Register untreated area.
        b.untreated[row.code] <- b.thresh$perc_cats[row.code] - b.treated[row.code]
        meanUAA[t.code] <- sum(meanUAA[t.code], b.untreated[row.code])
            
      } else
      {
        #No, end disturbance loop.
        
        #Switch value of end object to kill loop 2
        end <- 3
        
        #Register untreated area.
        b.untreated[row.code] <- b.thresh$perc_cats[row.code] - b.treated[row.code]
        meanUAA[t.code] <- sum(meanUAA[t.code], b.untreated[row.code])
        
      }
    }
          
    if(end < 3)
    {#2.2.1 ---------------------------------------------------------------------------
     if(end < 2)
     {#2.3.1 ---------------------------------------------------------------------------
      
      #Records treatments selected for treatment so they will not be selected again
      c.bun <- c(c.bun,bun)
      t.bun <- b.unit$unit[!b.unit$unit %in% c.bun]
      
      ##Update tracking of area treated. Considers area of management unit selected, not actual
      #area within the unit affected.
      b.treated[row.code] <- sum(b.treated[row.code], sum(Area.List[MU.List == bun]))
      meanTAA[t.code] <- sum(meanTAA[t.code], sum(Area.List[MU.List == bun]))
      
      #Determine the treatment area. This is governed by the available fuelbeds, minimum stand
      #area, and fraction of available area treated (beta distribution).
      tbsa <- round(((length(b.map[b.map == bun & s.map %in% elst])) * 
                       rbeta(1,shape1[t.code],shape2[t.code])),0)
      
      #Cell selection method is different for silvicultural treatments and prescribed fire because
      #there is a continuous set of values determining probability of spread for prescribed fires
      #while probability of spread for silvicultural treatments is bindary.
      if(t.code == 3)
      {
        #Prescribed fire: Initiate treatment in the proportion of available pixels specified 
        #at the beginning of the script.
        sct <- vector(mode = "numeric", length = 0)
        sct <- l.map[b.map == bun & s.map %in% elst]
        f_sct <- f.map[match(sct, l.map)]
        ss.n <- length(f_sct)
        ss <- rbinom(ss.n, 1, f.probability[,2][match(f_sct, f.probability[,1])])
        sct <- sct[ss == 1]
        sct <- resample(sct, round(max((tbsa * seed.cells[t.code]), 1),0), replace = T)
        sct <- unique(sct)
      } else
      {
        #Silvicultural treatments: Initiate treatment in the proportion of available pixels specified 
        #at the beginning of the script.
        sct <- vector(mode = "numeric", length = 0)
        sct <- resample(l.map[b.map == bun & s.map %in% elst], 
                        round(max((tbsa * seed.cells[t.code]), 1),0), replace = T)
        sct <- unique(sct)
      }
          if(tbsa >= 1)
          { #2.4.1 ---------------------------------------------------------------------------
            
            #LOOP 3333333333333333333333333333333333333333333333333333333333333333333333
            #Loop 3 (by treatment[b] by block). This loop keeps running until treatment
            #has been mapped or stops establishing. Each time this loop runs it means that 
            #Loop 4 was not able to completely map the treatment. Each time this loop runs 
            #it will try and locate treatment[b] in a new location while maintaining the old
            #one. Thus it is possible that a treatment will have multiple contigous areas. Each
            #contigous area is referred to as a block and can have more than one stand if 
            #multiple fuelbeds are involved.
            for (cc in 1:r.max)#cc <- 1
            { #3.0.0 ---------------------------------------------------------------------------
              
              #Updated here in case any original stands have been completely overwritten
              loopA.snO <- sort(unique(as.vector(s.map[!s.map %in% c(NoData.Unit, 
                                                                     loopB.new_stand,
                                                                     loopC.new_stand)])))
              #CAN YOU SWITCH THIS TO JUST c(NoData.Unit, loopB.new_stand, loopC.new_stand)
              #WOULD REQUIRE CHANGING HOW THIS OBJECT IS USED THROUGHOUT FDM
              
              #Pre-run Loop 4 number (used in tracking devices).
              d <- 0
              
              #Set up an intra loop tracking device for overwritten stand numbers.
              #This will be fed into loopC.snNol
              osnt <- vector(length = 0)
              
              #Intraloop tracking mechanism for new stand coordinates (same as ocod in wildfire loop)
              ocot <- vector(length = 0, mode = "numeric")
              
              #Record area occupied by treatment[b].
              #tbma <- max(length(sct), length(s.map[s.map %in% loopC.new_stand]))
              tbma <- ifelse(cc == 1, length(sct), sum(loopC.new_area))
  
              #Ends loop when treatment[b] has been completely mapped.
              if(tbma < tbsa & breaks != 432)
              { #3.1.1 ---------------------------------------------------------------------------
             
                #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>NEW
                #Differentiate between initial loop and subsequent loops where you want to keep the
                #treatment within the designated burn unit boundaries.
                if(cc > 1)
                {
                  #This option will try and locate treatable areas in the burn block (bun) that are
                  #isolated and could not be treated from previous rounds of seed cells.
                  
                  #List eligible stands in burn block
                  elst <- sort(unique(s.map[!b.map %in% c(NoData.Unit, Buffer.Unit, Unmanaged.Unit) & 
                                              f.map %in% avfb & s.map %in% loopA.snO]))
                  
                  #Reset seed cells
                  sct <- vector(mode = "numeric", length = 0)
                  
                  #Ask question: is the remaining number of available cells greater than the remaining
                  #area to be treated. If the answer is no break from this loop and move on to the next
                  #disturbance. If yes, then generate seed cells in the isolated areas and re-initiate
                  #the treatment.
                  if(length(l.map[b.map == bun & s.map %in% elst]) < (tbsa-tbma))
                  {
                    Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                                            tesn)])
                    PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
                      length(Treatment.Area)]/tbsa)*100),1)
                    
                    breaks <- 311
                    break
                  } else
                  {
                    #Differentiate between spread of prescribed fires (continuous probabilities based 
                    #on fuelbed) and silvicultural treatments (binary spread based on fuelbeds)
                    if(t.code == 3)
                    {
                      #Prescribed fire -- select cells that fire will spread to.
                      sct <- vector(mode = "numeric", length = 0)
                      sct <- l.map[b.map == bun & s.map %in% elst]
                      f_sct <- f.map[match(sct, l.map)]
                      ss.n <- length(f_sct)
                      ss <- rbinom(ss.n, 1, f.probability[,2][match(f_sct, f.probability[,1])])
                      sct <- sct[ss == 1]
                      sct <- resample(sct, round(max((tbsa-tbma * seed.cells[t.code]), 1),0), replace = T)
                      sct <- unique(sct)
                    } else
                    {
                      sct <- vector(mode = "numeric", length = 0)
                      sct <- resample(l.map[b.map == bun & s.map %in% elst], 
                                      round(max((tbsa-tbma * seed.cells[t.code]), 1),0), replace = T)
                      sct <- unique(sct)
                    }
                  }
  
                  #Establish treatment[b] in s.map and record old stand number
                  ocot <- c(ocot, sct) #tracks coordinates involved in disturbance.
                  s.map[sct] <- s.map[sct]*tesn_t
                  osnt <- c(osnt,s.map[sct])
                  tesn <- unique(osnt)
                  
                } else
                {
                  #Establish treatment[b] in s.map and record old stand number
                  ocot <- c(ocot, sct) #tracks coordinates involved in disturbance.
                  s.map[sct] <- s.map[sct]*tesn_t
                  osnt <- c(osnt,s.map[sct])
                  tesn <- unique(osnt)
                }
                #Then find stands within this subset that meet minimum age requirements for
                #the disturbance/treatment.
                
                
                if(length(sct) > 0)
                { #3.2.1 ---------------------------------------------------------------------------
              
                  #Tracks the highest stand number currently in s.map. 
                  #Used to assign new stand numbers for treatment[b].
                  #Can not place this below loop 4 in case treatment erases stand with highest number.
                  masn <- max(treat.stand,max(unique(as.vector(s.map[s.map < fire.stand]))))
                  
                  #For fires determine and apply growth rate cutoff
                  if(t.code == 3)
                  {
                    #Pick a cutoff growth rate between 0.5% and 5% from an exponential
                    #prob den function... that is much higher chance of the treatment
                    #being cutoff at 5% growth over 0.5% growth.
                    
                    #Object is a list of cut off growth rates from 0.5% to 5%
                    list.of.cutoff.growth.rates <- seq(0.5,5,0.1)
                    #Object is a vector of probabilites from an inverse exponential function.
                    list.of.cutoff.probabilities <- sort(mapply(function(y) 
                    {
                      exp(-5*y)
                    },
                    seq(0.01,1,1/length(list.of.cutoff.growth.rates))))
                    
                    cutoff.growth.rate <- resample(list.of.cutoff.growth.rates,
                                                   1,
                                                   prob = list.of.cutoff.probabilities)
                  } else
                  {
                    cc <- cc
                  }
                  
                  #LOOP 4444444444444444444444444444444444444444444444444444444444444444444444444444
                  #Loop 4 (by iterations). This loop keeps growing treatment[b] in block[cc] 
                  #until growth stops.
                  for (d in 1:r.max)#d <- 1
                  { #4.0.0 ---------------------------------------------------------------------------
                          
                    #Area mapped for treatment[b].
                    tbma <- length(s.map[s.map %in% c(tesn,loopC.new_stand)])
  
                    #This statement stops loop 4 when treatment[b] has been fully mapped.
                    if(tbma < tbsa) 
                    { #4.1.1 ---------------------------------------------------------------------------
                      #Object shows locations of 8 pixels surrounding each mapped pixel for 
                      #disturbance[e].
                      sdlo <- find_neighbors(find_actively_burning_cells_in_smap(), radius = 1)
           
                      #This object shows all unique locations available for establishment by treatment[b].
                      avlo <- unique(l.map[sdlo][l.map[sdlo] %in% l.map[b.map == bun & s.map %in% elst]])
                     
                      #Ends loop if there are no more locations available for treatment[b] in the 
                      #block[cc] that is currently being mapped.
                      if(length(avlo) > 0)
                        {#4.2.1 ----------------------------------------------------------------------
  
                        #Reset new.cells object.
                        new.cells <- vector(length=0, mode = "numeric")
                        
                        #Differentiate between spread of prescribed fires (continuous probabilities based 
                        #on fuelbed) and silvicultural treatments (binary spread based on fuelbeds)
                        if(t.code == 3)
                        {
                          #PRESCRIBED FIRE
                          
                          #Apply probability of ignition values to potentially burned cells to determine
                          #which will actually burn.
                          fual <- f.map[match(avlo, l.map)]
                          ss.n <- length(fual)
                          ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
                          new.cells <- avlo[ss == 1]
                        } else {
                          #SILVICULTURAL TREATMENT
                          new.cells <- avlo
                        }
                          
                        #This expression picks out which location values are of the same stand and are 
                        #available (i.e. they are not occupied by the another treatment) and makes sure 
                        #that the mapped regime does not exceed its prescribed area.
                        if((tbma + length(new.cells)) <= tbsa)
                        {
                          s.map[new.cells] <- s.map[new.cells]*tesn_t
                          osnt <- c(osnt, s.map[new.cells])
                          tesn <- unique(osnt)
                          ocot <- c(ocot, new.cells) #tracks coordinates involved in disturbance.
                        } else
                        {
                          new.cells <- resample(new.cells,(tbsa - tbma))
                          s.map[new.cells] <- s.map[new.cells]*tesn_t
                          osnt <- c(osnt, s.map[new.cells])
                          tesn <- unique(osnt)
                          ocot <- c(ocot, new.cells) #tracks coordinates involved in disturbance.
                        }
             
                        #Register mapping data after Loop 4 has finished running for iteration[d].
                        Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                                                tesn)])
                        PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
                          length(Treatment.Area)]/tbsa)*100),1)
                        
                        #Evaluate growth rate of prescribed fire and end loop 4 if growth
                        #rate has dropped below the predetermined cutoff
                        #Do not apply to silvicultural treatments.
                        if(t.code == 3)
                          {
                          #Determine if the rate of prescribed fire growth has dropped below 1% per
                          #iteration. When this happens the loop ends.
                          treatment.growth <- PrctTrmt.Mapped[length(PrctTrmt.Mapped)]-
                            PrctTrmt.Mapped[length(PrctTrmt.Mapped)-1]
                          
                          if(treatment.growth > cutoff.growth.rate)
                        {#4.3.1----------------------------------------------------------------------
                          d <- d#placeholder
                        } else#4.3.1-----------------------------------------------------------------
                        {#4.3.2----------------------------------------------------------------------
                        Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                                                  tesn)])
                          PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
                            length(Treatment.Area)]/tbsa)*100),1)
                          
                          breaks <- 432  
                          break
                        }#4.3.2----------------------------------------------------------------------
                        } else
                        {
                          d <- d#placeholder
                        }
                        
                      } else #4.2.1 ----------------------------------------------------------------------
                      
  {#4.2.2
    Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                            tesn)])
    PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
      length(Treatment.Area)]/tbsa)*100),1)
  
  breaks <- 422  
  break
  } #4.2.2 ---------------------------------------------------------------------------
  
                    } else #4.1.1 ----------------------------------------------------------------------
  
  { #4.1.2 ---------------------------------------------------------------------------
    Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                            tesn)])
    PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
      length(Treatment.Area)]/tbsa)*100),1)
  
  breaks <- 412    
  break
  } #4.1.2 ---------------------------------------------------------------------------
  
                 } #4.0.0 ---------------------------------------------------------------------------
  #Find unique fuelbeds in each management unit
  
  #Unique old stands
  osto <- sort(unique(tesn))
  
  #Log old stand numbers and area before they are changed in s.map.
  loopC.old_stand <- c(loopC.old_stand,osto)
  
  #Number of stands
  nobc <- length(osto)
  
  #Determine new stand numbers for treatment[b], block[cc].
  nebc <- seq((masn + 1), (masn + nobc), 1)
  
  #Map new stands
  tn <- data.frame(osnt = osnt, ocot = ocot)
  tn.b <- tn[order(tn$ocot),]
  v.nebc <- nebc[match(tn.b$osnt, osto)]
  s.map[l.map %in% tn.b$ocot] <- v.nebc
  
  #Log new stand numbers and associated treatments when they have been added to s.map.
  loopC.new_stand <- c(loopC.new_stand,nebc)
  loopC.treat_type <- c(loopC.treat_type,rep(t.code,nobc))
  loopC.new_mgmtUnit <- c(loopC.new_mgmtUnit,rep(bun, length(nebc)))
  l.nebc <- rep(1,length(v.nebc))
  s.nebc <- summarize(l.nebc,v.nebc,sum)
  loopC.new_area <- c(loopC.new_area, as.vector(s.nebc[,2]))
  
                } else #3.2.1 ----------------------------------------------------------------------
  
  { #3.2.2 ---------------------------------------------------------------------------
    Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                            tesn)])
    PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
      length(Treatment.Area)]/tbsa)*100),1)
    breaks <- 322  
    break
  } #3.2.2 ---------------------------------------------------------------------------
  
              } else #3.1.1 ----------------------------------------------------------------------
  
  { #3.1.2 ---------------------------------------------------------------------------
    Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                            tesn)])
    PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
      length(Treatment.Area)]/tbsa)*100),1)
    breaks <- 312
    break
  } #3.1.2 ---------------------------------------------------------------------------
  d.d <- sum(d.d, d)#tracks expansions
  
            } #3.0.0 --------------------------------------------------------------------------- 
  #Log new stand numbers and associated treatments when they have been added to s.map.
  loopB.new_stand <- c(loopB.new_stand,loopC.new_stand)
  loopB.treat_type <- c(loopB.treat_type, loopC.treat_type)
  loopB.new_mgmtUnit <- c(loopB.new_mgmtUnit,loopC.new_mgmtUnit)
  loopB.new_area <- c(loopB.new_area,loopC.new_area)
  loopB.old_stand <- c(loopB.old_stand,loopC.old_stand)
  loopB <- data.frame(new_stand = loopB.new_stand, 
                      treat_type = loopB.treat_type, 
                      new_mgmtUnit = loopB.new_mgmtUnit, 
                      new_area = loopB.new_area, 
                      old_stand = loopB.old_stand * -1)
  loopB <- loopB[order(loopB$old_stand),]
  
  #Date and time
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  
  #Tracking device
  t.summary <- paste(
    "Date: ", dt, 
    " Time: ", tm, 
    " Year: ", a, 
    " PercentComplete_ForYear: ", round((((sum(meanTAA)-sum(meanUAA))/sum(meanTAP))*100),2), 
    " Disturbance_No ", b,
    " Name: ", f.treatments$TreatmentTitle[t.code], 
    "MgmtOp: ", b.thresh$management_type[row.code], 
    "BurnBlock: ", bun, 
    "PercentBlack: ", round(sum(loopB.new_area)/sum(Area.List[MU.List == bun]), 1), 
    " TreatedArea_Expected: ", tbsa, 
    " TreatedArea_Actual: ", sum(loopB.new_area),
    "Untreated_Area: ", b.untreated[row.code], 
    " Blocks: ", cc, 
    " Expansions: ", d.d, 
    "HiStandNo: ", max(nebc)) 
  
  #Save run data.
  cat(t.summary, file = paste(output_path, "run_", run, "_disturbances.txt", 
                              sep = ""), fill = T, append = T)#
  
          } else #2.4.1 ----------------------------------------------------------------------
  
  { #2.4.2 ---------------------------------------------------------------------------
  
    #Register untreated area.
    b.untreated[row.code] <- b.thresh$perc_cats[row.code] - b.treated[row.code]
    meanUAA[t.code] <- sum(meanUAA[t.code], b.untreated[row.code])
  
    #Log new stand numbers and associated treatments when they have been added to s.map.
    loopB.new_stand <- c(loopB.new_stand,loopC.new_stand)
    loopB.treat_type <- c(loopB.treat_type, loopC.treat_type)
    loopB.new_mgmtUnit <- c(loopB.new_mgmtUnit,loopC.new_mgmtUnit)
    loopB.new_area <- c(loopB.new_area,loopC.new_area)
    loopB.old_stand <- c(loopB.old_stand,loopC.old_stand)
    loopB <- data.frame(new_stand = loopB.new_stand, 
                        treat_type = loopB.treat_type, 
                        new_mgmtUnit = loopB.new_mgmtUnit, 
                        new_area = loopB.new_area, 
                        old_stand = loopB.old_stand * -1)
    loopB <- loopB[order(loopB$old_stand),]
    
    #Date and time
    dt <- Sys.Date()
    tm <- format(Sys.time(), format = "%H.%M.%S", 
                 tz = "", usetz = FALSE)
    
    #Tracking device
    t.summary <- paste(
      "Date: ", dt, 
      " Time: ", tm, 
      " Year: ", a, 
      " PercentComplete_ForYear: ", round((((sum(meanTAA)-sum(meanUAA))/sum(meanTAP))*100),2), 
      " Disturbance_No ", b,
      " Name: ", f.treatments$TreatmentTitle[t.code], 
      "MgmtOp: ", b.thresh$management_type[row.code], 
      "BurnBlock: ", bun, 
      "PercentBlack: ", round(tbsa/sum(Area.List[MU.List == bun]), 1), 
      " TreatedArea_Expected: ", tbsa, 
      " TreatedArea_Actual: ", tbma,
      "Untreated_Area: ", b.untreated[row.code], 
      " Blocks: ", cc, 
      " Expansions: ", d.d, 
      "HiStandNo: ", max(nebc)) 
    
    #Save run data.
    cat(t.summary, file = paste(output_path, "run_", run, "_disturbances.txt", 
                                sep = ""), fill = T, append = T)#
    
  } #2.4.2 ---------------------------------------------------------------------------
     } else #2.3.1 ----------------------------------------------------------------------
  
  { #2.3.2 ---------------------------------------------------------------------------
    #Date and time
    dt <- Sys.Date()
    tm <- format(Sys.time(), format = "%H.%M.%S", 
                 tz = "", usetz = FALSE)
    
    #Tracking device
    t.summary <- paste(
      "Date: ", dt, 
      " Time: ", tm, 
      " Year: ", a, 
      " PercentComplete_ForYear: ", round((((sum(meanTAA)-sum(meanUAA))/sum(meanTAP))*100),2), 
      " Disturbance_No ", b,
      " Name: ", f.treatments$TreatmentTitle[t.code], 
      "MgmtOp: ", b.thresh$management_type[row.code], 
      "BurnBlock: ", bun, 
      "PercentBlack: ", round(tbsa/sum(Area.List[MU.List == bun]), 1), 
      " TreatedArea_Expected: ", tbsa, 
      " TreatedArea_Actual: ", tbma,
      "Untreated_Area: ", b.untreated[row.code], 
      " Blocks: ", cc, 
      " Expansions: ", d.d, 
      "HiStandNo: ", max(nebc)) 
  
    #Save run data.
    cat(t.summary, file = paste(output_path, "run_", run, "_disturbances.txt", 
                                sep = ""), fill = T, append = T)#
    
    #Move to next row code and t code.
    row.code <- row.code + 1
    t.code <- b.actual$t_code[row.code]
  } #2.3.2 --------------------------------------------------------------------------- 
    } else #2.2.1 ----------------------------------------------------------------------
  
  { #2.2.2 ---------------------------------------------------------------------------
    #Date and time
    dt <- Sys.Date()
    tm <- format(Sys.time(), format = "%H.%M.%S", 
                 tz = "", usetz = FALSE)
    
    #Tracking device
    t.summary <- paste(
      "Date: ", dt, 
      " Time: ", tm, 
      " Year: ", a, 
      " PercentComplete_ForYear: ", round((((sum(meanTAA)-sum(meanUAA))/sum(meanTAP))*100),2), 
      " Disturbance_No ", b, 
      " Name: ", f.treatments$TreatmentTitle[t.code], 
      "MgmtOp: ", b.thresh$management_type[row.code], 
      "BurnBlock: ", bun, 
      "PercentBlack: ", round(tbsa/sum(Area.List[MU.List == bun]), 1), 
      " TreatedArea_Expected: ", tbsa, 
      " TreatedArea_Actual: ", tbma,
      "Untreated_Area: ", b.untreated[row.code], 
      " Blocks: ", cc, 
      " Expansions: ", d.d, 
      "HiStandNo: ", max(nebc))  
  
    #Save run data.
    cat(t.summary, file = paste(output_path, "run_", run, "_disturbances.txt", 
                                sep = ""), fill = T, append = T)#
    breaks <- 222
    break
  } #2.2.2----------------------------------------------------------------------------
  } #2.1.2----------------------------------------------------------------------------
  
  } #2.0.0 -----------------------------------------------------------------------
    } #1.1.2--------------------------------------------------------------------------
  
  #Update time-since-last-fire to include latest treatments and add one year
  #treatments include herbicide and thinning
  TSLFxUnits[b.unit$unit %in% c.bun] <- 0
  TSLFxUnits <- TSLFxUnits + 1
    
  #Post-forest management treatment processing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #Update files if there were treatments in year[a].
  if(length(loopB.new_stand) > 0)
  {
  
    #Object shows fuelbeds associated with each new stand number created by treatments
    #in year[a].
    #ufxTa <- mapply(function(x) unique(f.map[s.map == x]), loopB.new_stand)
    ufxTa_1 <- Fuelbed.List[Stand.List %in% loopB$old_stand]
    usxTa_1 <- Stand.List[Stand.List %in% loopB$old_stand]
    ufxTa <- ufxTa_1[match(loopB$old_stand, usxTa_1)]
  
    #Add ufxTa to loopB data frame
    loopB <- data.frame(loopB, ufxTa = ufxTa)
  
  #Sort data frame by new stands
  loopB <- loopB[order(loopB$new_stand),]#probably unecessary
  
  #seperate out new stands from s.map, lists occurrences of new stands from min to max
  #coords.
  vs.map_a7 <- s.map[s.map %in% loopB$new_stand]
  
  #List of row numbers in t.post where fuelbeds need to be updated based on treatment
  FL1 <- which(t.post$fuelbed %in%loopB$ufxTa)
  
  #Fuelbeds that may be updated corresponding with row numbers in FL1
  LL1 <- t.post$fuelbed[t.post$fuelbed %in% ufxTa]
  
  #row numbers in t.post corresponding with each existing fuelbed in each new stand 
  LL2 <- FL1[match(loopB$ufxTa, LL1)]
  
  #Use row numbers (LL2) and column numbers (loopB.treat_type) to calculate 
  #"coordinate" in t.post
  LL3 <- ((loopB.treat_type - 1) * length(t.post$fuelbed)) + LL2
  
  #Convert t.post (ttxm is t.post) from a data.frame into a matrix so new fuelbeds 
  #can be identified by coordinates that corresond with row and column numbers.
  am_ttxm <- as.matrix(ttxm)  
  
  #Idenintify new fuelbed for each new stand.
  newFB_a7 <- am_ttxm[LL3]
  
  #Lists occurrences of new fuelbeds from min to max coords.
  v.newFB_a7 <- newFB_a7[match(vs.map_a7, loopB$new_stand)]
  
  #Replace old fuelbeds with new ones in f.map
  f.map[s.map %in% loopB$new_stand] <- v.newFB_a7
  
  #List ages associated with each stand that has been affected by treatment
  #These will be used to update Age.List but are unchanged since treatments do not
  #change overstory age.
  newAGE_a7_1 <- Age.List[Stand.List %in% loopB$old_stand]
  newAGE_a7 <- newAGE_a7_1[match(loopB$old_stand, usxTa_1)]
  
  #Add newAGE_a7 to loopB data.frame
  loopB <- data.frame(loopB, newAGE_a7 = newAGE_a7)
  
  #Add newFB_a7 to loopB data.frame
  loopB <- data.frame(loopB, newFB_a7 = newFB_a7)
  
  #List stands that have been altered by treatments.
  ss1 <- (loopB.old_stand*-1)
  
  #There can be duplicates, this will mess up the shortcut in a9
  stands <- sort(unique(ss1))
  
  #Sum areas for duplicates.
  sa <- summarize(loopB.new_area,ss1,sum)
  sareas <- as.vector(sa[,2])
  tt <- summarize(loopB.treat_type, ss1, min)
  stype <- as.vector(tt[,2])
    
  #Shelve fire history for stands that have been impacted by disturbance
  new_mfri_vec <- mapply(function(y) mfri.Matrix[Stand.List == y,], ss1)
  nmv <- t(new_mfri_vec)
    
  #Add a fire for stands that were prescrib burned
  nmv[,30] <- ifelse(loopB.treat_type == 3,1,0)
    
  #Change stand properties as needed for treatments.
  #Create a data.frame so you can switch order from stand number to
  #order in which stand was added.
  ID1.List <- 1:length(Stand.List)
  TL1 <- data.frame(ID = ID1.List, Stand = Stand.List, Area = Area.List)
  #Order by stand number
  TL1 <- TL1[order(TL1$Stand),]
  #Subtract area of new stands from corresponding old stands
  TL1$Area[TL1$Stand %in% stands] <- TL1$Area[TL1$Stand %in% stands] - sareas
  #Reorder by order stands were added
  TL1 <- TL1[order(TL1$ID),]
  #Replace Area.List with updated object
  Area.List <- TL1$Area
  
  #Update list to remove any stands that have been overwritten.
    Stand.List <- Stand.List[(Area.List == 0) == F]
    Fuelbed.List <- Fuelbed.List[(Area.List == 0) == F]
    Age.List <- Age.List[(Area.List == 0) == F]
    T1E.List <- T1E.List[(Area.List == 0) == F]
    T2E.List <- T2E.List[(Area.List == 0) == F]
    D1E.List <- D1E.List[(Area.List == 0) == F]
    D2E.List <- D2E.List[(Area.List == 0) == F]
    Coord.List <- Coord.List[(Area.List == 0) == F]
    MU.List <- MU.List[(Area.List == 0) == F]
    mfri.Matrix <- mfri.Matrix[(Area.List == 0) == F,]
    mfri_lower.List <- mfri_lower.List[(Area.List == 0) == F]
    mfri_upper.List <- mfri_upper.List[(Area.List == 0) == F]
    Area.List <- Area.List[(Area.List == 0) == F]
  
    #Update list to add new stands.
    Stand.List <- c(Stand.List,loopB.new_stand)
    Fuelbed.List <- c(Fuelbed.List,newFB_a7)
    Age.List <- c(Age.List,newAGE_a7)
    
    #List fuelbeds that need to be updated.
    pdaFB <- pda$pre[pda$pre %in% newFB_a7]
    
    #List corresponding updated age restrictions
    pdaTH <- pda$thin[pda$pre %in% newFB_a7]
    
    #List occurences of age restriction for each new stand
    v.THIN <- pdaTH[match(newFB_a7,pdaFB)]
  
    #Update
    T1E.List <- c(T1E.List,v.THIN)
    
    #List corresponding updated age restrictions
    pdaHE <- pda$herb[pda$pre %in% newFB_a7]
    
    #List occurences of age restriction for each new stand
    v.HERB <- pdaHE[match(newFB_a7,pdaFB)]
    
    #Update
    T2E.List <- c(T2E.List, v.HERB)
  
    #List corresponding updated age restrictions
    pdaSF <- pda$sfire[pda$pre %in% newFB_a7]
    
    #List occurences of age restriction for each new stand
    v.SFIRE <- pdaSF[match(newFB_a7,pdaFB)]
    
    #Update
    D1E.List <- c(D1E.List, v.SFIRE)
    
    #List corresponding updated age restrictions
    pdaCF <- pda$cfire[pda$pre %in% newFB_a7]
    
    #List occurences of age restriction for each new stand
    v.CFIRE <- pdaCF[match(newFB_a7,pdaFB)]
    
    #Update
    D2E.List <- c(D2E.List, v.CFIRE)
  
    #List new stand occurrences in s.map
    vs.map_a11 <- s.map[s.map %in% loopB.new_stand]
  
    #List corresponding coordinates (l.map) for new stand occurrences in s.map
    vl.map_a11 <- l.map[s.map %in% loopB.new_stand]
  
    #Use summarize function (w/ min()) to select a single coordinate value for each 
    #new stand.
    v.Coord_a11a <- summarize(vl.map_a11,vs.map_a11,min)
    
    #Subset coordinates
    v.Coord_a11b <- as.vector(v.Coord_a11a[,2])
  
    #Update
    Coord.List <- c(Coord.List,v.Coord_a11b)
  
    #Update
    MU.List <- c(MU.List, loopB.new_mgmtUnit)
  
    #Update
    mfri.Matrix <- rbind(mfri.Matrix,nmv)
  
    #Update
    Area.List <- c(Area.List,loopB.new_area)
    
    #Update
    mfri_lower.List <- c(mfri_lower.List, d.post$more_fire[match(newFB_a7,d.post$fuelbed)])
  
    #Select option 1 for fuelbed transition when mfri lengthens
    less1 <- d.post$less_fire1[match(newFB_a7,d.post$fuelbed)]
  
    #Select option 2 for fuelbed transition when mfri lengthens
    less2 <- d.post$less_fire1[match(newFB_a7,d.post$fuelbed)]
    
    #Randomly choose between option 1 and 2 for each fuelebd.
    less <- apply(matrix(data = c(less1,less2),length(less1), 2), 1, sample, size = 1)
    
    #Update
    mfri_upper.List <- c(mfri_upper.List, less)
  
    #Re-order loopB by new stand for tslt objects.
    loopB <- loopB[order(loopB$new_stand),]
    
    #Remove stands that have been overwritten
    tslt.List <- tslt.List[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    tslt.Fuelbeds <- tslt.Fuelbeds[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    tslt.Stands <- tslt.Stands[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    
    
    #Update time-since-last-treatment list and associated stand list
    tslt.List <- c(tslt.List, rep(0, length(loopB$new_stand[loopB$treat_type %in% c(2,3)])))
    tslt.Fuelbeds <- c(tslt.Fuelbeds, loopB$newFB_a7[loopB$treat_type %in% c(2,3)])
    tslt.Stands <- c(tslt.Stands, loopB$new_stand[loopB$treat_type %in% c(2,3)])
    
  if(any(c(length(Stand.List),
           length(Fuelbed.List),
           length(MU.List),
           length(T1E.List),
           length(T2E.List),
           length(D1E.List),
           length(D2E.List),
           length(Area.List),
           length(mfri.Matrix[,1]), 
           length(Age.List),
           length(mfri_lower.List),
           length(mfri_upper.List)) != (length(unique(as.vector(s.map)))-1)) == T)
  {
    r101 <- 4
    break
  } else
  {
    r101 <- ifelse(any(s.map < 0 & s.map > -9999),44,0)   
  } 
  
  } else
  {
    #No new stand numbers, enter a placeholder if there were no treatments for year[a].
    a <- a
  }
  
  if(length(diar) == 0)
  { #1.1.1 ---------------------------------------------------------------------------
    e <- 0
    title <- "None"
    da <- 0
    pm <- "NA"
  
    #Used for reporting after loop 8
    dema <- 0
    desa <- 0
    
  } else #1.1.1 ---------------------------------------------------------------------------
    { #1.1.2 ---------------------------------------------------------------------------
    #LOOP 888888888888888888888888888888888888888888888888888888888888888888888888888888
    #Loop 8 (by disturbances within year[a]). This loop runs all disturbances for 
    #year[a].
    for (e in tdn[tdy == a])#e <- 2
    { #8.0.0 ---------------------------------------------------------------------------
    
      ### 
     forceBurnOut <- 0
      
      g.g <- vector(length =, mode = 'numeric')
    
     #Resent tesn
        tesn <- -1
        
        #Reset object that tracks completely burned units, used in loop 9
        burned.units <- vector(length = 0, mode = 'numeric')
        
        #Update this tracking object so that new stands created in the last fire[e] are
        #excluded.
        loopA.snO <- sort(unique(as.vector(s.map[!s.map %in% c(NoData.Unit,
                                                               loopB.new_stand, 
                                                               loopE.NewStand)])))
        
        #This object tracks new stands that have been mapped on s.map for disturbance[e].
        loopF.NewStand <- vector(mode = "numeric", length = 0)
        #This object tracks the area of new stands.
        loopF.Area <- vector(mode = "numeric", length = 0)
        #This object tracks stand numbers that are being overwritten by loopF.new_stand.
        loopF.ReplacedStand <- vector(mode = "numeric", length = 0)
        #This object tracks management unit numbers associated with each stand.
        loopF.E_no <- vector(mode = "numeric", length = 0)
        loopF.F_no <- vector(mode = "numeric", length = 0)
        loopF.G_no <- vector(mode = "numeric", length = 0)
        
        #This object tracks which stands are surface fires and which are crown fires.
        loopF.fireType <- vector(mode = "numeric", length = 0)
        
        #Pre-run Loop 9 number (used in tracking devices).
        f <- 0
        
        #Object shows the number of pixels for disturbance[e].
        desa <- round(tda[e],0)
        
        #Object shows fuelbeds available for establishment by disturbance[e].
        avfb <- fbls[!fbls %in% Non.Flammable]
        
        #End script for disturbance[e] if there are no available cells to establish.
        #1) must not be a wilderness area (in w.map 2 = wilderness, 1 non-wilderness, 
        #and 0 = area that are null in f.map.
        #2) fuelbed must be applicable to disturbance type.
        #3) must be a stand that was not created during year[a].
        if(length(f.map[f.map %in% avfb & s.map %in% loopA.snO]) > 0)
        { #8.1.1 ---------------------------------------------------------------------------
          
          #LOOP 999999999999999999999999999999999999999999999999999999999999999999999999999999
          #Loop 9 (by % disturbance[e] completed). This loop keeps running until disturbance
          #has been mapped or stops establishing.
          for (f in 1:r.max)#f <- 1
          { #9.0.0 ---------------------------------------------------------------------------
            
            breaks <- ifelse(f == 1, 0, breaks)#monitoring object, if model crashes, it can help
            #pinpoint the last loop that was operating.
            if(forceBurnOut == 1 | f == 1)
            {
              spread.type <- 0
            } else
            {
              spread.type <- spread.type
            }
                   
              #Default g loop number.
              g <- 0
              h <- 0
              
              #Reset cumulative temporary stand numbers
              tesn_cum <- vector()
              
              #Set up an intra loop tracking device for overwritten stand numbers.
              #This will be fed into loopE.ReplacedStands
              osnd <- vector(length = 0, mode = "numeric")
              osnd_crown <- vector(length = 0, mode = "numeric")
              
              #Set up an intra loop tracking device for overwritten coordinates.
              ocod <- vector(length = 0, mode = "numeric")
              ocod_crown <- vector(length = 0, mode = "numeric")
              
              #Tracks the highest stand number. Used to assign stand numbers to new disturbance.
              #beneath loop 7. Can't place this below loop 7 in case disturbance erases stand with
              #highest number.
              mudn <- max(fire.stand,max(unique(as.vector(s.map))))
              
    #Record area occupied by disturbance[e].
            dema <- length(s.map[s.map %in% loopF.NewStand])
  
            #Ends loop when disturbance[e] has been completely mapped.
            if(dema < desa)
            { #9.1.1 ---------------------------------------------------------------------------
  
                #Updated here in case any original stands have been completely overwritten.
                #Exclude the no data unit, stands created in this year's treatment loop, and
                #stands created from previously burned areas this year.
                loopA.snO <- sort(unique(as.vector(s.map[!s.map %in% c(NoData.Unit,
                                                                       loopB.new_stand, 
                                                                       loopE.NewStand, 
                                                                       loopF.NewStand)])))
  
              if(spread.type == 0)
              { #9.2.1---------------------------------------------------------------
               
              ####################################################################################
              #This next section produces probabilities that help direct the location
              #of fire starts. The probabilities are based on spread probabilities and
              #area occupied by the fuelbed relative to burnable fuelbeds in f.map.
              
              #First, find stands with useable fuelbeds, i.e. disqualify fuelbeds 0 and 400.
              ss1 <- sort(unique(Stand.List[!Fuelbed.List %in% Non.Flammable & 
                                              Stand.List %in% loopA.snO]))
              
              #Then find stands within this subset that meet minimum age requirements for
              #the disturbance/treatment.
              ss2 <- Stand.List[Stand.List %in% ss1 & Age.List >= D1E.List]
              
              if(tdc[e] == 1)
              { #EAFB -----------------------------------------------------------
   
                #Pick a burn unit based on probability of fire start data in each unit
                #If the fire is small (less than 50 acres (224 pixels) you can use units 
                #that were treated this year, but if the fire is larger than 50 acres it
                #will be restricted to untreated units)
                if((tda[e]-dema) < 200)
                {
                  ignition.bun <- resample(b.unit$unit,1,prob = f.start$Prob)
                } else
                {
                  ignition.bun <- resample(b.unit$unit[!b.unit$unit %in% c.bun], 1, 
                                           prob = f.start$Prob[!b.unit$unit %in% c.bun])
                }
                
                #Create seperate stand lists for the buffer and eglin zones
                ss2.e <- sort(unique(as.vector(
                  s.map[s.map %in% ss2 & b.map == ignition.bun])))
                
                if(length(ss2.e) > 0)
                {
  
                  #Next, create a list of unique fuelbeds within these stands.
                  f.summary <- sort(unique(Fuelbed.List[Stand.List %in% ss2.e]))
                
                  #Areas associated with fuelbeds in f.summary.
                  a.summary <- mapply(function(x) sum(Area.List[Fuelbed.List == x & 
                                                                  Stand.List %in% ss2.e]),
                                      f.summary)
             
                  #Spread probabilities for each fuelbed.
                  p.summary <- mapply(function(x) f.probability[,2][f.probability[,1] == x],
                                      f.summary)
         
                  #Virtual area of fuelbeds a function of number of cells and spread probability.
                  #This merely determines multiples based on the fuelbed with the smallest probability.
                  #For example, if there is a map with four cells divided evenly between two fuelbeds
                  #and one fuelbed has a spread potential of 0.25 and the other of 0.5, then the
                  #the v.summary values will be 1 and 2 respectively. These numbers are used below
                  #to determine the relative weight of each fuelbed in conjunction with its relative
                  #area.
                  v.summary <- mapply(function(x) (x/min(p.summary)), p.summary)
          
                  #Probability of a fire starting at a location occupied by each fuelbed based
                  #on area.
                  fire.start <- (v.summary * a.summary)/(sum(v.summary * a.summary))
                } else
                {
  
                  fire.start <- vector(length=0,mode='numeric')
              
                }
              } else #EAFB -----------------------------------------------------------
              { #Buffer -----------------------------------------------------------
  
                ss2.b <- sort(unique(as.vector(
                  s.map[s.map %in% ss2 & b.map == Buffer.Unit])))
                
                if(length(ss2.b) > 0)
                {
  
                  #Next, create a list of unique fuelbeds within these stands.
                  f.summary <- sort(unique(Fuelbed.List[Stand.List %in% ss2.b]))
                 
                  #Areas associated with fuelbeds in f.summary.
                  a.summary <- mapply(function(x) sum(Area.List[Fuelbed.List == x & 
                                                                  Stand.List %in% ss2.b]),
                                      f.summary)
                 
                  #Spread probabilities for each fuelbed.
                  p.summary <- mapply(function(x) f.probability[,2][f.probability[,1] == x],
                                      f.summary)
                
                  #Virtual area of fuelbeds a function of number of cells and spread probability.
                  #This merely determines multiples based on the fuelbed with the smallest probability.
                  #For example, if there is a map with four cells divided evenly between two fuelbeds
                  #and one fuelbed has a spread potential of 0.25 and the other of 0.5, then the
                  #the v.summary values will be 1 and 2 respectively. These numbers are used below
                  #to determine the relative weight of each fuelbed in conjunction with its relative
                  #area.
                  v.summary <- mapply(function(x) (x/min(p.summary)), p.summary)
                
                  #Probability of a fire starting at a location occupied by each fuelbed based
                  #on area.
                  fire.start <- (v.summary * a.summary)/(sum(v.summary * a.summary))
  
                } else
                {
  
                  fire.start <- vector(length=0,mode='numeric')
  
                }
              } #Buffer -----------------------------------------------------------
            
              } else #9.2.1---------------------------------------------------------------
  { #9.2.2---------------------------------------------------------------
    save.desa <- desa
  } #9.2.2---------------------------------------------------------------
              #Ends loop if there are no locations to establish disturbance[e] where fuelbed 
              #requirements, wilderness designations, and stand numbers check out. 
              if(length(f.map[f.map %in% dtxm[,2] & s.map %in% loopA.snO]) > 0)
                #need something that measures previously assigned cells
              { #9.3.1 ---------------------------------------------------------------------------
                if(length(fire.start) > 0)
                { #9.4.1---------------------------------------------------------------
                if(spread.type != 11)
                { #9.5.1---------------------------------------------------------------
                   
                #Generates seed cell for disturbance[e]
                #scd object hold the fire location for disturbance[e].
                scd <- vector(mode = "numeric", length = 0)
   
    if(spread.type == 0)
    {#9.6.1 ---------------------------------------------------------------------------
   
                #determine the fuelbed disturbance[e] will start in.
                if(length(f.summary) == 1)
                {
                  ignition.fuelbed <- f.summary
                } else
                {
                  ignition.fuelbed <- sample(f.summary,size=1,prob=fire.start)
                }
          
                #Create an object that shows available burn units depending on whether the fire
                #is in Eglin or the buffer zone.
     if(tdc[e] == 1)
     {f.bun <- ignition.bun} else
     {f.bun <- Buffer.Unit}
  
     #Select a seed cell
     scd.1 <- resample(l.map[f.map %in% ignition.fuelbed & s.map %in% ss2 & 
                             b.map %in% f.bun],1)
     
     #Determine area that could be burned.
     a.bun <- round(((length(b.map[s.map %in% ss2 & b.map == f.bun])) * 
                       rbeta(1,shape1[3],shape2[3])),0)
     
     #Calculate seed cells to be used if block and burn loop is activated.
     scd.p <- resample(l.map[s.map %in% ss2 & b.map %in% f.bun], 
                       round(max((a.bun * seed.cells[3]),1),0), replace = T)
     scd.p <- unique(scd.p)
     
     #This statement tests to see if scd.1 coordinate was generated by function that creates 
     #scd.p and removes it in scd.p so it is not duplicated in scd <- c(scd.1, scd.p2)
     if(length(which(scd.p == scd.1)) == 0)
     {
       scd.2 <- scd.p
     } else
     {
       scd.2 <- scd.p[-which(scd.p == scd.1)]
     }
     
     #Reset seed cell vector.
     scd <- vector(mode = "numeric", length = 0)
     
     #Add seed cell vectors.
     scd <- c(scd.1, scd.2)
     
     #Save stand number in case loop 11 (Block and Burn) is activated.
     #If this occurs stand number needs to be converted from -1.
     scd.tesn <- s.map[scd[1]]#used if this fire moves directly to unit burn loop H/11.
     
     #Update vector that stores stand numbers of cells affected by fire.
     osnd <- c(osnd,s.map[scd[1]])
     
     #Update vector that stores coordinates of cells affected by fire.
     ocod <- c(ocod, scd[1])
     
     #Assign stand number
     s.map[scd[1]] <- tesn
     
     #Used for measuring area of fire in tracking objects
     tesn_cum <- tesn
  
      } else #9.6.1 ---------------------------------------------------------------------------
      { #9.6.2 ---------------------------------------------------------------------------
        #Code runs when a wildfire has burned beyond the original "block and burn" boundary.
  
        #Identify management units fire has spread into.
        f.bun <- sort(unique(b.map[s.map %in% neef]))
        
        #Remove units if they have been completely burned in loop 11.
        f.bun <- f.bun[!f.bun %in% burned.units]
        
        #If there are more than 21 burn blocks under consideration you will exceed the 
        #maximum vector size for R in the functions that generate fbc and AA.
        if(length(f.bun) > 21)
        {
          f.bun <- resample(f.bun,21)
        } else
        {
          f.bun <- f.bun
        }
        
        #Calculate the area that can be burned in these units.
        a.bun <- mapply(function(y) 
          {round(((length(b.map[b.map %in% y & s.map %in% ss2])) * 
                          rbeta(1,shape1[3],shape2[3])),0)
        }, f.bun)
        
        if(length(f.bun) == 1)
        {
          f.bun <- f.bun
          a.bun <- a.bun
        } else
        {
        #Show a list with all combinations of units.
        fbc <- list()
        for(i in 1:length(f.bun))
        {
        fbc[[i]] <- combinations(n = length(f.bun),r = i, v = f.bun)
        }
        
        #Turn list above into a matrix with zeros for spaces without combinations.
        AA <- matrix(data = 0, 1,length(fbc))
        for(i in 1:length(fbc))
        {
          AA <- rbind(AA, 
                      matrix(data = c(as.vector(fbc[[i]]), 
                                      rep(0, 
                                          ((length(fbc[[i]][,1]) * length(f.bun)) - length(fbc[[i]])))), 
                                 length(fbc[[i]][,1]), length(f.bun)))
        }
        AA <- AA[-1,]
        
        #Calculate the total area that can be burned for each combination of management units.
        BB <- mapply(function(y)
        {
          sum(a.bun[f.bun %in% AA[y,][AA[y,] != 0]])
        }, 1:length(AA[,1]))
        
        #Subtract the remaining area to be burned from area that can be burned in each combination.
        CC <- (BB - (desa - dema))
        
        #Which of the areas is closest to 0, but not less, i.e. which combination of management
        #units most closely match the remaining fire area.
        if(length(CC[CC >= 0]) == 0)
        {
          s.bunA <- length(AA[,1])
        } else
          {
            s.bunA <- which(CC == min(CC[CC >= 0]))
          }
        
        #If there is more than one combination that matches then pick one.
        s.bunB <- ifelse(length(s.bunA > 1), s.bunA[1], s.bunA)
  
        #Remove zero values
        AAbun <- AA[s.bunB,][AA[s.bunB,] != 0]
        
        #reset the a.bun and f.bun objects
        a.bun <- sum(a.bun[f.bun %in% AAbun])
        f.bun <- AAbun  
        }
        
        #Determine ignition points in the new block and burn units
        scd <- vector(mode = "numeric", length = 0)
        scd <- resample(l.map[b.map %in% f.bun & s.map %in% ss2], 
                        round(max((a.bun * seed.cells[3]),1),0), replace = T)
        scd <- unique(scd)
        
        #Establish disturbance[e] in s.map and record old stand number
        tesn <- c(tesn, neef)
  
      } #9.6.2 ---------------------------------------------------------------------------
  
                } else #9.5.1---------------------------------------------------------------
  { #9.5.2---------------------------------------------------------------
    desa <- 1
  } #9.5.2---------------------------------------------------------------
  
    ####################################################################################
    #Updated spread contrast based on fire area and location of burn.
    #If the fire is large and located in the buffer zone or non-burn unit, then
    #spread.probability among fuels approaches unity (i.e., the fuels are very
    #dry and the fire will spread in most fuel types).
    #This object scales the spread probabilities for all fuelbeds based on the severity 
    #of the fire year.
    
    if(any(f.bun %in% c(Unmanaged.Unit, Buffer.Unit)))
    {
      dc <- dist.curve[nearest(area.dist,min(tda[e], Truncate.Area[1]/MapRes),outside=T)]
      
      sf <- scale.factor[nearest(area.dist,min(tda[e], Truncate.Area[1]/MapRes), outside=T)]
      
      sp <- rep(dc, length(f.probability$spread_contrast)) + f.probability$spread_contrast * sf
      
      s.profile <- ifelse(f.probability[,2] < 0.1,f.probability[,2],sp)
    } else
      {
        ad <- which(area.dist == resample(area.dist,1))
        
        dc <- scale.factor[ad]
        
        sf <- dist.curve[ad]
        
        sp <- rep(dc, length(f.probability$spread_contrast)) + f.probability$spread_contrast * sf
        
        s.profile <- ifelse(f.probability[,2] < 0.1,f.probability[,2],sp)
    }
    
    if((desa-dema) < round((a.bun/3),0))
    {#9.7.1 (WILDFIRE LOOP)-------------------------------------------------------------- 
  
      #These objects record data for each mapping iteration.
      
      if(spread.type == 11)
      {
        desa <- save.desa
        tesn_cum <- neef
        } else
          {
            tesn <- tesn
          }
      
      #Profile of windspeeds
      #I am using the weibull distribution because it produces a left-skewed
      #distribution with high frequency low-mean wind speeds and a long tail of
      #high speeds. I feel that this most closely matches the wind
      #profile during wildfires (which is likely to be different than
      #the average wind profile)
      #Profile of windspeeds
      fireFraction <- round(min(tda[e],(windThresholdSize/MapRes))/(windThresholdSize/MapRes),3)
      #Scale parameter represents the average windspeed. The mean value
      #will be slightly less than this (range 1-6; anything below or above
      #these values may produce an undesirable distribution of windpeeds, 
      #most notably very long tails)
      scale <- max(1,min(6,rnorm(1,assoc.wsp[fire.ratio == fireFraction],
                                 (assoc.wsp[fire.ratio == fireFraction]/10))))
      #Shape parameter is the shape of the distribution
      #Designed to produce short tails, yet shift the skew from
      #left to right as windspeed increases, yet distribution
      #will is always left skewed (i.e. more low values)
      shape <- ((scale/10)+1.2)
      
      maxWindSpd <- round(max(rweibull(10000,shape,scale)),0)
      ws <- round(rweibull(1000,shape,scale),0)
      ws[ws > maxWindSpd] <- maxWindSpd
      
                #Corresponding wind duration
                #This is used to temporally correlate wind speeds to they are consistent
                #between iterations. The is a linear deline in values as wind
                #speed increases. A wind speed produced above will iterate based on the number
                #values produced here.
                wd <- log(exp(max(ws))/exp(ws))#creates a egative linear relationship
                wd <- round(wd,0)
                wd[wd < 1] <- 1
                
                #These are tracking mechanisms that will govern changes in windspeed.
                #Tracks wind duration.
                wp1 <- vector(length = length(ws), mode = 'numeric')
                
                #Tracks wind speed
                wp2 <- 1
                
                #Select a starting wind direction.
                vec <- resample(windDirs, 1, windProbs, replace = T)
                
                #Set windspeed to zero, this is for tracking purposes and will
                #be reset for the first iteration of the spread algorithms.
                windSpd <- 0
     
                #LOOP 10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10
                #Loop 10 (by iterations). This loop keeps trying to grow disturbance[e] until 
                #growth stops.
                for (g in 1:r.max)#g <- 1
                { #10.0.0 --------------------------------------------------------------------------
                  
                  #Area mapped by iteration[f].
                  #dema <- length(as.vector(s.map[s.map %in% loopF.NewStand]))
                  dema <- sum(loopF.Area)
  
                  #This statement stops loop 7 when disturbance[e] has been fully mapped.
                  if((dema + length(ocod)) < desa) 
                  { #10.1.1 --------------------------------------------------------------------------
                    #For tracking purposes, log the windspeed in the previous iteration
                    #Will be 
                    prevWS <- windSpd
                    
                    #Set windspeed. The wp2 object advances one when the number of 
                    #iterations for the current wind speed has been reached.
                    windSpd = ws[wp2]
                    
                    #Records how length of wind duration specified in wd object.
                    wp1[wp2] <- wp1[wp2] + 1
                    
                    #Adds one to object tracking which windspeed to use.
                    #When a wind speed reaches its duration specified in wd it advances to
                    #the next wind speed.
                    wp2 <- wp2 + floor(wp1[wp2]/wd[wp2])
                    
                    #Produces a new wind direction when wind speed decreases, otherwise
                    #wind direction is maintained.
                    vec <- ifelse(length(resample(windDirs, min(1,max(prevWS-windSpd,0)), 
                                                  windProbs, replace = T)) == 0, 
                                  vec, 
                                  resample(windDirs, min(1,max(prevWS-windSpd,0)), 
                                           windProbs, replace = T))
                    
                    #Adjust location of wind probabilities for...
                    #the first concentric ring of pixels around burning pixel
                    ws1 <- wind.set1[(9 - vec):(16 - vec)]
                    #the second concentric ring of pixels around burning pixel
                    ws2 <- wind.set2[(17 - vec*2):(32 - vec*2)]
                    #the third concentric ring of pixels around burning pixel
                    ws3 <- wind.set3[(25 - vec*3):(48 - vec*3)]
                    
                    #Reconfigure probabilities based adjusted locations for....
                    #the first concentric ring
                    wind_1b <- mapply(function(y) wind_1a[y], ws1)
                    #the second concentric ring
                    wind_2b <- mapply(function(y) wind_2a[y], ws2)
                    #the third concentric ring
                    wind_3b <- mapply(function(y) wind_3a[y], ws3)
                    
                    #Combine wind probabilities back into a single vector.
                    wind.coefficient <- c(wind_1b, wind_2b, wind_3b)
                    
                    #Increases forward momentum of fire as wind speed increases.
                    wind <- wind.coefficient^(((windSpd)*(1/maxWindSpd))^2)
                    
                    #Measures tesn that are still eligible for buring
                    #tesns in loop 10 have values of -1 to number of loops * -1
                    if(any(c(11,12) == spread.type))
                    {
                      if(g == 1)
                      {
                        tesn <- tesn_cum
                      } else
                      {
                        if(g <= burn.out)
                        {
                          tesn <- c(tesn_cum, 
                                    sort(unique(as.vector(s.map[s.map < min(0,((g*-1)+burn.out)) & s.map > NoData.Unit]))))
                        } else
                        {
                          tesn <- sort(unique(as.vector(s.map[s.map < min(0,((g*-1)+burn.out)) & s.map > NoData.Unit])))
                        }
                      }
                    } else
                    {
                      tesn <- sort(unique(as.vector(s.map[s.map < min(0,((g*-1)+burn.out)) & s.map > NoData.Unit])))
                    }
                    
                    #If spread type is 12, set to zero of this loop will cut out on the first iteration
                    spread.type <- ifelse(spread.type == 12, 0, spread.type)
      
                      if(length(tesn) > 0)
                    {#10.2.1 --------------------------------------------------------------------------
                     
                        #Set row number for spread tables
                        tsl <- length(s.map[s.map %in% tesn])
                        
                        #Object shows locations of 48 pixels surrounding each mapped pixel for 
                        #disturbance[e].
                        co.1 <- find_neighbors(find_actively_burning_cells_in_smap(), radius = 3)
                        co.2 <- matrix(data = co.1, nrow = tsl, ncol = 48)
                        co.3 <- co.2[,translate_to_matrix]
                        
                        #Applies
                        pr.1 <- matrix(data = rep(distance.coefficient^((max(0,(maxWindSpd-windSpd))*(1/maxWindSpd))^2),
                                                  tsl), nrow = tsl, ncol =dcl, 
                                       byrow = T)
                        
                        #Applies wind direction to each row of neighbor cell matrix. I.e. creates
                        #matrix of probabilities that will correspond with head, flanking, and 
                        #backing fires.
                        pr.2 <- round(sweep(pr.1, MARGIN = 2, wind, '*')/(max(pr.1 * wind)),3)               
                        
                        
                        #Replace locations currently burning with a zero, they no longer count and can't be
                        #eligible because they will likely have the highest burn probability.           
                        co.3[co.3 %in% l.map[!s.map %in% loopA.snO]] <-  0                
                        pr.3 <- summarize(as.vector(pr.2),as.vector(co.3),sum)
                        pr.3 <- pr.3[!pr.3[,1] == 0,] 
  
                    } else #10.2.1 --------------------------------------------------------------------------
  {#10.2.2 --------------------------------------------------------------------------
    spread.type <- 0# necessary because this loop isn't producing new burnable area and you
    #need to relocate the fire.
  
  breaks <- 1022
   break
  }#10.2.2 --------------------------------------------------------------------------
  #Ends loop if there are no more locations available for disturbance[e].
  if(all(pr.3[,2] == 0))
    
  {#10.3.1 --------------------------------------------------------------------------
   #Reset tesn from zero to -1
   tesn <- -1
   #If spread type is 11, set to 0.
   spread.type <- ifelse(spread.type == 11, 0, spread.type)
   breaks <- 1031
   break
  } else #10.3.1 ---------------------------------------------------------------------
  { #10.3.2 --------------------------------------------------------------------------
  
    pr.4 <- as.vector(pr.3[,2])/max(pr.3[,2])
    
    #Reset new.cells object.
    new.cells <- vector(length=0, mode = "numeric")
  
    #This expression picks out which location values are of the same stand and are 
    #available (i.e. they are not occupied by the another disturbance) and makes sure 
    #that the mapped regime does not exceed its prescribed area.
    fual <- s.profile[match(f.map[l.map %in% pr.3[,1]], f.probability[,1])]
  
    #Show unadjusted probability for each cell
    sProb <- f.probability[,2][match(f.map[l.map %in% pr.3[,1]], f.probability[,1])]
    
    #Multiply wind probability of spread by fuel-based probability of spread
    pr.5 <- pr.4 * fual
    
    #Calculate the probability of crown fire. This is a somewhat arbitary application
    #of probability to my understanding of fire severity. The higher the difference
    #between the fuel and weather driven affects on fire spread, the higher than
    #chance of crown fire. This is mediated by the location along the fire flank
    #The larger the difference between unadjusted pronability of ignition and wind-adjusted
    #probability of ignition, the lower the chance of crown fire (i.e. fires on the
    #flank and back will have lower probability of crown fire than the same fuels at
    #the head of the fire)
    crownProb <- (fual - sProb) - (fual - pr.5)
    
    #If prob is less than zero, change to zero
    crownProb[crownProb < 0] <- 0
    
    #Ends loop if there are no more locations available for disturbance[e].
    if(all(pr.5 == 0))
      {#10.4.1 --------------------------------------------------------------------------
       #Reset tesn from zero to -1
       tesn <- -1
       spread.type <- ifelse(spread.type == 11, 0, spread.type)#if spread type is 11, set to 0.
       breaks <- 1041
       break
       } else #10.4.1 ---------------------------------------------------------------------
    { #10.4.2 --------------------------------------------------------------------------
      #Rescale probability of spread to 1, this makes model run more efficiently without
      #sacrificing differences in fuel/wind field properties.
      pr.6 <- pr.5/max(pr.5)
      #Produces a list with locations selected for each fuelbed.
      initial.new.cells <- unlist(mapply(function(y){
        pr.3[y,1][resample(c(0,1), size = 1, replace = T, prob = c(1-pr.6[y], pr.6[y])) == 1]
      },1:length(pr.3[,1])))
      
      #Scale back the number of new cells if it will exceed area to be burned.
      if((dema + length(ocod) + length(new.cells)) <= desa)
      {
        new.cells <- initial.new.cells
      } else
      {
        #If the number of new cells + dema exceeds dema then reduce the number of new cells.
        new.cells <- resample(initial.new.cells, (desa-(dema + length(ocod))))
      }
      
      osnd <- c(osnd, s.map[new.cells]) #tracks stand numbers involved in disturbance.
      ocod <- c(ocod, new.cells) #tracks coordinates involved in disturbance.
      s.map[new.cells] <- ((g*-1)-1) #maps disturbance.
      
      #Determines cells that experienced crown fire
      binom.response_crownFire <- rbinom(length(pr.3[,1]),1,crownProb)
      binom.response_crownFire_for.new.cells <- binom.response_crownFire[match(new.cells, pr.3[,1])]
      
      #Remove cells that were not selected for burning
      ocod_crown <- c(ocod_crown, new.cells[binom.response_crownFire_for.new.cells == 1])
      
      if(spread.type == 11 & g >= 4 & length(unique(b.map[l.map %in% ocod])) > length(burned.units))
        {
        spread.type <- 12
        tesn_cum <- c(tesn_cum,((g*-1)-1))#update values representing pixels burned in this fire.
      } else
      {
          tesn_cum <- c(tesn_cum,((g*-1)-1))#update values representing pixels burned in this fire.
        }
    } #10.4.2 --------------------------------------------------------------------------
} #10.3.2 --------------------------------------------------------------------------
                } else #10.1.1 ---------------------------------------------------------------------
{ #10.1.2 -------------------------------------------------------------------------- 
  breaks <- 1012
  break
} #10.1.2 --------------------------------------------------------------------------
                if(spread.type == 12)
                {#10.5.1
                  breaks <- 1051
                  break
                } else#10.5.1
                {#10.5.2
                  breaks <- 1052
                }#10.5.2
              } #10.0.0 --------------------------------------------------------------------------
} else #9.7.1 (WILDFIRE LOOP)--------------------------------------------------------------
{#9.7.2 (RX FIRE LOOP)--------------------------------------------------------------

 #Establish disturbance[e] in s.map and record old stand number
 if(spread.type == 12)
   {
   osnd <- c(osnd,s.map[scd])
   ocod <- c(ocod, scd)
   s.map[scd] <- s.map[scd] * tesn_t
   tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
   } else
       {
         if(spread.type == 0)
           {
           if(length(scd) == 1)
             {
             osnd <- osnd
             ocod <- ocod
             s.map[scd[1]] <- scd.tesn
             s.map[scd] <- s.map[scd] * tesn_t
             tesn <- c(tesn, sort(unique(s.map[scd])))
             } else
               {
                 osnd <- c(osnd,s.map[scd[2:length(scd)]])
                 ocod <- c(ocod, scd[2:length(scd)])
                 s.map[scd[1]] <- scd.tesn
                 s.map[scd] <- s.map[scd] * tesn_t
                 tesn <- c(tesn, sort(unique(s.map[scd])))
               }
           } else
             {
               if(length(scd) == 1)
                 {
                 osnd <- osnd
                 ocod <- ocod
                 s.map[scd] <- s.map[scd] * tesn_t
                 } else
                   {
                     osnd <- c(osnd,s.map[scd[2:length(scd)]])
                     ocod <- c(ocod, scd[2:length(scd)])
                     s.map[scd] <- s.map[scd] * tesn_t
                   }
             }
       }
   
  #LOOP 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 
   #Loop 11 (by iterations). This loop keeps growing fire[e] in block[f] 
   #until growth stops.
   for (h in 1:r.max)#h <- 2
   { #11.0.0 ---------------------------------------------------------------------------
        
       #Area mapped for fire[e].
       dema <- length(as.vector(s.map[s.map %in% loopF.NewStand]))
  
     #This statement stops loop 11 when treatment[b] has been fully mapped.
     if((dema + length(ocod)) < desa) 
     { #11.1.1 ---------------------------------------------------------------------------
       sdlo <- find_neighbors(find_actively_burning_cells_in_smap(), radius = 1)
         
       #This object shows all unique locations available for establishment by treatment[b].
       avlo <- unique(as.vector(l.map[sdlo][l.map[sdlo] %in% l.map[b.map %in% f.bun & s.map %in% ss2]]))
         
       #Ends loop if there are no more locations available for treatment[b] in the 
       #block[cc] that is currently being mapped.
       
       if(length(avlo) > 0)
       { #11.2.1 ---------------------------------------------------------------------------
  
         #Reset new.cells object.
         inital.new.cells <- vector(length=0, mode = "numeric")
         new.cells <- vector(length=0, mode = "numeric")
                   
           #This expression picks out which location values are of the same stand and are 
           #available (i.e. they are not occupied by the another treatment) and makes sure 
           #that the mapped regime does not exceed its prescribed area.
           if((length(ocod) + length(avlo)) <= a.bun)#--------------------------A
             {#-----------------------------------------------------------------A-TRUE
             if((dema + length(ocod) + length(avlo)) <= desa)#------------------B
               {#---------------------------------------------------------------B-TRUE
               initial.new.cells <- avlo
               fual <- f.map[match(initial.new.cells, l.map)]
               ss.n <- length(fual)
               ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
               new.cells <- initial.new.cells[ss == 1]
               s.map[new.cells] <- s.map[new.cells]*tesn_t
               tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
               fual10 <- s.profile[match(f.map[l.map %in% new.cells], f.probability[,1])]
               sProb <- f.probability[,2][match(f.map[l.map %in% new.cells], f.probability[,1])]
               crownProb <- (fual10 - sProb)
               crownProb[crownProb < 0] <- 0
               initial.cells.with.crown.fire <- rbinom(length(new.cells),1,crownProb)
               } else#----------------------------------------------------------B-TRUE
                 {#-------------------------------------------------------------B-FALSE
                   initial.new.cells <- resample(avlo,(desa - (dema + length(ocod))))
                   fual <- f.map[match(initial.new.cells, l.map)]
                   ss.n <- length(fual)
                   ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
                   new.cells <- initial.new.cells[ss == 1]
                   s.map[new.cells] <- s.map[new.cells]*tesn_t
                   tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                   fual10 <- s.profile[match(f.map[l.map %in% new.cells], f.probability[,1])]
                   sProb <- f.probability[,2][match(f.map[l.map %in% new.cells], f.probability[,1])]
                   crownProb <- (fual10 - sProb)
                   crownProb[crownProb < 0] <- 0
                   initial.cells.with.crown.fire <- rbinom(length(new.cells),1,crownProb)
                   }#-----------------------------------------------------------B-FALSE
             } else #-----------------------------------------------------------A-TRUE
               {#---------------------------------------------------------------A-FALSE
                 if((dema + length(ocod) + length(avlo)) <= desa)#--------------C
                   {#-----------------------------------------------------------C-TRUE
                   initial.new.cells <- resample(avlo,(a.bun - length(ocod)))
                   fual <- f.map[match(initial.new.cells, l.map)]
                   ss.n <- length(fual)
                   ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
                   new.cells <- initial.new.cells[ss == 1]
                   s.map[new.cells] <- s.map[new.cells]*tesn_t
                   tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                   fual10 <- s.profile[match(f.map[l.map %in% new.cells], f.probability[,1])]
                   sProb <- f.probability[,2][match(f.map[l.map %in% new.cells], f.probability[,1])]
                   crownProb <- (fual10 - sProb)
                   crownProb[crownProb < 0] <- 0
                   initial.cells.with.crown.fire <- rbinom(length(new.cells),1,crownProb)
                   spread.type <- 11
                   } else#------------------------------------------------------C-TRUE
                     {#---------------------------------------------------------C-FALSE
                       if(a.bun < desa)#----------------------------------------D
                         {#-----------------------------------------------------D-TRUE
                         initial.new.cells <- resample(avlo,(a.bun - length(ocod)))
                         fual <- f.map[match(initial.new.cells, l.map)]
                         ss.n <- length(fual)
                         ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
                         new.cells <- initial.new.cells[ss == 1]
                         s.map[new.cells] <- s.map[new.cells]*tesn_t
                         tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                         fual10 <- s.profile[match(f.map[l.map %in% new.cells], f.probability[,1])]
                         sProb <- f.probability[,2][match(f.map[l.map %in% new.cells], f.probability[,1])]
                         crownProb <- (fual10 - sProb)
                         crownProb[crownProb < 0] <- 0
                         initial.cells.with.crown.fire <- rbinom(length(new.cells),1,crownProb)
                         spread.type <- 11
                         } else#------------------------------------------------D-TRUE
                           {#---------------------------------------------------D-FALSE
                             initial.new.cells <- resample(avlo,(desa - (dema + length(ocod))))
                             fual <- f.map[match(initial.new.cells, l.map)]
                             ss.n <- length(fual)
                             ss <- rbinom(ss.n, 1,  f.probability[,2][match(fual, f.probability[,1])])
                             new.cells <- initial.new.cells[ss == 1]
                             s.map[new.cells] <- s.map[new.cells]*tesn_t
                             tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                             fual10 <- s.profile[match(f.map[l.map %in% new.cells], f.probability[,1])]
                             sProb <- f.probability[,2][match(f.map[l.map %in% new.cells], f.probability[,1])]
                             crownProb <- (fual10 - sProb)
                             crownProb[crownProb < 0] <- 0
                             initial.cells.with.crown.fire <- rbinom(length(new.cells),1,crownProb)
                             }#-------------------------------------------------D-FALSE
                       }#-------------------------------------------------------C-FALSE
                 }#-------------------------------------------------------------A-FALSE
  
           osnd <- c(osnd, s.map[new.cells]) #tracks stand numbers involved in disturbance.
           ocod <- c(ocod, new.cells) #tracks coordinates involved in disturbance.
           ocod_crown <- c(ocod_crown, new.cells[initial.cells.with.crown.fire == 1])
           
       } else #11.2.1 ----------------------------------------------------------------------
       
  {#11.2.2
  #Fire has burned out and must be reassigned to a new area. Use spread.type = 0 to
  #direct loop 9 into section that will locate anew scd
    spread.type <- 0
  
    breaks <- 1122
    break
  } #11.2.2 ---------------------------------------------------------------------------
  
     } else #11.1.1 ----------------------------------------------------------------------
  
  { #11.1.2 ---------------------------------------------------------------------------
   breaks <- 1112
    break
  } #11.1.2 ---------------------------------------------------------------------------
  
  if(spread.type == 11)
  {#11.3.1
    #Save units that where burned so far.
    burned.units <- c(burned.units, f.bun)
  
    breaks < - 1131
    break
  } else #11.3.1
  {#11.3.2
  breaks <- 1132
  }#11.3.2
   } #11.0.0 ---------------------------------------------------------------------------
  }#9.7.2 (RX FIRE LOOP)--------------------------------------------------------------

  #Reset tesn, it should be -1 for FDM, except in loop 10 where it must include multiple
  #values to support the burn out function.
  tesn <- -1#temporary stand number.
  
  osnd <- abs(osnd)
  
  #Find affected stands that affected by crown fire.
  pos_crown <- which(is.na(match(ocod, ocod_crown)) == F)
  osnd_crown <- osnd[pos_crown]
  
  #Pare new stand and corresponding location objects to remove stands burned in crown fire.
  if(length(osnd_crown) == 0)
  {
    osnd_surface <- osnd
    ocod_surface <- ocod
  } else
  {
    osnd_surface <- osnd[-pos_crown]
    ocod_surface <- ocod[-pos_crown]
  }
  
  neef_surface <- vector()
  neef_crown <- vector()
  
  #SURFACE FIRE>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(length(osnd_surface) > 0)
  {
    #Show fuelbeds associated with stands affected by fire
    fb_f12_surface <- Fuelbed.List[Stand.List %in% osnd_surface]
    
    #List corresponding stand numbers.
    osno_surface <- Stand.List[Stand.List %in% osnd_surface]
    
    #Expand fuelbeds to include each occurence of stand number in osnd
    effb_surface <- fb_f12_surface[match(osnd_surface, osno_surface)]
    
    #Calculate number of new stands
    noef_surface <- length(osno_surface)
    
    #Determine new stand numbers for treatment[b], block[cc].
    if(noef_surface == 0)
    {
      neef_surface <- 0
    } else
    {
      neef_surface <- seq((mudn + 1), (mudn + noef_surface), 1)
    }
    
    #Map new stands
    ods <- data.frame(ocod_surface, osnd_surface)
    ods <- ods[order(ods$ocod_surface),]
    v.neef_surface <- neef_surface[match(ods$osnd_surface, osno_surface)]
    s.map[l.map %in% ods$ocod_surface] <- v.neef_surface
    
    #
    osno_all <- osno_surface
  } else
  {
    osno_surface <- vector()
    osno_all <- osno_surface
    v.neef_surface <- vector()
  }
  
  #CROWN FIRE>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(length(osnd_crown) > 0)
  {
    #Show fuelbeds associated with stands affected by fire
    fb_f12_crown <- Fuelbed.List[Stand.List %in% osnd_crown]
    
    #List corresponding stand numbers.
    osno_crown <- Stand.List[Stand.List %in% osnd_crown]
    
    #Expand fuelbeds to include each occurence of stand number in osnd
    effb_crown <- fb_f12_crown[match(osnd_crown, osno_crown)]
    
    #Crown Fire
    #Calculate number of new stands
    noef_crown <- length(osno_crown)
    
    #Determine new stand numbers for treatment[b], block[cc].
    if(noef_crown == 0)
    {
      neef_crown <- 0
    } else
    {
      neef_crown <- seq(ifelse(length(neef_surface) == 0, mudn, max(neef_surface) + 1), 
                        (ifelse(length(neef_surface) == 0, mudn + noef_crown, max(neef_surface) + noef_crown)), 1)
    }
    
    #Map new stands
    odc <- data.frame(ocod_crown, osnd_crown)
    odc <- odc[order(odc$ocod_crown),]
    v.neef_crown <- neef_crown[match(odc$osnd_crown, osno_crown)]
    s.map[l.map %in% odc$ocod_crown] <- v.neef_crown
    
    #
    osno_all <- c(osno_all, osno_crown)
  } else
  {
    osno_crown <- vector()
    osno_all <- c(osno_all, osno_crown)
    v.neef_crown <- vector()
  }
  
  if(sum(neef_surface,neef_crown) == 0)
  {
    loopF.NewStand <- loopF.NewStand
    loopF.Area <- loopF.Area
  } else
  {
    loopF.NewStand <- c(loopF.NewStand,neef_surface, neef_crown)
    l.neef <- rep(1,length(v.neef_surface) + length(v.neef_crown))
    s.neef_a <- summarize(l.neef, c(v.neef_surface, v.neef_crown), sum)
    s.neef <- as.vector(s.neef_a[,2])
    loopF.Area <- c(loopF.Area, s.neef)
  }
  
  loopF.ReplacedStand <- c(loopF.ReplacedStand, osno_all)
  loopF.E_no <- c(loopF.E_no, rep(e, length(osno_all)))
  loopF.F_no <- c(loopF.F_no, rep(f, length(osno_all)))
  loopF.G_no <- c(loopF.G_no, rep(g, length(osno_all)))
  loopF.fireType <- c(loopF.fireType, 
                      c(rep(1, length(osno_surface)), rep(2, length(osno_crown))))
  
    g.g <- sum(g.g, ifelse(g == 0,h,g))#tracks expansions
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                 #
  #DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                #
  
  if(length(unique(loopF.NewStand)) != length(loopF.NewStand) | 
       length(loopF.ReplacedStand) != length(loopF.NewStand) | 
     length(v.neef_surface) + length(v.neef_crown) != length(s.map[l.map %in% c(ocod_surface,ocod_crown)]))
  {  
    r101 <- 2
    break
  } else
  {
    r101 <- ifelse(any(s.map < 0 & s.map > -9999),22,0) 
  } 
  
  #TEMPOARY -- FORCES FDM TO CRASH IF -1 IS ASSSIGNED TO S.MAP
  if(length(s.map[s.map < 0 & s.map > -9999]) > 0)
  {
    aaa <- data.frame(B = loopB.new_stand, F = loopF.NewStand)
  } else
  {
    f <- f
  }
  #TEMPOARY -- FORCES FDM TO CRASH IF -1 IS ASSSIGNED TO S.MAP
  
              } else #9.4.1 ----------------------------------------------------------------------
  { #9.4.2 ---------------------------------------------------------------------------
  
  } #9.4.2 ---------------------------------------------------------------------------
  
              } else #9.3.1 ----------------------------------------------------------------------
  { #9.3.2 ---------------------------------------------------------------------------
  
    #I DON'T THINK THE CODE IN THIS SECTION IS NECESSARY ANYMORE. NO SCENRAIO
    #WHERE UNASSIGNED CELLS WOULD END UP HERE BECAUSE CELL ASSIGNMENTS ARE MADE IN 9.3.1
    #TEST TO FIND A SITUATION WHERE 9.3.2 IS HIT AND RE-RUN MODEL TO STOP JUST BEFORE
    #SO YOU CAN TEST FOR UNASSIGNED CELLS
    r101 <- 8
    
    #Reset tesn, it should be -1 for FDM, except in loop 10 where it must include multiple
      #values to support the burn out function.
      #tesn <- -1#temporary stand number.
      
      #osnd <- abs(osnd)
      
      #Show fuelbeds associated with stands affected by fire
      #fb_f12 <- Fuelbed.List[Stand.List %in% osnd]
      
      #List corresponding stand numbers.
      #osno <- Stand.List[Stand.List %in% osnd]
      
      #Expand fuelbeds to include each occurence of stand number in osnd
      #effb <- fb_f12[match(osnd, osno)]
  
      #Unique old stands
      #osno <- sort(unique(osnd))
      
      #Calculate number of new stands
      #noef <- length(osno)
      
      #Determine new stand numbers for treatment[b], block[cc].
      #if(noef == 0)
      #{
      #  neef <- 0
      #} else
      #{
      #  neef <- seq((mudn + 1), (mudn + noef), 1)
      #}
    
      #Map new stands
      #od <- data.frame(osnd = osnd, ocod = ocod)
      #od <- od[order(od$ocod),]
      #v.neef <- neef[match(od.b$osnd, osno)]
      #s.map[s.map %in% osno] <- v.neef
  
      #Log new stand numbers and associated disturbances when they have been added to 
      #s.map.
      #if(sum(neef) == 0)
      #{
      #  loopF.NewStand <- loopF.NewStand
      #  loopF.Area <- loopF.Area
      #  } else
      #    {
      #      loopF.NewStand <- c(loopF.NewStand,neef)
      #      l.neef <- rep(1,length(v.neef))
      #      s.neef_a <- summarize(l.neef, v.neef, sum)
      #      s.neef <- as.vector(s.neef_a[,2])
      #      loopF.Area <- c(loopF.Area, s.neef)
      #    }
      
      #loopF.ReplacedStand <- c(loopF.ReplacedStand, osno)
      #loopF.E_no <- c(loopF.E_no, rep(e, length(osno)))
      #loopF.F_no <- c(loopF.F_no, rep(f, length(osno)))
      #loopF.G_no <- c(loopF.G_no, rep(g, length(osno)))
      
      #g.g <- sum(g.g, ifelse(g == 0,h,g))#tracks expansions
  
    ##############################################################################
    ##############################################################################
    ##############################################################################
    #ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                 #
    #DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                #
    
    #if(length(unique(loopF.NewStand)) != length(loopF.NewStand) | 
    #     length(loopF.ReplacedStand) != length(loopF.NewStand))
    #{  
    #  r101 <- 8
    #} else
    #{
    #  r101 <- ifelse(any(s.map < 0 & s.map > -9999),88,0) 
    #} 
    
    #TEMPOARY -- FORCES FDM TO CRASH IF -1 IS ASSSIGNED TO S.MAP
    #if(length(s.map[s.map < 0 & s.map > -9999]) > 0)
    #{
    #  aaa <- data.frame(B = loopB.new_stand, F = loopF.NewStand)
    #} else
    #{
    #  f <- f
    #}
    #TEMPOARY -- FORCES FDM TO CRASH IF -1 IS ASSSIGNED TO S.MAP
    
    
    break
  } #9.3.2 ---------------------------------------------------------------------------
            } else #9.1.1 ----------------------------------------------------------------------
  { #9.1.2 ---------------------------------------------------------------------------
    break
  } #9.1.2 ---------------------------------------------------------------------------
  
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
          } #9.0.0 ---------------------------------------------------------------------------
  #Log new stand numbers and associated disturbances when they have been added to 
  #s.map.
  loopE.NewStand <- c(loopE.NewStand,loopF.NewStand)
  loopE.Area <- c(loopE.Area,loopF.Area)
  loopE.ReplacedStand <- c(loopE.ReplacedStand,loopF.ReplacedStand)
  loopE.E_no <- c(loopE.E_no, loopF.E_no)
  loopE.F_no <- c(loopE.F_no, loopF.F_no)
  loopE.G_no <- c(loopE.G_no, loopF.G_no)
  loopE.fireType <- c(loopE.fireType, loopF.fireType)
  loopE <- data.frame(NewStand = loopE.NewStand, 
                      ReplacedStand = loopE.ReplacedStand, 
                      Area = loopE.Area, 
                      Fire = loopE.fireType,
                      E_no = loopE.E_no, 
                      F_no = loopE.F_no,
                      G_no = loopE.G_no)
  loopE <- loopE[order(loopE$ReplacedStand),]
        } else #8.1.1 ----------------------------------------------------------------------
  { #8.1.2 ---------------------------------------------------------------------------
    #Log new stand numbers and associated disturbance when they have been added to 
    #s.map.
    loopE.NewStand <- c(loopE.NewStand,loopF.NewStand)
    loopE.Area <- c(loopE.Area,loopF.Area)
    loopE.ReplacedStand <- c(loopE.ReplacedStand,loopF.ReplacedStand)
    loopE.E_no <- c(loopE.E_no, loopF.E_no)
    loopE.F_no <- c(loopE.F_no, loopF.F_no)
    loopE.G_no <- c(loopE.G_no, loopF.G_no)
    loopE.fireType <- c(loopE.fireType, loopF.fireType)
    loopE <- data.frame(NewStand = loopE.NewStand, 
                        ReplacedStand = loopE.ReplacedStand, 
                        Area = loopE.Area, 
                        Fire = loopE.fireType, 
                        E_no = loopE.E_no, 
                        F_no = loopE.F_no,
                        G_no = loopE.G_no)
    loopE <- loopE[order(loopE$ReplacedStand),]
  
  } #8.1.2 ---------------------------------------------------------------------------
  
    #Record date and time.
    dt <- Sys.Date()
    tm <- format(Sys.time(), format = "%H.%M.%S", 
                 tz = "", usetz = FALSE)
    
  #Tracking device
  d.summary <- paste(
    " Date: ", dt, 
    " Time: ", tm, 
    " Year: ", a, 
    " PercentComplete_ForYear: ", round(((which(tdn[tdy == a] == e)/
                                            length(tdn[tdy == a]))*100),0), 
    " Disturbance_No: ", e, 
    " Name: ", f.disturbances$DisturbanceTitle[tdc[e]],
    "MgmtOp: ", "N/A", 
    "BurnBlock: ", "N/A", 
    " BurnedArea_Expected: ", desa, 
    " BurnedArea_Actual: ", sum(loopE.Area), 
    "UnBurned_Area: ", desa-sum(loopE.Area), 
    " Blocks: ", f, 
    " Expansions: ", g.g, 
    "HiStandNo: ", max(c(neef_surface, neef_crown)))
  
  #Save run data.
  cat(d.summary, file = paste(output_path, "run_", run, "_disturbances.txt", sep = ""), fill = T, append = T)#
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                  #
  #DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                 #
  if(r101 > 0)                                                                 #
  {                                                                            #
    r101 <- r101
    break
  } else                                                                       #
  {                                                                            #
    r101 <- r101
  }                                                                            #
  #                                                                            #
    } #8.0.0 ------------------------------------------------------------------#
  }                                                                            #
  #
  if(r101 > 0)                                                                 #
  {                                                                            #
    r101 <- r101
    break
  } else                                                                       #
  {                                                                            #
    r101 <- r101
  }                                                                            #
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  #Post-wildfire processing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #Update files if there were disturbances in year[a].
  if(length(loopE.NewStand) > 0)
  {
    
    #Code below replaces loop, vene though there are more lines it should run way faster.
    loopE_crownFire <- loopE[loopE$Fire == 2,]
    
    #Object shows fuelbeds associated with each new stand number created by 
      #disturbances in year[a].
    FB_burned.by.crownFire_1 <- Fuelbed.List[Stand.List %in% loopE_crownFire$ReplacedStand]
    SN_burned.by.crownFire_1 <- Stand.List[Stand.List %in% loopE_crownFire$ReplacedStand]
    FB_burned.by.crownFire_2 <- FB_burned.by.crownFire_1[match(loopE_crownFire$ReplacedStand, 
                                                               SN_burned.by.crownFire_1)]
    
    #Add SN_burned.by.crownFire_2 to loopE_crownFire data frame
    loopE_crownFire <- data.frame(loopE_crownFire, AffectedFuelbed = FB_burned.by.crownFire_2)
    
    #Sort data frame by new stands
    loopE_crownFire <- loopE_crownFire[order(loopE_crownFire$NewStand),]#probably unecessary
    
    #seperate out new stands from s.map, lists occurrences of new stands from min to max coords.
    crownFire_standsXpixel <- s.map[s.map %in% loopE_crownFire$NewStand]
    
    #List of row numbers in d.post where fuelbeds need to be updated based on treatment
    burned.fuelbeds.positions <- which(d.post$fuelbed %in% loopE_crownFire$AffectedFuelbed)
    
    #Fuelbeds that may be updated corresponding with row numbers in burned.fuelbeds.positions
    fuelbeds.burned.in.crownFire <- d.post$fuelbed[d.post$fuelbed %in% FB_burned.by.crownFire_2]
    
    #row numbers in d.post corresponding with each existing fuelbed in each new stand 
    burned.fuelbed.positionsXpixel <- burned.fuelbeds.positions[match(loopE_crownFire$AffectedFuelbed, 
                                                                      fuelbeds.burned.in.crownFire)]
    
    #Activate when you can differentiate between crown fire and surface fire
    crownFire_regen <- d.post[burned.fuelbed.positionsXpixel,5]
    
    #Lists occurrences of new fuelbeds from min to max coords.
    crownFire_regenXpixel <- crownFire_regen[match(crownFire_standsXpixel, loopE_crownFire$NewStand)]
    
    #Replace old fuelbeds with new ones in f.map
    f.map[s.map %in% loopE_crownFire$NewStand] <- crownFire_regenXpixel
    
    #Re-order loopE data frame by old stands because that is the order of ages in crownFire_newAge_1.
    loopE_crownFire <- loopE_crownFire[order(loopE_crownFire$ReplacedStand),]
    
    #List ages associated with each stand that has been affected by crown fire
    #These will be used to update Age.List
    crownFire_oldAge_1 <- Age.List[Stand.List %in% loopE_crownFire$ReplacedStand]
    crownFire_oldAge_2 <- crownFire_oldAge_1[match(loopE_crownFire$ReplacedStand, SN_burned.by.crownFire_1)]
    
    #List new age for crown fire stands
    crownFire_newAge <- rep(0, length(crownFire_oldAge_2))
    
    #Add crownFire_newAge_2 to loopE data.frame
    loopE_crownFire <- data.frame(loopE_crownFire, oldAge = crownFire_oldAge_2)
    
    #Add crownFire_regen to loopE data.frame
    loopE_crownFire <- data.frame(loopE_crownFire, newFuelbed = crownFire_regen)
    
    #Add crownFire_newAge_2 to loopE data.frame
    loopE_crownFire <- data.frame(loopE_crownFire, newAge = crownFire_newAge)
    
    #Re-order loopE data frame by new stands.
    loopE_crownFire <- loopE_crownFire[order(loopE_crownFire$NewStand),]
   
    #List stands that have been altered by disturbances.
    ss2 <- loopE.ReplacedStand
    standd <- sort(unique(ss2))#there can be duplicates, this will mess up the shortcut in a9
    sb <- summarize(loopE.Area,ss2,sum)#sum areas for duplicates.
    saread <- as.vector(sb[,2])
    smud <- mapply(function(y) MU.List[Stand.List == y], loopE.ReplacedStand)
    
    #Isolate stands affected by surface fire
    loopE_surfaceFire <- loopE[loopE$Fire == 1,]
    loopE_surfaceFire <- loopE_surfaceFire[order(loopE_surfaceFire$NewStand),]#probably unecessary
    
    #Add fuelbed and age info (needed to update .List objects)
    StandNumber_surfaceFire_1 <- Stand.List[Stand.List %in% loopE_surfaceFire$ReplacedStand]
    Fuelbed_surfaceFire_1 <- Fuelbed.List[Stand.List %in% loopE_surfaceFire$ReplacedStand]
    Fuelbed_surfaceFire_2 <- Fuelbed_surfaceFire_1[match(loopE_surfaceFire$ReplacedStand, 
                                                         StandNumber_surfaceFire_1)]
    Age_surfaceFire_1 <- Age.List[Stand.List %in% loopE_surfaceFire$ReplacedStand]
    Age_surfaceFire_2 <- Age_surfaceFire_1[match(loopE_surfaceFire$ReplacedStand, 
                                                 StandNumber_surfaceFire_1)]
    
    #Create new data frame. At this point new fuelbeds and new ages are old, these are 
    #just placeholders until further down this loop when surface fire impacts are assessed
    #using mFRI.
    loopE_surfaceFire <- data.frame(loopE_surfaceFire, 
                                    AffectedFuelbed = Fuelbed_surfaceFire_2,
                                    oldAge = Age_surfaceFire_2, newFuelbed = Fuelbed_surfaceFire_2, 
                                    newAge = Age_surfaceFire_2)
    
    #Combine surface fire and crown fire tables
    loopE_allFire <- rbind(loopE_surfaceFire, loopE_crownFire)
    loopE_allFire <- loopE_allFire[order(loopE_allFire$NewStand),]
    
    #Shelve fire history for stands that have been impacted by disturbance
    new_mfri_vec <- mapply(function(y) mfri.Matrix[Stand.List == y,], loopE_allFire$ReplacedStand)
    nmvd <- t(new_mfri_vec)
    
    #Add a fire for stands that were burned in wildfires
    nmvd[,30] <- 1
  
  #Change stand properties as needed for treatments.
  #Create a data.frame so you can switch order from stand number to
  #order in which stand was added.
  ID2.List <- 1:length(Stand.List)
  TL2 <- data.frame(ID = ID2.List, Stand = Stand.List, Area = Area.List)
  #Order by stand number
  TL2 <- TL2[order(TL2$Stand),]
  #Subtract area of new stands from corresponding old stands
  TL2$Area[TL2$Stand %in% standd] <- TL2$Area[TL2$Stand %in% standd] - saread
  #Reorder by order stands were added
  TL2 <- TL2[order(TL2$ID),]
  #Replace Area.List with updated object
  Area.List <- TL2$Area
  
    #Update list to remove any stands that have been overwritten.
    Stand.List <- Stand.List[(Area.List == 0) == F]
    Fuelbed.List <- Fuelbed.List[(Area.List == 0) == F]
    Age.List <- Age.List[(Area.List == 0) == F]
    T1E.List <- T1E.List[(Area.List == 0) == F]
    T2E.List <- T2E.List[(Area.List == 0) == F]
    D1E.List <- D1E.List[(Area.List == 0) == F]
    D2E.List <- D2E.List[(Area.List == 0) == F]
    Coord.List <- Coord.List[(Area.List == 0) == F]
    MU.List <- MU.List[(Area.List == 0) == F]
    mfri.Matrix <- mfri.Matrix[(Area.List == 0) == F,]
    mfri_lower.List <- mfri_lower.List[(Area.List == 0) == F]
    mfri_upper.List <- mfri_upper.List[(Area.List == 0) == F]
    Area.List <- Area.List[(Area.List == 0) == F]
    
    #Update list to add new stands.
    Stand.List <- c(Stand.List, loopE_allFire$NewStand)
    Fuelbed.List <- c(Fuelbed.List, loopE_allFire$newFuelbed)
    Age.List <- c(Age.List, loopE_allFire$newAge)
  
    #List fuelbeds that need to be updated.
    pdaFB_a20 <- pda$pre[pda$pre %in% loopE_allFire$newFuelbed]
    
    #List corresponding updated age restrictions
    pdaTH_a20 <- pda$thin[pda$pre %in% loopE_allFire$newFuelbed]
    
    #List occurences of age restriction for each new stand
    v.THIN_a20 <- pdaTH_a20[match(loopE_allFire$newFuelbed,pdaFB_a20)]
    
    #Update
    T1E.List <- c(T1E.List,v.THIN_a20)
    
    #List corresponding updated age restrictions
    pdaHE_a20 <- pda$herb[pda$pre %in% loopE_allFire$newFuelbed]
    
    #List occurences of age restriction for each new stand
    v.HERB_a20 <- pdaHE_a20[match(loopE_allFire$newFuelbed,pdaFB_a20)]
    
    #Update
    T2E.List <- c(T2E.List, v.HERB_a20)
    
    #List corresponding updated age restrictions
    pdaSF_a20 <- pda$sfire[pda$pre %in% loopE_allFire$newFuelbed]
    
    #List occurences of age restriction for each new stand
    v.SFIRE_a20 <- pdaSF_a20[match(loopE_allFire$newFuelbed,pdaFB_a20)]
    
    #Update
    D1E.List <- c(D1E.List, v.SFIRE_a20)
    
    #List corresponding updated age restrictions
    pdaCF_a20 <- pda$cfire[pda$pre %in% loopE_allFire$newFuelbed]
    
    #List occurences of age restriction for each new stand
    v.CFIRE_a20 <- pdaCF_a20[match(loopE_allFire$newFuelbed,pdaFB_a20)]
    
    #Update
    D2E.List <- c(D2E.List, v.CFIRE_a20)
  
    #List new stand occurrences in s.map
    vs.map_a20 <- s.map[s.map %in% loopE.NewStand]
    
    #List corresponding coordinates (l.map) for new stand occurrences in s.map
    vl.map_a20 <- l.map[s.map %in% loopE.NewStand]
    
    #Use summarize function (w/ min()) to select a single coordinate value for each new stand.
    v.Coord_a20a <- summarize(vl.map_a20,vs.map_a20,min)
    #Subset coordinates
    v.Coord_a20b <- as.vector(v.Coord_a20a[,2])
    
    #Update
    Coord.List <- c(Coord.List,v.Coord_a20b)
  
    MU.List <- c(MU.List, smud)
    mfri.Matrix <- rbind(mfri.Matrix,nmvd)
    mfri.List <- apply(mfri.Matrix,1,sum)
    mfri.List <- round(30/mfri.List,0)
    mfri.List <- ifelse(mfri.List == Inf, 32, mfri.List)
    
    #Update
    mfri_lower.List <- c(mfri_lower.List, d.post$more_fire[match(loopE_allFire$newFuelbed,d.post$fuelbed)])
    
    #Select option 1 for fuelbed transition when mfri lengthens
    less1 <- d.post$less_fire1[match(loopE_allFire$newFuelbed,d.post$fuelbed)]
    
    #Select option 2 for fuelbed transition when mfri lengthens
    less2 <- d.post$less_fire1[match(loopE_allFire$newFuelbed,d.post$fuelbed)]
    
    #Randomly choose between option 1 and 2 for each fuelebd.
    less <- apply(matrix(data = c(less1,less2),length(less1), 2), 1, sample, size = 1)
    
    #Update
    mfri_upper.List <- c(mfri_upper.List, less)
    
    #Update
    Area.List <- c(Area.List,loopE.Area)
    
    #Remove stands that have been overwritten
    tslt.List <- tslt.List[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    tslt.Fuelbeds <- tslt.Fuelbeds[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    tslt.Stands <- tslt.Stands[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
    
    #Update time-since-last-treatment list and associated stand list
    tslt.List <- c(tslt.List, rep(0, length(
      loopE_allFire$NewStand[mapply(function(y) 
      {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
      , loopE_allFire$AffectedFuelbed) %in% c(2,3,4,6,7,8)])))
    
    tslt.Fuelbeds <- c(tslt.Fuelbeds, loopE_allFire$AffectedFuelbed[mapply(function(y) 
    {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
    , loopE_allFire$AffectedFuelbed) %in% c(2,3,4,6,7,8)])
    
    tslt.Stands <- c(tslt.Stands, loopE_allFire$NewStand[mapply(function(y) 
    {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
    , loopE_allFire$AffectedFuelbed) %in% c(2,3,4,6,7,8)])
    
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                 #
  #DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                #
  if(any(c(length(Stand.List),
           length(Fuelbed.List),
           length(mfri.List),
           length(MU.List),
           length(T1E.List),
           length(T2E.List),
           length(D1E.List),
           length(D2E.List),
           length(Area.List),
           length(mfri_lower.List),
           length(mfri_upper.List), 
           length(Age.List)) != (length(unique(as.vector(s.map)))-1)) == T)
  {
    r101 <- 3
    break
  } else
  {
    r101 <- ifelse(any(s.map < 0 & s.map > -9999),33,0) 
  } 
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
    #Update fuelbeds based on impacts of changing mean fire return interval.
    #Make a copy of the Fuelbed.List
    feof <- Fuelbed.List
    
    #Create a parelle list that shows lower bound of mFRI for each stand
    start <- d.post$mfri_start[match(feof,d.post$fuelbed)]
    
    #Create a parellel list that shows upper bound of mFRI for each stand
    end <- d.post$mfri_end[match(feof,d.post$fuelbed)]
    
    #Replace fuelbed with one representing a more frequent fire regime if the mFRI is shorter
    #than the lower limit for the current fuelbed.
    feof[mfri.List < start] <- mfri_lower.List[mfri.List < start]
    
    #Replace fuelbed with one representing a less frequent fire regime if the mFRI is longer
    #than the lower limit for the current fuelbed.
    feof[mfri.List > end] <- mfri_upper.List[mfri.List > end]
    
    #Update f.map
    #Improved function to update f.map based on mFRI. Former code used a for()
    s.SL <- Stand.List[Fuelbed.List != feof]
    feof2 <- feof[Fuelbed.List != feof]
    vs.map <- s.map[s.map %in% s.SL]
    v.feof2 <- feof2[match(vs.map, s.SL)]
    f.map[s.map %in% s.SL] <- v.feof2
    Fuelbed.List <- feof
  
  } else
  {
  }
  
  #Post-wildfire processing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #Update files based on time-since-last-treatment>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #Show max time-since-last-treatment before state transitions for each fuelbed
  max_tslt <- t.path$max_tslt[t.path$pre %in% tslt.Fuelbeds]
  
  #List each of the fuelbeds corresponding with tslt object above
  max_tslt_fb <- t.path$pre[t.path$pre %in% tslt.Fuelbeds]
  
  #Expand to the max tslt object for each row in tslt.List
  max_tslt_x_stand <- max_tslt[match(tslt.Fuelbeds, max_tslt_fb)]
  
  #Crash model is Fuelbed.List turned into a list()
  if(length(max_tslt_x_stand) != length(tslt.List))
  {
    r101 <- "inconcistency in tslt tracking"
    break
  } else
  {
    r101 <- r101
  }
  
  #List stand numbers for stands that will transition.
  replace_stands <- tslt.Stands[tslt.List > max_tslt_x_stand]
  
  #List fuelbeds of stands that will transition.
  replace_fbs <- tslt.Fuelbeds[tslt.List > max_tslt_x_stand]
  
  #List unique current fuelbeds for stands that will transition.
  old_fbs <- t.path$pre[t.path$pre %in% replace_fbs]
  
  #List unique new fuelbeds for stands that will transition.
  new_fbs <- t.path$post_1[t.path$pre %in% replace_fbs]
  
  #Expand new unique fuelbeds to the number of stands that will transition.
  new_fbs_x_stand <- new_fbs[match(replace_fbs, old_fbs)]
  
  #Replace current fuelbeds with new ones in cases where tslt exceeds limit for 
  #state.
  tslt.Fuelbeds[tslt.Stands %in% replace_stands] <- new_fbs_x_stand              
  
  #Apply changes to f.map and Fuelbed.List
  vt.map <- s.map[s.map %in% tslt.Stands]
  v.tslt <- tslt.Fuelbeds[match(vt.map, tslt.Stands)]
  f.map[s.map %in% tslt.Stands] <- v.tslt
  Fuelbed.List[Stand.List %in% tslt.Stands] <- tslt.Fuelbeds
  
  #Remove stands that have been overwritten
  tslt.List <- tslt.List[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
  tslt.Fuelbeds <- tslt.Fuelbeds[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
  tslt.Stands <- tslt.Stands[!(is.na(match(tslt.Stands, Stand.List[Stand.List %in% tslt.Stands])))]
  
  
  #Remove rows where fuelbeds no longer represent a silvicultural treatment
  tslt.List <- tslt.List[which(mapply(function(y) 
  {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
  , tslt.Fuelbeds) %in% c(2,3,4,6,7,8) == T)]
  
  tslt.Fuelbeds <- tslt.Fuelbeds[which(mapply(function(y) 
  {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
  , tslt.Fuelbeds) %in% c(2,3,4,6,7,8) == T)]
  
  tslt.Stands <- tslt.Stands[which(mapply(function(y) 
  {as.numeric(strsplit(as.character(y), "")[[1]])[4]}
  , tslt.Fuelbeds) %in% c(2,3,4,6,7,8) == T)]
  
  #Update files based on succession>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
  #Identify potential changes in fuelbeds based on succession pathways.
  
  pmuf <- mapply(function(x) ifelse(
    Age.List[x] > f.path$end[f.path$pre == Fuelbed.List[x]],
    ifelse(f.path$post_2[f.path$pre == Fuelbed.List[x]] > 0,
           resample(c(f.path$post_1[f.path$pre == Fuelbed.List[x]],
                      f.path$post_2[f.path$pre == Fuelbed.List[x]]),1),
           f.path$post_1[f.path$pre == Fuelbed.List[x]]),
    Fuelbed.List[x]),1:length(Stand.List))
  
  #Update f.map
  #Improved function to update f.map based on mFRI. Former code used a for()
  s.SL2 <- Stand.List[Fuelbed.List != pmuf]
  pmuf2 <- pmuf[Fuelbed.List != pmuf]
  vs.map2 <- s.map[s.map %in% s.SL2]
  v.pmuf2 <- pmuf2[match(vs.map2, s.SL2)]
  f.map[s.map %in% s.SL2] <- v.pmuf2
  Fuelbed.List[Stand.List %in% s.SL2] <- pmuf2
  
  #Update D.List
  D.List <- cbind(T1E.List, T2E.List, D1E.List, D2E.List)
  
  #Create maps for interval years.
  if((a %% Interval) == 0)
    {
    
    #  #Save Fuelbed Map (f.map).
    dt <- Sys.Date()
    tm <- format(Sys.time(), format = "%H.%M.%S", 
                 tz = "", usetz = FALSE)
    
    write.table(s.map, file = paste(output_path, "sef_smap_run_", run, "_", 
                                    dt,"_",tm,"_R",rows,"xC",cols,"_Y",a,".txt",sep = ""), 
                append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", 
                dec = ".", row.names = FALSE,col.names = FALSE, qmethod = 
                  c("escape", "double"))#
    
    write.table(f.map, file = paste(output_path, "fdm_maps/sef_fmap_run_", run, "_",
                                    dt,"_",tm,"_R",rows,"xC",cols,"_Y",a,".txt",sep = ""), 
                append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", 
                dec = ".", row.names = FALSE,col.names = FALSE, qmethod = 
                  c("escape", "double"))#
    
    }
  } #1.0.0 ---------------------------------------------------------------------------

#}

#entireScript()
