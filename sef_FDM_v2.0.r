###START FDM####
install.packages("Hmisc", repos="http://cran.fhcrc.org/")
install.packages("GenKern", repos="http://cran.fhcrc.org/")
install.packages("SDMTools", repos="http://cran.fhcrc.org/")
install.packages("gtools", repos="http://cran.fhcrc.org/")
library(Hmisc)
library(GenKern)
library(SDMTools)
library(gtools)  #for combinations()

aa <- system.time({

aa1 <- vector(length = 0, mode = 'numeric')
aa2 <- vector(length = 0, mode = 'numeric')
aa3 <- vector(length = 0, mode = 'numeric')
aa4 <- vector(length = 0, mode = 'numeric')
aa5 <- vector(length = 0, mode = 'numeric')

ab1 <- vector(length = 0, mode = 'numeric')
ab2 <- vector(length = 0, mode = 'numeric')

ac1 <- vector(length = 0, mode = 'numeric')
ac2 <- vector(length = 0, mode = 'numeric')
ac3 <- vector(length = 0, mode = 'numeric')

ad1 <- vector(length = 0, mode = 'numeric')
ad2 <- vector(length = 0, mode = 'numeric')
ad3 <- vector(length = 0, mode = 'numeric')
ad4 <- vector(length = 0, mode = 'numeric')
ad5 <- vector(length = 0, mode = 'numeric')
ad6 <- vector(length = 0, mode = 'numeric')
ad7 <- vector(length = 0, mode = 'numeric')
ad8 <- vector(length = 0, mode = 'numeric')
ad9 <- vector(length = 0, mode = 'numeric')
ad10 <- vector(length = 0, mode = 'numeric')
ad11 <- vector(length = 0, mode = 'numeric')

ac4 <- vector(length = 0, mode = 'numeric')
ac5 <- vector(length = 0, mode = 'numeric')
ac6 <- vector(length = 0, mode = 'numeric')

ab3 <- vector(length = 0, mode = 'numeric')
ab4 <- vector(length = 0, mode = 'numeric')
ab5 <- vector(length = 0, mode = 'numeric')

aa6 <- vector(length = 0, mode = 'numeric')
aa7 <- vector(length = 0, mode = 'numeric')
aa8 <- vector(length = 0, mode = 'numeric')
aa9 <- vector(length = 0, mode = 'numeric')
aa10 <- vector(length = 0, mode = 'numeric')
aa11 <- vector(length = 0, mode = 'numeric')
aa12 <- vector(length = 0, mode = 'numeric')
aa13 <- vector(length = 0, mode = 'numeric')

ae1 <- vector(length = 0, mode = 'numeric')
ae2 <- vector(length = 0, mode = 'numeric')

af1 <- vector(length = 0, mode = 'numeric')
af2 <- vector(length = 0, mode = 'numeric')
af3 <- vector(length = 0, mode = 'numeric')
af4 <- vector(length = 0, mode = 'numeric')
af5 <- vector(length = 0, mode = 'numeric')
af6 <- vector(length = 0, mode = 'numeric')
af7 <- vector(length = 0, mode = 'numeric')
af8 <- vector(length = 0, mode = 'numeric')
af9 <- vector(length = 0, mode = 'numeric')
af10 <- vector(length = 0, mode = 'numeric')
af11 <- vector(length = 0, mode = 'numeric')

ag1 <- vector(length = 0, mode = 'numeric')
ag2 <- vector(length = 0, mode = 'numeric')
ag3 <- vector(length = 0, mode = 'numeric')
ag4 <- vector(length = 0, mode = 'numeric')
ag5 <- vector(length = 0, mode = 'numeric')
ag6 <- vector(length = 0, mode = 'numeric')
ag7 <- vector(length = 0, mode = 'numeric')
ag8 <- vector(length = 0, mode = 'numeric')
ag9 <- vector(length = 0, mode = 'numeric')
ag10 <- vector(length = 0, mode = 'numeric')
ag11 <- vector(length = 0, mode = 'numeric')
ag12 <- vector(length = 0, mode = 'numeric')
ag13 <- vector(length = 0, mode = 'numeric')
ag14 <- vector(length = 0, mode = 'numeric')
ag15 <- vector(length = 0, mode = 'numeric')
ag16 <- vector(length = 0, mode = 'numeric')
ag17 <- vector(length = 0, mode = 'numeric')
ag18 <- vector(length = 0, mode = 'numeric')

ah1 <- vector(length = 0, mode = 'numeric')
ah2 <- vector(length = 0, mode = 'numeric')
ah3 <- vector(length = 0, mode = 'numeric')
ah4 <- vector(length = 0, mode = 'numeric')
ah5 <- vector(length = 0, mode = 'numeric')
ah6 <- vector(length = 0, mode = 'numeric')
ah7 <- vector(length = 0, mode = 'numeric')
ah8 <- vector(length = 0, mode = 'numeric')
ah9 <- vector(length = 0, mode = 'numeric')
ah10 <- vector(length = 0, mode = 'numeric')
ah11 <- vector(length = 0, mode = 'numeric')
ah12 <- vector(length = 0, mode = 'numeric')
ah13 <- vector(length = 0, mode = 'numeric')

af12 <- vector(length = 0, mode = 'numeric')
af13 <- vector(length = 0, mode = 'numeric')
af14 <- vector(length = 0, mode = 'numeric')
af15 <- vector(length = 0, mode = 'numeric')
af16 <- vector(length = 0, mode = 'numeric')
af17 <- vector(length = 0, mode = 'numeric')
af18 <- vector(length = 0, mode = 'numeric')

ae3 <- vector(length = 0, mode = 'numeric')
ae4 <- vector(length = 0, mode = 'numeric')
ae5 <- vector(length = 0, mode = 'numeric')

aa14 <- vector(length = 0, mode = 'numeric')
aa15 <- vector(length = 0, mode = 'numeric')
aa16 <- vector(length = 0, mode = 'numeric')
aa17 <- vector(length = 0, mode = 'numeric')
aa18 <- vector(length = 0, mode = 'numeric')
aa19 <- vector(length = 0, mode = 'numeric')
aa20 <- vector(length = 0, mode = 'numeric')
aa21 <- vector(length = 0, mode = 'numeric')
aa22 <- vector(length = 0, mode = 'numeric')
aa23 <- vector(length = 0, mode = 'numeric')
aa24 <- vector(length = 0, mode = 'numeric')
aa25 <- vector(length = 0, mode = 'numeric')
aa26 <- vector(length = 0, mode = 'numeric')
aa27 <- vector(length = 0, mode = 'numeric')
aa28 <- vector(length = 0, mode = 'numeric')
aa29 <- vector(length = 0, mode = 'numeric')

#Version 17e corresponds with model documentation
####################################################################################
####################################################################################
#STEP 1: Administrative Information
# Accept arguments from Rscript/command line
args <- commandArgs(TRUE)

rows <- 1771#1 #544
cols <- 3491#2 #671
tx <- 4#3
Years <- 1#4
fh.adj <- 6#NEW
sh.adj <- 6#NEW
bh.adj <- 6#NEW
lh.adj <- 6#NEW
run <- args[1] 
#Trying to generate errors from runs 1028 and 1030.

fire.cut <- 10

#Create files to pass information about model run status.
file.create("run_iterations")
file.create("disturbance_summary")
file.create("annual_summary")

#Temporary tracking items to figure out why MU.List is being corrupted.
Loop.track <- list()
MU.track <- list()
mgmtUnit.track <- list()
a <- 0
b <- 0
cc <- 0
d <- 0
e <- 0
f <- 0
g <- 0

####################################################################################
####################################################################################
#STEP 2: Operational Parameters
MapRes <- 0.22239#The number of acres per pixel
Interval <- 5#6
r.max <- 1000#7
c.shape <- 1.5#8
c.scale <- 0.1#9
#These two parameters will cause flammability of fuels to slowly equilibrate
#as annual area burned increases. For these values (c.shape = 1.5 and 
#s.scale = 0.1) equlibration begins when area burned for a fire in the unmanaged
#unit (management unit = 9999) or buffer zone (management unit = 8888)
#almost immediately as fire size grows and all but unburnable fuels equilibrate to 1
#by the time fire size equals 1100 acres (5000 pixels).
#When wildfires are burned by the block and burn method flammability of fuels is based on
#probability. The meaning of the scale.factor and dist.curve are flipped and corresponding
#values are randomly selected from each dataset
NFR <- c(9954.38,99457.39)#c(5000,10000)#Natural fire rotation in years for Eglin, Buffer, and Combined.
MFS <- c(103.65,5.23)#c(1000,200)#Mean fire size in acres for Eglin, Buffer, and Combined.
DFS <- c(361.12,13.98)#c(20,10)#Standard deviation of mean fire size for Eglin and Buffer and Combined.
Truncate.AAB <- c(50000,25000)#Maximum annual area burned
Truncate.Area <- c(12000,6000)#Maximum fire size
Truncate.Number <- c(400,800,50,50,500)#Maximum number of fires and treatments in a given year
#Order is number of fires in Eglin, fires in buffer zone, and treatments (thinning, 
#herbicide, and prescribed fire) in Eglin.

#Vector of fuelbed numbers (corresponds with f.map) with a fixed age at zero.
Fixed.Age <- c(-9999,1061401,1069000,1071401,5079000, 5089000, 5099000, 6000000)
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

#Size threshold where fires are primarily wind driven
windThresholdSize <- 1000#acres
#Testing, 10,000 acres was my original limit. It seems to high.

#Vector of burn unit numbers (corresponds with b.map) that are within Eglin but unmanaged.
Unmanaged.Unit <- 9999
#Vector of burn unit numbers (corresponds with b.map) that are within the buffer perimeter.
Buffer.Unit <- 8888
#Vector of burn unit numbers (corresponds with b.map) that are outside of the Eglin perimeter.
NoData.Unit <- -9999999

#Staring stand numbers for...
#Treatments:
treat.stand <- 4000000
#Fires:
fire.stand <- 8000000

#Values to guide stochastic generation of treatments. Order is thinning, herbicide, and
#prescribed fire.
minSize <- c(5,20,1)#minimum treated stand size within a treatment unit
#Shape parameters are used to inform the beta distribution function that determines
#The percentage of a treatment unit to be effected for each treatment.
shape1 <- c(30,30,10)#shape 1 parameter
shape2 <- c(5,5,2.5)#shape 2 parameter
#Test out the beta distribution.
#test <- rbeta(10000,10,2.5)#rbeta(number of observations, shape 1, shape 2)
#hist(test, breaks = seq(0,1,0.01))
#range(test)

#Average annual area treated for thinning, herbicide, and prescribed fire.

#Read in third meanTAP parameter from file
meanTAP <- c(1000, 1000, as.numeric(args[3]))

#Proportion of available cells within a treatment unit to seed treatment.
seed.cells <- c(0.50,0.50,0.10)#thinning, herbicide, prescribed fire

####################################################################################
####################################################################################
#STEP 3: Import Spatial Database (Raster Subset)
f.map <- matrix(scan(paste("sef_fmap_v2_",rows,"x",cols,".txt",
                           sep = ""),skip = fh.adj),ncol=cols,byrow=T)#16

s.map <- matrix(scan(paste("sef_smap_092715_",rows,"x",cols,".txt",
                           sep = ""),skip = sh.adj),ncol=cols,byrow=T)#17

b.map <- matrix(scan(paste("sef_bmap_",rows,"x",cols,".txt",
                           sep = ""),skip = bh.adj),ncol=cols,byrow=T)#18
#Need to create a burn unit map

l.map <- matrix(scan(paste("sef_lmap_",rows,"x",cols,".txt",
                           sep = ""),skip = lh.adj),ncol=cols,byrow=T)#20

####################################################################################
####################################################################################
#STEP 4: Import Spatial Database (Pseudo-vector Subset)
Stand.List <- read.table(paste(
  "sef_StandList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Stand.List <- cleanup.import(Stand.List)
Stand.List <- as.vector(Stand.List[,2], mode = "numeric")#20
Stand.List <- Stand.List[-1]

Fuelbed.List <- read.table(paste(
  "sef_FuelbedList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Fuelbed.List <- cleanup.import(Fuelbed.List)
Fuelbed.List <- as.vector(Fuelbed.List[,2], mode = "numeric")#21
Fuelbed.List <- Fuelbed.List[-1]

Coord.List <- read.table(paste(
  "sef_CoordList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Coord.List <- cleanup.import(Coord.List)
Coord.List <- as.vector(Coord.List[,2], mode = "numeric")#21
Coord.List <- Coord.List[-1]

Age.List <- read.table(paste(
  "sef_AgeList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Age.List <- cleanup.import(Age.List)
Age.List <- as.vector(Age.List[,2], mode = "numeric")#22
Age.List <- Age.List[-1]

Area.List <- read.table(paste(
  "sef_AreaList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Area.List <- cleanup.import(Area.List)
Area.List <- as.vector(Area.List[,2], mode = "numeric")#23
Area.List <- Area.List[-1]

mfri.List <- read.table(paste(
  "sef_mfriList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
mfri.List <- cleanup.import(mfri.List)
mfri.List <- as.vector(mfri.List[,2], mode = "numeric")#23
mfri.List <- mfri.List[-1]

T1E.List <- read.table(paste(
  "sef_T1EList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
T1E.List <- cleanup.import(T1E.List)
T1E.List <- as.vector(T1E.List[,2], mode = "numeric")#24
T1E.List <- T1E.List[-1]

T2E.List <- read.table(paste(
  "sef_T2EList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
T2E.List <- cleanup.import(T2E.List)
T2E.List <- as.vector(T2E.List[,2], mode = "numeric")#24
T2E.List <- T2E.List[-1]

D1E.List <- read.table(paste(
  "sef_D1EList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
D1E.List <- cleanup.import(D1E.List)
D1E.List <- as.vector(D1E.List[,2], mode = "numeric")#25
D1E.List <- D1E.List[-1]

D2E.List <- read.table(paste(
  "sef_D2EList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
D2E.List <- cleanup.import(D2E.List)
D2E.List <- as.vector(D2E.List[,2], mode = "numeric")#25
D2E.List <- D2E.List[-1]

MU.List <- read.table(paste(
  "sef_MUList_",rows,"x",cols,".txt",
  sep = ""), header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
MU.List <- cleanup.import(MU.List)
MU.List <- as.vector(MU.List[,2], mode = "numeric")#25
MU.List <- MU.List[-1]

#Last line removes the first integer from the .List objects which contain the NoDate
#area and will not match up with any date in f.path, resulting in an error.

####################################################################################
####################################################################################
#STEP 5: Import Conditional Database
f.path <- read.table("sef_lut_pathways_succession.csv", header=TRUE, 
                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)
f.path <- cleanup.import(f.path)#26

f.treatments <- read.table("sef_lut_menu_treatment.csv", header=TRUE, 
                           sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
f.treatments <- cleanup.import(f.treatments)#27

f.disturbances <- read.table("sef_lut_menu_disturbance.csv", header=TRUE, 
                             sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
f.disturbances <- cleanup.import(f.disturbances)#28

t.post <- read.table("sef_lut_pathways_silviculture.csv", header=TRUE, 
                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)
t.post <- cleanup.import(t.post)#29

d.post <- read.table("sef_lut_pathways_fire.csv", header=TRUE, 
                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)
d.post <- cleanup.import(d.post)#30

pda <- read.table("sef_lut_thresholds_disturbances.csv", header=TRUE, 
                  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
pda <- cleanup.import(pda)#31

f.probability <- read.table("sef_lut_prob_burning.csv", header=TRUE, 
                            sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
f.probability <- cleanup.import(f.probability)#32

f.wind <- read.table("sef_lut_prob_wind.csv", header=TRUE, 
                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)
f.wind <- cleanup.import(f.wind)

b.unit <- read.table("sef_lut_burn_units.txt", header=TRUE, 
                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)
b.unit <- cleanup.import(b.unit)
b.unit <- data.frame(unit = b.unit[,2], area_ac = b.unit[,3], thin= b.unit[,4], 
                     herb = b.unit[,5], fire = b.unit[,6])#remove col 1 and rename cols 2-6.

#TODO: Add burn blocks in later. 
#b.block <- read.table("sef_lut_pathways_burnBlocks.csv", header=F, 
#           sep=",", na.strings="NA", dec=".", strip.white=TRUE)

####################################################################################
####################################################################################
#STEP 07: Simplify input data.
ttxm <- t.post[,-1]#removes the fuelbed column for t.post.
dtxm <- d.post[,-1]#removes the fuelbed column from d.post.
pdxm <- pda[,-1]#removes the fuelbed column from pda.

####################################################################################
####################################################################################
#STEP 08: Generate secondary data.
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
FF.e <- ((eglin.area*MapRes)/(NFR[1]*MFS[1]))#FF.e = Fire Frequency for Eglin
FF.b <- ((buffer.area*MapRes)/(NFR[2]*MFS[2]))#FF.b = Fire Frequency for buffer
Mu.e <- 2*log(MFS[1]) - 0.5*(log(DFS[1]^2 + MFS[1]^2))#mean of log transformed mean fire size
Mu.b <- 2*log(MFS[2]) - 0.5*(log(DFS[2]^2 + MFS[2]^2))#mean of log transformed mean fire size
Sigma.e <- sqrt(log(DFS[1]^2 + MFS[1]^2) - 2*log(MFS[1]))#variance of log transformed fire sizes
Sigma.b <- sqrt(log(DFS[2]^2 + MFS[2]^2) - 2*log(MFS[2]))#variance of log transformed fire sizes
#Pixels to search (y coordinates, difference from flame front)
search.set <- matrix(data = 0,48,2)             #Cellular automata input
search.set[,1] <- c(-1,-1,0,1,1,1,0,-1,
                    -2,-2,-2,-1,0,1,2,2,2,2,2,
                    1,0,-1,-2,-2,
                    -3,-3,-3,-3,-2,-1,0,1,2,3,3,3,3,3,3,3,2,1,0,-1,-2,-3,-3,-3)        

#Pixels to search (x coordinates, difference from flame front)
search.set[,2] <- c(0,rows,rows,rows,0,-rows,-rows,-rows,
                    0,rows,(rows*2),(rows*2),(rows*2),(rows*2),(rows*2),rows,0,-rows,(-rows*2),
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
windDirs <- c(0,1,2,3,4,5,6,7)
#0 = North
#1 = Northeast
#2 = East
#3 = Southeast
#4 = South
#5 = SouthWest
#6 = West
#7 = Northwest

#Describe probability of wind coming from a given direction
windProbs <- c(0.1,0.025,0.01,0.01,0.025,0.05,0.16,0.62)

Map.History <- list()                          #Tracking Database Template.
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
e.summary <- data.frame()

####################################################################################
####################################################################################
#STEP 09: Generate functions.
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
#STEP 10: Fire Regime Simulation for Eglin (excludes buffer zone).

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
#STEP 11: Create vectors that list which fuelbeds are eligible for treatment.

#Determine available fuelbeds:
#For thinning treatments
avfb_thin <- fbls[ttxm[,4] == 2]

#For herbicide treatments
avfb_herb <- fbls[ttxm[,5] == 2]

#For prescribed fire treatments
avfb_fire <- fbls[ttxm[,6] == 2]

####################################################################################
####################################################################################
#STEP 12: Update mFRI list and then create a mFRI format that can be updated.

#Update -- the mFRI list needs to be updated because the mFRI data layer was not
#used to inform the generation of all fuelbeds. For instance, small ponds are included
#in polygons representing frequently burned areas but themselves do not burn. This instance
#does not cause a problem because in the look-up tables open water has a mFRI range of 
#0-32 and the updating loop at the end of FDM will not consider these "stands". However
#there are thousands of forested stands of unburnable fuels with a long mFRI range.
#For instance sand pine, riparian forests, and young plantations that may be included
#in a short fire return interval polygon but do not burn. These stands present a 
#problem because the fire regime updating loop at the end of FDM will flag these stands
#as being outside of their mFRI but the look-up table will maintain the fuelbed (as it should)
#This is a problem because it is a large time burden on FDM run times. To fix this two things should
#happen. First before the loops initiate (i.e. right here) the mFRI list needs to be updated
#to reflect the actual mFRI likely for the stand, not what is shown by the generalized
#mFRI layer (as listed is mfri.List). Second the lookup tables must have alternatives
#for fuelbeds with low-medium to medium probability of burning or they will build up
#over the course of an FDM run as out of compliance with their mFRI range and bog down
#the model.


#Testing only (run 93)
pre.m1 <- mfri.List 
pre.f1 <- Fuelbed.List
pre.s1 <- Stand.List
pre.a1 <- Area.List

#This updates the mFRI list so all stands have the correct mFRI given
#their fuelbed. This mostly corrects short mFRIs in non-burnable/low probability burn fuelbeds.
#See above paragraph for more information.
mfri.List_v2 <- mapply(function(y) 
  {
  ifelse(any(mfri.List[y] < d.post$mfri_start[d.post$fuelbed == Fuelbed.List[y]], 
             mfri.List[y] > d.post$mfri_end[d.post$fuelbed == Fuelbed.List[y]]) == T,
         resample(seq(d.post$mfri_start[d.post$fuelbed == Fuelbed.List[y]], 
                      d.post$mfri_end[d.post$fuelbed == Fuelbed.List[y]], 1), 1), 
         mfri.List[y])
  }, 1:length(mfri.List))

#Replace old (and incorrect) mfri.List with new one.
mfri.List <- mfri.List_v2



matcols <- 30
mfri.Matrix <- matrix(data = 0, nrow = length(mfri.List), ncol = matcols)

for(i in 1:length(mfri.List)) 
{
  if(mfri.List[i] == -9999)
  {
    mfri.Matrix[i,] <- rep(-9999,matcols)
  } else
  {
   temp_mfri <- c(rep(1,ifelse(mfri.List[i] == 32,0,round(matcols/mfri.List[i],0))),
   rep(0,ifelse(mfri.List[i] == 32,30,(matcols-round(matcols/mfri.List[i],0)))))
   mfri.Matrix[i,] <- sample(temp_mfri, length(temp_mfri))
    
    #mfri.Matrix[i,] <- resample(c(rep(0,(mfri.List[i]-1)),1), 
    #                           matcols,
    #                           prob = rep((1/mfri.List[i]),mfri.List[i]), replace = T)
   ##Made these changes to the code on 10/19/2015. The new code ensures
   #that the list of fires reflects the actual mFRI value in mfri.List whereas this
   #old version was based on probability and could produce different values that could
   #result in a dramatically different mFRI if there where 2 rather than 1 fire.
  }
}



#Testing only (run 93)
pre.m3 <- mfri.List 
pre.f3 <- Fuelbed.List
pre.s3 <- Stand.List
pre.a3 <- Area.List


####################################################################################
####################################################################################
#STEP 04: RUN MODEL LOOP
})#aa

#LOOP 111111111111111111111111111111111111111111111111111111111111111111111111111111
#Loop 1 (by year). This loop encases the entire expression that maps regimes.
for(a in 1:Years)#a <- 1
{ #1.0.0 ---------------------------------------------------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  #The number of treatments per year is drawn from the normal distribution.
  #The mean and standard deviation for each treatment type is entered in step 1.

  a1 <- 0
  a2 <- 0
  a3 <- 0
  a4 <- 0
  a5 <- 0
  a6 <- 0
  a7 <- 0
  a8 <- 0
  a9 <- 0
  a10 <- 0
  a11 <- 0
  a12 <- 0
  a13 <- 0
  a14 <- 0
  a15 <- 0
  a16 <- 0
  a17 <- 0
  a18 <- 0
  a19 <- 0
  a20 <- 0
  a21 <- 0
  a22 <- 0
  a23 <- 0
  a24 <- 0
  a25 <- 0
  a26 <- 0
  a27 <- 0
  a28 <- 0
  a29 <- 0
  
  a1 <- system.time({
  
    #Objects to record annual area for thinning, herbicide, and prescribed fire treatments.
    meanTAA <- c(0, 0, 0)

  })#a1

  a2 <- system.time({
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
  
  })#a2
  

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TREATMENTS>>>>>>>>
  
  a3 <- system.time({
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
 
  #The number of fires per year is drawn from the Poisson Distribution (Wimberly 2002)
  #FF = Annual Fire Frequency. This is calculated in step 1.
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
})#a3

#A4 IS LOCATED BELOW SECTION 9.4.2

a4 <- system.time({
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>FIRES>>>>>>>>>>>>>>>
  
  #Update the age list. Age of fuelbed number is kept at zero.
  Age.List[!Fuelbed.List %in% Fixed.Age] <- Age.List[!Fuelbed.List %in% Fixed.Age] + min(a-1,1)
  
  #Update the mfri matrix. Age of fuelbed number is kept at zero.
  mfri.Matrix <- cbind(mfri.Matrix, rep(0,length(mfri.Matrix[,1])))
  mfri.Matrix <- mfri.Matrix[,-1]
  
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
})#a4
a5 <- system.time({
  #Tracks treatment history.
  Treatment.History <- list()
  #Tracks disturbance history.
  Disturbance.History <- list()
  
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
})#a5

if(sum(meanTAP) <= 0)
  {
  tbsa <- 0
  Treatment.Area <- 0
  PrctTrmt.Mapped <- 0
  #Log treatment run data for treatment[b].
  Treatment.History[[(length(Treatment.History)+1)]] <- paste(
    "No treatments planned for this year", collapse = "")
  
  #Add header to list.
  names(Treatment.History)[[length(Treatment.History)]] <- paste(c(
    "Treatment Number: ",b," ### Treatment Type: ", 
    f.treatments$TreatmentTitle[t.code]," ### Treatment Area: ", 
    tbsa, " ### Area Mapped: ", Treatment.Area[length(Treatment.Area)], 
    " ### Completed: ", PrctTrmt.Mapped[length(PrctTrmt.Mapped)], "% ###"), 
    collapse = "")
  } else
  {
    #LOOP 222222222222222222222222222222222222222222222222222222222222222222222222222222
    #Loop 2 (by treatments within year[a]). This loop runs all treatments for year[a].
    for (b in 1:r.max)#b <- 1
    { #2.0.0 ---------------------------------------------------------------------------
      #TESTING ONLY
      b1 <- 0
      b2 <- 0
      b3 <- 0
      b4 <- 0
      b5 <- 0
      
      #Lists treatment code for current treatment
      t.code <- ifelse(length(which(meanTAA < meanTAP)) == 0,4,min(which(meanTAA < meanTAP)))
      
      if(t.code == 4)
      { #2.1.1-----------------------------------------------------------------------------
        break
      } else #2.1.1------------------------------------------------------------------------
{ #2.1.2-----------------------------------------------------------------------------
      
      b1 <- system.time({
        d.d <- vector(length = 1, mode = 'numeric')#tracks expansions
        
        
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
        Iteration.cc <- vector(mode = "numeric", length = 1)
        Explanation.cc <- vector(mode = "character", length = 1)
        Iteration.d <- vector(mode = "numeric", length = 1)
        Explanation.d <- vector(mode = "numeric", length = 1)
        Treatment.Area <- vector(mode = "numeric", length = 1)
        PrctTrmt.Mapped <- vector(mode = "numeric", length = 1)
        
        #Record pre-run data (cc iteration = zero).
        Iteration.cc[cc+1] <- 0
        Explanation.cc[cc+1] <- "Setting up block."
        Iteration.d[cc+1] <- 0
        Explanation.d[cc+1] <- "Setting up expansion."
        Treatment.Area[cc+1] <- 0
        PrctTrmt.Mapped[cc+1] <- 0
        
      })#b1
      
      b2 <- system.time({
        
        #Determine available fuelbeds.
        avfb <- fbls[ttxm[,t.code + 3] == 2]
        
        #Find eligible stands with eligible fuelbeds
        elst <- sort(unique(s.map[!b.map %in% c(NoData.Unit, Buffer.Unit, 
                                                Unmanaged.Unit) & 
                                    f.map %in% avfb & s.map %in% loopA.snO]))
        
        #Determine the burn unit for treatment
        #Only consider management units where over 50% of the unit is available for treatment.
        #For units that are use percent of unit available for treatment as prob arg for selection.
        bun <- resample(b.unit$unit[pri[,t.code] > 0.50],
                        size = 1,
                        prob = pri[,t.code][pri[,t.code] > 0.50])
        
        #Determine the treatment area. This is governed by the available fuelbeds, minimum stand
        #area, and fraction of available area treated (beta distribution).
        tbsa <- round(((length(b.map[b.map == bun & s.map %in% elst])) * 
                         rbeta(1,shape1[t.code],shape2[t.code])),0)
        
        #Initiate treatment in the proportion of available pixels specified in step 1 (cuts)
        sct <- vector(mode = "numeric", length = 0)
        sct <- resample(l.map[b.map == bun & s.map %in% elst], 
                        round(max((tbsa * seed.cells[t.code]),1),0))
      })#b2
      
        if(tbsa >= 1)
        { #2.2.1 ---------------------------------------------------------------------------
          
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
            c1 <- 0
            c2 <- 0
            c3 <- 0
            c4 <- 0
            c5 <- 0
            c6 <- 0

c1 <- system.time({
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
            tbma <- (length(sct) + ifelse(cc > 1, length(new.cells), 0))
            })#c1

            #Ends loop when treatment[b] has been completely mapped.
            if(tbma < tbsa)
            { #3.1.1 ---------------------------------------------------------------------------
              c2 <- system.time({             
              #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>NEW
              #Differentiate between initial loop and subsequent loops were you want to keep the
              #treatment within the designated burn unit boundaries.
              if(cc > 1)
              {
                #Initiate treatment in the proportion of available pixels specified in step 1 (cuts)
                sct <- vector(mode = "numeric", length = 0)
              } else
              {
                #No, initial block
                sct <- sct
              }
              #Then find stands within this subset that meet minimum age requirements for
              #the disturbance/treatment.
              })#c2
              
              if(length(sct) > 0)
              { #3.2.1 ---------------------------------------------------------------------------
            
c3 <- system.time({
                #Tracks the highest stand number currently in s.map. 
                #Used to assign new stand numbers for treatment[b].
                #Can not place this below loop 4 in case treatment erases stand with highest number.
                masn <- max(treat.stand,max(unique(as.vector(s.map[s.map < fire.stand]))))
                
                #Establish treatment[b] in s.map and record old stand number
                ocot <- c(ocot, sct) #tracks coordinates involved in disturbance.
                s.map[sct] <- s.map[sct]*tesn_t
                osnt <- c(osnt,s.map[sct])
                tesn <- unique(osnt)
})#c3 
                #LOOP 4444444444444444444444444444444444444444444444444444444444444444444444444444
                #Loop 4 (by iterations). This loop keeps growing treatment[b] in block[cc] 
                #until growth stops.
                for (d in 1:r.max)#d <- 1
                { #4.0.0 ---------------------------------------------------------------------------
                  d1 <- 0
                  d2 <- 0
                  d3 <- 0
                  d4 <- 0
                  d5 <- 0
                  d6 <- 0
                  d7 <- 0
                  d8 <- 0
                  d9 <- 0
                  d10 <- 0
                  d11 <- 0
                  
d1 <- system.time({          
                  #Area mapped for treatment[b].
                  tbma <- length(s.map[s.map %in% c(tesn,loopC.new_stand)])
})#d1
                  #This statement stops loop 4 when treatment[b] has been fully mapped.
                  if(tbma < tbsa) 
                  { #4.1.1 ---------------------------------------------------------------------------
   

d2 <- system.time({
                    #Object shows locations of 8 pixels surrounding each mapped pixel for 
                    #disturbance[e].
                    malo <- matrix(data = sn.seeker(search.set[1:8,1], search.set[1:8,2]),
                                   length(s.map[s.map %in% tesn]),dcl_t, byrow = T)
})#d2
d3 <- system.time({   
                    #Object shows the upper and lower limits of pixel locations listed in the 'malo' 
                    #object.
                    mali <- matrix(data = sn.limit(search.set[1:8,2], search.set[1:8,1]), 
                                   length(s.map[s.map %in% tesn]),dcl_t, byrow = T)
                    
})#d3
d4 <- system.time({  
                    #Sets up information in malo and mali objects that replace location values with
                    #limits values if locations "spill" from map edges.
                    mixa <- c(malo[,1],malo[,2],mali[,3],mali[,4],mali[,5],mali[,6],mali[,7],malo[,8])
                    mixb <- c(mali[,1],mali[,2],malo[,3],malo[,4],malo[,5],malo[,6],malo[,7],mali[,8])
})#d4
d5 <- system.time({ 
                    #Replaces location values with limit values where necessary (first draft - works
                    #on top and bottom of map)
                    fdlo <- ifelse(mixa < mixb,mali,malo)
})#d5
d6 <- system.time({  
                    #Replaces location values with limit values where necessary (second draft - works
                    #on sides of map)
                    sdlo <- ifelse(fdlo < 1,fdlo + rows,ifelse(fdlo > length(s.map),fdlo - rows,fdlo))
})#d6
d7 <- system.time({          
                    #This object shows all unique locations available for establishment by treatment[b].
                    avlo <- unique(l.map[sdlo][l.map[sdlo] %in% l.map[b.map == bun & s.map %in% elst]])
})#d7
 
                    #Ends loop if there are no more locations available for treatment[b] in the 
                    #block[cc] that is currently being mapped.
                    if(length(avlo) > 0)
                    { #4.2.1 ---------------------------------------------------------------------------
                      #Reset new.cells object.
                      new.cells <- vector(length=0, mode = "numeric")

d8 <- system.time({                       
                      #This expression picks out which location values are of the same stand and are 
                      #available (i.e. they are not occupied by the another treatment) and makes sure 
                      #that the mapped regime does not exceed its prescribed area.
                      if((tbma + length(avlo)) <= tbsa)
                      {
                        new.cells <- avlo
                        s.map[new.cells] <- s.map[new.cells]*tesn_t
                        osnt <- c(osnt, s.map[new.cells])
                        tesn <- unique(osnt)
                        ocot <- c(ocot, new.cells) #tracks coordinates involved in disturbance.
                      } else
                      {
                        new.cells <- resample(avlo,(tbsa - tbma))
                        s.map[new.cells] <- s.map[new.cells]*tesn_t
                        osnt <- c(osnt, s.map[new.cells])
                        tesn <- unique(osnt)
                        ocot <- c(ocot, new.cells) #tracks coordinates involved in disturbance.
                      }
})#d8
d9 <- system.time({ 
                      #Register mapping data after Loop 4 has finished running for iteration[d].
                      Iteration.cc[(length(Iteration.cc)+1)] <- cc
                      Explanation.cc[(length(Explanation.cc)+1)] <- "Block is running."
                      Iteration.d[(length(Iteration.d)+1)] <- d
                      Explanation.d[(length(Explanation.d)+1)] <- paste(c("Expansion. New pixels: ",
                                                                          length(new.cells),"."), collapse = "")
                      Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                                              tesn)])
                      PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
                        length(Treatment.Area)]/tbsa)*100),1)
})#d9
                    } else #4.2.1 ----------------------------------------------------------------------
                    
{#4.2.2
d10 <- system.time({
  Iteration.cc[(length(Iteration.cc)+1)] <- cc
  Explanation.cc[(length(Explanation.cc)+1)] <- paste(
    "Cannot expand block. Advance.", collapse = "")
  Iteration.d[(length(Iteration.d)+1)] <- d
  Explanation.d[(length(Explanation.d)+1)] <- paste(c("End expansion. Total: ",
                                                      length(s.map[s.map %in% tesn]),"."), collapse = "")
  Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                          tesn)])
  PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
    length(Treatment.Area)]/tbsa)*100),1)
})#d10
ad10[length(ad10) + 1] <- d10[3]

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)

cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__", f.treatments$TreatmentName[t.code], 
          "_",b, "__block_",cc,"__expansion_" , "_",d,"__.txt",sep = ""), 
    file = "run_iterations", append = T)#

  break
} #4.2.2 ---------------------------------------------------------------------------

                  } else #4.1.1 ----------------------------------------------------------------------

{ #4.1.2 ---------------------------------------------------------------------------
d11 <- system.time({
  Iteration.cc[(length(Iteration.cc)+1)] <- cc
  Explanation.cc[(length(Explanation.cc)+1)] <- paste(
    "Treatment mapped. New block.", collapse = "")
  Iteration.d[(length(Iteration.d)+1)] <- d
  Explanation.d[(length(Explanation.d)+1)] <- paste(c("End expansion. Total: ",
                                                      length(s.map[s.map %in% tesn]),"."), collapse = "")
  Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                          tesn)])
  PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
    length(Treatment.Area)]/tbsa)*100),1)
})#d11
ad11[length(ad11) + 1] <- d11[3]

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)
cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__", f.treatments$TreatmentName[t.code], 
          "_",b, "__block_",cc,"__expansion_" , "_",d,"__.txt",sep = ""), 
    file = "run_iterations", append = T)#

  break
} #4.1.2 ---------------------------------------------------------------------------

ad1[length(ad1) + 1] <- d1[3]
ad2[length(ad2) + 1] <- d2[3]
ad3[length(ad3) + 1] <- d3[3]
ad4[length(ad4) + 1] <- d4[3]
ad5[length(ad5) + 1] <- d5[3]
ad6[length(ad6) + 1] <- d6[3]
ad7[length(ad7) + 1] <- d7[3]
ad8[length(ad8) + 1] <- d8[3]
ad9[length(ad9) + 1] <- d9[3]
#d10 --- break
#d11 --- break

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)
cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__", f.treatments$TreatmentName[t.code], 
          "_",b, "__block_",cc,"__expansion_" , "_",d,"__.txt",sep = ""), 
    file = "run_iterations", append = T)#
                } #4.0.0 ---------------------------------------------------------------------------

c4 <- system.time({
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
#loopC.new_area <- c(loopC.new_area,mapply(function(x) length(s.map[s.map == x]),nebc))
l.nebc <- rep(1,length(v.nebc))
s.nebc <- summarize(l.nebc,v.nebc,sum)
loopC.new_area <- c(loopC.new_area, as.vector(s.nebc[,2]))
})#c4

              } else #3.2.1 ----------------------------------------------------------------------

{ #3.2.2 ---------------------------------------------------------------------------
c5 <- system.time({
  Iteration.cc[(length(Iteration.cc)+1)] <- cc
  Explanation.cc[(length(Explanation.cc)+1)] <- paste(
    "Error - Treatment was not started (cc = 1), or not finished (cc = 2).", collapse = "")
  Iteration.d[(length(Iteration.d)+1)] <- 0
  Explanation.d[(length(Explanation.d)+1)] <- "Expansion not started."
  Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                          tesn)])
  PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
    length(Treatment.Area)]/tbsa)*100),1)
})#c5
ac5[length(ac5) + 1] <- c5[3]
  break
} #3.2.2 ---------------------------------------------------------------------------

            } else #3.1.1 ----------------------------------------------------------------------

{ #3.1.2 ---------------------------------------------------------------------------
  c6 <- system.time({
  Iteration.cc[(length(Iteration.cc)+1)] <- cc
  Explanation.cc[(length(Explanation.cc)+1)] <- paste(
    "Treatment mapped. End Mapping.", collapse = "")
  Iteration.d[(length(Iteration.d)+1)] <- d
  Explanation.d[(length(Explanation.d)+1)] <- "Expansion not started."
  Treatment.Area[(length(Treatment.Area)+1)] <- length(s.map[s.map %in% c(loopC.new_stand,
                                                                          tesn)])
  PrctTrmt.Mapped[(length(PrctTrmt.Mapped)+1)] <- round(((Treatment.Area[
    length(Treatment.Area)]/tbsa)*100),1)
  })#c6
  ac6[length(ac6) + 1] <- c6[3]
  break
} #3.1.2 ---------------------------------------------------------------------------
d.d <- sum(d.d, d)#tracks expansions

ac1[length(ac1) + 1] <- c1[3]
ac2[length(ac2) + 1] <- c2[3]
ac3[length(ac3) + 1] <- c3[3]
ac4[length(ac4) + 1] <- c4[3]
#c5 --- break
#c6 --- break


          } #3.0.0 --------------------------------------------------------------------------- 
b3 <- system.time({
#Log treatment run data for treatment[b].
if(length(Iteration.cc) > 1)
  Treatment.History[[(length(Treatment.History)+1)]] <- data.frame(
    "Blocks" = Iteration.cc, "Block History" = Explanation.cc, 
    "Expansions" = Iteration.d, "Expansion History" = Explanation.d, 
    "Treatment Area" = Treatment.Area, "Percent Mapped" = PrctTrmt.Mapped, 
    stringsAsFactors = F) else
      Treatment.History[[(length(Treatment.History)+1)]] <- paste(
        "If this message shows up something weird happened. Error 3.0", collapse = "")

#Add header to list.
names(Treatment.History)[[length(Treatment.History)]] <- paste(c(
  "Treatment Number: ",b," ### Treatment Type: ", 
  f.treatments$TreatmentTitle[t.code]," ### Treatment Area: ", 
  tbsa, " ### Area Mapped: ", Treatment.Area[length(Treatment.Area)], 
  " ### Completed: ",PrctTrmt.Mapped[length(PrctTrmt.Mapped)], "% ###"), 
  collapse = "")

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
})#b3
        } else #2.1.1 ----------------------------------------------------------------------

{ #2.1.2 ---------------------------------------------------------------------------
  b4 <- system.time({
  #Log treatment run data for treatment[b].
  if(length(Iteration.cc) > 1)
    Treatment.History[[(length(Treatment.History)+1)]] <- paste(
      "If this message shows up something weird happened. Error 2.1.", 
      collapse = "") else
        Treatment.History[[(length(Treatment.History)+1)]] <- paste(
          "No space available to map treatment. Mapping was not attempted.", collapse = "")
  
  #Add header to list.
  names(Treatment.History)[[length(Treatment.History)]] <- paste(c(
    "Treatment Number: ",b," ### Treatment Type: ", 
    f.treatments$TreatmentTitle[t.code]," ### Treatment Area: ", 
    tbsa, " ### Area Mapped: ", Treatment.Area[length(Treatment.Area)], 
    " ### Completed: ", PrctTrmt.Mapped[length(PrctTrmt.Mapped)], "% ###"), 
    collapse = "")
  
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
  })#b4
} #2.1.2 ---------------------------------------------------------------------------
b5 <- system.time({

  #Area tracking
  meanTAA[t.code] <- sum(meanTAA[t.code], tbsa)
  
  #Date and time
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  
  #Tracking device
  t.summary <- data.frame(
    Date = dt, 
    Time = tm, 
    Year = a, 
    Disturbance_No = b,
    PercentComplete = round(((sum(meanTAA)/sum(meanTAP))*100),0), 
    Name = f.treatments$TreatmentTitle[t.code], 
    Area_Actual = tbma, 
    Area_Expected = tbsa, 
    Blocks = cc, 
    Expansions = d.d) 
  
e.summary <- rbind(e.summary, t.summary)

#Save run data.
cat(t.summary, file = "disturbance_summary", append = T)#
})#b5


ab1[length(ab1) + 1] <- b1[3]
ab2[length(ab2) + 1] <- b2[3]
ab3[length(ab3) + 1] <- b3[3]
ab4[length(ab4) + 1] <- b4[3]
ab5[length(ab5) + 1] <- b5[3]
} #2.1.2-----------------------------------------------------------------------------
    } #2.0.0 ---------------------------------------------------------------------------
  } #1.3.2-----------------------------------------------------------------------------

#Post run step 1>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Update files if there were treatments in year[a].
if(length(loopB.new_stand) > 0)
{

a6 <- system.time({
  #Object shows fuelbeds associated with each new stand number created by treatments 
  #in year[a].
  #ufxTa <- mapply(function(x) unique(f.map[s.map == x]), loopB.new_stand)
  ufxTa_1 <- Fuelbed.List[Stand.List %in% loopB$old_stand]
  usxTa_1 <- Stand.List[Stand.List %in% loopB$old_stand]
  ufxTa <- ufxTa_1[match(loopB$old_stand, usxTa_1)]
  })#a6



a7 <- system.time({

#Add ufxTa to loopB data frame
loopB <- data.frame(loopB, ufxTa = ufxTa)

#Sort data frame by new stands
loopB <- loopB[order(loopB$new_stand),]#probably unecessary

#seperate out new stands from s.map, lists occurrences of new stands from min to max coords.
vs.map_a7 <- s.map[s.map %in% loopB$new_stand]

#List of row numbers in t.post where fuelbeds need to be updated based on treatment
FL1 <- which(t.post$fuelbed %in%loopB$ufxTa)

#Fuelbeds that may be updated corresponding with row numbers in FL1
LL1 <- t.post$fuelbed[t.post$fuelbed %in% ufxTa]

#row numbers in t.post corresponding with each existing fuelbed in each new stand 
LL2 <- FL1[match(loopB$ufxTa, LL1)]

#Use row numbers (LL2) and column numbers (loopB.treat_type) to calculate "coordinate" in t.post
LL3 <- ((loopB.treat_type - 1) * length(t.post$fuelbed)) + LL2

#Convert t.post (ttxm is t.post) from a data.frame into a matrix so new fuelbeds can be identified 
#by coordinates that corresond with row and column numbers.
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

#Re-order loopB data frame by old stands because that is the order of ages in newAGE_a7.
loopB <- loopB[order(loopB$old_stand),]

#Add newAGE_a7 to loopB data.frame
loopB <- data.frame(loopB, newAGE_a7 = newAGE_a7)

})#a7

a8 <- system.time({
  #List stands that have been altered by treatments.
  ss1 <- (loopB.old_stand*-1)
  stands <- sort(unique(ss1))#there can be duplicates, this will mess up the shortcut in a9
  sa <- summarize(loopB.new_area,ss1,sum)#sum areas for duplicates.
  sareas <- as.vector(sa[,2])
  tt <- summarize(loopB.treat_type, ss1, min)
  stype <- as.vector(tt[,2])
  
  #Shelve fire history for stands that have been impacted by disturbance
  new_mfri_vec <- mapply(function(y) mfri.Matrix[Stand.List == y,], ss1)
  nmv <- t(new_mfri_vec)
  
  #Add a fire for stands that were prescrib burned
  nmv[,30] <- ifelse(loopB.treat_type == 3,1,0)
})#a8

a9 <- system.time({ 

  #Change stand properties as needed for treatments.
  #sad <- data.frame(stands = stands, sareas = sareas)
  #sad.b <- sad[order(sad$stands),]
  #Area.List[Stand.List %in% sad.b$stands] <- Area.List[Stand.List %in% sad.b$stands] - sad.b$sareas
  for(i in 1:length(stands))
  {
  Area.List[Stand.List == stands[i]] <- 
              Area.List[Stand.List == stands[i]] - sareas[i]
  }
})#a9

a10 <- system.time({
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
  Area.List <- Area.List[(Area.List == 0) == F]
})#a10

a11 <- system.time({
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

  #Use summarize function (w/ min()) to select a single coordinate value for each new stand.
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
})#a11


if(any(c(length(Stand.List),
         length(Fuelbed.List),
         length(MU.List),
         length(T1E.List),
         length(T2E.List),
         length(D1E.List),
         length(D2E.List),
         length(Area.List),
         length(mfri.Matrix[,1]), 
         length(Age.List)) != (length(unique(as.vector(s.map)))-1)) == T)
{
  r101 <- 4
  break
} else
{
  r101 <- 0   
} 

a12 <- system.time({
  #Temporary
  Loop.track[[length(Loop.track) + 1]] <- c(a,b,cc,d,e,f,g)
  MU.track[[length(MU.track) + 1]] <- MU.List
  mgmtUnit.track[[length(mgmtUnit.track) + 1]] <- loopB.new_mgmtUnit
})#a12

} else
{
a13 <- system.time({
  #No new stand numbers, enter a placeholder if there were no treatments for year[a].
  if(length(Treatment.History) == 0)
  {
    Treatment.History[[(length(Treatment.History)+1)]] <- "No Data."
    names(Treatment.History)[[length(Treatment.History)]] <- paste(
      "No treatments this year.", collapse = "")
  }
})#a13
}

if(length(diar) == 0)
{ #1.1.1 ---------------------------------------------------------------------------
  e <- 0
  title <- "None"
  da <- 0
  pm <- "NA"
  #Log wildland fire run data for fires in year[a].
  Disturbance.History[[(length(Disturbance.History)+1)]] <- paste(
    "No fires for this year", collapse = "")
  
  #Add header to list.
  #Add header to list.
  names(Disturbance.History)[[length(Disturbance.History)]] <- paste(c(
    "Disturbance Number: ",e," ### Disturbance Type: ", 
    title," ### Disturbance Area: ", 
    tda[e], " ### Area Mapped: ", da, 
    " ### Completed: ", pm, "% ###"), 
    collapse = "")
} else #1.1.1 ---------------------------------------------------------------------------
  { #1.1.2 ---------------------------------------------------------------------------
  #LOOP 888888888888888888888888888888888888888888888888888888888888888888888888888888
  #Loop 8 (by disturbances within year[a]). This loop runs all disturbances for 
  #year[a].
  for (e in tdn[tdy == a])#e <- 2
  { #8.0.0 ---------------------------------------------------------------------------
    
    #TESTING ONLY
    e1 <- 0
    e2 <- 0
    e3 <- 0
    e4 <- 0
    e5 <- 0
    UnitList <- list()
   forceBurnOut <- 0
    
    g.g <- vector(length =, mode = 'numeric')
    e1 <- system.time({ 
      
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
      
      #Pre-run Loop 9 number (used in tracking devices).
      f <- 0
      
      #These objects record data for each mapping iteration.
      loop <- vector(mode = "character", length = 1)
      Unit <- vector(mode = "numeric", length = 1)
      Iteration.f <- vector(mode = "numeric", length = 1)
      Explanation.f <- vector(mode = "character", length = 1)
      Iteration.g <- vector(mode = "numeric", length = 1)
      Explanation.g <- vector(mode = "numeric", length = 1)
      Disturbance.Area <- vector(mode = "numeric", length = 1)
      PrctDist.Mapped <- vector(mode = "numeric", length = 1)
      
    })#e1
    
    e2 <- system.time({
      #OLD LOCATION OF loopE.snO CODE.
      
      #Object shows the number of pixels for disturbance[e].
      desa <- round(tda[e],0)
      
      #Object shows fuelbeds available for establishment by disturbance[e].
      avfb <- fbls[!fbls %in% Non.Flammable]
      
    })#e2
    
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
          f1 <- 0
          f2 <- 0
          f3 <- 0
          f4 <- 0
          f5 <- 0
          f6 <- 0
          f7 <- 0
          f8 <- 0
          f9 <- 0
          f10 <- 0
          f11 <- 0
          f12 <- 0
          f13 <- 0
          f14 <- 0
          f15 <- 0
          f16 <- 0
          f17 <- 0
          f18 <- 0
          check <- ifelse(f == 1, 0, check)#TEMP - REMOVE
          breaks <- ifelse(f == 1, 0, breaks)#TEMP - REMOVE
          if(forceBurnOut == 1 | f == 1)
          {
            spread.type <- 0
          } else
          {
            spread.type <- spread.type
          }
          
          f1 <- system.time({          
            #Default g loop number.
            
            
            g <- 0
            h <- 0
            
            #Set up an intra loop tracking device for overwritten stand numbers.
            #This will be fed into loopE.ReplacedStands
            osnd <- vector(length = 0, mode = "numeric")
            
            #Set up an intra loop tracking device for overwritten coordinates.
            ocod <- vector(length = 0, mode = "numeric")
            
          })#f1
          
          f2 <- system.time({
            
            #Tracks the highest stand number. Used to assign stand numbers to new disturbance.
            #beneath loop 7. Can't place this below loop 7 in case disturbance erases stand with
            #highest number.
            mudn <- max(fire.stand,max(unique(as.vector(s.map))))
            
            #loop 10/11 tracker
            #spread.type <- ifelse(f == 1, 0, spread.type)
            
  #Record area occupied by disturbance[e].
          dema <- length(s.map[s.map %in% loopF.NewStand])
})#f2 

          #Ends loop when disturbance[e] has been completely mapped.
          if(dema < desa)
          { #9.1.1 ---------------------------------------------------------------------------

            f3 <- system.time({        
              #Updated here in case any original stands have been completely overwritten.
              #Exclude the no data unit, stands created in this year's treatment loop, and
              #stands created from previously burned areas this year.
              loopA.snO <- sort(unique(as.vector(s.map[!s.map %in% c(NoData.Unit,
                                                                     loopB.new_stand, 
                                                                     loopE.NewStand, 
                                                                     loopF.NewStand)])))
            })#f3
            
            if(spread.type == 0)
            { #9.2.1---------------------------------------------------------------
            
f4 <- system.time({  
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
})#f4
        
            if(tdc[e] == 1)
            { #EAFB -----------------------------------------------------------

f5 <- system.time({ 
  #Create seperate stand lists for the buffer and eglin zones
              ss2.e <- sort(unique(as.vector(
                s.map[s.map %in% ss2 & b.map %in% 
                        sort(unique(b.map[!b.map %in% c(NoData.Unit, Buffer.Unit)]))])))
})#f5 
              
              if(length(ss2.e) > 0)
              {

f6 <- system.time({ 
                #Next, create a list of unique fuelbeds within these stands.
                f.summary <- sort(unique(Fuelbed.List[Stand.List %in% ss2.e]))
              
                #Areas associated with fuelbeds in f.summary.
                a.summary <- mapply(function(x) sum(Area.List[Fuelbed.List == x]),
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
})#f6
                #cbind(f.summary,a.summary,p.temp,p.summary,v.summary,fire.start)
                #sum(p.summary)
              } else
              {
f6 <- system.time({  
                fire.start <- vector(length=0,mode='numeric')
})#f6              
              }
            } else #EAFB -----------------------------------------------------------
            { #Buffer -----------------------------------------------------------
              f5 <- system.time({ 
              ss2.b <- sort(unique(as.vector(
                s.map[s.map %in% ss2 & b.map == Buffer.Unit])))
              
              })#f5
              
              if(length(ss2.b) > 0)
              {
                f6 <- system.time({ 
                #Next, create a list of unique fuelbeds within these stands.
                f.summary <- sort(unique(Fuelbed.List[Stand.List %in% ss2.b]))
               
                #Areas associated with fuelbeds in f.summary.
                a.summary <- mapply(function(x) sum(Area.List[Fuelbed.List == x]),
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
                })#f6
                #cbind(f.summary,a.summary,p.temp,p.summary,v.summary,fire.start)
                #sum(p.summary)
              } else
              {
                f6 <- system.time({ 
                fire.start <- vector(length=0,mode='numeric')
                })#f6
              }
            } #Buffer -----------------------------------------------------------
          
#REMOVED F10 - NO LONGER NECESSARY, s.profile is still an object but is produced in a4, which
#is now located below 9.4.2


          } else #9.2.1---------------------------------------------------------------
{ #9.2.2---------------------------------------------------------------
  save.desa <- desa
} #9.2.2---------------------------------------------------------------
            #Ends loop if there are no locations to establish disturbance[e] where fuelbed 
            #requirements, wilderness designations, and stand numbers check out. 
            if(length(f.map[f.map %in% dtxm[,2] & s.map %in% loopA.snO]) > 0 & length(fire.start) > 0)
              #need something that measures previously assigned cells
            { #9.3.1 ---------------------------------------------------------------------------
  
              if(spread.type != 11)
              { #9.4.1---------------------------------------------------------------
              
                f7 <- system.time({           
              
              #Generates seed cell for disturbance[e]
              #scd object hold the fire location for disturbance[e].
              scd <- vector(mode = "numeric", length = 0)
  })#f7
  
  if(spread.type == 0)
  {#9.5.1 ---------------------------------------------------------------------------
  f8 <- system.time({
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
              {abu <- c(Unmanaged.Unit,sort(unique(b.map[b.map < Buffer.Unit & b.map > 0])))} else
              {abu <- Buffer.Unit}

              #Select a seed cell
    scd <- resample(l.map[f.map %in% ignition.fuelbed & s.map %in% loopA.snO & 
                                      b.map %in% abu],1) 
              
              #Establish disturbance[e] in s.map and record old stand number
              f.bun <- b.map[scd]
              a.bun <- round(((length(b.map[b.map == f.bun & s.map %in% ss2])) * 
                          rbeta(1,shape1[3],shape2[3])),0)
    scd <- vector(mode = "numeric", length = 0)
    scd <- resample(l.map[b.map == f.bun & s.map %in% ss2], 
                    round(max((a.bun * seed.cells[3]),1),0))
    osnd <- c(osnd,s.map[scd[1]])
    scd.1 <- s.map[scd[1]]#used if this fire moves directly to unit burn loop H/11.
    ocod <- c(ocod, scd[1])
    s.map[scd[1]] <- tesn
              #Used for measuring area of fire in tracking objects
              tesn_cum <- tesn
  })#f8
    } else #9.5.1 ---------------------------------------------------------------------------
    { #9.5.2 ---------------------------------------------------------------------------
      #Code runs when a wildfire has burned beyond the original "block and burn" boundary.
      f9 <- system.time({
      #Identify management units fire has spread into.
      f.bun <- sort(unique(b.map[s.map %in% neef]))
      
      #Remove units if they have been completely burned in loop 11.
      f.bun <- f.bun[!f.bun %in% burned.units]
      
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
                      round(max((a.bun * seed.cells[3]),1),0))
      
      #Establish disturbance[e] in s.map and record old stand number
      #osnd <- c(osnd,s.map[scd])
      #ocod <- c(ocod, scd)
      #s.map[scd] <- tesn -- don't do this until you know the units will be intentionally burned.
      tesn <- c(tesn, neef)
      })#f9
    } #9.5.2 ---------------------------------------------------------------------------

              } else #9.4.1---------------------------------------------------------------
{ #9.4.2---------------------------------------------------------------
  desa <- 1
} #9.4.2---------------------------------------------------------------


f10 <- system.time({
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
})#f10
  
  if((desa-dema) < round((a.bun/3),0))
  {#9.6.1 (WILDFIRE LOOP)--------------------------------------------------------------
  f11 <- system.time({    
    
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
              
  })#f11
              #LOOP 10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10-10
              #Loop 10 (by iterations). This loop keeps trying to grow disturbance[e] until 
              #growth stops.
              for (g in 1:r.max)#g <- 1
              { #10.0.0 --------------------------------------------------------------------------
                g1 <- 0
                g2 <- 0
                g3 <- 0
                g4 <- 0
                g5 <- 0
                g6 <- 0
                g7 <- 0
                g8 <- 0
                g9 <- 0
                g10 <- 0
                g11 <- 0
                g12 <- 0
                g13 <- 0
                g14 <- 0
                g15 <- 0
                g16 <- 0
                g17 <- 0
                g18 <- 0
                sts <- spread.type#TEMP - REMOVE
                
g1 <- system.time({         
                #Area mapped by iteration[f].
                #dema <- length(as.vector(s.map[s.map %in% loopF.NewStand]))
                dema <- sum(loopF.Area)
})#g1
                #This statement stops loop 7 when disturbance[e] has been fully mapped.
                if((dema + length(ocod)) < desa) 
                { #10.1.1 --------------------------------------------------------------------------

g2 <- system.time({              
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
                  burn.out <- 3#iterations after which pixel burns out
                  if(spread.type == 11 & g == 1)
                  {
                    tesn <- tesn_cum
                  } else
                  {
                  tesn <- sort(unique(as.vector(s.map[s.map < min(0,((g*-1)+burn.out)) & s.map > NoData.Unit])))
                  }

                  })#g2
                  if(length(tesn) > 0)
                  {#10.2.1 --------------------------------------------------------------------------

g3 <- system.time({               
                   #Set row number for spread tables
                   tsl <- length(s.map[s.map %in% tesn])
})#g3
g4 <- system.time({             
                   #Object shows locations of 48 pixels surrounding each mapped pixel for 
                   #disturbance[e].
                   malo <- matrix(data = sn.seeker(search.set[,1], search.set[,2]),
                                  length(s.map[s.map %in% tesn]),dcl, byrow = T)
})#g4
g5 <- system.time({ 
                   #Obbject shows the upper and lower limits of pixel locations listed in the 'malo' 
                   #object.
                   mali <- matrix(data = sn.limit(search.set[,2], search.set[,1]), 
                                  length(s.map[s.map %in% tesn]),dcl, byrow = T)
})#g5
g6 <- system.time({             
                   #Sets up information in malo and mali objects that replace location values with
                   #limits values if locations "spill" from map edges.
                   mixa <- matrix(data = c(malo[,1],malo[,2],mali[,3],mali[,4],mali[,5],mali[,6],mali[,7],
                                           malo[,8], malo[,9],
                                           malo[,10],malo[,11],malo[,12],mali[,13],mali[,14],mali[,15],mali[,16],mali[,17],mali[,18],
                                           mali[,19],mali[,20],mali[,21],malo[,22],malo[,23],malo[,24],malo[,25],malo[,26],malo[,27],
                                           malo[,28],malo[,29],malo[,30],mali[,31],mali[,32],mali[,33],mali[,34],mali[,35],mali[,36],
                                           mali[,37],mali[,38],mali[,39],mali[,40],mali[,41],mali[,42],mali[,43],malo[,44],malo[,45],
                                           malo[,46],malo[,47],malo[,48]), nrow = tsl, ncol = dcl)
                   
                   mixb <- matrix(data = c(mali[,1],mali[,2],malo[,3],malo[,4],malo[,5],malo[,6],malo[,7],mali[,8],
                                           mali[,9],
                                           mali[,10],mali[,11],mali[,12],malo[,13],malo[,14],malo[,15],malo[,16],malo[,17],malo[,18],
                                           malo[,19],malo[,20],malo[,21],mali[,22],mali[,23],mali[,24],mali[,25],mali[,26],mali[,27],
                                           mali[,28],mali[,29],mali[,30],malo[,31],malo[,32],malo[,33],malo[,34],malo[,35],malo[,36],
                                           malo[,37],malo[,38],malo[,39],malo[,40],malo[,41],malo[,42],malo[,43],mali[,44],mali[,45],
                                           mali[,46],mali[,47],mali[,48]), nrow = tsl, ncol = dcl)
})#g6
g7 <- system.time({ 
                   #Replaces location values with limit values where necessary (first draft - works
                   #on top and bottom of map)
                   co.1 <- matrix(data = ifelse(mixa < mixb,mali,malo), nrow = tsl, ncol = dcl)
})#g7
g8 <- system.time({ 
                   pr.1 <- matrix(data = rep(distance.coefficient^((max(0,(maxWindSpd-windSpd))*(1/maxWindSpd))^2),
                                             length(malo[,1])), nrow = tsl, ncol =dcl, 
                                  byrow = T)
})#g8
g9 <- system.time({ 
  pr.2 <- round(sweep(pr.1, MARGIN = 2, wind, '*')/(max(pr.1 * wind)),3)               
                   #Replace locations currently burning with a zero, they no longer count and can't be
                   #eligible because they will likely have the highest burn probability.
  
  #10/15/2015 update removed redundant code. The "<- 0" take about 0.5 secs each and the g9
  #code segment uses the most amount of run time. Both lines are redundant
  #co.2 <- co.1                
  #co.2[co.2 %in% l.map[l.map %in% co.2 & s.map < 0]] <-  0               
  co.1[co.1 %in% l.map[!s.map %in% loopA.snO]] <-  0                
  pr.3 <- summarize(as.vector(pr.2),as.vector(co.1),sum)                 
  pr.3 <- pr.3[!pr.3[,1] == 0,] 
})#g9
                  } else #10.2.1 --------------------------------------------------------------------------
{#10.2.2 --------------------------------------------------------------------------
g10 <- system.time({
  spread.type <- 0# necessary because this loop isn't producing new burnable area and you
  #need to relocate the fire.
 loop[(length(Iteration.f)+1)] <- "g" 
 Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun) > 1, f.bun[1], f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
 Explanation.f[(length(Explanation.f)+1)] <- paste(
   "10.2.2: Fire burned out.", collapse = "")
 Iteration.g[(length(Iteration.g)+1)] <- g
 Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                     length(ocod),"."), 
                                                   collapse = "")
 Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
 PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
})#g10

ag10[length(ag10) + 1] <- g10[3]

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)
cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__free_",
          g,"__.txt",sep = ""), file = "run_iterations", append = T)#
breaks <- 1022
 break
}#10.2.2 --------------------------------------------------------------------------
#Ends loop if there are no more locations available for disturbance[e].
if(all(pr.3[,2] == 0))
  
{#10.3.1 --------------------------------------------------------------------------
g11 <- system.time({
  loop[(length(Iteration.f)+1)] <- "g" 
  Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun) > 1, f.bun[1], f.bun)
 Iteration.f[(length(Iteration.f)+1)] <- f
 Explanation.f[(length(Explanation.f)+1)] <- paste(
   "10.3.1: Cannot expand block.", collapse = "")
 Iteration.g[(length(Iteration.g)+1)] <- g
 Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                     length(ocod),"."), 
                                                   collapse = "")
 Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
 PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
})#g11
ag11[length(ag11) + 1] <- g11[3]

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)
cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__free_",
          g,"__.txt",sep = ""), file = "run_iterations", append = T)#

breaks <- 1031
 break
} else #10.3.1 ---------------------------------------------------------------------
{ #10.3.2 --------------------------------------------------------------------------

g12 <- system.time({ 
  pr.4 <- as.vector(pr.3[,2])/max(pr.3[,2])
  
  #Reset new.cells object.
  new.cells <- vector(length=0, mode = "numeric")
})#g12
g13 <- system.time({ 
  #This expression picks out which location values are of the same stand and are 
  #available (i.e. they are not occupied by the another disturbance) and makes sure 
  #that the mapped regime does not exceed its prescribed area.
  fual <- s.profile[match(f.map[l.map %in% pr.3[,1]], f.probability[,1])]
})#g13
g14 <- system.time({ 
  pr.5 <- pr.4 * fual
})#g14
g15 <- system.time({ 
  #Produces a list with locations selected for each fuelbed.
  new.cells <- unlist(mapply(function(y){
    pr.3[y,1][resample(c(0,1), size = 1, replace = T, prob = c(1-pr.5[y], pr.5[y])) == 1]
  },1:length(pr.3[,1])))
})#g15
g16 <- system.time({ 
  #Scale back the number of new cells if it will exceed area to be burned.
  if((dema + length(ocod) + length(new.cells)) <= desa)
  {
    new.cells <- new.cells
  } else
  {
    #If the number of new cells + dema exceeds dema then reduce the number of new cells.
    new.cells <- resample(new.cells, (desa-(dema + length(ocod))))
  }
})#g16
g17 <- system.time({ 
  osnd <- c(osnd, s.map[new.cells]) #tracks stand numbers involved in disturbance.
  ocod <- c(ocod, new.cells) #tracks coordinates involved in disturbance.
  s.map[new.cells] <- ((g*-1)-1) #maps disturbance.

  if(spread.type == 11 & g >= 4 & length(unique(b.map[l.map %in% ocod])) > length(burned.units))
  {
    spread.type <- 12
    tesn_cum <- c(tesn_cum,((g*-1)-1))#update values representing pixels burned in this fire.
  } else
  {
    tesn_cum <- c(tesn_cum,((g*-1)-1))#update values representing pixels burned in this fire.
  }
  
})#g17

} #10.3.2 --------------------------------------------------------------------------
                } else #10.1.1 ---------------------------------------------------------------------
{ #10.1.2 --------------------------------------------------------------------------
  g18 <- system.time({ 
    loop[(length(Iteration.f)+1)] <- "g" 
    Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), length(f.bun), f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
  Explanation.f[(length(Explanation.f)+1)] <- paste(
    "10.1.2: Mapping Complete.", collapse = "")
  Iteration.g[(length(Iteration.g)+1)] <- g 
  Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                      length(ocod),"."), 
                                                    collapse = "")
  Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
  PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
  })#g18
  ag18[length(ag18) + 1] <- g18[3]
  
  #Save run data.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__free_",
            g,"__.txt",sep = ""), file = "run_iterations", append = T)#
  
  breaks <- 1012
  break
} #10.1.2 --------------------------------------------------------------------------

if(spread.type == 12)
{#10.4.1
  ag1[length(ag1) + 1] <- g1[3]
  ag2[length(ag2) + 1] <- g2[3]
  ag3[length(ag3) + 1] <- g3[3]
  ag4[length(ag4) + 1] <- g4[3]
  ag5[length(ag5) + 1] <- g5[3]
  ag6[length(ag6) + 1] <- g6[3]
  ag7[length(ag7) + 1] <- g7[3]
  ag8[length(ag8) + 1] <- g8[3]
  ag9[length(ag9) + 1] <- g9[3]
  #g10 --- break
  #g11 --- break
  ag12[length(ag12) + 1] <- g12[3]
  ag13[length(ag13) + 1] <- g13[3]
  ag14[length(ag14) + 1] <- g14[3]
  ag15[length(ag15) + 1] <- g15[3]
  ag16[length(ag16) + 1] <- g16[3]
  ag17[length(ag17) + 1] <- g17[3]
  #g18 --- break
  
  loop[(length(Iteration.f)+1)] <- "g" 
  Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
  Explanation.f[(length(Explanation.f)+1)] <- paste(
    "10.4.1: Transition out of Wildfire", collapse = "")
  Iteration.g[(length(Iteration.g)+1)] <- g 
  Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                      length(ocod),"."), 
                                                    collapse = "")
  Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
  PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
  
  #Save run data.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__free_",
            g,"__.txt",sep = ""), file = "run_iterations", append = T)#
  
  breaks <- 1041
  break
} else#10.4.1
{#10.4.2
  ag1[length(ag1) + 1] <- g1[3]
ag2[length(ag2) + 1] <- g2[3]
ag3[length(ag3) + 1] <- g3[3]
ag4[length(ag4) + 1] <- g4[3]
ag5[length(ag5) + 1] <- g5[3]
ag6[length(ag6) + 1] <- g6[3]
ag7[length(ag7) + 1] <- g7[3]
ag8[length(ag8) + 1] <- g8[3]
ag9[length(ag9) + 1] <- g9[3]
#g10 --- break
#g11 --- break
ag12[length(ag12) + 1] <- g12[3]
ag13[length(ag13) + 1] <- g13[3]
ag14[length(ag14) + 1] <- g14[3]
ag15[length(ag15) + 1] <- g15[3]
ag16[length(ag16) + 1] <- g16[3]
ag17[length(ag17) + 1] <- g17[3]
#g18 --- break

loop[(length(Iteration.f)+1)] <- "g" 
Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
Iteration.f[(length(Iteration.f)+1)] <- f
Explanation.f[(length(Explanation.f)+1)] <- paste(
  "10.4.2: Block is running.", collapse = "")
Iteration.g[(length(Iteration.g)+1)] <- g 
Explanation.g[(length(Explanation.g)+1)] <- paste(c("Expansion. New pixels: ",
                                                    length(new.cells),"."), 
                                                  collapse = "")
Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)
cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__free_",
          g,"__.txt",sep = ""), file = "run_iterations", append = T)#
breaks <- 1042
}#10.4.2

              } #10.0.0 --------------------------------------------------------------------------

} else #New -- A.1.1 (WILDFIRE LOOP)--------------------------------------------------------------
{#New -- A.1.2 (RX FIRE LOOP)--------------------------------------------------------------
 
 
 #Establish disturbance[e] in s.map and record old stand number
 if(spread.type == 12)
 {
   osnd <- c(osnd,s.map[scd])
   ocod <- c(ocod, scd)
   s.map[scd] <- s.map[scd] * tesn_t
 } else
 {
   if(spread.type == 0)
   {
     osnd <- c(osnd,s.map[scd[2:length(scd)]])
     ocod <- c(ocod, scd[2:length(scd)])
     s.map[scd[1]] <- scd.1
     s.map[scd] <- s.map[scd] * tesn_t
     tesn <- c(tesn, sort(unique(s.map[scd])))
   } else
   {
     osnd <- c(osnd,s.map[scd[2:length(scd)]])
     ocod <- c(ocod, scd[2:length(scd)])
     s.map[scd] <- s.map[scd] * tesn_t
   }
 }
 
#LOOP 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 
 #Loop 11 (by iterations). This loop keeps growing fire[e] in block[f] 
 #until growth stops.
 for (h in 1:r.max)#h <- 2
 { #11.0.0 ---------------------------------------------------------------------------
   h1 <- 0
   h2 <- 0
   h3 <- 0
   h4 <- 0
   h5 <- 0
   h6 <- 0
   h7 <- 0
   h8 <- 0
   h9 <- 0
   h10 <- 0
   h11 <- 0
   h12 <- 0
   h13 <- 0
   sts <- spread.type#TEMP - REMOVE
   
   h1 <- system.time({          
     #Area mapped for fire[e].
     dema <- length(as.vector(s.map[s.map %in% loopF.NewStand]))
   })#h1
   
   #This statement stops loop 11 when treatment[b] has been fully mapped.
   if((dema + length(ocod)) < desa) 
   { #11.1.1 ---------------------------------------------------------------------------
     
     
     h2 <- system.time({
       #Object shows locations of 8 pixels surrounding each mapped pixel for 
       #disturbance[e].
       malo <- matrix(data = sn.seeker(search.set[1:8,1], search.set[1:8,2]),
                      length(s.map[s.map %in% tesn]),dcl_t, byrow = T)
     })#h2
     h3 <- system.time({   
       #Object shows the upper and lower limits of pixel locations listed in the 'malo' 
       #object.
       mali <- matrix(data = sn.limit(search.set[1:8,2], search.set[1:8,1]), 
                      length(s.map[s.map %in% tesn]),dcl_t, byrow = T)
       
     })#h3
     h4 <- system.time({  
       #Sets up information in malo and mali objects that replace location values with
       #limits values if locations "spill" from map edges.
       mixa <- c(malo[,1],malo[,2],mali[,3],mali[,4],mali[,5],mali[,6],mali[,7],malo[,8])
       mixb <- c(mali[,1],mali[,2],malo[,3],malo[,4],malo[,5],malo[,6],malo[,7],mali[,8])
     })#h4
     h5 <- system.time({ 
       #Replaces location values with limit values where necessary (first draft - works
       #on top and bottom of map)
       fdlo <- ifelse(mixa < mixb,mali,malo)
     })#h5
     h6 <- system.time({  
       #Replaces location values with limit values where necessary (second draft - works
       #on sides of map)
       sdlo <- ifelse(fdlo < 1,fdlo + rows,ifelse(fdlo > length(s.map),fdlo - rows,fdlo))
     })#h6
     h7 <- system.time({          
       #This object shows all unique locations available for establishment by treatment[b].
       avlo <- unique(as.vector(l.map[sdlo][l.map[sdlo] %in% l.map[b.map %in% f.bun & s.map %in% ss2]]))
     })#h7
     
     #Ends loop if there are no more locations available for treatment[b] in the 
     #block[cc] that is currently being mapped.
     if(length(avlo) > 0)
     { #11.2.1 ---------------------------------------------------------------------------
       #Reset new.cells object.
       new.cells <- vector(length=0, mode = "numeric")
       
       h8 <- system.time({                       
         #This expression picks out which location values are of the same stand and are 
         #available (i.e. they are not occupied by the another treatment) and makes sure 
         #that the mapped regime does not exceed its prescribed area.
         if((length(ocod) + length(avlo)) <= a.bun)#--------------------------A
           {#-----------------------------------------------------------------A-TRUE
           if((dema + length(ocod) + length(avlo)) <= desa)#------------------B
             {#---------------------------------------------------------------B-TRUE
             new.cells <- avlo
             s.map[new.cells] <- s.map[new.cells]*tesn_t
             tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
             } else#----------------------------------------------------------B-TRUE
               {#-------------------------------------------------------------B-FALSE
                 new.cells <- resample(avlo,(desa - (dema + length(ocod))))
                 s.map[new.cells] <- s.map[new.cells]*tesn_t
                 tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                 }#-----------------------------------------------------------B-FALSE
           } else #-----------------------------------------------------------A-TRUE
             {#---------------------------------------------------------------A-FALSE
               if((dema + length(ocod) + length(avlo)) <= desa)#--------------C
                 {#-----------------------------------------------------------C-TRUE
                 new.cells <- resample(avlo,(a.bun - length(ocod)))
                 s.map[new.cells] <- s.map[new.cells]*tesn_t
                 tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                 spread.type <- 11
                 } else#------------------------------------------------------C-TRUE
                   {#---------------------------------------------------------C-FALSE
                     if(a.bun < desa)#----------------------------------------D
                       {#-----------------------------------------------------D-TRUE
                       new.cells <- resample(avlo,(a.bun - length(ocod)))
                       s.map[new.cells] <- s.map[new.cells]*tesn_t
                       tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                       spread.type <- 11
                       } else#------------------------------------------------D-TRUE
                         {#---------------------------------------------------D-FALSE
                           new.cells <- resample(avlo,(desa - (dema + length(ocod))))
                           s.map[new.cells] <- s.map[new.cells]*tesn_t
                           tesn <- unique(s.map[s.map < 0 & s.map > NoData.Unit])
                           }#-------------------------------------------------D-FALSE
                     }#-------------------------------------------------------C-FALSE
               }#-------------------------------------------------------------A-FALSE
       })#h8
       h9 <- system.time({ 
         osnd <- c(osnd, s.map[new.cells]) #tracks stand numbers involved in disturbance.
         ocod <- c(ocod, new.cells) #tracks coordinates involved in disturbance.
       })#h9
     } else #11.2.1 ----------------------------------------------------------------------
     
{#11.2.2
  h10 <- system.time({
    loop[(length(Iteration.f)+1)] <- "h" 
    Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun) > 1, length(f.bun), f.bun)
    Iteration.f[(length(Iteration.f)+1)] <- f
    Explanation.f[(length(Explanation.f)+1)] <- paste(
      "11.2.2: Fire burned out.", collapse = "")
    Iteration.g[(length(Iteration.g)+1)] <- g
    Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                        length(ocod),"."), 
                                                      collapse = "")
    Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
    PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
  })#h10
  ah10[length(ah10) + 1] <- h10[3]
  
  #Save run data.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__blocked_",
            h,"__.txt",sep = ""), file = "run_iterations", append = T)#

#NOTE (12/6/2015)
#Fire has burned out and must be reassigned to a new area. Use spread.type = 0 to
#direct loop 9 into section that will locate anew scd
  spread.type <- 0

  breaks <- 1122
  break
} #11.2.2 ---------------------------------------------------------------------------

   } else #11.1.1 ----------------------------------------------------------------------

{ #11.1.2 ---------------------------------------------------------------------------
  h11 <- system.time({
    loop[(length(Iteration.f)+1)] <- "h" 
    Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun) > 1, length(f.bun), f.bun)
    Iteration.f[(length(Iteration.f)+1)] <- f
    Explanation.f[(length(Explanation.f)+1)] <- paste(
      "11.1.2: Mapping Complete.", collapse = "")
    Iteration.g[(length(Iteration.g)+1)] <- h
    Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                        length(ocod),"."), 
                                                      collapse = "")
    Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
    PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
  })#h11
  ah11[length(ah11) + 1] <- h11[3]
  
  #Save run data.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__blocked_",
            h,"__.txt",sep = ""), file = "run_iterations", append = T)#
  breaks <- 1112
  break
} #11.1.2 ---------------------------------------------------------------------------




if(spread.type == 11)
{#11.3.1
  h12 <- system.time({
  burned.units <- c(burned.units, f.bun)
  ah1[length(ah1) + 1] <- h1[3]
  ah2[length(ah2) + 1] <- h2[3]
  ah3[length(ah3) + 1] <- h3[3]
  ah4[length(ah4) + 1] <- h4[3]
  ah5[length(ah5) + 1] <- h5[3]
  ah6[length(ah6) + 1] <- h6[3]
  ah7[length(ah7) + 1] <- h7[3]
  ah8[length(ah8) + 1] <- h8[3]
  ah9[length(ah9) + 1] <- h9[3]
  #h10 --- break
  #h11 --- break
  #h12 --- break
  #h13 --- break
  
  loop[(length(Iteration.f)+1)] <- "h" 
  Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
  Explanation.f[(length(Explanation.f)+1)] <- paste(
    "11.3.1: Transition out of Block & Burn", collapse = "")
  Iteration.g[(length(Iteration.g)+1)] <- h 
  Explanation.g[(length(Explanation.g)+1)] <- paste(c("End expansion. Total: ",
                                                      length(ocod),"."), 
                                                    collapse = "")
  Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
  PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)
  
  })#h12
  ah12[length(ah12) + 1] <- h12[3]
  
  #Save run data.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__blocked_",
            h,"__.txt",sep = ""), file = "run_iterations", append = T)#
  breaks < - 1131
  break
} else #11.3.1
{#11.3.2

  h13 <- system.time({
ah1[length(ah1) + 1] <- h1[3]
ah2[length(ah2) + 1] <- h2[3]
ah3[length(ah3) + 1] <- h3[3]
ah4[length(ah4) + 1] <- h4[3]
ah5[length(ah5) + 1] <- h5[3]
ah6[length(ah6) + 1] <- h6[3]
ah7[length(ah7) + 1] <- h7[3]
ah8[length(ah8) + 1] <- h8[3]
ah9[length(ah9) + 1] <- h9[3]
#h10 --- break
#h11 --- break
#h12 --- break
#h13 --- break

#Register mapping data after Loop 11 has finished running for iteration[d].
loop[(length(Iteration.f)+1)] <- "h" 
Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
Iteration.f[(length(Iteration.f)+1)] <- f
Explanation.f[(length(Explanation.f)+1)] <- paste(
  "11.3.2: Block is running.", collapse = "")
Iteration.g[(length(Iteration.g)+1)] <- h
Explanation.g[(length(Explanation.g)+1)] <- paste(c("Expansion. New pixels: ",
                                                    length(new.cells),"."), collapse = "")
Disturbance.Area[(length(Disturbance.Area)+1)] <- dema + length(ocod)
PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round((((dema + length(ocod))/desa)*100),1)

  })#h13
ah13[length(ah13) + 1] <- h13[3]

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)

cat(paste("run_", run,"_", dt,"_",tm,"_year_",a,"__wildfire_",e, "__block_",f,"__blocked_",
          h,"__.txt",sep = ""), file = "run_iterations", append = T)#

breaks <- 1132
}#11.3.2

 } #11.0.0 ---------------------------------------------------------------------------

}#New -- A.1.2 (RX FIRE LOOP)--------------------------------------------------------------

#NEW F.1.2---------------------------------------------------------------
f12 <- system.time({

#Reset tesn, it should be -1 for FDM, except in loop 10 where it must include multiple
#values to support the burn out function.
tesn <- -1#temporary stand number.

osnd <- abs(osnd)

#Show fuelbeds in treatment[b], block[cc].
#effb <- list()
#effb <- mapply(function(y) 
#{
#  Fuelbed.List[Stand.List == y]
#}, osnd)

#Show fuelbeds associated with stands affected by fire
fb_f12 <- Fuelbed.List[Stand.List %in% osnd]

#List corresponding stand numbers.
osno <- Stand.List[Stand.List %in% osnd]

#Expand fuelbeds to include each occurence of stand number in osnd
effb <- fb_f12[match(osnd, osno)]

})#f12

f13 <- system.time({ 
#Calculate number of new stands
noef <- length(osno)

#Determine new stand numbers for treatment[b], block[cc].
if(noef == 0)
{
  neef <- 0
} else
{
  neef <- seq((mudn + 1), (mudn + noef), 1)
}
})#f13

f14 <- system.time({ 
#Map new stands
  od <- data.frame(ocod, osnd)
  od <- od[order(od$ocod),]
  v.neef <- neef[match(od$osnd, osno)]
  s.map[l.map %in% od$ocod] <- v.neef
 
#  for(i in 1:noef)
#{s.map[l.map %in% ocod[osnd == osno[i]]] <- neef[i]}
})#f14

f15 <- system.time({ 
#Log new stand numbers and associated disturbances when they have been added to 
#s.map.
  if(g == 0)
  {
    check <- as.numeric(paste(f,h, sep = ""))  
  } else
  {
  check <- as.numeric(paste(f,g, sep = ""))
  }

  if(sum(neef) == 0)
  {
    loopF.NewStand <- loopF.NewStand
    loopF.Area <- loopF.Area
  } else
  {
    loopF.NewStand <- c(loopF.NewStand,neef)
    l.neef <- rep(1,length(v.neef))
    s.neef_a <- summarize(l.neef, v.neef, sum)
    s.neef <- as.vector(s.neef_a[,2])
    loopF.Area <- c(loopF.Area, s.neef)
  }
  
loopF.ReplacedStand <- c(loopF.ReplacedStand, osno)
loopF.E_no <- c(loopF.E_no, rep(e, length(osno)))
loopF.F_no <- c(loopF.F_no, rep(f, length(osno)))
loopF.G_no <- c(loopF.G_no, rep(g, length(osno)))

#Temporary - 12/6-2015 used to shut down loop when error and warning in run 1028 occurs.
loopF1 <- data.frame(L1 = loopF.NewStand, L2 = loopF.Area, L3 = loopF.ReplacedStand, 
                    L4 = loopF.E_no, L5 = loopF.F_no, L6 = loopF.G_no)
})#f15

f16 <- system.time({ 
UnitList[[f]] <- f.bun
loop[(length(Iteration.f)+1)] <- ifelse(g == 0,"h","g") 
Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
Iteration.f[(length(Iteration.f)+1)] <- f
Explanation.f[(length(Explanation.f)+1)] <- paste(
  "9.0.0: Break in expansions.", collapse = "")
Iteration.g[(length(Iteration.g)+1)] <- ifelse(g == 0,h,g) 
Explanation.g[(length(Explanation.g)+1)] <- "Block running."
Disturbance.Area[(length(Disturbance.Area)+1)] <- length(
  s.map[s.map %in% c(loopF.NewStand)])
PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round(((Disturbance.Area[
  length(Disturbance.Area)]/desa)*100),1)
})#f16

f17 <- system.time({ 
  g.g <- sum(g.g, ifelse(g == 0,h,g))#tracks expansions
})#f17

af1[length(af1) + 1] <- f1[3]
af2[length(af2) + 1] <- f2[3]
af3[length(af3) + 1] <- f3[3]
af4[length(af4) + 1] <- f4[3]
af5[length(af5) + 1] <- f5[3]
af6[length(af6) + 1] <- f6[3]
af7[length(af7) + 1] <- f7[3]
af8[length(af8) + 1] <- f8[3]
af9[length(af9) + 1] <- f9[3]
af10[length(af10) + 1] <- f10[3]
af11[length(af11) + 1] <- f11[3]
af12[length(af12) + 1] <- f12[3]
af13[length(af13) + 1] <- f13[3]
af14[length(af14) + 1] <- f14[3]
af15[length(af15) + 1] <- f15[3]
af16[length(af16) + 1] <- f16[3]
af17[length(af17) + 1] <- f17[3]
#f18 --- break

##############################################################################
##############################################################################
##############################################################################
#ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                 #
#DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                #

if(length(unique(loopF.NewStand)) != length(loopF.NewStand) | 
     length(loopF.ReplacedStand) != length(loopF.NewStand))
{  
  r101 <- 2
  break
} else
{
  r101 <- 0 
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


            } else #9.3.1 ----------------------------------------------------------------------
{ #9.3.2 ---------------------------------------------------------------------------
  f12 <- system.time({
    
    #Reset tesn, it should be -1 for FDM, except in loop 10 where it must include multiple
    #values to support the burn out function.
    tesn <- -1#temporary stand number.
    
    osnd <- abs(osnd)
    
    #Show fuelbeds in treatment[b], block[cc].
    #effb <- list()
    #effb <- mapply(function(y) 
    #{
    #  Fuelbed.List[Stand.List == y]
    #}, osnd)
    
    #Show fuelbeds associated with stands affected by fire
    fb_f12 <- Fuelbed.List[Stand.List %in% osnd]
    
    #List corresponding stand numbers.
    osno <- Stand.List[Stand.List %in% osnd]
    
    #Expand fuelbeds to include each occurence of stand number in osnd
    effb <- fb_f12[match(osnd, osno)]
    
  })#f12
  
  f13 <- system.time({ 
    #Unique old stands
    osno <- sort(unique(osnd))
    
    #Calculate number of new stands
    noef <- length(osno)
    
    #Determine new stand numbers for treatment[b], block[cc].
    if(noef == 0)
    {
      neef <- 0
    } else
    {
      neef <- seq((mudn + 1), (mudn + noef), 1)
    }
  })#f13
  
  f14 <- system.time({ 
    #Map new stands
    od <- data.frame(osnd = osnd, ocod = ocod)
    od <- od[order(od$ocod),]
    v.neef <- neef[match(od.b$osnd, osno)]
    s.map[s.map %in% osno] <- v.neef
    #for(i in 1:noef)
    #{s.map[l.map %in% ocod[osnd == osno[i]]] <- neef[i]}
  })#f14
  
  f15 <- system.time({ 
    #Log new stand numbers and associated disturbances when they have been added to 
    #s.map.
    if(g == 0)
    {
      check <- as.numeric(paste(f,h, sep = ""))  
    } else
    {
      check <- as.numeric(paste(f,g, sep = ""))
    }
    
    if(sum(neef) == 0)
    {
      loopF.NewStand <- loopF.NewStand
      loopF.Area <- loopF.Area
      } else
        {
          loopF.NewStand <- c(loopF.NewStand,neef)
          l.neef <- rep(1,length(v.neef))
          s.neef_a <- summarize(l.neef, v.neef, sum)
          s.neef <- as.vector(s.neef_a[,2])
          loopF.Area <- c(loopF.Area, s.neef)
        }
    
    loopF.ReplacedStand <- c(loopF.ReplacedStand, osno)
    loopF.E_no <- c(loopF.E_no, rep(e, length(osno)))
    loopF.F_no <- c(loopF.F_no, rep(f, length(osno)))
    loopF.G_no <- c(loopF.G_no, rep(g, length(osno)))
    
    #Temporary - 12/6-2015 used to shut down loop when error and warning in run 1028 occurs.
    loopF2 <- data.frame(L1 = loopF.NewStand, L2 = loopF.Area, L3 = loopF.ReplacedStand, 
                        L4 = loopF.E_no, L5 = loopF.F_no, L6 = loopF.G_no)
  })#f15
  
  f16 <- system.time({ 
    UnitList[[f]] <- f.bun
    loop[(length(Iteration.f)+1)] <- "skip" 
    Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
  Explanation.f[(length(Explanation.f)+1)] <- paste(
    "9.3.2: Disturbance cannot expand. End Mapping.", collapse = "")
  Iteration.g[(length(Iteration.g)+1)] <- 0
  Explanation.g[(length(Explanation.g)+1)] <- "Expansion not started."
  Disturbance.Area[(length(Disturbance.Area)+1)] <- length(
    s.map[s.map %in% c(loopF.NewStand)])
  PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round(((Disturbance.Area[
    length(Disturbance.Area)]/desa)*100),1)
  })#f16
  
  f17 <- system.time({ 
    g.g <- sum(g.g, ifelse(g == 0,h,g))#tracks expansions
  })#f17
  
  af1[length(af1) + 1] <- f1[3]
  af2[length(af2) + 1] <- f2[3]
  af3[length(af3) + 1] <- f3[3]
  af4[length(af4) + 1] <- f4[3]
  af5[length(af5) + 1] <- f5[3]
  af6[length(af6) + 1] <- f6[3]
  af7[length(af7) + 1] <- f7[3]
  af8[length(af8) + 1] <- f8[3]
  af9[length(af9) + 1] <- f9[3]
  af10[length(af10) + 1] <- f10[3]
  af11[length(af11) + 1] <- f11[3]
  af12[length(af12) + 1] <- f12[3]
  af13[length(af13) + 1] <- f13[3]
  af14[length(af14) + 1] <- f14[3]
  af15[length(af15) + 1] <- f15[3]
  af16[length(af16) + 1] <- f16[3]
  af17[length(af17) + 1] <- f17[3]
  #f18 --- break_9.1.2
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #ONLY TO DIAGNOSE ERRORS FROM MODEL RUN 101                                 #
  #DRAG ON TIME, REMOVE AFTER ERRORS DIAGNOSED                                #
  
  if(length(unique(loopF.NewStand)) != length(loopF.NewStand) | 
       length(loopF.ReplacedStand) != length(loopF.NewStand))
  {  
    r101 <- 8
  } else
  {
    r101 <- 0 
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
  
  
  break
} #9.3.2 ---------------------------------------------------------------------------
          } else #9.1.1 ----------------------------------------------------------------------
{ #9.1.2 ---------------------------------------------------------------------------
  f18 <- system.time({ 
    UnitList[[f]] <- f.bun
    loop[(length(Iteration.f)+1)] <- ifelse(g == 0,"h","g") 
    Unit[(length(Iteration.f)+1)] <- ifelse(length(f.bun > 1), f.bun[1], f.bun)
  Iteration.f[(length(Iteration.f)+1)] <- f
  Explanation.f[(length(Explanation.f)+1)] <- paste(
    "9.1.2: Disturbance Mapped. End mapping.", collapse = "")
  Iteration.g[(length(Iteration.g)+1)] <- 0
  Explanation.g[(length(Explanation.g)+1)] <- "Expansion not started."
  Disturbance.Area[(length(Disturbance.Area)+1)] <- length(
    s.map[s.map %in% c(loopF.NewStand)]) + length(s.map[l.map %in% ocod])
  PrctDist.Mapped[(length(PrctDist.Mapped)+1)] <- round(((Disturbance.Area[
    length(Disturbance.Area)]/desa)*100),1)
  })#f18
  af18[length(af18) + 1] <- f18[3]
  break
} #9.1.2 ---------------------------------------------------------------------------


##############################################################################
##############################################################################
##############################################################################

        } #9.0.0 ---------------------------------------------------------------------------
e3 <- system.time({
#Log disturbance run data for disturbance[e].
if(length(Iteration.f) > 1)
  Disturbance.History[[(length(Disturbance.History)+1)]] <- data.frame(
    "Loop" = loop,
    "Unit" = Unit, 
    "Blocks" = Iteration.f, "History" = Explanation.f, 
    "Expansions" = Iteration.g, "History" = Explanation.g, 
    "Disturbance Area" = Disturbance.Area, "Percent Completed" = PrctDist.Mapped, 
    stringsAsFactors = F) else
      Disturbance.History[[(length(Disturbance.History)+1)]] <- paste(
        "If this message shows up something weird happened. Error 6.0", collapse = "")

#Add header to list.
names(Disturbance.History)[[length(Disturbance.History)]] <- paste(c(
  "Disturbance Number: ",e," ### Disturbance Type: ", 
  f.disturbances$DisturbanceTitle[tdc[e]]," ### Disturbance Area: ", 
  tda[e], " ### Area Mapped: ", Disturbance.Area[length(Disturbance.Area)], 
  " ### Completed: ", PrctDist.Mapped[length(PrctDist.Mapped)], "% ###"), 
  collapse = "")
})#e3

e4 <- system.time({
#Log new stand numbers and associated disturbances when they have been added to 
#s.map.
loopE.NewStand <- c(loopE.NewStand,loopF.NewStand)
loopE.Area <- c(loopE.Area,loopF.Area)
loopE.ReplacedStand <- c(loopE.ReplacedStand,loopF.ReplacedStand)
loopE.E_no <- c(loopE.E_no, loopF.E_no)
loopE.F_no <- c(loopE.F_no, loopF.F_no)
loopE.G_no <- c(loopE.G_no, loopF.G_no)
loopE <- data.frame(NewStand = loopE.NewStand, 
                    ReplacedStand = loopE.ReplacedStand, 
                    Area = loopE.Area, 
                    E_no = loopE.E_no, 
                    F_no = loopE.F_no,
                    G_no = loopE.G_no)
loopE <- loopE[order(loopE$ReplacedStand),]
})#e4
      } else #8.1.1 ----------------------------------------------------------------------
{ #8.1.2 ---------------------------------------------------------------------------
  #Log disturbance run data for disturbance[e].
  e3 <- system.time({
  if(length(Iteration.f) > 1)
    Disturbance.History[[(length(Disturbance.History)+1)]] <- paste(
      "Loop" = loop,
      "Unit" = Unit, 
      "If this message shows up something weird happened. Error 5.1.", collapse = "") else 
        Disturbance.History[[(length(Disturbance.History)+1)]] <- paste(c(
          "No space available to map disturbance. Mapping was not attempted."), collapse = "")
  #Add header to list.
  names(Disturbance.History)[[length(Disturbance.History)]] <- paste(c(
    "Disturbance Number: ",e," ### Disturbance Type: ", 
    f.disturbances$DisturbanceTitle[tdc[e]]," ### Disturbance Area: ", 
    tda[e], " ### Area Mapped: ", Disturbance.Area[length(Disturbance.Area)], 
    " ### Completed: ", PrctDist.Mapped[length(PrctDist.Mapped)], "% ###"), 
    collapse = "")
  })#e3
  
  e4 <- system.time({
  #Log new stand numbers and associated disturbance when they have been added to 
  #s.map.
  loopE.NewStand <- c(loopE.NewStand,loopF.NewStand)
  loopE.Area <- c(loopE.Area,loopF.Area)
  loopE.ReplacedStand <- c(loopE.ReplacedStand,loopF.ReplacedStand)
  loopE.E_no <- c(loopE.E_no, loopF.E_no)
  loopE.F_no <- c(loopE.F_no, loopF.F_no)
  loopE.G_no <- c(loopE.G_no, loopF.G_no)
  loopE <- data.frame(NewStand = loopE.NewStand, 
                      ReplacedStand = loopE.ReplacedStand, 
                      Area = loopE.Area, 
                      E_no = loopE.E_no, 
                      F_no = loopE.F_no,
                      G_no = loopE.G_no)
  loopE <- loopE[order(loopE$ReplacedStand),]
  })#e4
} #8.1.2 ---------------------------------------------------------------------------
e5 <- system.time({

  #Record date and time.
  dt <- Sys.Date()
  tm <- format(Sys.time(), format = "%H.%M.%S", 
               tz = "", usetz = FALSE)
  
  #Tracking device
  d.summary <- data.frame(
    Date = dt, 
    Time = tm, 
    Year = a, 
    Disturbance_No = e, 
    PercentComplete = round(((e/length(tdn[tdy == a]))*100),), 
    Name = f.disturbances$DisturbanceTitle[tdc[e]],
    Area_Actual = dema, 
    Area_Expected = desa, 
    Blocks = f, 
    Expansions = g.g)
  e.summary <- rbind(e.summary, d.summary)
  
#Save run data.
cat(d.summary, file = "disturbance_summary", append = T)#
})#e5

ae1[length(ae1) + 1] <- e1[3]
ae2[length(ae2) + 1] <- e2[3]
ae3[length(ae3) + 1] <- e3[3]
ae4[length(ae4) + 1] <- e4[3]
ae5[length(ae5) + 1] <- e5[3]

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
                                                                             #
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

#Post run step 2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Update files if there were disturbances in year[a].
if(length(loopE.NewStand) > 0)
{
  a14 <- system.time({
    #Object shows fuelbeds associated with each new stand number created by 
    #disturbances in year[a].
    #ufxDa <- mapply(function(x) unique(f.map[s.map == x]), loopE.NewStand)
    ufxDa_1 <- Fuelbed.List[Stand.List %in% loopE$ReplacedStand]
    usxDa_1 <- Stand.List[Stand.List %in% loopE$ReplacedStand]
    ufxDa <- ufxDa_1[match(loopE$ReplacedStand, usxDa_1)]
    #Storage for fuelbeds and ages of new stands.
    #sfubed <- vector()
    #sagesd <- vector()
  })#a14
  
  a15 <- system.time({
  #Update portion of f.map affected by disturbances for year[a]. 
  #for(i in 1:length(loopE.NewStand))
  #{
    #Updates f.map.
    #f.map[s.map == loopE.NewStand[i]] <- dtxm[,2][fblo[d.post$fuelbed == ufxDa[i]]]
    #replaced with dummy code on Oct 20 b/c this code should be for crown fires.
    #f.map[s.map == loopE.NewStand[i]] <- d.post$fuelbed[fblo[d.post$fuelbed == ufxDa[i]]]
    
    #Forms a list of the new fuelbeds.
    #sfubed[i] <- dtxm[,2][fblo[d.post$fuelbed == ufxDa[i]]]
    #replaced with dummy code on Oct 20 b/c this code should be for crown fires.
    #sfubed[i] <- d.post$fuelbed[fblo[d.post$fuelbed == ufxDa[i]]]
    
    #Forms a list of the new ages.
    #sagesd[i] <- (f.path$start[f.path$pre == dtxm[,2][fblo[
    #  d.post$fuelbed == ufxDa[i]]]] - 1)
    #replaced with dummy code on Oct 20 b/c this code should be for crown fires.
    #sagesd[i] <- Age.List[Stand.List == loopE.ReplacedStand[i]]
  #}

    #Code below replaces loop, vene though there are more lines it should run way faster.
    
    #Add ufxDa to loopE data frame
    loopE <- data.frame(loopE, ufxDa = ufxDa)
    
    #Sort data frame by new stands
    loopE <- loopE[order(loopE$NewStand),]#probably unecessary
    
    #seperate out new stands from s.map, lists occurrences of new stands from min to max coords.
    vs.map_a15 <- s.map[s.map %in% loopE$NewStand]
    
    #List of row numbers in d.post where fuelbeds need to be updated based on treatment
    LL1_a15 <- which(d.post$fuelbed %in% loopE$ufxDa)
    
    #Fuelbeds that may be updated corresponding with row numbers in LL1_a15
    FL1_a15 <- d.post$fuelbed[d.post$fuelbed %in% ufxDa]
    
    #row numbers in d.post corresponding with each existing fuelbed in each new stand 
    LL2_a15 <- LL1_a15[match(loopE$ufxDa, FL1_a15)]
    
    #Use row numbers (LL2) and column numbers (loopB.treat_type) to calculate "coordinate" in t.post
    #LL3_a15 <- ((loopE$TreatType - 1) * length(d.post$fuelbed)) + LL2_a15
    #Activate when you can differentiate between crown fire and surface fire
  
    #Convert t.post (ttxm is t.post) from a data.frame into a matrix so new fuelbeds can be identified 
    #by coordinates that corresond with row and column numbers.
    #am_ttxm <- as.matrix(ttxm)  
  #Activate when you can differentiate between crown fire and surface fire
  
    #Idenintify new fuelbed for each new stand.
    #newFB_a15 <- am_ttxm[LL3]
  #Activate when you can differentiate between crown fire and surface fire
    newFB_a15 <- d.post[LL2_a15,1]

    #Lists occurrences of new fuelbeds from min to max coords.
    v.newFB_a15 <- newFB_a15[match(vs.map_a15, loopE$NewStand)]
    
    #Replace old fuelbeds with new ones in f.map
    f.map[s.map %in% loopE$NewStand] <- v.newFB_a15
    
    #List ages associated with each stand that has been affected by treatment
    #These will be used to update Age.List but are unchanged since treatments do not
    #change overstory age.
    newAGE_a15_1 <- Age.List[Stand.List %in% loopE$ReplacedStand]
    newAGE_a15 <- newAGE_a15_1[match(loopE$ReplacedStand, usxDa_1)]
    
    #Re-order loopB data frame by old stands because that is the order of ages in newAGE_a7.
    loopE <- loopE[order(loopE$ReplacedStand),]
    
    #Add newAGE_a7 to loopB data.frame
    loopE <- data.frame(loopE, newAGE_a15 = newAGE_a15)

  })#a15

a16 <- system.time({
  #List stands that have been altered by disturbances.
  ss2 <- loopE.ReplacedStand
  standd <- sort(unique(ss2))#there can be duplicates, this will mess up the shortcut in a9
  sb <- summarize(loopE.Area,ss2,sum)#sum areas for duplicates.
  saread <- as.vector(sb[,2])
  smud <- mapply(function(y) MU.List[Stand.List == y], loopE.ReplacedStand)
})#a16

a17 <- system.time({
  #Shelve fire history for stands that have been impacted by disturbance
  new_mfri_vec <- mapply(function(y) mfri.Matrix[Stand.List == y,], loopE.ReplacedStand)
  nmvd <- t(new_mfri_vec)
  
  #Add a fire for stands that were burned in wildfires
  nmvd[,30] <- 1
})#a17

a18 <- system.time({

  #Improved method removes loop
  #sad_a18 <- data.frame(standd = standd, saread = saread)
  #sad_a18b <- sad_a18[order(sad_a18$standd),]
  #Area.List[Stand.List %in% sad_a18b$standd] <- Area.List[Stand.List %in% sad_a18b$standd] - sad_a18b$saread
  
#Change stand properties as needed for treatments.
for(i in 1:length(standd))
  {
    Area.List[Stand.List == standd[i]] <- 
                Area.List[Stand.List == standd[i]] - saread[i]
  }
})#a18

a19 <- system.time({
  
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
  Area.List <- Area.List[(Area.List == 0) == F]
})#a19

a20 <- system.time({
  #Update list to add new stands.
  Stand.List <- c(Stand.List, loopE.NewStand)
  Fuelbed.List <- c(Fuelbed.List, newFB_a15)
  Age.List <- c(Age.List, newAGE_a15)
  
  #List fuelbeds that need to be updated.
  pdaFB_a20 <- pda$pre[pda$pre %in% newFB_a15]
  
  #List corresponding updated age restrictions
  pdaTH_a20 <- pda$thin[pda$pre %in% newFB_a15]
  
  #List occurences of age restriction for each new stand
  v.THIN_a20 <- pdaTH_a20[match(newFB_a15,pdaFB_a20)]
  
  #Update
  T1E.List <- c(T1E.List,v.THIN_a20)
  
  #List corresponding updated age restrictions
  pdaHE_a20 <- pda$herb[pda$pre %in% newFB_a15]
  
  #List occurences of age restriction for each new stand
  v.HERB_a20 <- pdaHE_a20[match(newFB_a15,pdaFB_a20)]
  
  #Update
  T2E.List <- c(T2E.List, v.HERB_a20)
  
  #List corresponding updated age restrictions
  pdaSF_a20 <- pda$sfire[pda$pre %in% newFB_a15]
  
  #List occurences of age restriction for each new stand
  v.SFIRE_a20 <- pdaSF_a20[match(newFB_a15,pdaFB_a20)]
  
  #Update
  D1E.List <- c(D1E.List, v.SFIRE_a20)
  
  #List corresponding updated age restrictions
  pdaCF_a20 <- pda$cfire[pda$pre %in% newFB_a15]
  
  #List occurences of age restriction for each new stand
  v.CFIRE_a20 <- pdaCF_a20[match(newFB_a15,pdaFB_a20)]
  
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
  Area.List <- c(Area.List,loopE.Area)
})#a20

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
         length(Age.List)) != (length(unique(as.vector(s.map)))-1)) == T)
{
  r101 <- 3
  break
} else
{
  r101 <- 0   
} 
##############################################################################
##############################################################################
##############################################################################

a21 <- system.time({
  feof <- mapply(function(x) ifelse(
    mfri.List[x] < d.post$mfri_start[d.post$fuelbed == Fuelbed.List[x]], 
    d.post$more_fire[d.post$fuelbed == Fuelbed.List[x]], 
    ifelse(
    mfri.List[x] > d.post$mfri_end[d.post$fuelbed == Fuelbed.List[x]],
    ifelse(d.post$less_fire2[d.post$fuelbed == Fuelbed.List[x]] > 0,
           resample(c(d.post$less_fire1[d.post$fuelbed == Fuelbed.List[x]],
                      d.post$less_fire2[d.post$fuelbed == Fuelbed.List[x]]),1),
           d.post$less_fire1[d.post$fuelbed == Fuelbed.List[x]]),
    Fuelbed.List[x])),1:length(Stand.List))
  #cbind(Stand.List,Fuelbed.List,feof)
})#a21

a22 <- system.time({

  #Update f.map
  #Improved function to update f.map based on mFRI. Former code used a for()
  s.SL <- Stand.List[Fuelbed.List != feof]
  feof2 <- feof[Fuelbed.List != feof]
  vs.map <- s.map[s.map %in% s.SL]
  v.feof2 <- feof2[match(vs.map, s.SL)]
  f.map[s.map %in% s.SL] <- v.feof2
  Fuelbed.List[Stand.List %in% s.SL] <- feof2
  
  #Update f.map (old code, works on the order of minutes: 500 stands/minute)
  #for(i in 1:length(Stand.List))
  #  if(Fuelbed.List[i] != feof[i])
  #  {
  #    f.map[s.map == Stand.List[i]] <- feof[i]
  #    Fuelbed.List[i] <- feof[i]
  #  }
})#a22

a23 <- system.time({
  #Temporary -- run 41
  Loop.track[[length(Loop.track) + 1]] <- c(a,b,cc,d,e,f,g)
  MU.track[[length(MU.track) + 1]] <- MU.List
  mgmtUnit.track[[length(mgmtUnit.track) + 1]] <- loopB.new_mgmtUnit
  len <- vector(length = 0, mode = 'numeric')
  lea <- vector(length = 0, mode = 'numeric')
  ler <- vector(length = 0, mode = 'numeric')
  len <- loopE.NewStand
  lea <- loopE.Area
  ler <- loopE.ReplacedStand
})#a23

  #cbind(Stand.List,Fuelbed.List,Area.List)
} else
{
  #No new stand numbers, enter a placeholder.
  if(length(Disturbance.History) == 0)
  {
    a24 <- system.time({
    Disturbance.History[[(length(Disturbance.History)+1)]] <- "No Data."
    names(Disturbance.History)[[length(Disturbance.History)]] <- paste(
      "No disturbances this year.", collapse = "")
    })#a24
  }
}

#Post run step 3>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Update files based on succession

#Identify potential changes in fuelbeds based on succession pathways.

a25 <- system.time({
pmuf <- mapply(function(x) ifelse(
  Age.List[x] > f.path$end[f.path$pre == Fuelbed.List[x]],
  ifelse(f.path$post_2[f.path$pre == Fuelbed.List[x]] > 0,
         resample(c(f.path$post_1[f.path$pre == Fuelbed.List[x]],
                    f.path$post_2[f.path$pre == Fuelbed.List[x]]),1),
         f.path$post_1[f.path$pre == Fuelbed.List[x]]),
  Fuelbed.List[x]),1:length(Stand.List))
#cbind(Stand.List,Fuelbed.List,pmuf)
})#a25


a26 <- system.time({
#Update f.map
  
#Update f.map
#Improved function to update f.map based on mFRI. Former code used a for()
s.SL2 <- Stand.List[Fuelbed.List != pmuf]
pmuf2 <- pmuf[Fuelbed.List != pmuf]
vs.map2 <- s.map[s.map %in% s.SL2]
v.pmuf2 <- pmuf2[match(vs.map2, s.SL2)]
f.map[s.map %in% s.SL2] <- v.pmuf2
Fuelbed.List[Stand.List %in% s.SL2] <- pmuf2

#for(i in 1:length(Stand.List))
#  if(Fuelbed.List[i] != pmuf[i])
#  {
#    f.map[s.map == Stand.List[i]] <- pmuf[i]
#    Fuelbed.List[i] <- pmuf[i]
#  }
})#a26

a27 <- system.time({
#Update D.List
D.List <- cbind(T1E.List, T2E.List, D1E.List, D2E.List)
})#a27

a28 <- system.time({
#})#TESTING

#Save run data.
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)

#Save run data.
cat(e.summary, file = "annual_summary", append = T)#

})#a28

a29 <- system.time({

#Create maps for interval years.
if((a %% Interval) == 0)
{

#  #Save Fuelbed Map (f.map).
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)

#write.table(s.map, file = paste("C:\\usfs_sef_outputs_FDM\\run_", run,"maps\\sef_smap_",
#                                dt,"_",tm,"_R",rows,"xC",cols,"_Y",a,".txt",sep = ""), 
#            append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", 
#            dec = ".", row.names = FALSE,col.names = FALSE, qmethod = 
#              c("escape", "double"))#

write.table(f.map, file = paste("run_", run,"maps/sef_fmap_",
                                dt,"_",tm,"_R",rows,"xC",cols,"_Y",a,".txt",sep = ""), 
            append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", 
            dec = ".", row.names = FALSE,col.names = FALSE, qmethod = 
              c("escape", "double"))#

}

#Log treatment and disturbance mapping data for year[a].
Map.History[[a]] <- c(Treatment.History,Disturbance.History)
names(Map.History)[[a]] <- paste("Year", a,":", collapse = "")
})#a29

aa1[length(aa1) + 1] <- a1[3]
aa2[length(aa2) + 1] <- a2[3]
aa3[length(aa3) + 1] <- a3[3]
aa4[length(aa4) + 1] <- a4[3]
aa5[length(aa5) + 1] <- a5[3]
aa6[length(aa6) + 1] <- a6[3]
aa7[length(aa7) + 1] <- a7[3]
aa8[length(aa8) + 1] <- a8[3]
aa9[length(aa9) + 1] <- a9[3]
aa10[length(aa10) + 1] <- a10[3]
aa11[length(aa11) + 1] <- a11[3]
aa12[length(aa12) + 1] <- a12[3]
aa13[length(aa13) + 1] <- a13[3]
aa14[length(aa14) + 1] <- a14[3]
aa15[length(aa15) + 1] <- a15[3]
aa16[length(aa16) + 1] <- a16[3]
aa17[length(aa17) + 1] <- a17[3]
aa18[length(aa18) + 1] <- a18[3]
aa19[length(aa19) + 1] <- a19[3]
aa20[length(aa20) + 1] <- a20[3]
aa21[length(aa21) + 1] <- a21[3]
aa22[length(aa22) + 1] <- a22[3]
aa23[length(aa23) + 1] <- a23[3]
aa24[length(aa24) + 1] <- a24[3]
aa25[length(aa25) + 1] <- a25[3]
aa26[length(aa26) + 1] <- a26[3]
aa27[length(aa27) + 1] <- a27[3]
aa28[length(aa28) + 1] <- a28[3]
aa29[length(aa29) + 1] <- a29[3]

} #1.0.0 ---------------------------------------------------------------------------

