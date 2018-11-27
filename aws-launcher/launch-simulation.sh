#!/bin/sh

# create (hopefully!) unique simulation id
SIM_ID=`date +%y%b%d-%H%M%S`

# create folder to hold simulation files
mkdir $SIM_ID

# create user data files for each line of script
if [ "$3" == "simple" ]; then
  simple="y"
else
  simple="n"
fi
awk -F, -v sim_id=$SIM_ID -v simple=$simple -f create_user_data_files.awk $1

# copy the relevant launch details file to simulation folder
cp $1 $SIM_ID/

# prompt user to accept job before starting
while true; do
  echo "Multiple EC2 instances will be launched and R will start running as soon as initiated."
  read -p "Do you wish to kick off this simulation? [Y/n] " response
  case $response in
    [Yy]* ) break;;  # continues after while loop
    [Nn]* ) rm -rf $SIM_ID; exit;;  # ends script
    * ) echo "\nPlease answer with \"Y[es]\" or \"N[o]\".";;
  esac
done

# create S3 bucket to hold this simulation's results
# aws s3 mb s3://cronan-fdm-eglin-simulations/

# launches instance for each parameter set
#MAC=`curl http://169.254.169.254/latest/meta-data/mac`
#SUBNET_ID=`curl http://169.254.169.254/latest/meta-data/network/interfaces/macs/$MAC/subnet-id`
if [ $2 == "personal" ]
  then
    AWS_KEY_NAME="--key-name EglinAirForceBase"
    AWS_PROFILE="--profile default"
    IAM_INSTANCE_PROFILE="--iam-instance-profile Name=wildfire-simulation"
elif [ $2 == "escience" ]
  then
    AWS_KEY_NAME="--key-name wildfire-simulation"
    SUBNET_ID="--subnet-id subnet-ae1370f4"
    AWS_PROFILE="--profile escience"
    IAM_INSTANCE_PROFILE="--iam-instance-profile Name=wildfire-simulation"
elif [ $2 == "airfire" ]
  then
    AWS_KEY_NAME="--key-name cronan"
    SUBNET_ID="--subnet-id subnet-15284970"
    AWS_PROFILE="--profile airfire"
    IAM_INSTANCE_PROFILE="--iam-instance-profile Name=cronan-fdm-eglin-simulations"
fi
line=2
tail -n +2 $1 | while IFS=',' read count params; do
  aws ec2 run-instances --image-id ami-06775690a658b0847 $AWS_KEY_NAME --user-data file://$SIM_ID/user_data_$line.ud --instance-type r3.large $IAM_INSTANCE_PROFILE --count $count $SUBNET_ID $AWS_PROFILE --security-group-ids "sg-06cb1607a02875a8e"
  ((line++))
done
