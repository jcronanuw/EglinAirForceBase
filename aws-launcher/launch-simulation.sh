#!/bin/sh

# create (hopefully!) unique simulation id
SIM_ID=`date +%y%b%d-%H%M%S`

# create folder to hold simulation files
mkdir $SIM_ID

# create user data files for each line of script
awk -F, -v sim_id=$SIM_ID -f create_user_data_files.awk $1

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
# aws s3 mb s3://jcronanuw-wildfire/

# launches instance for each parameter set
#MAC=`curl http://169.254.169.254/latest/meta-data/mac`
#SUBNET_ID=`curl http://169.254.169.254/latest/meta-data/network/interfaces/macs/$MAC/subnet-id`
if [ $2 ]
  then
    SUBNET_ID="--subnet-id $2"
fi
if [ $3 ]
  then
    AWS_PROFILE="--profile $3"
fi
line=2
tail -n +2 $1 | while IFS=',' read count params; do
  aws ec2 run-instances --image-id ami-88ac6ee8 --key-name wildfire-simulation --user-data file://$SIM_ID/user_data_$line.ud --instance-type t2.large --iam-instance-profile Name=wildfire-simulation --count $count $SUBNET_ID $AWS_PROFILE
  ((line++))
done
