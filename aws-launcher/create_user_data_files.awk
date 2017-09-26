BEGIN { instance_count = 0 }

{ if (NR==1) {
   header = $0
  } else {
   file = sim_id"/user_data_"NR".ud"
   instance_count = instance_count + $1

   print "#!/bin/bash" > file;

   # Export environmental variables
   print "export LC_ALL=en_US.UTF-8" > file;  # https://bugs.python.org/issue18378
   print "export LANG=en_US.UTF-8" > file;    # see above
   print "export AWS_DEFAULT_REGION=us-west-2" > file;

   # Pulls repo from Github
   print "cd /home/ubuntu" > file;
   print "curl -L https://www.github.com/jcronanuw/EglinAirForceBase/archive/master.zip > /home/ubuntu/eafb.zip" > file;
   print "unzip /home/ubuntu/eafb.zip" > file;
   print "cd /home/ubuntu/EglinAirForceBase-master" > file;

   # Create run id for each separate host
   print "RUN_ID=$(eval echo $(curl -s http://169.254.169.254/latest/meta-data/instance-id) | tail -c 5)" > file;

   # Add appropriate folders
   print "mkdir /home/ubuntu/" sim_id > file;
   print "mkdir /home/ubuntu/" sim_id "/$RUN_ID" > file;
   print "output_path=/home/ubuntu/" sim_id "/$RUN_ID" > file;

   # Create host-specific simulation parameters csv file
   print "echo \"" header ",sim_id,run_id,input_path,output_path\" > host_sim_params.csv" > file;
   print "echo \"" $0 "," sim_id ",$RUN_ID,$PWD/,$output_path/\" >> host_sim_params.csv" > file;

   # Run R script
   print "Rscript sef_FDM_v2.0.r" > file;

   # Check status after completion of script (0=success=true, 1=failed=false)
   print "if [ $? -eq 0 ]; then" > file;

   #   Result folder pushed to S3 (if success)
   print "  cd /home/ubuntu/" sim_id "/$RUN_ID" > file;
   print "  for file in *; do " > file;
   print "    aws s3 cp $file s3://jcronanuw-wildfire/" sim_id "/$file" > file;
   #print "    aws s3 cp $file s3://bernease/wildfire-simulation/" sim_id "/$file" > file;
   print "  done" > file;

   #   Terminate this instance (if success)
   # print "  aws ec2 terminate-instances instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id)" > file;

   print "else" > file;

   #   Send email with instance id, output files (and error message?) (if fail)
   print "  aws ses send-email --subject \"R Failure for Simulation " sim_id "\" --text \"There was an error for $RUN_ID and the R script stopped. To access this simulation use: ssh -i wildfire-simulation.pem ubuntu@$(curl -s http://169.254.169.254/latest/meta-data/public-hostname). A link to the s3 bucket with the failing run will soon be included. Print and stop(messages), Disturbance loop variables are: years: a; treatment number: b; treatment block: cc; treatment expansion: d; wildfire number: e; wildfire block: f; unsuppressed wildfire expansion: g; block and burn expansion number: h.  \" --to jcronan@uw.edu --from jcronan@uw.edu" > file;

   #   Stop instance (if fail)
   # print "  aws ec2 stop-instances --instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id)" > file;
   print "fi" > file;
  }
}

END { print "\nSimulation \"" sim_id "\" will launch " instance_count " EC2 instances."}
