BEGIN { 
  instance_count = 0
  slash = "\\\\"
  home_dir = slash "Users" slash "Administrator" slash
  r_dir = ""
  # r_dir = "\"" slash "Program Files" slash "R" slash "R-3.4.2" slash "bin" slash "\""
  bash_dir = slash "cygwin64" slash "bin" slash
  s3_dir = "s3://cronan-fdm-eglin-simulations/"
}

{ if (NR==1) {
   header = $0
  } else {
   file = sim_id"/user_data_"NR".ud"
   instance_count = instance_count + $1

   # If Windows, print <script> tag
   # https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-windows-user-data.html
   print "<script>" > file;

   print "#!" bash_dir "bash" > file;

   # Export environmental variables
   print "export LC_ALL=en_US.UTF-8" > file;  # https://bugs.python.org/issue18378
   print "export LANG=en_US.UTF-8" > file;    # see above
   print "export AWS_DEFAULT_REGION=us-west-2" > file;

   # Pulls repo from Github
   print "cd " home_dir > file;
   print "curl -L https://www.github.com/jcronanuw/EglinAirForceBase/archive/master.zip > " home_dir "eafb.zip" > file;
   print "unzip " home_dir "eafb.zip" > file;
   print "cd " home_dir "EglinAirForceBase-master" > file;

   # Create run id for each separate host
   print "RUN_ID=$(eval echo $(curl -s http://169.254.169.254/latest/meta-data/instance-id) | tail -c 5)" > file;

   # Change key for host in EC2
   print "aws ec2 create-tags --resources $(eval echo $(curl -s http://169.254.169.254/latest/meta-data/instance-id)) --tags Key=Name,Value=FDM_Instance_" sim_id "_$RUN_ID" > file;

   # Add appropriate folders
   print "mkdir " home_dir sim_id > file;
   print "mkdir " home_dir sim_id slash "$RUN_ID" > file;
   print "input_path=" home_dir sim_id slash > file;
   print "output_path=" home_dir sim_id slash "$RUN_ID" slash> file;

   # Create host-specific simulation parameters csv file
   print "echo \"" header ",sim_id,run_id,input_path,output_path\" > host_sim_params.csv" > file;
   print "echo \"" $0 "," sim_id ",$RUN_ID,$input_path,$output_path\" >> host_sim_params.csv" > file;

   # Run R script
   if (simple=="n") {
     print r_dir "Rscript sef_FDM_v2.0.r" > file;
   } else {
     print "echo \"Fake results supposed to replace R script output. Launched as simple.\" > ${output_path}/test_result.txt" > file;
   }

   # Check status after completion of script (0=success=true, 1=failed=false)
   print "if [ $? -eq 0 ]; then" > file;

   #   Result folder pushed to S3 (if success)
   print "  cd " home_dir sim_id slash "$RUN_ID" > file;
   print "  for file in *; do " > file;

   print "    aws s3 cp $file " s3_dir sim_id "/$RUN_ID/$file" > file;

   print "  done" > file;

   #   Terminate this instance (if success)
   print "  aws ec2 terminate-instances --instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id) --region us-west-2" > file;

   print "else" > file;

   #   Send email with instance id, output files (and error message) (if fail)
   print "  aws ses send-email --subject \"R Failure for Simulation " sim_id "\" --text \"There was an error for $RUN_ID and the R script stopped. To access this simulation use: ssh -i wildfire-simulation.pem ubuntu@$(curl -s http://169.254.169.254/latest/meta-data/public-hostname). A link to the s3 bucket with the failing run will soon be included. Print and stop(messages), Disturbance loop variables are: years: a; treatment number: b; treatment block: cc; treatment expansion: d; wildfire number: e; wildfire block: f; unsuppressed wildfire expansion: g; block and burn expansion number: h.  \" --to jcronan@uw.edu --from jcronan@uw.edu" > file;

   #   Stop instance (if fail)
   # print "  aws ec2 stop-instances --instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id)" > file;
   print "fi" > file;

   # Print end script tag for Windows
   print "</script>" > file;
  }
}

END { print "\nSimulation \"" sim_id "\" will launch " instance_count " EC2 instances."}
