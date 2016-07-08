BEGIN { instance_count = 0 }

{ if (NR==1) {
   header = $0
  } else {
   file = sim_id"/user_data_"NR".ud"
   instance_count = instance_count + $1

   print "#!/bin/bash" > file;

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
   print "    aws s3 cp $file s3://bernease/wildfire-simulation/" sim_id "/$RUN_ID/$file" > file;
   print "  done" > file;

   #   Terminate this instance (if success)
   print "  aws ec2 terminate-instances instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id)" > file;

   print "else" > file;

   #   Send email with instance id, output files (and error message?) (if fail)
   print "  aws ses send-email --subject \"R Failure for Simulation " sim_id "\" --text \"This is a test message for $RUN_ID on host $(hostname). A link to the s3 bucket with the failing run will soon be included.\" --to jcronan@uw.edu --from jcronan@uw.edu" > file;

   #   Stop instance (if fail)
   print "  aws ec2 stop-instances --instance-ids $(curl -s http://169.254.169.254/latest/meta-data/instance-id)" > file;
   print "fi" > file;
  }
}

END { print "\nSimulation \"" sim_id "\" will launch " instance_count " EC2 instances."}
