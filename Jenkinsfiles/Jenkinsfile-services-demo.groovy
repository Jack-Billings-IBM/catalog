

node('master') {
   jdk = tool name: 'JDK8'
   env.JAVA_HOME = "${jdk}"
   env.PATH="${env.JAVA_HOME}/bin:${env.PATH}"
   
   stage('Checkout Git Code to Jenkins on OpenShift') { // for display purposes
      // Get some code from a GitHub repository
      println "project name is catalog"
      git credentialsId: 'git', url: 'https://github.com/Jack-Billings-IBM/catalog.git'
   }

   stage('Rebuild zOS Connect Services') {
        println "Calling zconbt"
        def output = sh (returnStdout: true, script: 'pwd')
        println output
        sh "${WORKSPACE}/zconbt/bin/zconbt --properties=${WORKSPACE}/properties/inquireSingle.properties --file=${WORKSPACE}/archives/inquireSingle.sar "
        println "Called zconbt for inquireSingle"
        println "Exiting Stage 2, entering Stage 3!"
   }
   stage('Check for and Handle Existing Services') {
       println "Going to stop and remove existing service from zOS Connect Server if required"
       def resp = stopAndDeleteRunningService("inquireSingle")
       println "Cleared the field for service deploy: "+resp
       }

    stage('Deploy to z/OS Connect Server'){
       //call code to deploy the service.  passing the name of the service as a param
       def sarFileName ="inquireSingle.sar"
       installSar(sarFileName)
    }
   
   
    stage("Publish Artifacts to Artifactory") {
       // Obtain an Artifactory server instance, defined in Jenkins --> Manage:
       def server = Artifactory.server "artifactory"

       // Read the upload spec which was downloaded from github.
       def uploadSpec = readFile 'Artifactory/services-upload-demo.json'
       // Upload to Artifactory.
       def buildInfo = server.upload spec: uploadSpec

       // Publish the build to Artifactory
       server.publishBuildInfo buildInfo

      
    }
}


   //Will stop a running service if required and delete it
   def stopAndDeleteRunningService(service_name){

       println("Checking existence/status of Service name: "+service_name)

       //will be building curl commands, so saving the tail end for appending
       def urlval = "169.61.209.207:31078/zosConnect/services/"+service_name
       def stopurlval = "169.61.209.207:31078/zosConnect/services/"+service_name+"?status=stopped"

       //complete curl command will be saved in these values
       def command_val = ""
       def stop_command_val = ""
       def del_command_val = ""

       //call utility to get saved credentials and build curl command with it.  Commands were built to check, stop and delete service
       //curl command spits out response code into stdout.  that's then held in response field to evaluate
       command_val = "curl -o response.json -w %{response_code} --header 'Content-Type:application/json' --insecure "+urlval
       stop_command_val = "curl -X PUT -o responseStop.json --header \'Accept: application/json\' --header 'Content-Type:application/json' --insecure "+stopurlval
       del_command_val = "curl -X DELETE -o responseDel.json --header 'Content-Type:application/json' --insecure "+urlval
      
       // this checks the initial status of the service.  If it exists, HTTP Response Code will be 200
       def response = sh (script: command_val, returnStdout: true)
       println ""
       println "Response code is: "+response

       if(response != "200"){
          println "This Service does not exist on the server.  Deploying for first Time"
          return true
       }
       else{
           println "Service already exists, stopping and deleting it now"
           //reading status of existing service from response file.  file was created during curl command.
           def myObject = readJSON file: 'response.json'
           def status = myObject.zosConnect.serviceStatus
           println "Service status is "+status
           if(status == "Started"){
               //Stop API
               def responseStop = sh (script: stop_command_val, returnStdout: true)
               def myObjectStop = readJSON file: 'responseStop.json'
               def statusStop = myObjectStop.zosConnect.serviceStatus
              //ensure that status was actually stopped
               println "New status of service : "+statusStop

              //Delete API using curl command that was built
               def responseDel = sh (script: del_command_val, returnStdout: true)
               println "Service delete call complete"
               return true
           }
           else{
               println "Don't know why we hit this! Service is currently stopped......handle this if you like"
               return false
           }    
         return true
       }
   }

   def installSar(sarFileName){
       println "Starting sar deployment now"

       def urlval = "169.61.209.207:31078/zosConnect/services/"
       def respCode = ""

      //call utility to get saved credentials and build curl command with it and sar file name and then execute command
      //curl command spits out response code into stdout.  that's then held in respCode field to evaluate
       def command_val = "curl -X POST -o response.json -w %{response_code} --header 'Content-Type:application/zip' --data-binary @${WORKSPACE}/archives/"+sarFileName+" --insecure "+urlval
       respCode = sh (script: command_val, returnStdout: true)

       println "Service Installation Response code is: "+respCode
       if(respCode == "201"){
           println "Deployment completed successfully"
       }else if(respCode == "409"){
           error("Deployment failed due to it already existing")
       }
   }
   
   
