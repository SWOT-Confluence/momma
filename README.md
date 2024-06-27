# momma-docker

MOMMA docker container hosted on AWS ECR used to create containers in the cloud.

## publishing disclaimer

Please **do not share this** repository, algorithm code contained within, or containers with others. 

***Also note that all Docker containers are currently hosted in AWS. You may build the containers using the Dockerfiles located in each algorithm's directory.***

## Container

### Arguments

Each container takes the name of a reach file as an argument. This file contains a list of reaches or sets of reaches where each reach identifier or set is on one line.

### Input and output operations

#### FLPE
- EFS Input is mounted to the container at `/mnt/data/input`
- EFS Output is mounted to the container at `/mnt/data/output`

### AWS Environment variable

AWS_BATCH_JOB_ARRAY_INDEX is used to determine the reach identifier to process for all containers. The index value is used to retrieve a line in a file that lists all reach identifiers to be processed.

## deployment

There is a script to deploy the Docker container image and Terraform AWS infrastructure found in the `deploy` directory.

Script to deploy Terraform and Docker image AWS infrastructure

REQUIRES:

- jq (<https://jqlang.github.io/jq/>)
- docker (<https://docs.docker.com/desktop/>) > version Docker 1.5
- AWS CLI (<https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html>)
- Terraform (<https://developer.hashicorp.com/terraform/tutorials/aws-get-started/install-cli>)

Command line arguments:

[1] registry: Registry URI
[2] repository: Name of repository to create
[3] prefix: Prefix to use for AWS resources associated with environment deploying to
[4] s3_state_bucket: Name of the S3 bucket to store Terraform state in (no need for s3:// prefix)
[5] profile: Name of profile used to authenticate AWS CLI commands

Example usage: ``./deploy.sh "account-id.dkr.ecr.region.amazonaws.com" "container-image-name" "prefix-for-environment" "s3-state-bucket-name" "confluence-named-profile"`

Note: Run the script from the deploy directory.
