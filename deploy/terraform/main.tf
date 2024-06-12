terraform {
  backend "s3" {
    encrypt = true
  }
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
  }
}

# Configure the AWS Provider
provider "aws" {
  default_tags {
    tags = local.default_tags
  }
  region  = var.aws_region
  profile = var.profile
}

# Data sources
data "aws_caller_identity" "current" {}

data "aws_cloudwatch_log_group" "cw_log_group" {
  name = "/aws/batch/job/${var.prefix}-momma/"
}

data "aws_efs_file_system" "aws_efs_input" {
  creation_token = "${var.prefix}-input"
}

data "aws_efs_file_system" "aws_efs_flpe" {
  creation_token = "${var.prefix}-flpe"
}

data "aws_iam_role" "job_role" {
  name = "${var.prefix}-batch-job-role"
}

data "aws_iam_role" "exe_role" {
  name = "${var.prefix}-ecs-exe-task-role"
}

# Local variables
locals {
  account_id = data.aws_caller_identity.current.account_id
  default_tags = length(var.default_tags) == 0 ? {
    application : var.app_name,
    environment : var.environment,
    version : var.app_version
  } : var.default_tags
}