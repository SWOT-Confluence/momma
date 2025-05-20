# Job Definition
resource "aws_batch_job_definition" "generate_batch_jd_momma" {
  name = "${var.prefix}-momma"
  type = "container"
  platform_capabilities = ["FARGATE"]
  propagate_tags = true
  tags = { job_definition = "${var.prefix}-momma" }

  container_properties = jsonencode({
    image = "${local.account_id}.dkr.ecr.us-west-2.amazonaws.com/${var.prefix}-momma:${var.image_tag}"
    executionRoleArn = var.iam_execution_role_arn
    jobRoleArn = var.iam_job_role_arn
    fargatePlatformConfiguration = {
      platformVersion = "LATEST"
    }
    logConfiguration = {
      logDriver = "awslogs"
      options = {
        awslogs-group = aws_cloudwatch_log_group.cw_log_group.name
      }
    }
    resourceRequirements = [{
      type = "MEMORY"
      value = "2048"
    }, {
      type = "VCPU",
      value = "1"
    }]
    mountPoints = [{
      sourceVolume = "input",
      containerPath = "/mnt/data/input"
      readOnly = false
    }, {
      sourceVolume = "flpe"
      containerPath = "/mnt/data/output"
      readOnly = false
    }]
    volumes = [{
      name = "input"
      efsVolumeConfiguration = {
        fileSystemId = var.efs_file_system_ids["input"]
        rootDirectory = "/"
      }
    }, {
      name = "flpe"
      efsVolumeConfiguration = {
        fileSystemId = var.efs_file_system_ids["flpe"]
        rootDirectory = "/momma"
      }
    }]
  })
}

# Log group
resource "aws_cloudwatch_log_group" "cw_log_group" {
  name = "/aws/batch/job/${var.prefix}-momma/"
}
