variable "policies" {
  type = list(string)
}

variable "hostname" {
  type = string
}

variable "host" {
  type = string
}

variable "user" {
  type = string
}

variable "metadata" {
  type = any
}

variable "vault_auth_approle" {
  type = any
}

resource "vault_approle_auth_backend_role" "system" {
  backend        = var.vault_auth_approle.path
  role_name      = var.hostname
  token_policies = var.policies
}

data "vault_approle_auth_backend_role_id" "system" {
  backend   = var.vault_auth_approle.path
  role_name = vault_approle_auth_backend_role.system.role_name
}

resource "vault_approle_auth_backend_role_secret_id" "system" {
  backend   = var.vault_auth_approle.path
  role_name = vault_approle_auth_backend_role.system.role_name

  metadata = jsonencode(var.metadata)
}

resource "null_resource" "approles" {
  triggers = {
    secret_id = vault_approle_auth_backend_role_secret_id.system.secret_id
    role_id = data.vault_approle_auth_backend_role_id.system.role_id
  }

  connection {
    host = var.host
    user = var.user
  }

  provisioner "remote-exec" {
    inline = [
      "#!/usr/bin/env bash",
      "echo \"${vault_approle_auth_backend_role_secret_id.system.secret_id}\" > /var/secrets/approle.secretid",
      "echo \"${data.vault_approle_auth_backend_role_id.system.role_id}\" > /var/secrets/approle.roleid"
    ]
  }
}
