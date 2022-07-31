variable "hostname" {
  description = "Host of the Nomad server"
  type = string
}

variable "datacenter" {
  description = "Which DC to create the consul policy in."
  type = string
}

variable "vault_consul_secret_backend" {
  description = "Consul secret backend instance in Vault"
  type = any
}

variable "encryption_key_path" {
  type = string
}

variable "encryption_key" {
  type = string
}

variable "replication_token_path" {
  type = string
  default = ""
}

variable "vault_token_path" {
  type = string
}

variable "consul_token_path" {
  type = string
}

variable "vault_mount" {
  type = any
}

resource "vault_policy" "nomad-server-integration" {
  name = "${var.hostname}-nomad-server-agent"

  policy = <<EOF
path "${var.vault_mount.path}/data/${var.encryption_key_path}" {
  capabilities = ["read"]
}

path "${var.vault_mount.path}/data/${var.vault_token_path}" {
  capabilities = ["read"]
}

path "${var.vault_mount.path}/data/${var.consul_token_path}" {
  capabilities = ["read"]
}

path "${var.vault_mount.path}/data/${var.replication_token_path}" {
  capabilities = [${var.replication_token_path != null ? "\"read\"" : ""}]
}
EOF
}

output "vault_policy" {
  value = vault_policy.nomad-server-integration
}
