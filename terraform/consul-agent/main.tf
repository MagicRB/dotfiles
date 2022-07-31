variable "hostname" {
  description = "Host of the consul agent"
  type = string
}

variable "datacenter" {
  description = "Which DC to create the Consul policy in"
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

variable "agent_token_path" {
  type = string
}

variable "replication_token_path" {
  type = string
  default = ""
}

variable "anonymous_token_path" {
  type = string
}

variable "vault_mount" {
  type = any
}

resource "consul_acl_policy" "agent" {
  name        = "${var.hostname}-consul-agent"
  rules       = <<EOF
node "${var.hostname}" {
  policy = "write"
}
agent "${var.hostname}" {
  policy = "write"
}
service_prefix "" {
  policy = "write"
}
EOF
}

resource "consul_acl_token" "consul-agent" {
  description = "Consul agent token on ${var.hostname}"
  policies = [
    consul_acl_policy.agent.name,
  ]
  local = false
}

data "consul_acl_token_secret_id" "consul-agent" {
  accessor_id = consul_acl_token.consul-agent.id
}

resource "vault_kv_secret_v2" "consul-agent" {
  mount = var.vault_mount.path
  name = var.agent_token_path
  delete_all_versions = true
  data_json = jsonencode({
    secret = data.consul_acl_token_secret_id.consul-agent.secret_id
    accessor = consul_acl_token.consul-agent.id
  })
}

resource "vault_policy" "consul" {
  name = "${var.hostname}-consul-agent-agent"

  policy = <<EOF
path "${var.vault_mount.path}/data/${var.encryption_key_path}" {
  capabilities = ["read"]
}

path "${var.vault_mount.path}/data/${var.agent_token_path}" {
  capabilities = ["read"]
}

path "${var.vault_mount.path}/data/${var.replication_token_path}" {
  capabilities = [${var.replication_token_path != null ? "\"read\"" : ""}]
}

path "${var.vault_mount.path}/data/${var.anonymous_token_path}" {
  capabilities = ["read"]
}
EOF
}

output "vault_policy" {
  value = vault_policy.consul
}
