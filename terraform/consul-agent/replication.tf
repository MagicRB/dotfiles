resource "consul_acl_policy" "replication" {
  count = var.replication_token_path != "" ? 1 : 0

  name = "${var.hostname}-consul-replication"
  datacenters = ["homelab-1"]
  rules = <<EOF
acl = "write"

operator = "write"

service_prefix "" {
  policy = "read"
  intentions = "read"
}
EOF
}

resource "consul_acl_token" "consul-replication" {
  count = var.replication_token_path != "" ? 1 : 0

  description = "Consul replication token on ${var.hostname}"
  policies = [
    consul_acl_policy.replication[0].name,
  ]
  local = false
}

data "consul_acl_token_secret_id" "consul-replication" {
  count = var.replication_token_path != "" ? 1 : 0

  accessor_id = consul_acl_token.consul-replication[0].id
}


resource "vault_kv_secret_v2" "consul-replication" {
  count = var.replication_token_path != "" ? 1 : 0

  mount = var.vault_mount.path
  name = var.replication_token_path
  delete_all_versions = true
  data_json = jsonencode({
    secret = data.consul_acl_token_secret_id.consul-replication[0].secret_id
    accessor = consul_acl_token.consul-replication[0].id
  })
}
