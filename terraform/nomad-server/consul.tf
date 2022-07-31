resource "consul_acl_policy" "nomad-server" {
  name        = "${var.hostname}-nomad-server"
  rules       = <<EOF
agent_prefix "" {
  policy = "read"
}

node_prefix "" {
  policy = "read"
}

service_prefix "" {
  policy = "write"
}

acl = "write"
EOF
}

resource "consul_acl_token" "nomad-server" {
  description = "Consul token for nomad-server on ${var.hostname}"
  policies = [
    consul_acl_policy.nomad-server.name
  ]
  local = false
}

data "consul_acl_token_secret_id" "nomad-server" {
  accessor_id = consul_acl_token.nomad-server.id
}

resource "vault_kv_secret_v2" "nomad-server-consul" {
  mount = var.vault_mount.path
  name = var.consul_token_path
  delete_all_versions = true
  data_json = jsonencode({
    secret = data.consul_acl_token_secret_id.nomad-server.secret_id
    accessor = consul_acl_token.nomad-server.accessor_id
  })
}
