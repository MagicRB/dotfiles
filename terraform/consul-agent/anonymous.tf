variable "consul-anonymous" {
  type = object({
    secret = string,
    accessor = string
  })
}

# resource "consul_acl_policy" "anonymous" {
#   name        = "${var.hostname}-consul-anonymous"
#   rules       = <<EOF
# service_prefix "" { policy = "read" }
# node_prefix    "" { policy = "read" }
# EOF
# }

# resource "consul_acl_token" "consul-anonymous" {
#   description = "Consul anonymous token on ${var.hostname}"
#   policies = [
#     consul_acl_policy.anonymous.name,
#   ]
#   local = false
# }

# data "consul_acl_token_secret_id" "consul-anonymous" {
#   accessor_id = consul_acl_token.consul-anonymous.id
# }

resource "vault_kv_secret_v2" "consul-anonymous" {
  mount = var.vault_mount.path
  name = var.anonymous_token_path
  delete_all_versions = true
  data_json = jsonencode(var.consul-anonymous) # jsonencode({
  #   secret = data.consul_acl_token_secret_id.consul-anonymous.secret_id
  #   accessor = consul_acl_token.consul-anonymous.id
  # })
}
