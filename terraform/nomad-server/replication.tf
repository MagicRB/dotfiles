resource "nomad_acl_token" "replication" {
  count = var.replication_token_path != "" ? 1 : 0

  name = "${var.hostname} in ${var.datacenter} replication token"
  type = "management"
}


resource "vault_kv_secret_v2" "consul-replication" {
  count = var.replication_token_path != "" ? 1 : 0

  mount = var.vault_mount.path
  name = var.replication_token_path
  delete_all_versions = true
  data_json = jsonencode({
    secret = nomad_acl_token.replication[0].secret_id
    accessor = nomad_acl_token.replication[0].id
  })
}
