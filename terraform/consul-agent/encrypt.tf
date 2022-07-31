resource "vault_kv_secret_v2" "encryption_key" {
  mount = var.vault_mount.path
  name = var.encryption_key_path
  delete_all_versions = true
  data_json = jsonencode({
    key = var.encryption_key
  })
}
