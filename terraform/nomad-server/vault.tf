resource "vault_policy" "nomad-server" {
  name = "${var.hostname}-nomad-server"

  policy = <<EOF
# Allow creating tokens under "nomad-cluster" token role. The token role name
# should be updated if "nomad-cluster" is not used.
path "auth/token/create/nomad-cluster" {
  capabilities = ["update"]
}

# Allow looking up "nomad-cluster" token role. The token role name should be
# updated if "nomad-cluster" is not used.
path "auth/token/roles/nomad-cluster" {
  capabilities = ["read"]
}

# Allow looking up the token passed to Nomad to validate the token has the
# proper capabilities. This is provided by the "default" policy.
path "auth/token/lookup-self" {
  capabilities = ["read"]
}

# Allow looking up incoming tokens to validate they have permissions to access
# the tokens they are requesting. This is only required if
# `allow_unauthenticated` is set to false.
path "auth/token/lookup" {
  capabilities = ["update"]
}

# Allow revoking tokens that should no longer exist. This allows revoking
# tokens for dead tasks.
path "auth/token/revoke-accessor" {
  capabilities = ["update"]
}

# Allow checking the capabilities of our own token. This is used to validate the
# token upon startup. Note this requires update permissions because the Vault API
# is a POST
path "sys/capabilities-self" {
  capabilities = ["update"]
}

# Allow our own token to be renewed.
path "auth/token/renew-self" {
  capabilities = ["update"]
}
EOF
}

resource "vault_token_auth_backend_role" "nomad-server" {
  role_name = "${var.hostname}-nomad-server"
  allowed_policies = [
    vault_policy.nomad-server.name
  ]
  orphan = true
  renewable = true
}

resource "vault_token" "nomad-server" {
  policies = [
    vault_policy.nomad-server.name
  ]
  renewable = true
  ttl = "24h"
  explicit_max_ttl = 0
  role_name = vault_token_auth_backend_role.nomad-server.role_name
  display_name = "${var.hostname}-nomad-server-Vault-token"
}

resource "vault_kv_secret_v2" "nomad-server-vault" {
  mount = var.vault_mount.path
  name = var.vault_token_path
  delete_all_versions = true
  data_json = jsonencode({
    secret = vault_token.nomad-server.client_token
  })
}
