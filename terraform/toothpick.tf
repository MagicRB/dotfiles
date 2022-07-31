module "toothpick-consul-agent" {
  source = "./consul-agent"

  hostname = "toothpick"
  datacenter = "do-1"

  vault_consul_secret_backend = vault_consul_secret_backend.consul
  vault_mount = vault_mount.kv

  encryption_key_path = local.toothpick.consul.encryption_key_path
  encryption_key = random_id.do-1_consul_encryption_key.b64_std

  agent_token_path = local.toothpick.consul.agent_token_path
  anonymous_token_path = local.toothpick.consul.anonymous_token_path
  replication_token_path = local.toothpick.consul.replication_token_path

  consul-anonymous = {
    secret = data.consul_acl_token_secret_id.consul-anonymous.secret_id
    accessor = consul_acl_token.consul-anonymous.id
  }
}

module "toothpick-nomad-server" {
  source = "./nomad-server"

  hostname = "toothpick"
  datacenter = "do-1"

  vault_consul_secret_backend = vault_consul_secret_backend.consul
  vault_mount = vault_mount.kv
  vault_token_path = local.toothpick.nomad.vault_token_path

  replication_token_path = local.toothpick.nomad.replication_token_path
  encryption_key_path = local.toothpick.nomad.encryption_key_path
  encryption_key = random_id.nomad_encryption_key.b64_std

  consul_token_path = local.toothpick.nomad.consul_token_path
}

module "toothpick-upload-approles" {
  source = "./upload-approles"

  hostname = "toothpick"
  host = "10.64.0.1"
  user = "main"

  policies = [
    module.toothpick-consul-agent.vault_policy.name,
    module.toothpick-nomad-server.vault_policy.name
  ]

  metadata = {
    "ip_address" = "redalder.org"
  }

  vault_auth_approle = vault_auth_backend.approle
}
