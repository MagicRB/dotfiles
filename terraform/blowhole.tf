module "blowhole-consul-agent" {
  source = "./consul-agent"

  hostname = "blowhole"
  datacenter = "homelab-1"

  vault_consul_secret_backend = vault_consul_secret_backend.consul
  vault_mount = vault_mount.kv

  encryption_key_path = local.blowhole.consul.encryption_key_path
  encryption_key = random_id.homelab-1_consul_encryption_key.b64_std

  agent_token_path = local.blowhole.consul.agent_token_path
  anonymous_token_path = local.blowhole.consul.anonymous_token_path

  consul-anonymous = {
    secret = data.consul_acl_token_secret_id.consul-anonymous.secret_id
    accessor = consul_acl_token.consul-anonymous.id
  }
}

module "blowhole-nomad-server" {
  source = "./nomad-server"

  hostname = "blowhole"
  datacenter = "homelab-1"

  vault_consul_secret_backend = vault_consul_secret_backend.consul
  vault_mount = vault_mount.kv
  vault_token_path = local.blowhole.nomad.vault_token_path

  encryption_key_path = local.blowhole.nomad.encryption_key_path
  encryption_key = random_id.nomad_encryption_key.b64_std

  consul_token_path = local.blowhole.nomad.consul_token_path
}

module "blowhole-upload-approles" {
  source = "./upload-approles"

  hostname = "blowhole"
  host = "10.64.1.201"
  user = "main"

  policies = [
    module.blowhole-consul-agent.vault_policy.name,
    module.blowhole-nomad-server.vault_policy.name
  ]

  metadata = {
    "ip_address" = "blowhole.in.redalder.org"
  }

  vault_auth_approle = vault_auth_backend.approle
}
