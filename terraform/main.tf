provider "vault" {
  address = "https://vault.in.redalder.org:8200"
}

provider "consul" {
  address = "http://10.64.1.201:8500"
}

provider "nomad" {
  address = "http://10.64.1.201:4646"
}

provider "external" {}

locals {
  blowhole = {
    consul = {
      encryption_key_path = "homelab-1/blowhole/consul/encryption_key"
      agent_token_path = "homelab-1/blowhole/consul/agent_token"
      anonymous_token_path = "homelab-1/blowhole/consul/anonymous_token"
    }
    nomad = {
      encryption_key_path = "homelab-1/blowhole/nomad/encryption_key"
      vault_token_path = "homelab-1/blowhole/nomad/vault_token"
      consul_token_path = "homelab-1/blowhole/nomad/consul_token"
    }
  }
  toothpick = {
    consul = {
      encryption_key_path = "do-1/toothpick/consul/encryption_key"
      agent_token_path = "do-1/toothpick/consul/agent_token"
      anonymous_token_path = "do-1/toothpick/consul/anonymous_token"
      replication_token_path = "do-1/toothpick/consul/replication_token"
    }
    nomad = {
      encryption_key_path = "do-1/toothpick/nomad/encryption_key"
      vault_token_path = "do-1/toothpick/nomad/vault_token"
      consul_token_path = "do-1/toothpick/nomad/consul_token"
      replication_token_path = "do-1/toothpick/nomad/replication_token"
    }
  }
}

# Vault backend setup

resource "vault_auth_backend" "approle" {
  type = "approle"

  tune {
    max_lease_ttl      = "90000s"
    listing_visibility = "unauth"
  }
}

resource "vault_mount" "kv" {
  path        = "kv"
  type        = "kv"
  options     = { version = "2" }
  description = "KV Version 2 secret engine mount"
}

resource "vault_kv_secret_backend_v2" "config" {
  mount                      = vault_mount.kv.path
  max_versions               = 5
}

## Create Consul secret backend in Vault to enable it to hand out tokens

resource "consul_acl_token" "vault-management-token" {
  description = "vault-management-token"
  policies = ["global-management"]
  local = true
}

resource "vault_consul_secret_backend" "consul" {
  path        = "consul"
  description = "Manages the Consul backend"

  address = "10.64.1.201:8500"
  token = consul_acl_token.vault-management-token.id
}


resource "random_id" "nomad_encryption_key" {
  byte_length = 32
}

resource "random_id" "homelab-1_consul_encryption_key" {
  byte_length = 32
}

resource "random_id" "do-1_consul_encryption_key" {
  byte_length = 32
}

resource "consul_acl_policy" "anonymous" {
  name        = "consul-anonymous"
  rules       = <<EOF
service_prefix "" { policy = "read" }
node_prefix    "" { policy = "read" }
EOF
}

resource "consul_acl_token" "consul-anonymous" {
  description = "Consul anonymous token"
  policies = [
    consul_acl_policy.anonymous.name,
  ]
  local = false
}

data "consul_acl_token_secret_id" "consul-anonymous" {
  accessor_id = consul_acl_token.consul-anonymous.id
}
