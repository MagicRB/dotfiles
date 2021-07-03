path "auth/cert/certs/vault.toothpick" {
  capabilities = [ "update" ]
  allowed_parameters = {
    ttl = [ "3600" ]
    policies = [ "vault.toothpick" ]
    display_name = [ "vault.toothpick" ]
    certificate = []
  }
}

path "pki_dynra/issue/vault.toothpick" {
  capabilities = [ "update" ]
  allowed_parameters = {
    common_name = [ "vault.toothpick.dyn.redalder.org" ]
    ttl = [ "24h" ]
    alt_names = [ "localhost" ]
    ip_sans = [ "127.0.0.1" ]
  }
}

path "pki_dynra/issue/consul.toothpick" {
  capabilities = [ "update" ]
  allowed_parameters = {
    common_name = [ "consul.toothpick.dyn.redalder.org" ]
    ttl = [ "24h" ]
    alt_names = [ "localhost" ]
    ip_sans = [ "127.0.0.1" ]
  }
}

path "pki_dynta/cert/ca" {
  capabilities = [ "read" ]
  allowed_parameters = {}
}

path "kv/data/systems/toothpick/nomad" {
  capabilities = [ "read" ]
  allowed_parameters = {}
}

path "kv/data/systems/toothpick/consul" {
  capabilities = [ "read" ]
  allowed_parameters = {}
}


# Consul Managed PKI Mounts
path "/sys/mounts" {
  capabilities = [ "read" ]
}

path "/sys/mounts/connect_root" {
  capabilities = [ "create", "read", "update", "delete", "list" ]
}

path "/sys/mounts/connect_inter" {
  capabilities = [ "create", "read", "update", "delete", "list" ]
}

path "/connect_root/*" {
  capabilities = [ "create", "read", "update", "delete", "list" ]
}

path "/connect_inter/*" {
  capabilities = [ "create", "read", "update", "delete", "list" ]
}
