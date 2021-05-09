path "kv/data/concourse/workers/*" {
  capabilities = ["read", "update", "delete", "create"]
}

path "kv/data/concourse/web" {
  capabilities = ["read"]
}
