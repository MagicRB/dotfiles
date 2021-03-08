path "kv/data/concourse/workers/*" {
  capabilities = ["read", "update", "delete"]
}

path "kv/data/concourse/web" {
  capabilities = ["read"]
}
