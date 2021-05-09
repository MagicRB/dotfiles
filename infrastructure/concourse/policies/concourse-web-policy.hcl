path "kv/data/concourse/workers/*" {
  capabilities = ["read"]
}

path "kv/metadata/concourse/workers" {
  capabilities = ["list"]
}

path "kv/data/concourse/web" {
  capabilities = ["read"]
}

path "kv/data/concourse/db" {
  capabilities = ["read"]
}

path "kv/data/concourse/pipelines/+/+/*" {
  capabilities = ["read"]
}
