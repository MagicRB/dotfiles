agent_prefix "" {
  policy = "read"
}

node_prefix "" {
  policy = "read"
}

service_prefix "" {
  policy = "write"
}

# uncomment if using Consul KV with Consul Template
key_prefix "" {
   policy = "read"
}
