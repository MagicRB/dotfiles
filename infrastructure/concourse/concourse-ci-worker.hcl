job "concourse-ci-worker" {
  datacenters = ["homelab-1"]
  type = "system"

  group "svc" {
    count = 1
    
    network {
      mode = "bridge"
    }

    task "create-secret" {
      driver = "docker"

      config {
	image = "concourse-vault-sidecar:local"
      }

      vault {
	policies = ["concourse-worker-policy"]
      }

      lifecycle {
	sidecar = false
	hook = "prestart"
      }

      template {
	data = <<EOF
HOST_HOSTNAME="{{ env "node.unique.name" }}"
VAULT_ADDR="https://vault.in.redalder.org:8200/"
EOF
	env = true
	destination = "${NOMAD_TASK_DIR}/data.env"
      }
    }

    task "worker" {
      driver = "docker"

      config {
	image = "concourse/concourse@sha256:fa136abb336f2c2aed8d41d21b382d364c3387c24f3fdef15c720c292c9216d4"
	command = "worker"
	privileged = true
      }

      vault {
	policies = ["concourse-worker-policy"]
      }

      template {
	data = <<EOF
CONCOURSE_WORK_DIR=/opt/concourse/worker
CONCOURSE_TSA_HOST=10.64.1.201:1922
CONCOURSE_TSA_PUBLIC_KEY={{ env "NOMAD_SECRETS_DIR" }}/tsa_host_key.pub
CONCOURSE_TSA_WORKER_PRIVATE_KEY={{ env "NOMAD_SECRETS_DIR" }}/worker.key
EOF
	env = true
	destination = "${NOMAD_SECRETS_DIR}/data.env"
      }

      template {
	data = <<EOF
{{ with secret (printf "kv/data/concourse/workers/%s" (env "node.unique.name") ) }}
{{ .Data.data.private_key }}
{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/worker.key"
      }

      template {
	data = <<EOF
{{ with secret "kv/data/concourse/web" }}{{ .Data.data.tsa_host_key_pub }}{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/tsa_host_key.pub"
      }
    }
  }
}
