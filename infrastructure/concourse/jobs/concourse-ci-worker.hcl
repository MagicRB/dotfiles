job "concourse-ci-worker" {
  datacenters = ["homelab-1"]
  type = "system"

  group "svc" {
    count = 1

    constraint {
      attribute = "${attr.unique.hostname}"
      operator = "regexp"
      value = "(heater|fractal)"
    }
    
    network {
      mode = "bridge"
    }

    task "create-secret" {
      driver = "docker"

      config {
	image = "magicrb/concourse-vault-runner@sha256:595011233c15e05ae23092cfb6e9fe0459d1c24fffc9bd519e5d32bec3b8e519"
	args = [
	  "${NOMAD_TASK_DIR}/main.sh"
	]
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

      template {
	data = <<EOF
if ! vault kv get kv/concourse/workers/{{ env "attr.unique.hostname" }} > /dev/null 2>&1
then
    concourse generate-key -t ssh -f /worker_key

    _worker_key="$(cat /worker_key)"
    _worker_key_pub="$(cat /worker_key.pub)"
    echo -e "$${_worker_key//$'\n'/\\\\n}" > /worker_key
    echo -e "$${_worker_key_pub//$'\n'/\\\\n}" > /worker_key.pub


    JSON_FMT='{"public_key":"%s","private_key":"%s"}'
    printf "$JSON_FMT" "$(< /worker_key.pub)" "$(< /worker_key)" > secret.json

    vault kv put kv/concourse/workers/{{ env "attr.unique.hostname" }} @secret.json
fi
EOF
	destination = "${NOMAD_TASK_DIR}/main.sh"
      }
    }

    task "worker" {
      driver = "docker"

      config {
	image = "concourse/concourse@sha256:9adc59ea1ccdb2d0262451d30ff0298dc92139ba7cfb8bfd99b1a469441594e0"
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

      kill_timeout = "1h"
      kill_signal = "SIGUSR2"

      resources {
	cpu = 32000
	memory = 2048 
      }
    }
  }
}
