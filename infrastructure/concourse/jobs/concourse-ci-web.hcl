job "concourse-ci-web" {
  datacenters = ["homelab-1"]
  type = "service"

  group "svc" {
    count = 1

    volume "concourse-ci-web-db" {
      type = "csi"
      source = "concourse-ci-web-db"
      read_only = false
    }

    network {
      mode ="bridge"

      port "db" {
	to = "5432"
      }
      port "http" {
	static = "8019"
	to = "8080"
      }
      port "tsa" {
	static = "1922"
	to = "2222"
      }
    }

    service {
      name = "concourse-web"
      port = "http"
      
      check {
	type = "http"
	path = "/"
	interval = "2s"
	timeout = "2s"
      }
    }

    service {
      name = "concourse-tsa"
      port = "2222"
    }

    service {
      name = "concourse-db"
      port = "db"
    }

    task "db" {
      driver = "docker"

      config {
	image = "postgresql:local"
	ports = ["db"]

	volumes = [
	  "secrets/main.sh:/data/scripts/main.sh",
	]
      }

      volume_mount {
	volume = "concourse-ci-web-db"
	destination = "/data/postgresql"
	read_only = false
      }

      vault {
	policies = ["concourse-db-policy"]
      }

      template {
	data = <<EOF
{{ with secret "kv/data/concourse/db" }}
USER={{ .Data.data.root_user }}
PASSWORD={{ .Data.data.root_password }}
{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/data.env"
	env = true
      }

      template {
	data = <<EOF
#!/usr/bin/env bash

env

{{ with secret "kv/data/concourse/db" }}
if process_psql -tc "SELECT 1 FROM pg_database WHERE datname = '{{ .Data.data.database }}'" | grep -q 1
then
    process_psql -c "ALTER USER {{ .Data.data.user }} WITH PASSWORD '{{ .Data.data.password }}'";
else
    process_psql -c "CREATE DATABASE {{ .Data.data.database }}"
    process_psql -c "CREATE USER {{ .Data.data.user }} WITH ENCRYPTED PASSWORD '{{ .Data.data.password }}'"
    process_psql -c "GRANT ALL PRIVILEGES ON DATABASE {{ .Data.data.database }} TO {{ .Data.data.user }}"
{{ end }}

    echo "host all all all md5" >> /data/postgresql/pg_hba.conf
    cat << EOD >> /data/postgresql/postgresql.conf
listen_addresses = '0.0.0.0'
password_encryption = md5
EOD
fi
EOF
	destination = "${NOMAD_SECRETS_DIR}/main.sh"
      }

      resources {
	cpu = 3000
	memory = 512 
      }
    }

    task "web" {
      driver = "docker"

      config {
	image = "concourse/concourse@sha256:9adc59ea1ccdb2d0262451d30ff0298dc92139ba7cfb8bfd99b1a469441594e0"
	command = "web"
	ports = ["http", "tsa"]
      }

      vault {
	policies = ["concourse-web-policy"]
      }

      restart {
	attempts = 5
	delay = "15s"
      }

      template {
	data = <<EOF
[[ with secret "kv/data/concourse/web" ]]
CONCOURSE_ADD_LOCAL_USER=[[ .Data.data.local_user_name ]]:[[ .Data.data.local_user_pass ]]
CONCOURSE_MAIN_TEAM_LOCAL_USER=[[ .Data.data.local_user_name ]]
[[ end ]]

CONCOURSE_SESSION_SIGNING_KEY=[[ env "NOMAD_SECRETS_DIR" ]]/session_signing_key
CONCOURSE_TSA_HOST_KEY=[[ env "NOMAD_SECRETS_DIR" ]]/tsa_host_key
CONCOURSE_TSA_AUTHORIZED_KEYS=[[ env "NOMAD_SECRETS_DIR" ]]/authorized_worker_keys

CONCOURSE_EXTERNAL_URL=http://blowhole.in.redalder.org:8019/

CONCOURSE_POSTGRES_HOST=127.0.0.1
CONCOURSE_POSTGRES_PORT=5432
[[ with secret "kv/data/concourse/db" ]]
CONCOURSE_POSTGRES_DATABASE=[[ .Data.data.database ]]
CONCOURSE_POSTGRES_USER=[[ .Data.data.user ]]
CONCOURSE_POSTGRES_PASSWORD=[[ .Data.data.password ]]
[[ end ]]

CONCOURSE_VAULT_URL=https://vault.in.redalder.org:8200/
CONCOURSE_VAULT_CA_CERT=[[ env "NOMAD_SECRETS_DIR" ]]/vault.crt
CONCOURSE_VAULT_PATH_PREFIX=kv/concourse/pipelines

CONCOURSE_VAULT_CLIENT_TOKEN=[[ env "VAULT_TOKEN" ]]
CONCOURSE_VAULT_LOOKUP_TEMPLATES=/{{.Team}}/{{.Pipeline}}/{{.Secret}},/{{.Team}}/{{.Secret}}
EOF
	destination = "${NOMAD_SECRETS_DIR}/data.env"
	env = true

	left_delimiter = "[["
	right_delimiter = "]]"
      }

      template {
	data = <<EOF
{{ with secret "kv/data/concourse/web" }}{{ .Data.data.session_signing_key }}{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/session_signing_key"
      }

      template {
	data = <<EOF
{{ with secret "kv/data/concourse/web" }}{{ .Data.data.tsa_host_key }}{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/tsa_host_key"
      }

      template {
	data = <<EOF
{{ with secret "kv/data/concourse/web" }}{{ .Data.data.redalder_org_cert }}{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/vault.crt"
      }


      template {
	data = <<EOF
{{ range secrets "kv/metadata/concourse/workers/" }}
{{ with secret (printf "kv/data/concourse/workers/%s" .) }}
{{ .Data.data.public_key }}
{{ end }}
{{ end }}
EOF
	destination = "${NOMAD_SECRETS_DIR}/authorized_worker_keys"
	change_mode = "signal"
	change_signal = "SIGHUP"
      }

      resources {
	cpu = 3000
	memory = 512 
      }
    }
  }
}
