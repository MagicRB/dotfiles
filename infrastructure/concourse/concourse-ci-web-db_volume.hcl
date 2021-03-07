type = "csi"
id = "concourse-ci-web-db"
name = "concourse-ci-web-db"
plugin_id = "nfs"
access_mode = "single-node-writer"
attachment_mode = "file-system"

context {
  server = "blowhole.in.redalder.org"
  share = "/concourse-ci-web-db"
}

mount_options {
  fs_type = "nfs"
}
