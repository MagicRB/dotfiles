set -u +e

## Args
# - vars - a space separated list of variables
save_env() {
    local vars="$1"

    for var in $vars ; do
	echo "$var=${!var}"
    done
}

make_opt() {
    local name="$1"

    echo "_${name,,}"
}

## Args
# - var - variable name, for example PG_DATA
# - default - default value
default_opt() {
    local var="$1"
    local default="$2"

    local parsed="$(make_opt $var)"
    local result="${!var:-$default}"
    eval "$parsed=\"$result\""
}

## Args
# - var - variable name, for example PG_DATA
# - error - error message
required_opt() {
    local var="$1"
    local error="$2"

    if [[ ! -z "${!var:-}" ]] ; then
	local parsed="$(make_opt $var)"
	eval "$parsed=\"\$$var\""
    else
	echo_exit "$error"
    fi
}

## Args
# - path - path to the inode to check
# - uid - desired uid
# - gid - desired gid
check_owner() {
    local path="$1"
    local desired_uid="$2"
    local desired_gid="$3"

    uid=$($_prog_busybox/bin/stat -c '%u' $path || \
	      echo_exit "Failed to get uid of $path")
    gid=$($_prog_busybox/bin/stat -c '%g' $path || \
	      echo_exit "Failed to get gid of $path")

    [[ $uid == $desired_uid ]] && [[ $gid == $desired_gid ]] || \
	echo_exit "Invalid owner for \`$path\`, has $uid:$gid wanted $desired_uid:$desired_gid"

}

## Args
# - path - path to the directory to create
# - uid - desired uid
# - gid - desired gid
mkdir_chown() {
    path="$1"
    uid="$2"
    gid="$3"

    if [[ ! -e "$path" ]]
    then
	$_prog_busybox/bin/mkdir -p "$path"
	$_prog_busybox/bin/chown "$uid:$gid" "$path"
    else
	echo_exit "Path $path already exists"
    fi
}

## Args
# - uid - currently set uid
check_root() {
    local uid="$1"

    [[ "$uid" = "0" ]] \
	&& echo_exit "UID is set to $uid, which would cause an infinite loop!"
}

## Args
# - message - message to exit with
echo_exit() {
    local msg="$1 Exiting..."

    echo $msg
    exit 1
}

## Args
# - user - passwd formated user string
# - group - passwd formated group string
add_user() {
    local user="$1"
    local group="$2"

    (
	set -e
	echo "$user" > /etc/passwd
	echo "$group" > /etc/group
    )
}

## Args
# - ca-certificates.crt - nix path to ca-certificates.crt
create_ssl_certs() {
    local ca_certificates="$1/etc/ssl/certs/ca-bundle.crt"

    $_prog_busybox/bin/mkdir -p /etc/ssl/certs
    $_prog_busybox/bin/ln $ca_certificates /etc/ssl/certs/ca-bundle.crt
    $_prog_busybox/bin/ln $ca_certificates /etc/ssl/certs/ca-certificates.crt
}
