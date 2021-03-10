# -*- mode: shell-script; -*-

cat << EOF
### Nix Image Manual

/nix/var/nix/db
/nix/var/nix/daemon-socket
/nix/store-host

EOF

if [[ -d "/nix/store-host" ]]
then
    mount -t overlay overlay -o lowerdir=/nix/store:/nix/store-host /nix/store
    export NIX_REMOTE=daemon
else
    echo "Running in single user mode!"
fi

bash "$@"

# nix --experimental-features 'nix-command flakes' build github:edolstra/dwarffs
