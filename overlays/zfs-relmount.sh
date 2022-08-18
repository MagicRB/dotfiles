#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later

function recurse_children()
{
    local volume="${1}"
    local dir="${2}"
    local relmount="${3}"
    local children="${4}"
    local action="${5}"

    for child in $children
    do
        if [ "${child}" == "${volume}" ]
        then
            continue
        fi

        recursive_perform "${child}" "${dir}/$(basename "${child}")" "${action}"
    done
}

function recursive_perform()
{
    local volume="${1}"
    local dir="${2}"
    local action="${3}"

    local relmount="$(zfs get -Ho value :relmount "${volume}")"
    local children="$(zfs list -Hrd 1 "${volume}" -o name | tr '\n' ' ')"

    if ! [ -z "${relmount}" ] && [ "${relmount}" != "-" ]
    then
        case "${relmount}" in
            "yes")
                eval "${action}"
                recurse_children "${volume}" "${dir}" "${relmount}" "${children}" "${action}"
                ;;
            "pass")
                recurse_children "${volume}" "${dir}" "${relmount}" "${children}" "${action}"
                ;;
            "*")
                ;;
        esac
    fi
}

action="${1}"
shift 1

case $action in
    "mount")
        zfs_src="${1}"
        dst_dir="${2}"

        recursive_perform "${zfs_src}" "${dst_dir}" 'mount -o X-mount.mkdir -t zfs "${volume}" "${dir}"'
        ;;
    "mount-snapshot")
        zfs_src="${1}"
        dst_dir="${2}"
        snapshot="${3}"

        recursive_perform "${zfs_src}" "${dst_dir}" 'mount -o X-mount.mkdir -t zfs "${volume}"@'"${snapshot}"' "${dir}"'
        ;;
    "umount")
        zfs_src="${1}"
        dst_dir="${2}"

        recursive_perform "${zfs_src}" "${dst_dir}" 'umount -t zfs "${dir}"'
        ;;
    "snapshot")
        root="${1}"
        snapshot="${2}"

        recursive_perform "${root}" "${root}" 'zfs snapshot "${volume}"@'"${snapshot}"
        ;;
    "*")
        ;;
esac
