#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later

zfs_src="${1}"
dst_dir="${2}"

function recurse_children()
{
    local volume="${1}"
    local dir="${2}"
    local relmount="${3}"

    for child in $children
    do
        if [ "${child}" == "${volume}" ]
        then
            continue
        fi

        mount_wc "${child}" "${dir}/$(basename "${child}")"
    done
}

function mount_wc()
{
    local volume="${1}"
    local dir="${2}"

    local relmount="$(zfs get -Ho value :relmount "${volume}")"
    local children="$(zfs list -Hrd 1 "${volume}" -o name | tr '\n' ' ')"

    if ! [ -z "${relmount}" ] && [ "${relmount}" != "-" ]
    then
        case "${relmount}" in
            "yes")
                mount -o X-mount.mkdir -t zfs "${volume}" "${dir}"
                recurse_children "${volume}" "${dir}" "${relmount}"
            ;;
            "pass")
                recurse_children "${volume}" "${dir}" "${relmount}"
            ;;
            "*")
            ;;
        esac
    fi
}

mount_wc "${zfs_src}" "${dst_dir}"
