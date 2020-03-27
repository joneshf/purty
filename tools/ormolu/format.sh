#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

readonly this_script="${0}"
readonly script_name="$(basename "${this_script}")"
readonly temporary_dir="$(mktemp --directory -t "${script_name}.XXXXXXXXXX")"
readonly log_file="$(mktemp -t "${script_name}.log.XXXXXXXXXX")"

ormolu=''
srcs=()
verbose=''

log() {
    local level="${1}"
    local msg="${2}"

    if [[ 'DEBUG' = "${level}" && -z "${verbose}" ]]; then
        printf '%-12s %s\n' "[${level}]" "${msg}" >> "${log_file}"
    else
        printf '%-12s %s\n' "[${level}]" "${msg}" | tee --append "${log_file}" >&2
    fi
}

debug()     { log 'DEBUG'     "${1}" ; }
info()      { log 'INFO'      "${1}" ; }
warning()   { log 'WARNING'   "${1}" ; }
error()     { log 'ERROR'     "${1}" ; }
emergency() { log 'EMERGENCY' "${1}" ; exit 1 ; }

#/ Formats Haskell files with ormolu
#/
#/ Mandatory arguments to long options are mandatory for short options too.
#/       --help                 display this help and exit
#/       --ormolu               path to ormolu binary
#/   -V, --verbose              display logs from all levels
#/                                normally only displays logs above 'DEBUG'
#/                                will still output all logs to the log file
usage() {
    grep '^#/' "${this_script}" | cut --characters 4-
    exit 0
}

while [[ $# -gt 0 ]]; do
    option="${1}"
    case "${option}" in
        --help) usage;;
        --ormolu)
            shift
            ormolu=$(readlink "${1}");;
        -V|--verbose) verbose='verbose';;
        *) srcs+=("${1}");;
    esac
    shift
done

cleanup() {
    local exit_code="$?"

    debug 'Removing temp directory'
    rm --force --recursive "${temporary_dir}"

    exit "$exit_code"
}

trap cleanup EXIT

debug "Created temp directory: ${temporary_dir}"
debug "Created log file: ${log_file}"

# End Boilerplate

cd "${BUILD_WORKSPACE_DIRECTORY}"

debug "Formatting ${srcs[*]} with ormolu"
"${ormolu}" --mode inplace "${srcs[@]}"
