#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

readonly DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly THIS_SCRIPT="${0}"
readonly SCRIPT_NAME="$(basename "${THIS_SCRIPT}")"
readonly TEMPORARY_DIR="$(mktemp --directory -t "${SCRIPT_NAME}.XXXXXXXXXX")"
readonly LOG_FILE="$(mktemp -t "${SCRIPT_NAME}.log.XXXXXXXXXX")"

FILE=''
VERBOSE=''
VERSION=''

log() {
    local level="${1}"
    local msg="${2}"

    if [[ 'DEBUG' = "${level}" && -z "${VERBOSE}" ]]; then
        printf '%-12s %s\n' "[${level}]" "${msg}" >> "${LOG_FILE}"
    else
        printf '%-12s %s\n' "[${level}]" "${msg}" | tee --append "${LOG_FILE}" >&2
    fi
}

debug()     { log 'DEBUG'     "${1}" ; }
info()      { log 'INFO'      "${1}" ; }
warning()   { log 'WARNING'   "${1}" ; }
error()     { log 'ERROR'     "${1}" ; }
emergency() { log 'EMERGENCY' "${1}" ; exit 1 ; }

#/ Creates the bintray JSON file and tarball
#/
#/ Mandatory arguments to long options are mandatory for short options too.
#/       --help                 display this help and exit
#/       --file <FILE>          name of the output tarball
#/       --version <VERSION>    version of the binary
#/   -V, --verbose              display logs from all levels
#/                                normally only displays logs above 'DEBUG'
#/                                will still output all logs to the log file
usage() {
    grep '^#/' "${THIS_SCRIPT}" | cut --characters 4-
    exit 0
}

while [[ $# -gt 0 ]]; do
    option="${1}"
    case "${option}" in
        --help) usage;;
        --file)
            shift
            FILE="${1}";;
        --version)
            shift
            VERSION="${1}";;
        -V|--verbose) VERBOSE='verbose';;
        *)
            error "${THIS_SCRIPT}: unrecognized option '${option}'"
            error "Try '${THIS_SCRIPT} --help' for more information"
            exit 2;;
    esac
    shift
done

cleanup() {
    local exit_code="$?"

    debug 'Removing temp directory'
    rm --force --recursive "${TEMPORARY_DIR}"

    exit "$exit_code"
}

trap cleanup EXIT

debug "Created temp directory: ${TEMPORARY_DIR}"
debug "Created log file: ${LOG_FILE}"

# End Boilerplate

if [[ -z "${FILE}" ]]; then
    error '--file is required'
    error "Try '${THIS_SCRIPT} --help' for more information"
    exit 1
fi

if [[ -z "${VERSION}" ]]; then
    error '--version is required'
    error "Try '${THIS_SCRIPT} --help' for more information"
    exit 1
fi

cd "${DIR}/../.."

DATE=$(date '+%Y-%m-%d')

debug 'Installing dhall-to-json if it is not already installed'
stack build dhall-json

info 'Generating JSON file'
stack exec dhall-to-json \
      <<< "./ci/bintray.dhall {date = \"${DATE}\", version = \"${VERSION}\", tarFile = \"${FILE}\"}" \
      > ci/bintray.json

info 'Creating tarball'
tar --create --file "${FILE}" --directory bin --gzip purty
