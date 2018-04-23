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

VERBOSE=''
VERSION="${npm_package_version:?npm_package_version env var is required}"

BASE_URL='https://dl.bintray.com/joneshf/generic'
LINUX_URL="${BASE_URL}/purty-${VERSION}-linux.tar.gz"
OSX_URL="${BASE_URL}/purty-${VERSION}-osx.tar.gz"
WIN_URL="${BASE_URL}/purty-${VERSION}-win32.tar.gz"

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

#/ Sets up npm package for packaging
#/
#/ Mandatory arguments to long options are mandatory for short options too.
#/       --help                 display this help and exit
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

    # debug 'Removing temp directory'
    # rm --force --recursive "${TEMPORARY_DIR}"

    exit "$exit_code"
}

trap cleanup EXIT

debug "Created temp directory: ${TEMPORARY_DIR}"
debug "Created log file: ${LOG_FILE}"

# End Boilerplate

cd "${DIR}/../.."

debug "version: ${VERSION}"
debug "creating directories for binaries"
mkdir --parents bin/linux bin/osx bin/win

debug "Downloading linux tar from ${LINUX_URL}"
curl --location --output "${TEMPORARY_DIR}/linux.tar.gz" "${LINUX_URL}" 2>&1 \
    | tee --append "${LOG_FILE}"

debug "Extracting linux binary"
tar --extract \
    --file "${TEMPORARY_DIR}/linux.tar.gz" \
    --directory "${TEMPORARY_DIR}" \
    --gzip

debug "Moving linux binary to the appropriate location"
mv "${TEMPORARY_DIR}/purty" "bin/linux/purty"

debug "Downloading osx tar from ${OSX_URL}"
curl --location --output "${TEMPORARY_DIR}/osx.tar.gz" "${OSX_URL}" 2>&1 \
    | tee --append "${LOG_FILE}"

debug "Extracting osx binary"
tar --extract \
    --file "${TEMPORARY_DIR}/osx.tar.gz" \
    --directory "${TEMPORARY_DIR}" \
    --gzip

debug "Moving osx binary to the appropriate location"
mv "${TEMPORARY_DIR}/purty" "bin/osx/purty"

debug "Downloading win tar from ${WIN_URL}"
curl --location --output "${TEMPORARY_DIR}/win.tar.gz" "${WIN_URL}" 2>&1 \
    | tee --append "${LOG_FILE}"

debug "Extracting win binary"
tar --extract \
    --file "${TEMPORARY_DIR}/win.tar.gz" \
    --directory "${TEMPORARY_DIR}" \
    --gzip

debug "Moving win binary to the appropriate location"
mv "${TEMPORARY_DIR}/purty.exe" "bin/win/purty.exe"
