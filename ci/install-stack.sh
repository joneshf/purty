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

INSTALL_DIR="${HOME}/.local/bin"
OS='linux'
VERBOSE=''

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

#/ Install the stack binary at a specific path
#/
#/ Mandatory arguments to long options are mandatory for short options too.
#/   -d, --dir PATH             path to install the stack binary
#/                                defaults to '${HOME}/.local/bin'
#/       --help                 display this help and exit
#/       --os OS                install the stack binary for the given OS
#/                                defaults to 'linux'
#/                                valid values: 'linux', 'osx'
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
        -d|--dir)
            shift
            INSTALL_DIR="${1}";;
        --help) usage;;
        --os)
            shift
            os="${1}"
            case "${os}" in
                linux) OS='linux';;
                osx) OS='osx';;
                *)
                    error "${THIS_SCRIPT}: unrecognized os: '${os}'"
                    error "valid values: 'linux', 'osx'"
                    exit 2;;
            esac;;
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

readonly URL="https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-${OS}-x86_64.tar.gz"

debug 'Checking if Stack is already installed'

if [ -f "${INSTALL_DIR}/stack" ]; then
    info "Stack is already installed in ${INSTALL_DIR}"
else
    debug "Did not find binary in ${INSTALL_DIR}"
    info "Installing Stack for ${OS}..."

    debug "Downloading tar from ${URL}"
    wget --append-output "${LOG_FILE}" \
         --output-document "${TEMPORARY_DIR}/stack.tar.gz" \
         --show-progress \
         "${URL}"

    debug "Extracting stack binary"
    tar --extract \
        --file "${TEMPORARY_DIR}/stack.tar.gz" \
        --directory "${TEMPORARY_DIR}" \
        --gzip \
        --strip-components 1

    debug "Moving stack binary to the appropriate location"
    mv "${TEMPORARY_DIR}/stack" "${INSTALL_DIR}/stack"

    info "Installed Stack for ${OS}!"
fi

"${INSTALL_DIR}/stack" --version
