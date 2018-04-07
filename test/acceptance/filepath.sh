#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

readonly DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly THIS_SCRIPT="${0}"
readonly LOG_FILE="/tmp/$(basename "${THIS_SCRIPT}").log"

SYSTEM_GHC=''

#/ Run an acceptance test
#/
#/ Options:
#/       --help     display this help and exit
#/       --system-ghc
#/                  Use the GHC version install outside of stack
usage() {
    grep '^#/' "${THIS_SCRIPT}" | cut --characters 4-
    exit 0
}

while [[ $# -gt 0 ]]; do
    option="${1}"
    case "${option}" in
        --help) usage;;
        --system-ghc) SYSTEM_GHC='--system-ghc';;
        *)
            echo "${THIS_SCRIPT}: unrecognized option '${option}'"
            echo "Try '${THIS_SCRIPT} --help' for more information"
            exit 2;;
    esac
    shift
done

info()    { echo "[INFO]    $*" | tee --append "${LOG_FILE}" >&2 ; }
warning() { echo "[WARNING] $*" | tee --append "${LOG_FILE}" >&2 ; }
error()   { echo "[ERROR]   $*" | tee --append "${LOG_FILE}" >&2 ; }
fatal()   { echo "[FATAL]   $*" | tee --append "${LOG_FILE}" >&2 ; exit 1 ; }

cleanup() {
    local exit_code="$?"

    popd

    exit "$exit_code"
}

trap cleanup EXIT

pushd "${DIR}"

# End Boilerplate

stack "${SYSTEM_GHC}" build purty:exe:purty

echo 'Absolute file paths work'
stack "${SYSTEM_GHC}" exec purty "$(pwd)/Test.purs" > /dev/null

echo 'Relative file paths work'
stack "${SYSTEM_GHC}" exec purty "./Test.purs" > /dev/null
