#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

# There's a good chunk of boilerplate here.
# Head down to the end of the boilerplate to see the tests.

readonly THIS_SCRIPT="${0}"
readonly SCRIPT_NAME="$(basename "${THIS_SCRIPT}")"
readonly TEMPORARY_DIR="$(mktemp --directory -t "${SCRIPT_NAME}.XXXXXXXXXX")"
readonly LOG_FILE="$(mktemp -t "${SCRIPT_NAME}.log.XXXXXXXXXX")"

PURTY='../bin/purty'
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

#/ Run the acceptance tests
#/
#/ Options:
#/       --help                 display this help and exit
#/       --purty                path to the purty binary
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
        --purty)
            shift
            PURTY="${1}";;
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

    exit "$exit_code"
}

trap cleanup EXIT

debug "Created temp directory: ${TEMPORARY_DIR}"
debug "Created log file: ${LOG_FILE}"

# End Boilerplate

# To write a test:
# 1. Start off with a `debug` about what the test will do
# 2. Run the test. In most cases, letting it crash is fine.
# 3. Give some `info` about the test that just passed.

function suite_format() {
    local purty=("$@")

    debug 'Testing if exit code is non-zero for parse errors'
    if "${purty[@]}" './test/acceptance/Unparsable.purs' 2> /dev/null; then
        error 'Exit code is zero for parse errors'

        exit 1
    else
        debug "unparseable exit code: ${?}"
        info 'Exit code is non-zero for parse errors'
    fi

    debug 'Testing if absolute paths work'
    "${purty[@]}" "$(pwd)/test/acceptance/Test.purs" > /dev/null
    info 'Absolute file paths work'

    debug 'Testing if relative paths work'
    "${purty[@]}" "./test/acceptance/Test.purs" > /dev/null
    info 'Relative file paths work'

    debug 'Testing if paths with .. work'
    "${purty[@]}" "./test/../test/acceptance/Test.purs" > /dev/null
    info 'Paths with .. work'

    debug 'Testing if STDIN works'
    "${purty[@]}" - < "$(pwd)/test/acceptance/Test.purs" > /dev/null
    info 'STDIN works'

    debug 'Testing if directories work'
    "${purty[@]}" "$(pwd)/test/acceptance/directories" > /dev/null
    info 'Directories works'
}

function suite_validate() {
    local purty=("$@")

    debug 'Testing if validate mode works'
    debug 'Testing if exit code is non-zero for unformatted files'
    if "${purty[@]}" validate './test/acceptance/Unformatted.purs' 2> /dev/null; then
        error 'Exit code is zero for unformatted files'

        exit 1
    else
        debug "unformatted exit code: ${?}"
        info 'Exit code is non-zero for unformatted files'
    fi

    debug 'Testing if exit code is non-zero for unformatted directories'
    if "${purty[@]}" validate './test/acceptance/directories' 2> /dev/null; then
        error 'Exit code is zero for unformatted directories'

        exit 1
    else
        debug "unformatted exit code: ${?}"
        info 'Exit code is non-zero for unformatted directories'
    fi

    debug 'Testing if exit code is zero for formatted absolute file paths'
    "${purty[@]}" validate "$(pwd)/test/acceptance/Formatted.purs" > /dev/null
    info 'Exit code is zero for formatted absolute file paths'

    debug 'Testing if exit code is zero for formatted relative paths'
    "${purty[@]}" validate "./test/acceptance/Formatted.purs" > /dev/null
    info 'Exit code is zero for formatted relative file paths'

    debug 'Testing if exit code is zero for formatted paths with ..'
    "${purty[@]}" validate "./test/../test/acceptance/Formatted.purs" > /dev/null
    info 'Exit code is zero for formatted paths with ..'

    debug 'Testing if exit code is zero for formatted STDIN works'
    "${purty[@]}" validate - < "$(pwd)/test/acceptance/Formatted.purs" > /dev/null
    info 'Exit code is zero for formatted STDIN'

    debug 'Testing if exit code is zero for formatted directories work'
    "${purty[@]}" validate "$(pwd)/test/acceptance/directories/formatted" > /dev/null
    info 'Exit code is zero for formatted directories'
}

function suite_version() {
    local purty=("$@")

    debug 'Testing if version mode works'
    expected_version="Purty version: $(cat version/purty)"
    actual_version=$("${purty[@]}" version)
    if [[ "${expected_version}" != "${actual_version}" ]]; then
        error "Expected: ${expected_version}"
        error "Actual: ${actual_version}"

        exit 1
    fi
    info 'Version mode works'

    debug 'Testing if version --numeric flag works'
    expected_version=$(cat version/purty)
    actual_version=$("${purty[@]}" version --numeric)
    if [[ "${expected_version}" != "${actual_version}" ]]; then
        error "Expected: ${expected_version}"
        error "Actual: ${actual_version}"

        exit 1
    fi
    info 'Version --numeric flag works'
}

info 'Testing format without any command'
suite_format "${PURTY}"

info 'Testing format with format command'
suite_format "${PURTY}" format

info 'Testing validate command'
suite_validate "${PURTY}"

info 'Testing version command'
suite_version "${PURTY}"
