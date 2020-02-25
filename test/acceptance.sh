#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

# There's a good chunk of boilerplate here.
# Head down to the end of the boilerplate to see the tests.

readonly DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
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

    debug "Removing config file if it exists"
    rm -f .purty.dhall

    debug "Moving back to ${OLDPWD}"
    cd "${OLDPWD}"

    exit "$exit_code"
}

trap cleanup EXIT

debug "Created temp directory: ${TEMPORARY_DIR}"
debug "Created log file: ${LOG_FILE}"

debug "Moving to ${DIR}"
cd "${DIR}"

# End Boilerplate

# To write a test:
# 1. Start off with a `debug` about what the test will do
# 2. Run the test. In most cases, letting it crash is fine.
# 3. Give some `info` about the test that just passed.

debug 'Testing if exit code is non-zero for parse errors'
debug 'Turning off "errexit" so the command does not exit the script'
set +o errexit
"${PURTY}" './acceptance/Unparsable.purs' 2&> /dev/null
unparseable_exit_code="${?}"
debug 'Turning on "errexit" so failed commands exit the script'
set -o errexit
debug "unparseable_exit_code: ${unparseable_exit_code}"
[[ "${unparseable_exit_code}" -ne 0 ]]
info 'Exit code is non-zero for parse errors'

debug 'Testing if absolute paths work'
"${PURTY}" "$(pwd)/acceptance/Test.purs" > /dev/null
info 'Absolute file paths work'

debug 'Testing if relative paths work'
"${PURTY}" "./acceptance/Test.purs" > /dev/null
info 'Relative file paths work'

debug 'Testing if paths with .. work'
"${PURTY}" "../test/acceptance/Test.purs" > /dev/null
info 'Paths with .. work'

debug 'Testing if STDIN works'
"${PURTY}" - < "$(pwd)/acceptance/Test.purs" > /dev/null
info 'STDIN works'

debug 'Testing if directories work'
"${PURTY}" "$(pwd)/acceptance/directories" > /dev/null
info 'Directories works'

debug 'Testing if version mode works'
expected_version="Purty version: $(cat "$(pwd)/../version/purty")"
actual_version=$("${PURTY}" version)
if [[ "${expected_version}" != "${actual_version}" ]]; then
    error "Expected: ${expected_version}"
    error "Actual: ${actual_version}"

    exit 1
fi
info 'Version mode works'

debug 'Testing if version --numeric flag works'
expected_version=$(cat "$(pwd)/../version/purty")
actual_version=$("${PURTY}" version --numeric)
if [[ "${expected_version}" != "${actual_version}" ]]; then
    error "Expected: ${expected_version}"
    error "Actual: ${actual_version}"

    exit 1
fi
info 'Version --numeric flag works'
