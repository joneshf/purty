## What is our release process?

We are currently doing time-based releases.
This is a release process inspired by many other projects:
[GitLab release process][], [GNOME's Time-Based Release Schedule][], [Ubuntu's TimeBasedReleases][].

Choosing time-based releases gives us a few benefits:
* We can keep a regular pace of development
* Motivation for finishing something at a given time increases
* People working on the software can have some expectations of what they're working on
* People using the software can have confidence about when releases come out

The most important part is to release when we say we will,
even if that means cutting items from the release.
In this process, it's better to be consistent with release dates than it is to hold out for a specific feature.

## When do we release?

We release a new version every Tuesday at 9AM Pacific.
The date and time were chosen arbitrarily and will probably change in the future as development slows down.

The Monday before release we freeze the code at 9AM Pacific.
This is to prevent a last-minute dash to get something in without checking it over.
The rest of the time between freeze and release is dedicated to making release notes, testing the version and preparing distributions.

## How do we release a new version?

1. Freeze the code at 9AM Pacific the Monday prior.
1. Find all of the issues between the previous release and the current release.
1. Add an entry to the [CHANGELOG.md][] that has the new version and all changes.
    * Separate the changes into additions, [breaking] changes, and deletions.
    * Even if the change is a "bug fix" it fits into one of these categories.
1. Merge the [CHANGELOG.md][] into `master`.
1. Create a tag with the version being released.
    * This will kick off builds of the OSX and Windows binaries and upload them to our [Bintray package][].
    * The Linux binary has to be manually added to our [Bintray package][].
        1. Download the binary from the pipeline.
        1. Add the version to our [Bintray package][].
            * If there is already a version for this release, add the Linux binary to the version.
            * If there is not a version for this release, [create a new version][] with the appropriate details.
1. Download a binary on your machine and test a few files.
1. If you notice any problems, issue them like normal.
    * Show stopping issues will turn into patch fixes and be released ASAP.
    * Normal issues will be prioritized like any other issue and released when it makes sense.
    * You are not on the hook to fix a problem just because you found it.
1. Celebrate! :tada:
    * Releasing software is a great thing, and no small effort. Be proud of your accomplishment.

[Bintray package]: https://bintray.com/joneshf/generic/purty
[CHANGELOG.md]: ./CHANGELOG.md
[create a new version]: https://bintray.com/joneshf/generic/purty/new/version
[GitLab release process]: https://about.gitlab.com/2015/12/17/gitlab-release-process/
[GNOME's Time-Based Release Schedule]: https://wiki.gnome.org/ReleasePlanning/TimeBased
[Ubuntu's TimeBasedReleases]: https://wiki.ubuntu.com/TimeBasedReleases