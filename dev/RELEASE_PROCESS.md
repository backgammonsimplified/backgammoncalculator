# Release Process

## Development repository

- Develop locally at version `0.0.0.9000`.
- Do not create a remote, public release, or release tag.
- Require documentation, tests, and package checks for accepted changes.

## First public development release

The planned first public version is `0.1.0`.

Before preparing it:

1. Freeze the accepted package source and generated documentation.
2. Run package tests and `R CMD check` on the built source tarball.
3. Run CRAN-oriented checks on current R release and R-devel.
4. Review `THIRD_PARTY_NOTICES.md` and all copied or adapted code.
5. Replace the GitHub noreply maintainer address with an email address
   that can receive and confirm CRAN submission mail.
6. Recheck the package name against current and archived CRAN packages
   and current Bioconductor packages.
7. Update `Version` from `0.0.0.9000` to `0.1.0`.
8. Build the final source tarball from the accepted source tree.

## Clean public repository

Create the public repository from a clean copy of the accepted `0.1.0` source tree, without the development `.git` directory.

Before removing the development history, create an offline Git bundle or equivalent archive. The archive is not part of the public release.

Then:

1. initialize the clean public Git repository;
2. make the initial public commit;
3. tag that commit `v0.1.0`;
4. publish the repository and release artifacts only after approval;
5. submit the already checked source tarball to CRAN separately.
