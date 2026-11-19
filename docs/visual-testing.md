# Visual regression tests

The approved SVG references live in `tests/testthat/_snaps/` and are versioned
with their tests. Every active comparison specifies an explicit relative
reference path, a title, and a plot:

```r
expect_flexplot_snapshot("feature/example.svg", "Example", plot)
```

The helper uses `vdiffr::write_svg()` and the same exact text comparator as
vdiffr. It never creates or updates an approved reference. Missing references
fail before the plot is evaluated during ordinary testing; changed output fails comparison. The
inventory test also checks all references exist, including when graphics tests
are skipped on CRAN, and rejects orphan SVGs or leftover `.new.svg` artifacts.
It also rejects reintroduction of the auto-accepting visual expectation API.
No rendering differences, identifiers, coordinates, or text are normalized away.

## Ordinary testing

```sh
Rscript --vanilla dev/visual-tests.R
Rscript --vanilla -e 'devtools::test()'
```

The visual command runs every test file containing a visual comparison plus the
helper/inventory regression tests. It uses a noninteractive device and opens no
application windows. SVG renders are temporary and removed after comparison.
The full package suite uses the same strict helper. Existing CRAN graphics-test
skips are preserved; baseline inventory and the helper's regression tests still
run in built-package checks.

## Intentional changes and new references

Generate review candidates explicitly, outside the reference directory:

```sh
Rscript --vanilla dev/visual-tests.R --candidates /tmp/flexplot-visual-review
```

This writes candidates in the same relative directory structure, while still
reporting failures for missing or changed approved references. A nonzero exit
status is expected until intentional differences have been reviewed. The
command refuses a candidate directory inside the approved reference directory.
Direct test runs can use `FLEXPLOT_SNAPSHOT_CANDIDATES` to request the same
candidate output; leave it unset for ordinary tests.

Inspect each changed/new SVG beside its reference in a browser or image viewer.
Investigate differences before acceptance. After review, copy only the specific
approved candidate, for example:

```sh
cp /tmp/flexplot-visual-review/feature/example.svg tests/testthat/_snaps/feature/example.svg
git diff -- tests/testthat/_snaps/feature/example.svg
Rscript --vanilla dev/visual-tests.R
```

For a new case, create its reference subdirectory before copying. Add the test
and approved SVG together. Remove a reference when its last test is removed.
Do not use blanket snapshot acceptance or commit generated candidates.

## Reproducibility and platform limits

The initial inventory contains 137 active references in 20 test files, totaling
about 9 MB. These are existing references reviewed against the current renderer,
not regenerated replacements. All use SVG engine version 2.0. Review found no
embedded machine paths or timestamps; clip identifiers derive from geometry.
Repeated exact comparisons are required rather than stripping SVG metadata.

The references were verified on macOS ARM64 with R 4.5.0, vdiffr 1.0.8 and ggplot2
4.0.3. Font metrics, graphics-engine changes, and plotting-package versions can
produce genuine environment differences. Cross-platform equivalence is not
assumed. Investigate and document such differences; do not weaken the comparator
or automatically accept new output. The SVG ignore exception is restricted to
the approved test directory; unrelated generated SVGs remain ignored.
