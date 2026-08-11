# Website handoff: backgammoncalculator documentation

## Status

The package-side documentation work is collected in `backgammonsimplified/backgammoncalculator` PR #5 on branch `docs/api-examples`.

The companion website integration is `backgammonsimplified/backgammonsimplified.github.io` PR #16 on branch `docs/backgammoncalculator-package-docs`.

The intended published route is:

`/about/backgammoncalculator/`

The package documentation should be generated from the package source. Do not maintain a second copied API reference in the website repository.

## What the package PR provides

PR #5 contains the package-facing material the website build should consume:

- a task-oriented package landing page and `?backgammoncalculator-examples` runnable API tour;
- README quick-start material, a five-minute ER workflow, and examples for the public API families;
- a visual gallery for `plot_er_comparison()`, `plot_pair_outcomes()`, and `plot_density_distributions()`;
- checked-in gallery SVGs under `man/figures/` so the README and generated site have stable previews;
- `_pkgdown.yml` with grouped reference navigation and the canonical `/about/backgammoncalculator/` site URL;
- `pkgdown/extra.css` plus the shared `/assets/bs-shared.css` stylesheet hook and Backgammon Simplified typography/brand variables;
- the package `DESCRIPTION` documentation URL pointing at the canonical website route;
- `.Rbuildignore` entries keeping `_pkgdown.yml`, `pkgdown/`, and `docs/` out of the CRAN source package;
- a minimal `patchwork::plot_layout` namespace import required so the documented density-comparison plot composes correctly in a clean R session;
- generated `.Rd` files kept in sync with the roxygen source.

The documentation work does not change GNUID/XGID conversion semantics or redesign the plotting API.

## Website integration contract

Website PR #16 implements the hosting side:

- Quarto runs `scripts/build_backgammoncalculator_docs.R` as a post-render step;
- the script clones `backgammonsimplified/backgammoncalculator` and builds pkgdown from the actual package source;
- the default package ref is `master`;
- `BACKGAMMONCALCULATOR_REF` can point a preview build at another branch or tag;
- `BACKGAMMONCALCULATOR_SOURCE` can point a preview build at a local package checkout;
- the generated pkgdown output is copied into `site/_site/about/backgammoncalculator/`;
- the `/about/` package card links to the generated documentation while retaining a source-repository link.

For a pre-merge preview, run the website repository's normal Quarto render with:

`BACKGAMMONCALCULATOR_REF=docs/api-examples`

After package PR #5 is merged, the website build can use its default `master` ref.

## Merge and publication order

1. Review and merge package PR #5 after its package checks and documentation review are satisfactory.
2. Update or refresh website PR #16 against the package's merged `master` if needed.
3. Render the website and verify the generated package docs at `/about/backgammoncalculator/`.
4. Merge website PR #16 when the integrated site preview is satisfactory.

Do not deploy a separate GitHub Pages site from the package repository. The canonical browser documentation lives inside the main Backgammon Simplified website.

## Website review checklist

Verify the following in the integrated site build:

- `/about/backgammoncalculator/` opens the pkgdown landing page;
- the package reference navigation contains Start here, Position identifiers, Error rate and runtime, Outcomes and distributions, Plots, and Branding and themes;
- the README/landing-page gallery renders all three SVG examples;
- `?backgammoncalculator-examples` content appears as the corresponding pkgdown reference page and the runnable examples are readable in the browser;
- package links resolve within the `/about/backgammoncalculator/` route rather than to a separate package Pages site;
- the Marty Gale author link resolves to `/about/`;
- `/assets/bs-shared.css` is loaded and the package pages visually match the main Backgammon Simplified site;
- mobile and narrow layouts keep code blocks, navigation, and gallery images usable;
- the package source-repository link remains available from `/about/`.

## Validation already completed on the package PR

At the package PR head immediately before this handoff was added, both GitHub workflows were green:

- `R-CMD-check`: success;
- `Release tarball validation`: success.

A local `R CMD build .` also completed successfully during manual review. A final reviewer may still run the built tarball and inspect the generated browser documentation before merge.

## Files a website worker should treat as source material

The main package-side source files are:

- `README.md`
- `R/api_examples.R`
- `R/backgammoncalculator-package.R`
- `R/plot_er_comparison.R`
- `R/prepare_pair_outcomes.R`
- `_pkgdown.yml`
- `pkgdown/extra.css`
- `man/figures/gallery-er-comparison.svg`
- `man/figures/gallery-pair-outcomes.svg`
- `man/figures/gallery-density-distributions.svg`

Generated `.Rd` files are outputs of the roxygen documentation and should not become a separately maintained website content source.
