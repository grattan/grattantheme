# CLAUDE.md - AI Assistant Guide for grattantheme

## Project Overview

**grattantheme** is an R package that enables users to create ggplot2 charts conforming to the Grattan Institute style guide. It provides themes, color palettes, saving utilities, and helper functions for creating publication-ready visualizations with consistent styling.

- **Version:** 1.3.0
- **License:** MIT (Copyright 2020 Matt Cowgill)
- **Primary Language:** R (requires R >= 3.5.0)
- **Package Type:** ggplot2 extension package
- **Repository:** https://github.com/grattan/grattantheme

## Repository Structure

```
grattantheme/
├── R/                      # Source code (30 files, ~4,630 lines)
│   ├── theme_grattan*.R    # Theme functions
│   ├── grattan_save*.R     # Chart export functions
│   ├── create_grattan_colours.R  # Color definitions (958 lines)
│   ├── grattan_pal.R       # Palette functions
│   ├── grattan_scales.R    # ggplot2 scales
│   ├── save_chartdata.R    # Excel export
│   ├── chart_helpers.R     # Utility functions
│   └── ...
├── man/                    # Auto-generated documentation (180 .Rd files)
├── tests/testthat/         # Test files (24 test files)
│   └── _snaps/             # Visual regression test snapshots
├── data/                   # Package data objects (.rda files)
├── data-raw/               # Scripts to create package data
├── inst/                   # Package resources
│   ├── extdata/            # PowerPoint templates (9 .pptx files)
│   └── rmarkdown/templates/ # R Markdown templates
├── vignettes/              # Package documentation
│   └── using_grattantheme.Rmd
├── .github/workflows/      # CI/CD automation (3 workflows)
├── DESCRIPTION             # Package metadata
├── NAMESPACE               # Exported functions (roxygen2-generated)
├── NEWS.md                 # Version history and changelog
└── README.md               # User-facing documentation
```

## Key Components

### Core Functionality

1. **Theming Functions** (R/theme_grattan*.R)
   - `theme_grattan()` - Main ggplot2 theme
   - `theme_grattan_base()` - Base theme implementation
   - `theme_grattan_normal()` - Normal chart variant
   - `theme_grattan_scatter()` - Scatter plot variant

2. **Color Management** (R/create_grattan_colours.R, R/grattan_pal.R)
   - 172 exported color objects (e.g., `grattan_orange`, `grattan_blue`)
   - Each color has 8 tint variants (e.g., `grattan_orange1` through `grattan_orange8`)
   - Color palette functions: `make_grattan_pal()`, `grattan_pal()`
   - Scale functions: `scale_colour_grattan()`, `scale_fill_grattan()`

3. **Chart Saving** (R/grattan_save*.R)
   - `grattan_save()` - Save in various formats (PNG, PDF, etc.)
   - `grattan_save_pptx()` - PowerPoint export with templates
   - `grattan_save_all()` - Save in all formats at once
   - `save_chartdata()` - Export chart data to Excel

4. **Utility Functions**
   - `grattan_y_continuous()`, `grattan_x_continuous()` - Axis configuration
   - `wrap_labs()` - Label wrapping
   - `grattan_label()`, `grattan_label_repel()` - Text annotations
   - `create_fullslide()` - Full-slide chart layouts

## Development Workflows

### Package Build and Check

```r
# Install dependencies
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)

# Load package for development
devtools::load_all()

# Build documentation
devtools::document()

# Run R CMD check
devtools::check()

# Build package
devtools::build()
```

### Testing

The package uses **testthat** (>= 3.0.0) for unit testing:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-grattan_save.R")

# Run tests with coverage
covr::package_coverage()
```

**Testing Structure:**
- 24 test files in `tests/testthat/`
- Visual regression tests using `vdiffr` (snapshots in `_snaps/`)
- Test fixtures in `tests/testthat/test/` and `tests/testthat/default_height/`
- Coverage reporting via codecov

**Key Test Files:**
- `test-grattan_save.R` (10,889 lines) - Chart saving tests
- `test-grattan_save_pptx.R` - PowerPoint export tests
- `test-grattan_colours.R` - Color palette tests
- `test-grattan_axes.R` - Axis configuration tests
- `test-save_chartdata.R` - Data export tests

### CI/CD Pipeline

Three GitHub Actions workflows in `.github/workflows/`:

1. **R-CMD-check.yaml** - Package validation
   - Runs on: macOS, Windows, Ubuntu (latest R release)
   - Triggers: Push to main/master, pull requests
   - Tests: R CMD check with vignette compilation
   - Uploads visual test snapshots

2. **covr.yaml** - Code coverage
   - Runs on: macOS
   - Reports to: codecov.io
   - Includes xquartz dependency for graphics

3. **pkgdown.yaml** - Documentation site
   - Builds and deploys package website
   - Triggers: Push to main/master branch
   - Generates HTML documentation from .Rd files and vignettes

## Coding Conventions

### Documentation (roxygen2)

All functions MUST be documented using roxygen2 comments:

```r
#' Function title (brief, one line)
#'
#' Detailed description of what the function does.
#'
#' @param param_name Description of the parameter
#' @param another_param Description with default. Defaults to NULL.
#'
#' @return Description of return value
#'
#' @examples
#' # Example usage
#' function_name(param = value)
#'
#' @export
#' @import ggplot2
#' @importFrom package function
function_name <- function(param_name, another_param = NULL) {
  # Implementation
}
```

**Key Roxygen Tags:**
- `@export` - Make function available to users (adds to NAMESPACE)
- `@import package` - Import entire package
- `@importFrom package function` - Import specific function
- `@examples` - Provide usage examples (critical for user understanding)
- `@param` - Document each parameter
- `@return` - Describe what the function returns

### Code Style

1. **Naming Conventions:**
   - Functions: `snake_case` (e.g., `grattan_save`, `theme_grattan`)
   - Variables: `snake_case`
   - Constants: Color names like `grattan_orange` (lowercase)

2. **Function Organization:**
   - One function per file for major functions
   - Related helpers can share a file (e.g., `chart_helpers.R`)
   - File names should match main function name

3. **Dependencies:**
   - Use `::` for explicit package calls when clarity is needed
   - Import frequently used functions with `@importFrom`
   - Import entire namespace with `@import` only when necessary (e.g., ggplot2)

4. **Error Handling:**
   - Use `stop()` for errors with clear, user-friendly messages
   - Use `warning()` for non-fatal issues
   - Use `message()` for informational output

### Testing Conventions

1. **Test File Naming:** `test-[function_name].R`
2. **Test Structure:**
   ```r
   test_that("descriptive test name", {
     # Setup
     test_data <- data.frame(...)

     # Execute
     result <- function_to_test(test_data)

     # Assert
     expect_equal(result, expected_value)
     expect_true(condition)
     expect_error(bad_call(), "expected error message")
   })
   ```

3. **Visual Tests:** Use `vdiffr::expect_doppelganger()` for chart comparisons
4. **Temporary Files:** Use `tempdir()` for test file creation

## Key Files and Their Purpose

### Package Configuration
- **DESCRIPTION** - Package metadata, dependencies, version, authors
- **NAMESPACE** - Exported functions (auto-generated by roxygen2 - DO NOT EDIT MANUALLY)
- **.Rbuildignore** - Files to exclude from package build
- **grattantheme.Rproj** - RStudio project configuration

### Documentation
- **README.md** - Generated from README.Rmd (DO NOT EDIT DIRECTLY)
- **README.Rmd** - Source for README (edit this instead)
- **NEWS.md** - Version history and changelog (update with each release)
- **vignettes/using_grattantheme.Rmd** - Comprehensive user guide

### Data Files
- **data/*.rda** - User-facing data objects
- **R/sysdata.rda** - Internal package data (not exported)
- **data-raw/** - Scripts to generate data objects

### Templates
- **inst/extdata/template_*.pptx** - PowerPoint templates for various chart types
  - `template_normal.pptx` - Standard 4:3 charts
  - `template_169.pptx` - 16:9 aspect ratio
  - `template_fullslide.pptx` - Full-slide charts
  - And 6 other variants for different use cases

## Git Workflow

### Branching Strategy
- **master** - Main branch (stable releases)
- **Feature branches** - Named descriptively (e.g., `grattan_save_bugfixes`)
- **Claude branches** - Follow pattern: `claude/claude-md-[session-id]`

### Commit Messages
- Use clear, descriptive messages
- Reference PR numbers when merging (e.g., "Merge pull request #240 from grattan/deendash")
- Keep commits focused on single changes

### Pull Request Process
1. Create feature branch from master
2. Make changes with clear commits
3. Update NEWS.md if user-facing changes
4. Ensure all tests pass (`devtools::check()`)
5. Update documentation if needed (`devtools::document()`)
6. Create PR with descriptive title and description
7. Wait for CI checks to pass
8. Merge to master

## Common Tasks for AI Assistants

### Adding a New Function

1. **Create the R file** in `R/` directory:
   ```r
   #' Function title
   #'
   #' Description
   #'
   #' @param x Description
   #' @return Description
   #' @export
   #' @examples
   #' new_function(x = 1)
   new_function <- function(x) {
     # Implementation
   }
   ```

2. **Update documentation:**
   ```r
   devtools::document()  # Regenerates NAMESPACE and .Rd files
   ```

3. **Add tests** in `tests/testthat/test-new_function.R`:
   ```r
   test_that("new_function works correctly", {
     result <- new_function(x = 1)
     expect_equal(result, expected)
   })
   ```

4. **Run checks:**
   ```r
   devtools::test()      # Run tests
   devtools::check()     # Full package check
   ```

### Modifying Existing Functions

1. **Read the function file** to understand current implementation
2. **Check existing tests** to understand expected behavior
3. **Make changes** to the function
4. **Update roxygen documentation** if parameters/behavior changes
5. **Update or add tests** for new behavior
6. **Run `devtools::document()`** to update documentation
7. **Run `devtools::test()`** to ensure tests pass
8. **Update NEWS.md** with user-facing changes

### Updating Colors

Colors are defined in `R/create_grattan_colours.R` (958 lines). This file:
- Defines base colors
- Creates 8 tint variants for each color using `colorRampPalette`
- Exports all color objects

To modify colors:
1. Edit base color definitions in `create_grattan_colours.R`
2. Run `devtools::document()`
3. Update `data-raw/create_grattan_palette_set.R` if needed
4. Run tests to ensure color functions still work

### Adding PowerPoint Templates

1. Create template .pptx file in `inst/extdata/`
2. Follow naming: `template_[type].pptx`
3. Update `grattan_save.R` and/or `grattan_save_pptx.R` to reference new template
4. Add corresponding chart type to documentation
5. Add tests for new template type

### Updating Dependencies

1. Add to DESCRIPTION under appropriate section:
   - `Imports:` - Required dependencies
   - `Suggests:` - Optional dependencies (tests, vignettes)
2. Update `@import` or `@importFrom` in function roxygen
3. Run `devtools::document()`
4. Run `devtools::check()` to verify

## Package Release Process

1. **Update version** in DESCRIPTION (use semantic versioning)
2. **Update NEWS.md** with all changes in this version
3. **Run full checks:**
   ```r
   devtools::check()
   devtools::test()
   covr::package_coverage()
   ```
4. **Build vignettes:**
   ```r
   devtools::build_vignettes()
   ```
5. **Commit all changes**
6. **Tag release** in git:
   ```bash
   git tag -a v1.3.0 -m "Release version 1.3.0"
   git push origin v1.3.0
   ```
7. **Build and check package:**
   ```r
   devtools::build()
   devtools::check_built()
   ```

## Important Notes for AI Assistants

### DO's
- ✅ Always read existing code before modifying
- ✅ Use roxygen2 for all documentation
- ✅ Write tests for new functions and modified behavior
- ✅ Run `devtools::document()` after changing roxygen comments
- ✅ Run `devtools::test()` before committing
- ✅ Update NEWS.md for user-facing changes
- ✅ Follow existing code style and naming conventions
- ✅ Use `tempdir()` for test files
- ✅ Check that NAMESPACE is properly generated (don't edit manually)
- ✅ Preserve backward compatibility when possible

### DON'Ts
- ❌ Don't edit NAMESPACE manually (it's auto-generated)
- ❌ Don't edit README.md directly (edit README.Rmd instead)
- ❌ Don't add dependencies without good reason
- ❌ Don't break existing tests without understanding why
- ❌ Don't commit without running `devtools::check()`
- ❌ Don't modify color values without consulting team
- ❌ Don't change PowerPoint template layouts without verification
- ❌ Don't skip documentation for exported functions
- ❌ Don't use `library()` inside package code (use `::` or imports)

### Special Considerations

1. **Graphics Dependencies:**
   - The package requires graphics capabilities (Cairo, fonts)
   - macOS CI requires xquartz
   - Tests involving plots need careful handling

2. **PowerPoint Templates:**
   - Templates are binary files - handle with care
   - Test thoroughly when modifying save functions
   - Different templates for different chart types

3. **Color Palette:**
   - Colors are central to package identity
   - Changes to colors affect many users
   - Each color has 8 tints - maintain consistency

4. **Style Guide Compliance:**
   - Package enforces Grattan Institute style guide
   - Changes should align with institutional standards
   - Consult style guide for design decisions

## Useful Commands Reference

```r
# Development
devtools::load_all()                    # Load package for development
devtools::document()                    # Generate documentation
devtools::test()                        # Run tests
devtools::check()                       # Full package check
devtools::build()                       # Build package

# Testing
testthat::test_file("tests/testthat/test-file.R")
covr::package_coverage()                # Check test coverage
vdiffr::manage_cases()                  # Manage visual test cases

# Installation
devtools::install()                     # Install from source
devtools::install_github("grattan/grattantheme")

# Documentation
pkgdown::build_site()                   # Build documentation website
?grattantheme::function_name            # View function help
```

## Package Statistics

- **Total R Code:** ~4,630 lines across 30 files
- **Functions Exported:** 172
- **Test Files:** 24 files
- **Documentation Files:** 180 .Rd files
- **PowerPoint Templates:** 9 templates
- **Dependencies:** 23 imported packages

## Recent Changes (v1.3.0)

- Enhanced `grattan_save()` with `no_new_folder` and `rich_subtitle` options
- Updated PowerPoint templates for consistency
- Enhanced `save_chartdata()` with `select_data` and `round` parameters
- Set cairo_pdf as default device
- Bug fixes for patchwork plot handling
- Improved en dash removal function

## Resources

- **Package Website:** https://grattan.github.io/grattantheme/
- **Vignette:** https://grattan.github.io/grattantheme/articles/using_grattantheme.html
- **R at Grattan Guide:** https://grattan.github.io/R_at_Grattan/data-visualisation.html
- **GitHub Repository:** https://github.com/grattan/grattantheme
- **Codecov:** https://codecov.io/gh/grattan/grattantheme

---

*This document was created to help AI assistants understand the grattantheme codebase and development practices. Keep it updated as the project evolves.*
