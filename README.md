# ert-runner.el

Ert-runner is a tool for Emacs projects tested using Ert. It assumes a
certain test structure setup and can therefore make running tests
easier.

## Installation

Add `ert-runner` to your [Cask](https://github.com/rejeep/cask.el) file:

```lisp
(depends-on "ert-runner")
```

## Usage

View usage information:

    $ cask exec ert-runner help

Test files should be placed under the `test/` directory as `*-test.el`
files. Or let ert-runner create the test directory and files for you:

    $ cask exec ert-runner init

Run all tests:

    $ cask exec ert-runner

Run specific tests:

    $ cask exec ert-runner test/foo-test.el test/bar-test.el

Run test whose name matches a pattern:

    $ cask exec ert-runner -p pattern

Load custom files before running tests:

    $ cask exec ert-runner -l test/custom.el

Run all tests whose name matches `emacs-23-only` and load `ert.el`
from vendor since it's not included in Emacs-23:

    $ cask exec ert-runner -p emacs-23-only -l vendor/ert.el

Run all tests which are tagged `emacs-23-only` or `all-emacsen`:

    $ cask exec ert-runner -t emacs-23-only,all-emacsen

Run all tests which are *not* tagged `emacs-24-only`:

    $ cask exec ert-runner -t !emacs-24-only

Run all tests, whose name matches `request`, and which are tagged `fast` or
`important`, but *not* `network`:

    $ cask exec ert-runner -p request -t fast,important -t !network

Run in "no win" mode:

    $ cask exec ert-runner --no-win

## Configuration

You can add a `.ert-runner` file (one option per row) with options you
always want to include, for example:

```
-l test/ert-loader.el
```

## Projects using

* [f.el](https://github.com/rejeep/f.el)
* [ecukes.el](https://github.com/rejeep/ecukes)
* [commander.el](https://github.com/rejeep/commander.el)
* [ansi.el](https://github.com/rejeep/ansi)
* [flycheck.el](https://github.com/flycheck/flycheck)
* ...

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/ert-runner.el
    $ cask

Run all tests with:

    $ make
