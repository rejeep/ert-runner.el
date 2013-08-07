# ert-runner.el

Ert-runner is a tool for Emacs projects tested using Ert. It assumes a
certain test structure setup and can therefore make running tests
easier.

## Installation

Add to your projects `Cask` file:

    (development
     (depends-on "ert-runner"))

And then install:

    $ cask install

## Usage

View usage information:

    $ cask exec ert-runner help

Create new test directory and files:

    $ cask exec ert-runner init

Run all tests:

    $ cask exec ert-runner run

Run specific tests:

    $ cask exec ert-runner run test/foo-test.el test/bar-test.el

Same as above:

    $ cask exec ert-runner run foo-test.el bar-test.el

Run test whose name matches a pattern:

    $ cask exec ert-runner run -p pattern

Load custom files before running tests:

    $ cask exec ert-runner run -l test/custom.el

Run all tests whose name matches `emacs-23-only` and load `ert.el`
from vendor since it's not included in Emacs-23:

    $ cask exec ert-runner run -p emacs-23-only -l vendor/ert.el

## Projects using

* [f.el](https://github.com/rejeep/f.el)
* [ecukes.el](https://github.com/rejeep/ecukes)
* [commander.el](https://github.com/rejeep/commander.el)
* [ansi.el](https://github.com/rejeep/ansi)
* ...
