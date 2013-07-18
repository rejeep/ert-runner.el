# ert-runner.el

Ert-runner is a tool for Emacs projects tested using Ert. It assumes a
certain test structure setup and can therefore make running tests
easier.

## Installation

Add to your projects `Carton` file:

    (development
     (depends-on "ert-runner"))

And then install:

    $ carton install

## Usage

View usage information:

    $ carton exec ert-runner help

Create new test directory and files:

    $ carton exec ert-runner init

Run all tests:

    $ carton exec ert-runner run

Run specific tests:

    $ carton exec ert-runner run test/foo-test.el test/bar-test.el

Same as above:

    $ carton exec ert-runner run foo-test.el bar-test.el

Run test whose name matches a pattern:

    $ carton exec ert-runner run -p pattern

Load custom files before running tests:

    $ carton exec ert-runner run -l test/custom.el

Run all tests whose name matches `emacs-23-only` and load `ert.el`
from vendor since it's not included in Emacs-23:

    $ carton exec ert-runner run -p emacs-23-only -l vendor/ert.el

## Projects using

* [f.el](https://github.com/rejeep/f.el)
* [ecukes.el](https://github.com/rejeep/ecukes)
* [commander.el](https://github.com/rejeep/commander.el)
* [ansi.el](https://github.com/rejeep/ansi)
* ...
