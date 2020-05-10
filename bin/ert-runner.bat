@echo off

setlocal enabledelayedexpansion

set ert_runner=ert-runner

if defined inside_emacs (
  set ert_runner_emacs=emacs
) else if defined emacs (
  set ert_runner_emacs=%emacs%
) else (
  set ert_runner_emacs=emacs
)

if [%1]==[] (
  goto :no_args
) else (
  set ert_runner_args=%*
)

:next_arg
set current_arg=%1
if not [%current_arg%]==[] (
  if !current_arg!==--win (
    set ert_runner_args_win=y
  ) else if !current_arg!==--no-win (
    set ert_runner_args_no_win=y
  )
  shift /1
  goto next_arg
)

if not [%ert_runner_args_win%%ert_runner_args_no_win%]==[] (
  set ert_runner_outfile=%tmp%\ert-runner.%random%
  copy nul !ert_runner_outfile! > nul
  if [%ert_runner_args_win%]==[y] (
    %ert_runner_emacs% -l %ert_runner% -Q
  ) else (
    %ert_runner_emacs% -nw -l %ert_runner% -Q
  )
  set status=!errorlevel!
  type !ert_runner_outfile!
  del !ert_runner_outfile!
  exit /b !status!
)

:no_args
%ert_runner_emacs% -batch -l %ert_runner% -Q
