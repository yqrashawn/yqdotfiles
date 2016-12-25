This package provides `counsel-osx-app' function which is inspired by
`counsel-linux-app'.

In order to use `counsel-osx-app' simply call `counsel-osx-app' function.  It
will allow you to select an app to launch using ivy completion.  Optionally
one can select any file to edit in selected application via ivy actions.

By default `counsel-osx-app' searches for applications in "/Applications"
directory, but it's configurable via `counsel-osx-app-location' variable.  It can
be either string representing root location for all applications or list of
such strings.

The last configurable thing (but not least) is command for launching
application.  Please refer to `counsel-osx-app-launch-cmd' for more information.

Although the name of this package is `counsel-osx-app', it's not restricted
to OSX only.  One can easily tune it to run under Linux (not sure about
Windows).  Just make sure to configure described variables and change
implementation of `counsel-osx-app-list' function.  PRs are welcome on making
this package cross-platform.
