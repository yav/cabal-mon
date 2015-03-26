This is a utility which monitors changes to the files in a directory.
It was designed to make it easy to monitor parallel Cabal builds.

The utility works by monitoring the log files produced by Cabal,
using `fswatch`, which is available here:

http://emcrisostomo.github.io/fswatch/

A typical use would be to simply invoke it in a sandbox:

    cabal-mon

This will automatically find the directory with the Cabal logs,
and start up an instance of `fswatch` to monitor the files for modifications.

The default `fswatch` monitor for Mac OS does not report events very
frequently, so you may have better luck by using the `kqueue` one:

    cabal-mon -m kqueue_monitor

When files are updated, their content will be displayed by the utility.
The menu uses the following colors:

  * green: this is the package that is currently being watched
  * red:   this package has changes that you have not yet looked at
  * white: this package has not changed since the last time you looked at it





