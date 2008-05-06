================
Network Debugger
================

-------------
Configuration
-------------

From within the current directory, which should be the installation
directory, run

% ./configure

---------
Execution
---------

Load
- network-debugger.lisp
and you're all set.

(If you're missing network-debugger.lisp, then, you first need to
 configure your installation.  See the configuration step.)

--------
Examples
--------

The tests directory contains example sessions with the Network Debugger.

-------------
Documentation
-------------

- spec.txt
  declarative language to specify the network model and experimental data

- API.txt
  query interface

-----
Files
-----

- nd.lisp
- nd-rules.lisp
- loader.lisp
- utils.lisp