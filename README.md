Trvial API for application initialization and messaging in Common Lisp
======================================================================


## An initial summary about the APPLICATION system

[AFFTA] represents the first usage case for this application
notification framework.

In the origins of this framework's first application within AFFTA, it
was observed: That if an _error_, _warning_, or other unhandled
_condition_ occurs during the application of a _test protocol_, it may
be both feasible and supportive to the application developer, to
ensure that the developer would be notified of the _condition_ --
moreover, that not only may the _notification_ object include a
representation of the _condition_ object - as would be in a format
appropriate to the respective _notification media_ - but that
furthermore, the _notification_ object may include a representation of
a single _context object_ indicating the origin of the _condition_
object.

In the initial development of this application notification protocol,
it was observed that the class NULL may serve as a convenient
designator for a "null application" -- as in a context in which an
application's development is essentially being conducted, informally,
by way of direct stream I/O onto a locally accessible Common Lisp
implementation.

This notification protocol may serve as a component of a system for
supporting application design within an integrated development
environment, and may be furthermore extended for appliation within a
server environment.
          

## "TO DO"

0. **Document how this <framework> is used within DO-RECORDED-TEST**

1. **[COMPLETE] Move this into a new 'application' system [MCi]**

2. **Document NOTIFY [standard generic function]**

3. **Describe %APPLICATION% [Variable]**

4. **(4.A) Extend this simple framework for an appliation system** such that
 would be running within an Amazon Web Services (AWS) Elastic Compute
 Cloud (EC2) _instance_, and therefore would be able to utilize the
 AWS personal messaging API, [SNS](http://aws.amazon.com/sns/) -
 namely, as for notifying an application's maintainer and/or
 maintenance staff of any conditions observed within the Common Lisp
 appliation's runtime environment(w/ a top priority notification) any
 error conditions, if not also (w/ a less priority notification)
 warning conditions occurring within the Common Lisp application's
 environment on the server - cf. also Nagios, Daemontools' Multilog,
 and SystemD, as in a context of server (if not also desktop) shell
 intefaces, as well as (in a Java server domain) Tomcat, Glassfish,
 and Liferay
     
4. **(4.B)) Extend this simple framework for a build system**
utilizing Hudson [[1]] [[2]] [[3]]
within Fusion Forge[[4]] [[5]] [[6]] [[7]] [Java]
as in which an `%application%` would be:

    1. In a development domain, an object of functional
       testing in a context of continuous integration[[8]]

    2. In a resource management and resource distribution
       domain, an object for distribution e.g. to individual
       Maven, Debian, Cygwin, and other package repositories,
       as in a context of continuous delivery[[8]]

    3. Once distributed, then an object for "issue tracking"
       such as with regards to individual issue tracking
       services utilized within indivudal Linux distributions
       (cf. Debian's 'reportbug' interface and broader issue
       tracking system), and  desktop environments (e.g. KDE
       and GNOME) as well as within individual applications 
       (e.g. Firefox, Google Chrome, and applications developed
       as components of the respective desktop environment,
       e.g. WebKit) in a context of developer support, if not
       moreover in a context of service customer support and
       overall customer fulfillment

    4. In a  _network services domain_, an object running on
       one or more _application hosts_, such that would be
       _managed_ -- directly and/or indirectly -- by a
       developer developing this software, likewise cf. AWS,
       furthermore cf. DeLorme inReach

4. **(4.C) Illustrate how this simple generic function may be applied**
within each of

    * A. a desktop `%application%` using Garnet or CLIM

    * B. an HTTP server `%application%` using CL-HTTP

    * C. an Eclpse IDE extension using ABCL and/or {CL+J, FOIL, ...}
    in which the %application% may represent simply an interface onto
    the Eclipse IDE - as the Eclipse IDE being, from the perspective
    of the Common Lisp peer, a _Java Application_ [Java]

    * D. then define CORBA IDL for Nr. C, and extend as at least a
    simple (?) CORBA-integrated message passing / application 
    notification framework for Common Lisp peer applications

### API "TO DO"

* Task: Extend [osicat] for interface with host process information
  (e.g. PID, real UID, GID, effective UID, GID, priority) and process
  control procedures (e.g.  *nice*, *sched_setscheduler*, *chroot*)
  and CLIM presentation/interaction methods 


* Class: `PROCESS`
    * Class: `THREAD-PROCESS` (`PROCESS`)
        * Summary: Within a multi-thread environment, a _thread
          process_ defines a _process_ that executes within the
          _process environment_ of the containing _shell process_
    * Class: `PROCESS-FORK` (`PROCESS`)
        * Summary: Essentially a "copy" of the calling process,
          created via _fork_
        * Note: Streams after _fork_. See, for example:
          `SB-EXT:RUN-PROGRAM`, which (#-w32 always) allocates a new
          pseudo-terminal for the forked process, using
          `SB-IMPL::OPEN-PTY`. See also: manual page `PTS(4)`. Note
          also that osicat must be patched to implement `grantpt`,
          `unlockpt` and `ptsname`.
        * PROCESS-PID : The _fork_ function should return, to the
          _parent process_, the PID of the _child process_. The _child 
          process_ must also be able to access the _parent procoess_
        * Note: Quitting the forked process (unless `SHELL-PROCESS`)
        * Class: `SHELL-PROCESS` (`PROCESS-FORK`)
            * Summary: A `SHELL-PROCESS` executes within a new _process
              environment_, within the _process group_ of the containing
              _Lisp process_, and launches a shell command via _exec..._
            * See also: {SETENV bindings...}
    * Class: #-W32 `PSEUDOTERMINAL`
        * As an alternative to `grantpt`, `unlockpt` and `ptsname`,
          a FIFO may be created for (by default) each of the standard
          input, standard output, and standard error descriptors, for I/O
          onto each respetive descriptor, between the "parent process"
          and the "child process" -- calling `mkfifo` (ensuring that
          the FIFO is created as to read/writable only by the calling
          user) then `dup2` via OSICAT-POSIX, for each respective
          FIFO. Although this methodology would result in the creation
          of (by default) three FIFOs for each spawned process,
          however it may be more portable....
    * Function: `GET-PARENT-PROCESS` => process-or-null
    * Accessor: `PROCESS-NAME` (string)
    * Accessor: `PROCESS-CHILD-FUNCTION`
        * (For a _thread process_, a Lisp function evaluating
          arbitrary Lisp code; for a _shell process_, a Lisp function
          that calls FORK, ... and EXEC or similar)
    * Accessors (POSIX)
        * TBD: POSIX process interface, where applicable
        * For a `THREAD-PROCESS`, accessors would be applicable within
          and external to the `THREAD-PROCESS`, and would be applied
          to the process repreenting the Lisp environment, within the
          host operationg system.
        * For a `SHELL-PROCESS`, accessors would be applicable only
          outside of the shell process.
    * Accessor: `PROCESS-LOCAL-VARIABLES-FUNCTION`
        * Type: Function
        * Summary: The `PROCESS-LOCAL-VARIABLES-FUNCTION`, when
          evaluted, must return a single value for each of the
          `PROCESS-LOCAL-VARIABLES`. (Note: If the list of values 
          returned by the function is shorter than the list of
          variables denoted in `PROCESS-LOCAL-VARIABLES` then those
          variables effectively following after the values - as
          returned by the function - are exhausted will each be bound
          to the value NIL)
    * Accessor: `PROCESS-LOCAL-VARIABLES`
        * Syntax for process-local vaiable declations: NAME
        * Implementation detail: bind `PROCESS-LOCAL-VAIABLES` with
          `MULTIPLE-VALUE-BIND`, in a FUNCALL to the
          `PROCESS-LOCAL-VAIABLES-FUNCTION`
        * Purpose: To ensure that a process' `PROCESS-CHILD-FUNCTION`
          will be evaluated within a lexical environment in which any
          of the `PROCESS-LOCAL-VAIABLES` is defined as local to the
          same lexical environment.
* Function: `MAKE-PROCESS`
* Macro: `DEFPROCESS`
    * Syntax: `DEPROCESS NAME (TYPE {(VAR BINDING)}*) {DECLARATION}? {FORM}*`
        * `NAME`: A _process name_ (i.e. a string)
        * `TYPE`: A symbol (denoting the type of the process to define)
        * `VAR`: A symbol
        * `BINDING`: A Lisp _form_, such that will be evaluted for
          assigning a binding to `VAR` within the lexical environment
          of the set of _FORMs_
        * `DECLARATION`: ...
        * `FORM`: A Lisp _form_
* Function: `FIND-PROCESS`
* Function: `UNDEFINE-PROCESS`
    * Syntax: `UNDEFINE-PROCESS NAME => PROCESS`
       * `NAME`: A _process name_
       * `PROCESS`: The _process_ denoted by `NAME`
   * Summary: The function `UNDEFINE-PROCESS` ensures that if a
     process named `NAME` is currently defined such as to be
     accessible to `FIND-PROCESS`, that the process will be made
     inaccessible to `FIND-PROCESS`
* Function: `CURRENT-PROCESS`


* Class: APPLICATION
    * _To Do: Differentiate "Application" from "Process",
      semantically, or else join the two concepts into one API_
    * Accessor: APPLCIATION-PROCESS
    * Accessor: APPLICATION-NAME
    * Accessor: APPLICATION-PARAMETERS
	* Accessor: APPLICATION-DEBUGGER-HOOK
    * Class: SHELL-APPLICATION (APPLICATION)
	* Class: GRAPHICAL-APPLICATION (APPLICATION)
	    * Class: GARNET-APPLIATION (GRAPHICAL-APPLICATION)
		* Class: CLIM-APPLICATION (GRAPHICAL-APPLICATION)
		    * For _application debugger hook_, use
		      `clim-debugger:debugger` (defined in
		      `mcclim:Apps;Debugger;clim-debugger.lisp`; depends on
		      McCLIM _Clouseau_)
	
    * Class: JAVA-APPLICATION
        * Accessor: JAVA-APPLICATION-MAIN-CLASS
        * Class: JAVA-SHELL-APPLICATION (APPLICATION)
		* Class: JAVA-CL+J-APPLICATION
		* Class: JAVA-FOIL-APPLICATION
		* Class: JAVA-ABCL-APPLICATION


## Resource Notes

1. [Hudson Continuous Integration](http://hudson-ci.org/)
2. [Hudson Continus Integration - Eclipse Foundation](http://www.eclipse.org/hudson/)
3. [Installing Hudson - Eclipsepedia](http://wiki.eclipse.org/Hudson-ci/Installing_Hudson)
4. [Forum: GForge is now FusionForge](https://fusionforge.org/forum/forum.php?forum_id=7)
5. [FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Main_Page)
6. [Installing - FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Installing)
7. [Debian Package Tracking System - fusionforge](https://packages.qa.debian.org/f/fusionforge.html)
8. Prakash, Winston. [Practicing Continuous Delivery Using Hudson](http://www.eclipse.org/hudson/the-hudson-book/book-hudson.pdf)
