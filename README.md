API for application packaging, initialization, and messaging in Common Lisp
=========================================================================

## Overview

The [dobelle-app][dobelle-app] defines a class, `APPLICATION`,
within a Common Lisp namespace.

_Ed. Note: This documentation endeavors to define a baseline
architecture towards a framework for application development in Common
Lisp, focusing on three distinct types of host architecture: Server
platform; Desktop platform; Mobile platform. As such, this
documentation would be developed for an audience of software
developers._

_This documentation has served largely as a page for collecting notes,
as firstly with regards to the concepts of hardware architectures,
computing platforms, operating system environments, software
applications, application packaging systems, application debugging,
and issue tracking._

_Secondly, this documentation has served as a page for collecting
notes also with regads to specific application architectures --
focusing, herein, about CORBA as a network application architecture
and Linux as an operating system._

_Thirdly, this documentation has served as a page for collecting a few
notes with regards to software development process and
development process support._

_Regarding the items of those "Second" and "Third" topics: Those items
would be essentially orthogonal to the baseline concepts of this
system, and should likewise be developed within seperate items of
documentation, in altogether seperate application systems. However,
insofar as those concepts may bear at least an orthogonal relation to
the baseline concepts of this system -- furthermore, pending
additional development of an architecture for development and
application of such orthogonal concepts, within this heterogenous
conceptual framework -- presently, this documentation will continue to
serve as a generic "Notes collection" towards development of an
application architecture._


_Orthogonal concepts addressed in the following sections:_
* _Unified interface for process and threading protocols, as primarily
   within POSIX environments - i.e. `BRANCH` class protocol_
* _Desktop virtualization - context, Development Environment_
* _CORBA_

## Availability

Source tree at Github:
  [git@github.com:MetaCommunity/dobelle-app.git][dobelle-app] 

## Initial Documentation

### Usage Cases (and orthogonal notes)

_FIXME: Move orthogonal notes into other sections of this outline or
other documentation _

* Desktop Applications
    * McCLIM Applications
    * Utility Applications
        * DocBook Toolchain
* Mobile Applications
    * Android platform
* Server Applications
    * Known models for remote application control, monitoring, and
      notification
        * Notification: AWS [SNS](http://aws.amazon.com/sns/)
          (simple notification system)
        * Control: AWS EB / EC2 / RDS ... and host operating system
          configuration (e.g. Linux 'init')
        * Monitoring: Nagios
        * Monitoring/Logging: Multilog (Daemontools)
        * Monitoring/Control: Java Management Extensions (JMX)
        * Control: Puppetlabs Puppet (Ruby)
        * Control/Monitoring/Notification: "DIY" w/ CORBA. See also: JMX
    * Amazon Web Services
        * Modular service components in the AWS architecture (EB, RDS, EC2, ...)
        * Platform Virtualization (Xen Dom0...)
        * AWS JSON API
        * Pre-Packaged AWS Instances
            * Liferay web portal (Tomcat 7) (Java EE)
            * Allegrograph (Ontology) (CLtL2)
        * "Blue Sky"
            * Traveler's Cube
                * VPN (strongSwan)
                * Desktop Applications
    * Common Lisp applications for server environments
        * Allegrograph
        * BKNR
        * CLORB
        * ...
* Platform-Centric Augmentative Design
    * McCLIM
    * Java
    * AWS
    * ...
* Support for Developer Toolchains, Agile Development
    * Functional testing
        * Continuous Integration
        * Testing Frameworks
        * Notifications - Remote build hosts (distributed development
          service network)
    * End-of-Sprint
        * Software Release
        * Developer Commentary
        * Issue Tracking
    * Maintenance
       * Issue Tracking
       * Reusability Analysis
    * Platforms
       * Desktop environments
           * Development environments
               * Developer expertise
           * Desktop user environments
               * Pragmatism
               * Issue tracking
               * Documentation
       * Mobile applications
           * Mobile application prototyping (destkop and mobile)
           * Server monitoring/notification
           * Developer notes and mobile apps
               * The illustrious _placemat sketch_, reduxed into
                 digital media
               * Outlines for documentation
                   * cf. Omnigraffle; iThoughts; Cubetto mobile
               * Elements for documentation
                   * cf. UX Write; Wordnet; iCloud Notes
               * Pseudocode/Prototypes
                   * cf. Textastic; ...
           * Task list and task tracking (PIM)
               * cf. Omnifocus
           * Community content development
               * Web logs
               * Social networking
       * Server environments
           * AWS as instance of _scalable service architecture_
           * Digital Ocean as instance of _scalable classic VPS host_
           * Enterprise server architectures - effective "black box"
             in any proprietary elements, otherwise described in
             community process and, in detail, within open technical
             specifications

### Application Concepts

#### Application Life Cycle

* Trivial Outline - Application as Module
    * Application installation
    * Application initialization
    * Application runtime
    * Application I/O onto local data serialization (conventional filesystems)
    * Application communication over network protocols (common)
    * Application error state
    * Application restart
    * Application close
    * See also: `initctl(8)`, _upstart_ and `init(8)` [Linux platforms]


### Package System Concepts

### Integration with Host Packaging System

_(TBD. See also: Debian Packaging System; Cygwin; Android app
store(s); Apache Maven; Apache Ivy; ...)_

#### Package Life Cycle

_cf. Debian package system_

* Concepts
    * Software release (development context)
    * Software installation (user context)
    * Issue tracking (developers and users)

#### Modeling (?) of Output Files for Application Package Assembly

_(TBD. See also: [mci-doc-docbook][mci-doc-docbook])_


### Desktop Platform Concepts

#### Integration with CLIM

_(TBD. See also: "Initial Summary", below)_ 


* Class: `CLIM-APPLICATION`
    * Description: Essentially, this class would provide at least an
      interface onto both of `CLIM:MAKE-APPLICATION-FRAME` and
      `CLIM:RUN-FRAME-TOP-LEVEL`.
        * In the first instance, some specific attention should be
          made for the `:CALLING-FRAME` argument, specificaly for any
          `CLIM-APPLICTION` representing a nested CLIM application
          frame.
        * Extensionally, some attention should also be made to the
          `:FRAME-MANAGER` argument to `MAKE-APPLICATION-FRAME`, and
          correspondingly, `CLIM:FIND-FRAME-MANAGER` -- this may serve
          to allow for selecting a specific display in a mult-head
          configuration, or for selecting a specific frame manager for
          a CLIM environment applying multiple ports, or for creating
          an application frame on a remote X server.
        * Some additional functionality may be provided as to present
          a convenient interface for 'splash screen' display, in any
          image format supported by McCLIM and the respective port
          implementation.
        * See also:
            * `CLIM:*DEFAULT-FRAME-MANAGER`
            * `CLIM:FIND-PORT`
            * `#+MCCLIM` `CLIMI:FIND-DEFAULT-SERVER-PATH`.
        * Note moreover that `CLIM:FIND-PORT` may result in a new
          `PORT` instance being created, such as with the following
          form is evaluated in McCLIM: `(clim:find-port :server-path
          '(:clx :display ":0"))` .
        * This may be applied as onto integration with SSH tunneling
          (To Do: Develop usage cases) 
* Related Concepts
    * Port [CLIM]
    * Frame Manager [CLIM]
    * _Concerning CLX_
        * Xauth (X Window System architecture)
        * SSH tunnelling (X Window System architecture; OpenSSL; PuTTY)
    * _Concerning the McCLIM Gtkairo backend_
        * ?


#### Desktop Application Initialization (cf. CLIM)

_(TBD)_

_Notes (Non-Normative)_

* This project denotes [McCLIM][mcclim] as [McCLIM][mcclim] being
  effectively a _reference implmentation_ of CLIM 2. [McCLIM][mcclim]
  is published as licensed under a _free/open source software_
  license. Therefore, [McCLIM][mcclim] is avaialble for development
  and for application, within any compatible host environment. Pending
  appropriate licensing, alternate CLIM implementations may be
  available, such as for _Allegro Common Lisp_, published by Franz
  Inc.
  
* Host Window System Architectures and CLIM
	* CLIM is developed essentially around a framework in which a _host
	  windowing system_ is defined.
    * Normatively, CLIM interacts with a _host window system_ as
      via a _frame manager_ implementation, such that would be
      provided for application with a single _port_ extending of
      CLIM 2.
	* CLIM suports the X Window System, in multiple _port_
	  implementations, as avaialble within individual CLIM
	  Implementations (e.g in [McCLIM][mcclim]: CLX,
	  OpenGL, and GTK Cairo _backends_, such that all effectively
	  extend of the X Window System, namely when running on a Linux
	  host environment)
        * The X Window System allows for an _application_ to be _launched_
          on a _remote client machine_, with the application's _graphical
          user interface_ being displayed on an _X Window host_ running
          on a local, _controlling terminal_.
	        * Typically, the _remote client_ functionality of the X
	          Window System would be augmented with an _SSL tunneling_
	          provider, such as OpenSSL
              
* Additional Resources Available of a Conventional Desktop Host
  Environment, in most conventional Desktop Linux platforms
	* In addition to a _windowing system_, a _host operating system_
	  may provide features including:
		* _Desktop Environment_ (KDE, GNOME, XFCE, etc)
		* _Packaging System_ (typically, specific to the operating system)
		* Locally installed _software_ (see also:
		  [xstow](http://xstow.sourceforge.net/)] 
	* A _host machine_ may implement multiple _host operating systems_,
	  simultaneously
		* A _host operating system_ may be initalized via a _virtualization
		 host_, such as a Xen _Dom0 hypervisor_. (See also:
		 [Dom0 - Xen Wiki](http://wiki.xen.org/wiki/Dom0)) or a
		 VirtualBox _Virtual machine_ manager (See also:
		 [VirtualBox](https://www.virtualbox.org/))
    * Some features may not be provided, by default, of any single
      _embedded Linux_ (RTOS) platform


#### See also (cf. CLIM)

* CLIM-Desktop [[CLiki](http://www.cliki.net/clim-desktop)][[source tree](http://common-lisp.net/viewvc/clim-desktop/)]
* History of Desktop Interfaces for Lisp Machines, for example
    * [_Symbolics_, Wikipedia Republished](http://en.wiki2.org/wiki/Symbolics),
      specifically, _[Ivory and Open Genera](http://en.wiki2.org/wiki/Symbolics#Ivory_and_Open_Genera)_

#### Usage Case: IDE

This notification protocol may serve as a component of a system for
supporting application design within an integrated development
environment, and may be furthermore extended for appliation within a
server environment.
          

## Server Platform Concepts

### Integration with Amazon Web Services

_(TBD)_

### Integration with CLORB

_(TBD)_


### An initial summary about the APPLICATION system

Sidebar: [AFFTA][affta] was conceived as to represent the first usage
case for this application framework. That would be in a context of
_continuous integration_ and _functional testing_ within a distributed
development service network. See also: `AFFTA:DO-RECORDED-TEST`

In the origins of this framework's initial design, as for application
within [AFFTA][affta], it was observed: That if an _error_, _warning_,
or other unhandled _condition_ occurs during the application of a
_test protocol_, it may be both feasible and supportive to the
application developer, to ensure that the developer would be notified
of the _condition_ -- moreover, that not only may the _notification_
object include a representation of the _condition_ object - as would
be in a format appropriate to the respective _notification media_ -
but that furthermore, the _notification_ object may include a
representation of a single _context object_ indicating the origin of
the _condition_ object. In a simple sense, this would be a matter of
_encapsulated notifications_.


#### The Null Application

In the initial development of this application notification protocol,
it was observed that the class `NULL` may serve as a convenient
designator for a "null application" -- as in a context in which an
application's development is essentially being conducted, informally,
by way of direct stream I/O onto a locally accessible Common Lisp
implementation.


## "TO DO"

00. Make documentation modular - _AWS, Build System, Desktop Application System, etc._

0. **Document how this <framework> is used within DO-RECORDED-TEST**
    * First: Review design documents for AFFTA 

1. **[COMPLETE] Move this into a new 'application' system [MCi]**
    * Result: `dobelle-app` source tree

2. **Document NOTIFY [standard generic function]**
    * First: Integration with CLIM (mobile)
    * Second: Development and implementation of IDL interfaces for
      application notification messages (First: Kerberos, SSL, and CLORB)
    
3. **Describe %APPLICATION% [Variable]**
    * First: Architecture

4. **(4.A) Develop architecture: An appliation system for Common Lisp** 
     
4. **(4.B)) Develop archicture: Hybrid interactive/automated build
   system for "Software and things"**

(Context: Development environment)

Example: Utilizing Hudson [[1]] [[2]] [[3]] (alternately: Travis CI)
within Fusion Forge[[4]] [[5]] [[6]] [[7]] (alternately: "Just
Ubunutu") hypothetically an `%application%` may represent:

    1. In a development model, an object of functional
       testing in a context of continuous integration[[8]]

    2. In a resource management and resource distribution
       model, an object for distribution e.g. to individual
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

    4. If the `%application%` implements a _network service_, then in
       a  _network services domain_: Once installed, an object running
       on one or more _application hosts_, such that would be
       _managed_ -- directly and/or indirectly -- by a
       network service administrator -- observing that a developer
       developing a network service application may be serving also as
       a network service administrator, in such development.
       

# Orthogonal Concepts


### 'Branch' (i.e. Process/Thread) API "TO DO"

* Task: Extend [osicat] for interface with host process information
  (e.g. PID, real UID, GID, effective UID, GID, priority) and process
  control procedures (e.g.  *nice*, *sched_setscheduler*, *chroot*)
  and CLIM presentation/interaction methods 

* Class: `BRANCH`
    * Generic protocol class for interfaces onto existing
      multi-process architectures
        * i.e. "Root" class for an implementation of a unified
          interface onto host processes and threads
        * and a bit of a lighthearted metaphor with regards to 
          the details of processes and threads under POSIX
          and the pertinence of _namespaces_ in the Linux kernel --
          i.e namespaces of (referencing `clone(2)`)
            * file descriptors - `CLONE_FILES`
            * mount points (`CLONE_NEWNS`) -- othogonal to process-local
              interface to filesystem (e.g `CLONE_FS` unset)
              (cf. `chdir` etc)
            * I/O contexts - `CLONE_IO` (when set, relevant for any
              IO-intensive applications. when not set, no shared I/O
              scheduling with thread group) (orthogonal)
            * IPC descriptors (`CLONE_NEWIPC`) or SysV semaphores
             (`CLONE_SYSVSEM`)
            * signal handlers - `CLONE_SIGHAND`
            * properties of networking protocols - `CLONE_NEWNET`
            * PID namespace - `CLONE_NEWPID`
            * host UTS fields - `CLONE_NEWUTS`, cf `uname(2)`
            * thread local storage - `CLONE_SETTLS` (orthogonal)
            * memory space - `CLONE_VM` as with relation to parent
              process (orthogonal)
        * ...within the Linux kernel, such as -- in an instance of
          "All isolated namespaces" -- may pertain to definition of:
            * sensitive applications on the Linux platform, if not
               moreover...
            * a manner of ad-hoc host virtualization in the kernel
          space (question: What about the relevance of virtual
          filesystems? cf. `/proc` and `CLONE_NEWPID`), albeit with a
          shared process scheduling environment (presumably), and 
          (absolutely) a shared kernel configuration, as of a single
          Linux host, but otherwise towards something like a complete 
          vhost framework on Linux (effects TBD with regards to shared
          memory registers within the controlling kernel environment)
          (effects TBD with regards to simultaneous access to same
          root filesystem) (effects TBD with regards to unique mount
          namespsaces and real and virtual filesystems onto "Kernel
          space") (may not be suitable for embedded applications) (may
          not be suitable for direct interactive desktop applications,
          including desktop virtualization frameworks) (effects TBD
          with regards to networking hardware and protocols.) (Formal 
          SDN frameworks and normative virtualization environments may be
          preferred, or virtual hosting limited at least as with
          regards to `CLONE_NEWNET` ). See also: `clone(2)`, `init`
    * This class presents an architecture-neutral metaphor in its
      implementation, furthermore avoiding ambiguity between POSIX 
      processes, POSIX-compliant implementations of pthreads, and
      (historic/optional) LinuxThreads implementations of pthreads
    * Class: `THREAD` (`BRANCH`)
        * Summary: Within a multi-thread Lisp implementation, a
          _thread branch_ defines a _branch_ that executes within the 
          _process environment_ of the containing _process branch_.
        * Observing that `clone` [`clone(2)`] may be applied
          effectively to "Work around" numerous conventions
          for POSIX threading [`pthreads(7)`]. Note, however
          `CLONE_THREAD` (`clone(2)`) (Linux kernel - thread groups)
          and also `CLONE_VM` (same manual page)
        * TBD: How to provide a "Null interface" with this class, in
          non-multithreading Lisp implementations
        * With regards to Linux host operating systems, see
          also: `pthreads(7)` and subsequent notes in this outline
    * Class: `PROCESS` (`BRANCH`)
       * Effectively presents an object-oriented interface onto
         features of host-specific _process_ implementation (focusing
         on POSIX and Linux, however)
       * See also: `SHELL-PROCESS`
    * Class: `FORK` (`OS-PROCESS`)
        * Summary: Initially a "copy" of the calling process, 
          as created via `fork(2)` or optionally `clone(2)` (note:
          `CLONE_IO`, as with regards to serialization of data objects
          within a Common Lisp implementation; `CLONE_NEWIPC` as with
          regards to isolation of IPC namespaces for senstive
          applications - effects with regards to SBCL POSIX waitqueues
          and mutexes TBD. See also: `sysvipc(7)` and
          `mq_overview(7)`, as well as `clone(2)`). If created via
          `fork(2)`, always has a unique PID . Is effectively a
          "branch" extending of the implementation's initial _host
          process_ (cf. POSIX process groups), though effectively
          limited across _process boundaries_ (?) and memory
          registers. (See also: also `CLONE_VM` [`clone(2)`])
        * Deterministic scheduling between parent processes and
          single time-critical child processes: note `CLONE_VFORK`
          [`clone(2)`]
        * See also: `execve(2)` but note side-effects with regards to
          thread groups, as specifically denoted about `CLONE_THREAD`
          in `clone(2)`
        * **Note: Streams interaction after _fork_**. See, for example: 
          `SB-EXT:RUN-PROGRAM`, which (#-w32 always) allocates a new
          pseudo-terminal for the forked process, using
          `SB-IMPL::OPEN-PTY`. See also: manual page `PTS(4)`. Note
          also that osicat must be patched to implement `grantpt`,
          `unlockpt` and `ptsname`.
        * PROCESS-PID : The _fork_ function should return, to the
          _parent process_, the PID of the _child process_. The _child 
          process_ must also be able to access the _parent procoess_
        * Class: `SHELL-PROCESS` (`PROCESS-FORK`)
            * Summary: A `SHELL-PROCESS` executes within a new _process
              environment_, within the _process group_ of the containing
              _Lisp process_, and launches a shell command via _exec..._
            * See also: {SETENV bindings...}
    * Class: #-W32 `FIFO-PSEUDOTERMINAL` (via FIFOs - TBD, prototype)
        * As an alternative to `grantpt`, `unlockpt` and `ptsname`,
        * a FIFO may be created for (by default) each of the standard
          input, standard output, and standard error descriptors, for I/O
          onto each respetive descriptor, between the "parent process"
          and the "child process" -- calling `mkfifo` (ensuring that
          the FIFO is created as to read/writable only by the calling
          user) then `dup2` via OSICAT-POSIX, for each respective
          FIFO.
        * This methodology would result in the creation
          of (by default) three FIFOs for each spawned process,
          without any PTY type encapsulation within the host operating
          system. Possibly not adequate for interactive application,
          this may at least suffice to provide a "bundle of streams"
          within an encapsualted manner, for forked applications
    * Class: `PTY-PSEUDOTERMINAL`
        * May be recommended for interactive applications (if
          applicable distinct to `FIFO-PSEUDOTERMINAL`, with 
          such as a CLIM pane as an intermediary)
    * Function: `GET-PARENT-BRANCH &OPTIONAL PROCESS` =>
      process-or-null
        * Null: Only e.g. when PROCESS has PID `0`
        * **Design Issue:** Ephemeral interfaces for host OS processes
          not within current process' thread group. (May be limited
          according to user permissions within host operating system)
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
          host operating system.
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
* Function: `MAKE-BRANCH`
* Macro: `DEFPROCESS` (**FIXME:** Reconsider whether this may present
  any too informal of an interface to the underlying process object
  system)
    * Syntax: `DEFPROCESS NAME (TYPE {(VAR BINDING)}*) {DECLARATION}? {FORM}*`
        * `NAME`: A _process name_ (i.e. a string)
        * `TYPE`: A symbol (denoting the type of the process to define)
        * `VAR`: A symbol (**FIXME:** Reconsider this 'implicit let' design)
        * `BINDING`: A Lisp _form_, such that will be evaluted for
          assigning a binding to `VAR` within the lexical environment
          of the set of _FORMs_ (**FIXME:** Reconsider this 'implicit
          let' design)
        * `DECLARATION`: (**FIXME:** Reconsider this 'implicit
          let' design)
        * `FORM`: An _implicit progn_ (i.e. thread function body??)
          (**FIXME:** Reconsider this 'implicit let' design) 
* Function: `FIND-BRANCH`
    * Drilldown: Trivial.
    * Definition depends on convention for process/thread
      naming/indexing, as defined by the host OS and as then as
      interfaced by the specific Lisp implementation
    * **TO DO:** Define architecture for process naming/indexing
        * Note that this would apply only in regards to
          threads created via `dobelle-app`. Conceivably, without
          closer implementation-specific integration, it would be
          possible for an application to create a thread that would
          not be indexed in this sytem
        * Applicability: ????
            * In what instances may a program need to locate a
              thread by its "ID" ?
            * Correspondingly, in what instances may a "Branch Id" be
              applied within a program, other than to produce a list
              of "Indexed threads"?
                  * See also `pthread_sigqueue(3)`, `sigqueue(3)`,
                    `sigaction(2)` (?) 
            * See also: `pthreads(7)`
                * Shared data/heap segments
                * Unique stack
                * Shared process attributes
                * Unique branch id, signal mask, errno, signal stack,
                  scheduling priority (see `sched_setscheduler(2)`,
                  `sched_setparam(2)`)
                * Linux: unique process capabilities set; unique CPU
                  affinity
                * Note branch id reuse ("Branch Ids")
                * Linux: NPTL (kernel 2.6 and later)
                * Note also: `ulimit` and thread-specific handling of
                  `RLIMIT_STACK`
        * Should be extensible onto _thread processes_ (i.e. threads
          of process having same PID,) as well as PID-unique processes
        * Note: "Largest possible real PID" even on 64 bit Linux
          implementation: unsigned 32 bit - see
          `/usr/include/bits/typesizes.h`
        * Note: Branch Ids (implementation specific interfaces)
            * In SBCL, refer to: `sbcl:src;code;target-thread.lisp`
              specifically around definition of
              `SB-THREAD::%CREATE-THREAD` and the corresponding C
              language source code within `sbcl:src;runtime;thread.c`
              namely as in regards to type `pthread_mutex_t` (size of
              which is defined in `/usr/include/bits/pthreadtypes.h`,
              see also `pthread.h` and `pthread_mutex_init(3)`) ...but
              also, note applciations of `os_thread_t` type in
              SBCL source code. `os_thread_t` is defined in
              `sbcl:src;runtime;runtime.h` as synonymous with type
              `pthread_t` (i.e unsigned long, referencing
              `pthreadtypes.h`)
        * To define a native `host-process-pid` type, See also: POSIX
          `pid_t`; _osicat_; _cffi-grovel_.
        * To define a native `host-thread-id` type, See also: POSIX
          `pthread_t`; _osicat_; _cffi-grovel_; `pthreads(7)`
        * To define a "Branch Ids" type:
            * Concept: Unique, platform-agnostic identification of
              host processes and threads
            * Derived type: `(simple-array #.(widest-type 'host-process-pid 'host-thread-id) (2))`
            * Note that `sb-thread:make-thread` requires a _string_
              type _thread name_. Although useful for identifying
              threads to a human user, within a thread index
              (cf. SLIME, SWANK/BACKEND) but for indentifying threads
              within a software program, a numeric type or
              numeric-vectors type may be preferred.
            * A _branch id_ may be assigned to a _thread_ object,
              as external to any _multithreading_ implementation's
              specific _threading architecture_. Though it will 
              require initialization of third object, complimentay to
              the _thread_ and its _branch iD_, however it would
              provide a platform-agnostic method for thread indexing,
              no less allowing for a fixnum-constrained (or lesser,
              e.g. unsigned 32 bit or even unsigned 31 bit, if in the
              latter, a limit of 2 billion indexed threads may be
              practically sufficient, observing furthermore that SBCL
              defines a type of optimized vector for simple arrays of
              unsigned 31 bit elements. see also:
              `SB-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-31-WIDETAG`) in
              indexing of _branch ids_.
            * Ensure that the _branch id_ with _thread id_ `0` for any
              single _process_  object will always denote a 'primary
              controlling thread', within the same _process_ object as
              any _thread id_ under the same _process ID_--
              similar to the _managing thread_ of a _Linux threads_
              implementation. Conceivably, if a _branch_ with
              _thread id_ `0` would be terminated, that should result
              in termination of every _thread branch_ spawned by the same
              _process branch_.
             * See also: `fork(2)`, `clone(2)`, and `pthreads(7)`
               (Linux as host OS)
* Function: `TERMINATE-BRANCH` (?)
    * Syntax: `TERMINATE-BRANCH NAME => PROCESS`
       * `NAME`: A _branch name_
       * `BRANCH`: The _process_ denoted by `NAME`
    * Summary: The function `TERMINATE-BRANCH` ensures that if a
      branch named `NAME` is currently defined such as to be
      accessible to `FIND-BRANCH`, that the branch will be terminated
    * see also: `pthread_kill(3)`, `kill(2)`, `tgkill(2)`, and
      `clone(2)` as with regards to _thread groups_ namely: _"Signal
      dispositions  and actions are process-wide: if an unhandled
      signal is delivered to a thread, then it will affect (terminate,
      stop, continue, be ignored in) all members of the thread
      group."_ [`clone(2)`]

      `CLONE_THREAD`
* Function: `CURRENT-BRANCH`


* Class: `APPLICATION`
    * _To Do: Differentiate "Application" from "Process",
      semantically, or else join the two concepts into one API_
    * Accessor: `APPLCIATION-BRANCH` - i.e. controlling process/thread
    * Accessor: `APPLICATION-NAME` - object naming?
    * Accessor: `APPLICATION-PARAMETERS` - _too generic?_
	* Accessor: `APPLICATION-DEBUGGER-HOOK`
        * This accessor should need some particular attention for its
          application. In one regard, the CLIM Debugger may present a
          interseting feature towards remote application debugging in
          Common Lisp, but it should be integrated with a **debugger
          I/O protocol**, optionally onto CORBA (alternate to X.org +
          VNC) so as to not require installation of graphical desktop
          applications within server networks.
    * Class: `SHELL-APPLICATION` (APPLICATION)
        * Relatively trivial?
        * Should extend of features of POSIX, insofar as within POSIX
          environments
            * Process environment
            * Process priority
            * Process scheduling policy (RTOS environments / Kernel space)
            * Process input/output/error streams
                * Byte-limited logging + log rotation (cf. `multilog`), and/or
                * PTY, and/or
                * File
            * Process effective UID, GID
            * Process root filesystem (cf. `chroot`)
            * See also: `credentials(7)`
            * See also: `capabilities(7)`
            * See also: UNIX `fork`
            * The "Null Process" i.e. the containing Lisp
              implementation, within any conventional POSIX system or
              any eerily, incompletely POSIX-like system
	* Class: `GRAPHICAL-APPLICATION` (APPLICATION)
	    * Class: GARNET-APPLIATION (GRAPHICAL-APPLICATION)
            * e.g _Garnetdraw_
		* Class: CLIM-APPLICATION (GRAPHICAL-APPLICATION)
		    * For _application debugger hook_, use
		      `clim-debugger:debugger` (defined in
		      `mcclim:Apps;Debugger;clim-debugger.lisp`; depends on
		      McCLIM _Clouseau_)
	
    * Class: `JAVA-APPLICATION`
        * JVM memory management 
            * Heap / Stack Limits
            * Garbage Collection
        * JVM compatibility (1.6, 1.7, ...)
        * Accessor: `JAVA-APPLICATION-MAIN-CLASS`
        * Differentiating subclasses by interface system:
            * Class: JAVA-SHELL-APPLICATION (SHELL-APPLICATION)
                * Java application within a distinctly POSIX wrapper
            * Class: JAVA-CL+J-APPLICATION
                * via "Null process" in any environment in which CL+J
                  is supported (e.g. ECL)
            * Class: JAVA-FOIL-APPLICATION
                * via "Null process" in any environment in which Foil
                  is supported
            * Class: JAVA-ABCL-APPLICATION
                * via "Null process" in ABCL
            * Class: JAVA-FOO-APPLICATION
                * via hypothetical Java class file interpreter in
                  Common Lisp, therefore via "Null process" in same

#### Thread Management

* Thread pools (Server platforms)
* Scheduling (RTOS / Embedded Applications - Mobile Platforms)
* Contexts: Development; configuration
* _..._


### CORBA - ORB and Interface Definitions (Orthogonal)

_(TBD Context: "Mobile, Desktop, and Server Applications")_

* Concepts
    * Service application networks
    * Data-oriented computing
    * CORBA services for mobile embedded platforms
        * Inter-process communication (IPC) protocol and architectures 
          for application coordination within _chroot jails_ on mobile
          embedded platforms
    * CORBA and Microkernel architectures (desktop and server platforms)

* **Notes**
    * Effectively, this functionality would require an extension onto 
      CLORB, towards a definition of a seperate system for support of
      CORBA application development in Common Lisp.
    * That system should provide support for protocols implementing
      Kerberos authentication and SSL tunnelling onto CORBA, as also
      implemented in [JacORB](http://www.jacorb.org)
    * Focusing on the Linux platform, that same CORBA aplication
      development system may develop a model for "Insulated execve",
      namely applying specific _flags_ to `clone()`, cf `clone(2)` and
      observing the notes about those same _flags_ within the section,
      below, about the definition of the `BRANCH` class
        * On a Linux host system (post kernel 2.6) `clone()` may be
          applied in a manner as to provide a level of _process
          insulation_, extending far beyond the simple _chroot jail_,
          namely as ensure that a process created with `clone(2)` will
          have unique namespaces even insofar as with regards to
          networking protocols. Avoiding the `CLONE_NEWNET` flag to
          `clone()`, however, `clone()` may be called as to create
          largely an insulated process space for a server
          application.
        * With sufficient procedures being implemented in an
          applicaiton architecture, such as for ensuring appropriate
          transition across `clone()` -- namely, concerning
          filesystems, and avoiding the `CLONE_NEWNET`
          flag, however -- then an _process_ may be created such that
          would be _insulated_ within the Linux kernel space, though
          nonetheless accessible via the host's same networking
          interfaces. Effectively, `clone()` may be applied as to
          create an_"On-host network DMZ"_ for a networked server
          application, though such that would nonetheless use the same
          networking interfaces (and coresponding _iptables_
          configuration) as within the _cloning host_. Thus, with
          appropriate procedures in the application, then in event
          of a _buffer overrun_ exploint, not only would the _cloned
          process_ be completely inaccessible to the filesystem of the
          _cloning host_ (insofar as mount points, with `CLONE_NEWNS`
          set), but would also be inaccessible with regards to file
          descriptors (`CLONE_FILES` unset), IPC namespace
          (`CLONE_NEWIPC` set), and PID namespace (`CLONE_NEWPID` set)
          of the _cloning process_ and the broader _host_ (also unset: 
          `CLONE_IO`,; `CLONE_SETTLS`, `CLONE_UTS`, `CLONE_VM`,
          `CLONE_FS`, etc) ... with a procedure ensuring it would be
          impossible for the  _cloning process_ to access the memory
          space of the the _child stack_  argument provided to
          _clone_.
        * Effectively, this may be approched with a call to simple
          `fork()` followed with a call to `execve()`, followed with
          appropriate calls within the _spawned process_, to close all
          open file descriptors, excepting a "lock file," to release
          all mutexes, and release all network resources, to allocate
          memory if necessary, then to call `clone()` with the
          appropriate _flags_ (as specified in the previous), thus
          creating a  _cloned process_ -- at which time, the intermediary
          _spawned process_ would _terminate_, thus preventing access
          from the _spawned process_, to any memory areas within the
          _cloned process_. The _cloned process_ may then _mount_ a
          filesystem as would be the location of the lock file creaetd
          by the _cloning process_, as wait until the
          _lock file_ has been removed by the _cloning process_ --
          such that would have been  to allow the _cloning process_ to 
          terminate -- then to unmount that shared filesystem. 
          The _cloned process_  would then be altogether isolated from 
          the _spawned process_, except by way of networking and
          kernel drivers. The _cloned process_ may then make calls to
          mount its own virtual  `/proc` and `/sys` filesystems 
          within its new _mount namespace_ and new _PID namespace_,
          subsequently to mount any isolated block-special filesystems
          for the application's normal operation, and to initialize a
          CORBA ORB, such that may then be accessed -- specifically
          via network interfaces -- from within the original _forking
          process_. The host's firewall may furethremore be configured
          as to prevent LAN or other network access to the host,
          except via the CORBA ORB created in the final _cloned
          process_. Subsequently, any network communication to be
          directed from the host may be managed via the single
          _insulated ORB_
    * See also: CLORB

## Development Environment

### Desktop Virtualization - VirtualBox SDK 

* Concepts
    * Desktop Virtualization
    * Software-Defined Networking (SDN)
    * Oracle xVM Virtualbox
        * Originally developed by Sun Microsystems
        * Focused primarily for emulation of Intel platforms
* Usage cases:
	* Cross-platform development
	    * Application display via _Seamless_ and _Full Screen_
	      presentations
	    * Alternate to Xen virtualization (Note: Xen is implemented
	      within Amazon Web Services instances)
    * Interactive testing environment (temporary VMs)
* [Download VirtualBox](https://www.virtualbox.org/wiki/Downloads)
* [VirtualBox Main API Documentation](https://www.virtualbox.org/sdkref/index.html)
    * [VirtualBox API Class Hierarchy](https://www.virtualbox.org/sdkref/hierarchy.html) (Interfaces)
    * [VirtualBox IEvent Interface](https://www.virtualbox.org/sdkref/interface_i_event.html)
    * [VirtualBox IGuest::createSession(..)](https://www.virtualbox.org/sdkref/interface_i_guest.html#ad01dc4d81f1f0be4b9097977ddf2dc19)
    * VirtualBox [IGuestSession::processCreate(...)](https://www.virtualbox.org/sdkref/interface_i_guest_session.html#abe43b79ce8bd8d02454c60456c2a44a9)
      and [IGuestSession::ProcessCreateEx(...)](https://www.virtualbox.org/sdkref/interface_i_guest_session.html#a1353ebd47bb078594127a491d3af9797) methods
    * [VirtualBox IProcess Interface](https://www.virtualbox.org/sdkref/interface_i_process.html)
    * [VirtualBox IGuestProcess Interface](https://www.virtualbox.org/sdkref/interface_i_guest_process-members.html)


## Appendix: Symbolic Name of the Dobelle-App Source Tree

The [Dobelle-App][dobelle-app] source tree was named after a feature
of a science-fiction novel, _Summertide_ -- the first novel of the
series, _The Heritage Universe_, by Charles Scheffield.

As a short synopsis of the symbolism represented in the naming of the
[Dobelle-App][dobelle-app] source tree: In the first book of the
_Heritage Universe_ series, _Dobelle_ represents a planetary system
defined of two contrasting worlds, _Quake_ and _Opal_. The two planets
are connected by a mysterious _artifact_ denoted as _Umblical_, all
situated within a binary star system in a region of the _Milky Way
galaxy_ far remote to the planet _Earth_.

Even in an amateur analysis, it may be understood that a physical
system of planets and stars would present a number of intriguing
concerns with regards to tidal effects of gravitational objects --
moreso, an obvious concern when the  two planetary bodies in such a
system would be inhabited. Within the _Dobelle_ system, the
_Umbilical_ itself presents a novel solution, effectivelly challenging
the limits of engineering in the artifact's known
functionaltiy. Though _Umbilical_ may seem to play only a _bit part_
in the overall storyline as developed in the _Heritage Universe_
books, but as in regards to so many works of engineering -- even
insofar as works of engineering developed in works of fictional
literature -- certainly, the _Dobelle_ system may well belong in a
world literary hall of fame.

Insofar as with regards to a concept of naming a software project
after such a physical system, perhaps it may serve as towards a
keynote that in reciprocal of a certain popular adage: Even the most
advanced engineering system may be understood insofar as with regards
to elements of its mechanical nature, provided a sufficient overview
of the system's composition. Thus, even a binary planetary system of a
fictional binary star system -- and a strange artifact interposed 
between the planets -- that even such an exotic phsycial system may be
understood for its mechanical nature, without any undue
mystification.

Of course, the _Dobelle_ system would require no specific maintenance, 
itself, as with regards to its nature as an artifact obviously of a
manner of genius -- beyond the hypothetical material nature of such a
phsycial system, it being moreover an artifact developed essentially
as a work of storytelling, in a medium of science fiction. As to any 
more of details of the nature of the _Dobelle_ system -- but it might 
well spoil the reader's attention, if this document was to summarize
all of the story developed in _Summertide_, if in any too simple
regards.

Essentially: If there is any single work in the modern science
fiction, such that the work that may serve to represent an inspiring
character of engineering, Thomas Scheffield's _Heritage Universe_
presents a friendly and, in a manner, furthermore a scientifically
compelling hypothesis as to the nature of engieering within the
physical universe. Of course, it would be a hypothesis developed unto
a universe in which the theories of general and special relativity
would have been effectively amended across technical developments in
material travel at interstellar distances.

So, insofar as a name, it seemed to serve as a convenient thing
after which to name an aspiring software project -- in a simple
regards, towards developing a convenient application bridge between
the Common Lisp program environment and a small number of modular
computing environments on desktop, mobile, and server computing
architectures, somewhere north of _Summertide_.

## Resource Notes

1. [Hudson Continuous Integration](http://hudson-ci.org/)
2. [Hudson Continus Integration - Eclipse Foundation](http://www.eclipse.org/hudson/)
3. [Installing Hudson - Eclipsepedia](http://wiki.eclipse.org/Hudson-ci/Installing_Hudson)
4. [Forum: GForge is now FusionForge](https://fusionforge.org/forum/forum.php?forum_id=7)
5. [FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Main_Page)
6. [Installing - FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Installing)
7. [Debian Package Tracking System - fusionforge](https://packages.qa.debian.org/f/fusionforge.html)
8. Prakash, Winston. [Practicing Continuous Delivery Using Hudson](http://www.eclipse.org/hudson/the-hudson-book/book-hudson.pdf)


[dobelle-app]: https://github.com/MetaCommunity/dobelle-app
[mci-doc-docbook]: https://github.com/MetaCommunity/mci-doc-docbook
[affta]: https://github.com/MetaCommunity/affta
[mcclim]: http://common-lisp.net/project/mcclim/
