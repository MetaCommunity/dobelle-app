API for application packaging, initialization, and messaging in Common Lisp
=========================================================================

## Overview

The [dobelle-app][dobelle-app] defines a class, `APPLICATION`,
within a Common Lisp namespace.

_Ed. Note: This documentation endeavors to define a baseline
architecture towards a framework for application development in Common
Lisp_

## Availability

Source tree at Github:
  [git@github.com:MetaCommunity/dobelle-app.git][dobelle-app] 

## Initial Documentation

### Concepts

### Usage Cases

* Desktop Applications
    * McCLIM Applications
    * Utility Applications
        * DocBook Toolchain
* Mobile Applications
    * Android platform
* Server Applications
    * Known models for remote application control, monitoring, and
      notification
        * Notification: AWS ... notification architecture
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

#### Package Life Cycle

_cf. Debian package system_

* Concepts
    * Software release (development context)
    * Software installation (user context)
    * Issue tracking (developers and users)

#### Application Life Cycle

* Trivial Outline - Application as Module
    * Application initialization
    * Application runtime
    * Application error state
    * Application restart
    * Application close
    * See also: `initctl(8)`, _upstart_ and `init(8)` [Linux platforms]

#### Thread Management

* Thread pools (Server platforms)
* Scheduling (RTOS / Embedded Applications - Mobile Platforms)
* Contexts: Development; configuration
* _..._

### CORBA Interface Definitions

_(TBD. Effectively, this functionality would require an extension onto
CLORB, for definition of a seperate system for support of CORBA
application development in Common Lisp. That system should provide
support for protocols implementing Kerberos authentication and SSL
tunnelling onto CORBA, viz (JacORB)[http://www.jacorb.org]. Context:
"Mobile, Desktop, and Server Applications". See also: CLORB)_

* Concepts
    * Service application networks
    * Data-oriented computing
    * CORBA services for mobile embedded platforms
        * Inter-process communication (IPC) protocol and architectures 
          for application coordination within _chroot jails_ on mobile
          embedded platforms
    * CORBA and Microkernel architectures (desktop and server platforms)
    

### Integration with CLIM

_(TBD. See also: "Integration with ASDF," following; "Initial
Summary", following. Context: "Desktop Applications")_ 


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


#### Application Initialization

_(TBD. See also: "Integration with ASDF," following)_

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


## Development Environment

### Notes: The VirtualBox SDK

* Concepts
    * Desktop Virtualization
    * Software-Defined Networking (SDN)
    * Oracle xVM Virtualbox
        * Originally developed by Sun Microsystems
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

#### See also

* CLIM-Desktop [[CLiki](http://www.cliki.net/clim-desktop)][[source tree](http://common-lisp.net/viewvc/clim-desktop/)]
* History of Desktop Interfaces for Lisp Machines, for example
    * [_Symbolics_, Wikipedia Republished](http://en.wiki2.org/wiki/Symbolics),
      specifically, _[Ivory and Open Genera](http://en.wiki2.org/wiki/Symbolics#Ivory_and_Open_Genera)_

### Integration with Amazon Web Services

_(TBD. See "Initial Summary", following. Context: "Server Applications")_

### Integration with ASDF

_(TBD)_

#### Modeling of Output Files for Application Package Assembly

_(TBD. See also: [mci-doc-docbook][mci-doc-docbook])_

### Integration with Host Packaging System

_(TBD. See also: [mci-doc-docbook][mci-doc-docbook]; Debian Packaging
System; Cygwin; Android app store(s))_


### An initial summary about the APPLICATION system

Sidebar: [AFFTA][affta] was conceived as to represent the first usage
case for this application framework. That would be in a context of
_continuous integration_ and _functional testing_ within a distributed
development service network. 

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

#### Usage Case: IDE

This notification protocol may serve as a component of a system for
supporting application design within an integrated development
environment, and may be furthermore extended for appliation within a
server environment.
          

## "TO DO"

00. Make documentation modular - _AWS, Build System, Desktop Application System, etc._

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
