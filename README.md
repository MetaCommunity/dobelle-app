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

A. a desktop %application% using Garnet

B. an HTTP server %application% using Alexandria and/or CL-HTTP

C. an Eclpse IDE extension using ABCL and/or {CL+J, FOIL, ...}
in which the %application% may represent simply an interface onto the
Eclipse IDE - as the Eclipse IDE being, from the perspective of the
Commonn Lisp peer, a _Java Application_ [Java]

D. then define CORBA IDL for Nr. C, and extend as at least a simple
(?) CORBA-integrated message passing / application notification
framework for Common Lisp peer applications

## Resource Notes

1. [Hudson Continuous Integration](http://hudson-ci.org/)
2. [Hudson Continus Integration - Eclipse Foundation](http://www.eclipse.org/hudson/)
3. [Installing Hudson - Eclipsepedia](http://wiki.eclipse.org/Hudson-ci/Installing_Hudson)
4. [Forum: GForge is now FusionForge](https://fusionforge.org/forum/forum.php?forum_id=7)
5. [FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Main_Page)
6. [Installing - FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Installing)
7. [Debian Package Tracking System - fusionforge](https://packages.qa.debian.org/f/fusionforge.html)
8. Prakash, Winston. [Practicing Continuous Delivery Using Hudson](http://www.eclipse.org/hudson/the-hudson-book/book-hudson.pdf)
