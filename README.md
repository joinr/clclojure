clclojure
=========

An experimental port of Clojure to Common Lisp.

Background
==========
Porting Clojure seems to be the thing to do these days.  After the clojurescript
compiler came out, people had a really good blueprint for hosting clojure in other
languages.  The fact that the clojurescript compiler is defined, largely, in terms 
of protocols, makes it a bit closer to the "clojure in clojure" goal.  As a result, 
you should be able to bootstrap the language (as they did with JavaScript) with 
a minimum of primitives.  Most of the language is defined in Clojure, so voila. 

We currently have Clojure ports targetting Scheme, C, C-via-Scheme, Python, etc.
I've found Common Lisp to be a particularly slick environment with some cool tools 
and a fairly active community.  I think it would be interesting to port the 
ideas from Clojure to Common Lisp, ultimately implementing a compliant Clojure in
Common Lisp.  

Goals
=====
Bridge the gap between the cool stuff in clojure and Common Lisp.

Implement really useful bits of clojure in portable common lisp, and provide them as 
stand-alone libraries.

This includes lazy sequences, the generic sequence abstraction, 
and the fundamental persistent hash-array-mapped-trie data structures in clojure:
-persistent vectors 
-persistent maps
-persistent sets.

Extend the generic sequence abstraction and other idioms to Common Lisp's built-in mutable
structures.  Common Lisp already has a sequence library, but I think Clojure's is more general and can be trivially extended to new types.  

Protocols are really nice, as are Clojure's arbitrary dispatch multimethods. 
Data literals are also highly useful.  I think Clojure nailed the choice of literals,
so providing reader macros for these guys would be very nice.

Possibly wrap a Common Lisp STM implementation, or cheat and use something like lparallel 
or just delegate to clojure.core.async (like clojurescript).

Bootstrap a fully functional Clojure onto Common Lisp.

Learn.

Status
======
Began work porting Clojure's persistent data structures from Java about 2 years ago, while 
simultaneously learning Common Lisp (and by necessity Java :/ ).
Got a working persistent vector implementation, with compatible clojure literals about a year ago.  
Started working on persistent hash maps around November 2012.

Built a temporary implementation of clojure-style protocols in Common Lisp ~ Dec 2012.
Pulled the bits into an actual ASDF system and pushed everything to the Github August 2013.

Implemented a baby clojure evaluator that __should__ bridge the lisp1/lisp2 gap between clojure and the Common Lisp host.  Unsure if this is going to work out in the long term, but eh.
It's real trivial at the moment.  

Working on library code in my spare time, still need to port the persistent map.

Hurdles
======
A couple of big hurdles: 
1. Lisp1 vs Lisp2.  I'll hack the evaluator for this.
2. Persistent structures.  Already built Pvector and 1/2 done with Pmap.
3. Protocols.  Already implemented as generic functions. 
4. Multimethds.  Need to find a way to implement multiple dispatch.
5. Reader.  
CL macros use , and ,@ in place of ~ and ~@ in Clojure.
We'll need to either cook the common lisp reader, or  build a separate clojure
reader that will perform the appropriate replacements. 
 
@ is a literal for #'deref in clojure, is whitespace in clojure.  
[] denote vectors -> already have a reader macro in pvector.lisp  
{} denote maps    -> already have a reader macro in pmap.lisp  
#{} denote sets

6.  Destructuring.  
This may be a bit tricky, although there are a limited number of clojure forms.  
7.  Seq library.  
This shouldn't be too hard.  I already have a lazy list lib prototype as well as
generic functions for the basic ops.  I think I'll try to use the protocols
defined in the clojurescript version as much possible, rather than baking in a
lot of the seq abstraction in the host language like clojure does.                

Usage
=====
Currently just clone the repository.  Assuming you have ASDF setup properly, 
you should be able to evaluate (require 'clclojure) at your Lisp REPL and it'll 
compile.  You can mess with the currently limited clojure evaluator in the 
:clclojure.base package.  
I'm a bit new to building Common Lisp projects, so the packaging will likely 
change as I learn.  At some point, if anything useful comes out of this
experiment, I may see if it can get pushed to quicklisp.

License
=======
Eclipse Public License, just like Clojure.