The radicle guide
==================

radicle is a language for building and interacting with chains of the Open
Source Network.  What these chains do--how they behave--is entirely up to you. 

This guide provides an introduction to radicle--as a language, as a tool for
building chains, and as a means of interacting with existing chains.


An example
------------

As a running example, we'll create a chain for managing the issues of a
repository. We'll see how to define the format of issues that can be created;
how to define rights and permissions for certain types of interaction; and
how to allow for future changes to the chain. 

::

     (load! "rad/prelude.rad")
     (def issues (ref { :open nil :closed nil }))

     (def new-feature-request
       (fn [author description test-case] 
         ...))

     (def new-bug-report
       (fn [author description test-case] 
         ...))

     (def new-question
       (fn [author description] 
         ...))

     (def close-issue ...)

     (def handle-input 
       (fn [expr]
         ...))


.. toctree::
  :maxdepth: 1

  GettingStarted.md
  Basics.lrad
