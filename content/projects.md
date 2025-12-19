---
template: projects.html
projects:
  - repo_name: propnet
    built-in: haskell
    article: /articles/2024/09/05/propagator_networks_pcg.html
    blurb: >
      An embedded domain sepcific language for relational programming and constraint 
      satisfaction problems.
      Under the hood this is powered by propagator networks, a model for computation
      laid out by Gerald Sussman and Alexey Radul in "The Art of the Propagator" (2009).

  - repo_name: nebulizer
    built-in: rust
    blurb: >
      A standalone granular synthesizer for playing music on your computer.
      Takes any audio sample and turns it into a fully-playable instrument.
      Ready to use with a MIDI device or incorporate into your music livecoding setup.

  - repo_name: hydraulic-erosion
    built-in: c++
    article: /articles/2023/12/01/hydraulic_erosion.html
    blurb: >
      A program that creates realistic looking terrain through simulated hydraulic erosion.  
      Allows you to generate an initial heighmap with different types of 2D noise and then 
      simulate rainfall over time, where each droplet of water makes small modifications
      to the terrain.

  - repo_name: legs-and-gaits
    built-in: godot
    article: /articles/2024/05/10/parametric_legs_and_gaits.html
    blurb: >
      Proof-of-concept implementation of technique that I developed for procedurally animating 
      quadruped locomotion. Introduces a parametric leg structure that can model plantigrade, 
      digitigrade, and ungiligrade legs.
      Uses analytic IK for bone transformations (instead of the usual numerical 
      IK techniques used in procedural animation) and coordinates foot movement 
      with specially tuned wave functions and phase offsets between the legs.

  - repo_name: psyn
    built-in: golang
    blurb: >
      An audio sampler that is controlled with UDP messages.
      Each message plays a slice of an audio file and can specify effects for that slice.
      I made this to use with the Orca sequencer, but anything that can send strings 
      over UDP should work just as well.

  - repo_name: shove
    built-in: haskell
    blurb: >
      A scripting language designed with an emphasis on composing functions into pipelines 
      in an intuitive and readable way.
      Not very practical in its current state, but contains some interesting
      design features that I'd like to take further at some point.
---
