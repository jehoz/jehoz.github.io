---
template: projects.html
projects:
  - repo_name: propnet
    image: /static/img/thumbnails/propnet-wfc.gif
    built-in: haskell
    category: DSL
    blurb: >
      An embedded domain sepcific language for relational programming and solving 
      constraint satisfaction problems.
      The underlying mechanism is a novel implementation of propagator networks
      as proposed by Gerald Sussman and Alexey Radul in "The Art of the Propagator" (2009).

  - repo_name: nebulizer
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: rust
    category: music
    blurb: >
      A featureful granular synthesizer for playing music on your computer.
      Takes any audio sample and turns it into a fully-playable instrument.
      Ready to use with a MIDI device or incorporate into your music livecoding setup.

  - repo_name: terrain-erosion
    image: /static/img/thumbnails/erosion-terrain.png
    built-in: zig
    category: graphics/simulation
    blurb: >
      An experimental terrain generation program that simulates hydraulic erosion.  
      Uses different types of 2D-noise to generate an initial heightmap and then 
      transforms the terrain via a large number of simulated water droplets, each
      of which transfers a small amount of "sediment" as it moves along the terrain.

  - repo_name: legs-and-gaits
    image: /static/img/thumbnails/creature-walk.gif
    built-in: godot
    category: procedural animation
    blurb: >
      Proof-of-concept for a novel procedural animation technique for quadruped locomotion.
      Introduces a parametric leg structure that can model plantigrade, digitigrade, 
      and ungiligrade legs.
      Uses analytic IK for bone transformations (instead of the usual numerical 
      IK techniques used in procedural animation) and coordinates foot movement 
      with special parametric wave functions.

  - repo_name: psyn
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: golang
    category: music
    blurb: >
      An audio sampler that is controlled with UDP messages.
      Each UDP message plays a slice of an audio file and can specify effects for that slice.
      I made this to use with the Orca sequencer, but anything that can send UDP
      messages could work just as well.

  - repo_name: shove
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: haskell
    category: programming language
    blurb: >
      Scripting language designed around composing pipelines
---
