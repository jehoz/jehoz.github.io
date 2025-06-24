---
template: projects.html
projects:
  - repo_name: propnet
    image: /static/img/thumbnails/propnet-wfc.gif
    built-in: haskell
    category: DSL
    blog-posts:
      - /articles/2024/09/05/propagator_networks_pcg.html
    blurb: >
      An embedded domain sepcific language for relational programming and constraint 
      satisfaction problems.
      Under the hood this is powered by propagator networks, a model for computation
      laid out by Gerald Sussman and Alexey Radul in "The Art of the Propagator" (2009).

  - repo_name: nebulizer
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: rust
    blurb: >
      A standalone granular synthesizer for playing music on your computer.
      Takes any audio sample and turns it into a fully-playable instrument.
      Ready to use with a MIDI device or incorporate into your music livecoding setup.

  - repo_name: terrain-erosion
    image: /static/img/thumbnails/erosion-terrain.png
    built-in: zig
    blog-posts:
      - /articles/2023/12/01/hydraulic_erosion.html
    blurb: >
      A program that generates realistic terrain through simulated hydraulic erosion.  
      Uses different types of 2D-noise to generate an initial heightmap and then 
      simulates a large number of water droplets, each of which makes small modifications
      to the terrain.

  - repo_name: legs-and-gaits
    image: /static/img/thumbnails/creature-walk.gif
    built-in: godot
    blog-posts:
      - /articles/2024/05/10/parametric_legs_and_gaits.html
    blurb: >
      An experimental technique for procedurally animating quadruped locomotion.
      Introduces a parametric leg structure that can model plantigrade, digitigrade, 
      and ungiligrade legs.
      Uses analytic IK for bone transformations (instead of the usual numerical 
      IK techniques used in procedural animation) and coordinates foot movement 
      with specially tuned wave functions.

  - repo_name: psyn
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: golang
    blurb: >
      An audio sampler that is controlled with UDP messages.
      Each message plays a slice of an audio file and can specify effects for that slice.
      I made this to use with the Orca sequencer, but anything that can send strings 
      over UDP should work just as well.

  - repo_name: shove
    image: /static/img/thumbnails/naenae-cat.gif
    built-in: haskell
    blurb: >
      An interpreter for a scripting language that I created.  The language was
      designed with an emphasis on composing functions into intuitively readable pipelines.
      Not especially practical in its current state, but contains some interesting
      design features that I'd like to expand on.
---
