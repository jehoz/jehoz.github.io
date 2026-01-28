---
template: module.html
name: MK VCO
size: 8HP
thumbnail: /static/img/synth/mk_vco.jpg
blurb: >
  Simple VCO based on Moritz Klein's schematic.  Saw and pulse outputs,
  additional inputs for FM and PWM.
---

This is the first module I decided to build (an oscillator seemed like the obvious place to start).
The schematic is mostly taken from Moritz Klein's [EDU DIY VCO](https://www.ericasynths.lv/edu-diy-vco-2112),
but the PCB and front panel are my own.

![](/static/img/synth/mk_vco/schematic.svg)

When I was stocking up on components I made the mistake of buying a whole bunch of solder lug potentiometers instead of PCB-mount ones.
Why I did this I could not tell you, but as a result this PCB is (along with many future ones) designed to be hand-wired to the pots and jacks on the front panel.
This makes it a bit more annoying to build, but it's not the end of the world.

![](/static/img/synth/mk_vco/pcb.jpg)

I got the PCB and front panel made by JLCPCB, and I'm pretty happy with how they came out.
The gerber files for both are available on [github](https://github.com/jehoz/modular-synth/tree/master/mk_vco/pcb_gerbers).

This was the first PCB I've ever designed, so I was pretty excited when it actually worked after I soldered everything together.

![](/static/img/synth/mk_vco/built.jpg)