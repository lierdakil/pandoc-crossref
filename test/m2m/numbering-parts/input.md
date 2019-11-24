---
prefixes:
  prt:
    from: sec
    title: Part
    ref: ["prt.", "prts."]
    numbering: roman
  sec:
    captionTemplate: '$$i$$$$titleDelim$$ $$t$$' # show part/etc numbers
    titleDelim: '.'
    numbering: arabic
    title: Chapter
    ref: ["chp.", "chps."]
    sub:
      # sub-sections can override any settings from parent
      # Here we want to have sections scoped to chapters, so
      scope: sec
      title: Section
      ref: ["sec.", "secs."]
      # We also want to show parent scope number in reference and title
      captionIndexTemplate: '$$s.i$$.$$ri$$'
      referenceIndexTemplate: '$$s.i$$.$$ri$$'
...

# First part {#prt:prt1}
## First chapter {#sec:cha1}
## Second chapter {#sec:cha2}
### A section {#sec:sec21}
### Another section {#sec:sec22}
# Second part {#prt:prt2}
## Third chapter {#sec:cha3}

@prt:prt1;

@sec:cha1;

@sec:cha2;

@sec:sec21;

@sec:sec22;

@prt:prt2;

@sec:cha3;
