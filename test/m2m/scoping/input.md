---
prefixes:
  dfn:
    ref: ["dfn.", "dfns."]
    title: "Definition"
    scope: "sec"
    referenceIndexTemplate: "$$i$$$$scp# (%)$$"
  cl:
    ref: ["cl.", "cls."]
    scope: "dfn"
    referenceIndexTemplate: "$$i$$$$scp# (%)$$"
  sec:
    captionTemplate: '$$titleName[lvl]% $$$$i$$$$titleDelim$$ $$t$$'
    captionIndexTemplate: '$$s%.$$$$i$$'
    referenceIndexTemplate: '$$i$$$$scp# (%)$$'
    scope: sec
    titleName:
      - Chapter
      - Section
      - Paragraph
    ref:
      - ["chp.", "sec.", "par."]
      - ["chps.", "secs.", "pars."]
    title: Section
    titleDelim: '.'
chapters: false
...

# Section 1 {#sec:1}

## Section 1.1 {#sec:11}

<div id="dfn:group">
A _group_ is a pair $(R,*)$ satisfying:

#. [$*$ is a monoid]{#cl:grpmul}
</div>

# Section 2 {#sec:2}

## Section 2.1 {#sec:21}

### Section 2.1.1 {#sec:211}

### Section 2.1.2 {#sec:212}

<div id="dfn:ring">
A _ring_ is a triple $(R,+,*)$ satisfying:

#. [$+$ is an abelian group]{#cl:addgp}
#. [$*$ is a monoid]{#cl:multmon}
#. [$*$ distributes over $+$]{#cl:distrib}
</div>

# Section 3

- @sec:1
- @sec:11
- @dfn:group
- @cl:grpmul

- @sec:2
- @dfn:ring
- @cl:addgp
- @Cl:multmon
- @Cl:distrib
