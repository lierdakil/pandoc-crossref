You can define subfigures:

<div id="fig:subfigures">
  ![1](fig1.png){#fig:subfig1}
  ![2](fig2.png){#fig:subfig2}
  ![3](fig3.png)

  ![4](fig4.png){#fig:subfig4}
  ![5](fig5.png)
  ![6](fig6.png){#fig:subfig6}

  ![7](fig7.png){#fig:subfig7}
  ![8](fig8.png)
  ![9](fig9.png){#fig:subfig9}

  Caption
</div>

<div id="fig:subfigures2">
  ![1](fig1.png){#fig:subfig21}

  ![2](fig2.png){#fig:subfig22}

  ![3](fig3.png)

  ![4](fig4.png){#fig:subfig24}

  ![5](fig5.png)

  ![6](fig6.png){#fig:subfig26}

  ![7](fig7.png){#fig:subfig27}

  ![8](fig8.png)

  ![9](fig9.png){#fig:subfig29}

  Caption
</div>

Figures themselves can be referenced @fig:subfigures2, as well as individual subfigures: [@fig:subfig1; @fig:subfig2; @fig:subfig29]
