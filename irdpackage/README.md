# irgn: An R package for Interpretable Regional Descriptors 

The `irdpackage` package provides various (model-agnostic)
methods for interpretable regional descriptors (IRD) via a unified R6-based interface.

IRD address questions of the form: “For
input $\mathbf{x}$, the model predicted $\hat{y}$. 
How can $\mathbf{x}$ be changed such that $\hat{y}$ is still predicted”.  

## Available methods

The implemented IRD methods are adapted/modified versions of the following methods: 

-  [Maximum-Box Approach (MaxBox)](https://link.springer.com/article/10.1023/A:1020546910706)
-  [Patient Rule Induction Method (PRIM)](https://link.springer.com/article/10.1023/A:1008894516817)
-  [Model-Agnostic Interpretable Rule Extraction Procedure (MAIRE)](https://link.springer.com/chapter/10.1007/978-3-030-84060-0_21)
-  [Anchors](https://ojs.aaai.org/index.php/AAAI/article/view/11491)

In addition a post-processing method is available, originally proposed by the authors (PostProcessing).

