---
title: Quantum operator representation of the noisy quantum walk
---

**P.S.:** This is a work in progress, and this post is incomplete. I will update this page soon, when I make some progress on this topic. 

This is a continuation of the [page](/work/quantumwalk_noisy.html) which described noisy quantum walk. Here we try to recast the Schrodinger picture of the noisy quantum walk in Kraus operator (quantum operator) representation.

### Kraus representation of the quantum walk

Now we try the Kraus operator representation of noisy quantum walk. Here, the coin operator is the source of the noise. Assume the initial coin state (or spin state) is:

$$ \mid\!s\!\rangle = a_{\uparrow} \mid\!\uparrow\!\rangle + a_{\downarrow} \mid\!\downarrow\!\rangle $$

The corresponding density matrix would look:
$$
\begin{eqnarray}
  \rho_{0} = \mid\!s\!\rangle \langle\!s\!\mid = \left[\begin{array}{c} a_{\uparrow}\\ a_{\downarrow}\end{array}\right] \left[\begin{array}{c}a_{\uparrow}^{*}\quad a_{\downarrow}^{*}\end{array} \right]
\rho_{0}  = \left[\begin{array}{cc}\mid\! a_{\uparrow}\!\mid^{2} & a_{\uparrow}a_{\downarrow}^{*} \\
               a_{\uparrow}^{*}a_{\downarrow} & \mid a_{\downarrow}\mid^{2}
    \end{array} \right]
\end{eqnarray}
$$

The noisy quantum operator acts on the initial state to give the final state as:
$$
\left[\begin{array}{cc} r & \sqrt{1 - r^{2}} \\\sqrt{1 - r^{2}} & -r \end{array}\right] \left[\begin{array}{c} a_{\uparrow} \\ a_{\downarrow} \end{array}\right] = \left[\begin{array}{c}ra_{\uparrow} + a_{\downarrow}\sqrt{1 - r^{2}} \\ a_{\uparrow}\sqrt{1 - r^{2}} - ra_{\downarrow} \end{array}\right]
$$

The density of the final state in the above equation can be written:
$$ \begin{equation}\label{eq:finalstate-densitymatrix}
\begin{aligned}
\rho^{\prime} & = {} \left[\begin{array}{c}ra_{\uparrow} + a_{\downarrow}\sqrt{1 - r^{2}} \\ a_{\uparrow}\sqrt{1 - r^{2}} - ra_{\downarrow} \end{array}\right] \left[\begin{array}{cc}ra_{\uparrow}^{*} + a_{\downarrow}^{*}\sqrt{1 - r^{2}} & a_{\uparrow}^{*}\sqrt{1 - r^{2}} - ra_{\downarrow}^{*} \end{array}\right] \\
& = \left[\begin{array}{cc} r^{2}\mid\! a_{\uparrow}\!\mid^2+(1-r^{2})\mid\! a_{\downarrow}\!\mid^{2}+(r\sqrt{1-r^{2}})(a_{\uparrow}a_{\downarrow}^{*}+a_{\uparrow}^{*}a_{\downarrow}) & (r\sqrt{1-r^{2}})(\mid\! a_{\uparrow}\!\mid^{2} - \mid\! a_{\downarrow}\!\mid^{2})+(1-r^{2})a_{\downarrow}a_{\uparrow}^{*}-r^{2}a_{\uparrow}a_{\downarrow}^{*} \\
(r\sqrt{1-r^{2}})(\mid\!a_{\uparrow}\!\mid^{2} - \mid\! a_{\downarrow}\!\mid^{2})+(1-r^{2})a_{\uparrow}a_{\downarrow}^{*}-r^{2}a_{\downarrow}a_{\uparrow}^{*} & (1-r^{2})\mid\! a_{\uparrow}\!\mid+ r^{2}\mid\! a_{\uparrow}\!\mid^2-(r\sqrt{1-r^{2}})(a_{\uparrow}a_{\downarrow}^{*}+a_{\uparrow}^{*}a_{\downarrow})
  \end{array}\right]
\end{aligned}
\end{equation}  $$

We have tried to represent the density matrix $\rho^{\prime}$ given in \eqref{eq:finalstate-densitymatrix} in Kraus representatiion. It can be separated into 3 matrices (2 of which are in Kraus form) as:

$$ \begin{equation}
  \begin{aligned}
  \rho^{\prime} = {} & r^{2} \left[\begin{array}{cc}\mid\! a_{\uparrow}\!\mid^{2} & -a_{\uparrow}a_{\downarrow}^{*} \\
               -a_{\uparrow}^{*}a_{\downarrow} & \mid\! a_{\downarrow}\!\mid^{2} \end{array} \right] \\
  & + (1-r^{2})\left[\begin{array}{cc}\mid\! a_{\downarrow}\!\mid^{2} & a_{\downarrow}a_{\uparrow}^{*} \\
               a_{\uparrow}a_{\downarrow}^{*} & \mid\! a_{\uparrow}\!\mid^{2} \end{array} \right] \\
  & + (r\sqrt{1-r^{2}})\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\mid\! a_{\uparrow}\!\mid^{2}-\mid\! a_{\downarrow}\!\mid^{2}) \\
             (\mid\! a_{\uparrow}\!\mid^{2}-\mid\! a_{\downarrow}\!\mid^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*})\end{array} \right]
  \end{aligned}
\end{equation}  $$
which can be written in Kraus form as:

$$ \begin{equation}
  \begin{aligned}
    \rho^{\prime} = {} & r^{2}Z\rho_{0}\!Z^{\dagger} + (1-r^{2})X\rho_{0}\!X^{\dagger}
                  & + (r\sqrt{1-r^{2}})\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\mid\! a_{\uparrow}\!\mid^{2}-\mid\! a_{\downarrow}\!\mid^{2}) \\
             (\mid\! a_{\uparrow}\!\mid^{2}-\mid\! a_{\downarrow}\!mid^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} \end{array}\right]
  \end{aligned}
\end{equation}  $$

The problem boils down to casting all the terms of the above equation in the Kraus operator representation. The first two terms here are in the Kraus form, whereas the last one couldn't be cast in that form. (work in progress...)


<sub><sup><br/><br/>Note: If you can't see the equations, please wait for a few seconds and check your internet connection. This site uses the cloud version of MathJax, and it may take time to render on slow connetions. If you still see errors in rendering, please let me know.</sup></sub>
