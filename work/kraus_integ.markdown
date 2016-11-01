---
title: Kraus representation of the noisy quantum walk
---

### Quantum walk in Kraus representation

#### Kraus representation

This is continued from the [part](/workposts/quantumwalk.html). Here, we try to cast the noisy quantum walk in Kraus operator representation. Here, the "coin" operator is the source of the noise. Assume the coin (or spin ) begins in a state of equal superposition of two chiralities:

$$
\begin{equation}
\mid\!s\!\rangle = a_{\uparrow} \mid\!\uparrow\!\rangle + a_{\downarrow} \mid\!\downarrow\!\rangle
\end{equation}
$$

The corresponding density matrix would look in this form:
$$
\begin{eqnarray}
  \rho_{0} = \mid\!s\rangle \langle\!s\mid = \left[\begin{array}{c} a_{\uparrow}\\ a_{\downarrow}\end{array}\right] \left[\begin{array}{c}a_{\uparrow}^{*}\quad a_{\downarrow}^{*}\end{array} \right]
\rho_{0}  = \left[\begin{array}{cc}\lvert a_{\uparrow}\rvert^{2} & a_{\uparrow}a_{\downarrow}^{*} \\
               a_{\uparrow}^{*}a_{\downarrow} & \lvert a_{\downarrow}\rvert^{2}
    \end{array} \right]
\end{eqnarray}
$$

The noisy quantum operator acts on the initial state to give the final state as:
$$
\left[\begin{array}{cc} r & \sqrt{1 - r^{2}} \\\sqrt{1 - r^{2}} & -r \end{array}\right] \left[\begin{array}{c} a_{\uparrow} \\ a_{\downarrow} \end{array}\right] = \left[\begin{array}{c}ra_{\uparrow} + a_{\downarrow}\sqrt{1 - r^{2}} \\ a_{\uparrow}\sqrt{1 - r^{2}} - ra_{\downarrow} \end{array}\right]
$$

The density of the final state in the above equation can be written:
$$
\begin{equation}\label{eq:finalstate-densitymatrix}
\begin{aligned}
\rho^{\prime} & = {} \left[\begin{array}{c}ra_{\uparrow} + a_{\downarrow}\sqrt{1 - r^{2}} \\ a_{\uparrow}\sqrt{1 - r^{2}} - ra_{\downarrow} \end{array}\right] \left[\begin{array}{cc}ra_{\uparrow}^{*} + a_{\downarrow}^{*}\sqrt{1 - r^{2}} & a_{\uparrow}^{*}\sqrt{1 - r^{2}} - ra_{\downarrow}^{*} \end{array}\right] \\
& = \left[\begin{array}{cc} r^{2}\lvert a_{\uparrow}\rvert^2+(1-r^{2})\lvert a_{\downarrow}\rvert^{2}+(r\sqrt{1-r^{2}})(a_{\uparrow}a_{\downarrow}^{*}+a_{\uparrow}^{*}a_{\downarrow}) & (r\sqrt{1-r^{2}})(\lvert a_{\uparrow}\rvert^{2} - \lvert a_{\downarrow}\rvert^{2})+(1-r^{2})a_{\downarrow}a_{\uparrow}^{*}-r^{2}a_{\uparrow}a_{\downarrow}^{*} \\
(r\sqrt{1-r^{2}})(\lvert a_{\uparrow}\rvert^{2} - \lvert a_{\downarrow}\rvert^{2})+(1-r^{2})a_{\uparrow}a_{\downarrow}^{*}-r^{2}a_{\downarrow}a_{\uparrow}^{*} & (1-r^{2})\lvert a_{\uparrow}\rvert+ r^{2}\lvert a_{\uparrow}\rvert^2-(r\sqrt{1-r^{2}})(a_{\uparrow}a_{\downarrow}^{*}+a_{\uparrow}^{*}a_{\downarrow})
  \end{array}\right]
\end{aligned}
\end{equation}
$$
The idea is to represent the density matrix $\rho^{\prime}$ given in \eqref{eq:finalstate-densitymatrix} in Kraus representatiion. This new density matrix can be separated into 3 matrices as:
$$
\begin{equation}
  \begin{aligned}
  \rho^{\prime} = {} & r^{2} \left[\begin{array}{cc}\lvert a_{\uparrow}\rvert^{2} & -a_{\uparrow}a_{\downarrow}^{*} \\
               -a_{\uparrow}^{*}a_{\downarrow} & \lvert a_{\downarrow}\rvert^{2} \end{array} \right] 
   + (1-r^{2})\left[\begin{array}{cc}\lvert a_{\downarrow}\rvert^{2} & a_{\downarrow}a_{\uparrow}^{*} \\
               a_{\uparrow}a_{\downarrow}^{*} & \lvert a_{\uparrow}\rvert^{2} \end{array} \right] 
   + (r\sqrt{1-r^{2}})\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) \\
             (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*})\end{array} \right]
  \end{aligned}
\end{equation}
$$

If the limits of $r$ are from $-p$ to $+p$, it can be written in Kraus form as:
$$
\begin{equation}\label{eq:Krausform-integral}
  \begin{aligned}
    \rho_{s} = \int\limits_{r=-p}^{+p}\rho^{\prime}\,\mathrm{d}r = {} & \int\limits_{r=-p}^{+p}\mathrm{d}r\,r^{2}Z\rho_{0}\!Z^{\dagger} + \int\limits_{r=-p}^{+p}\mathrm{d}r\,(1-r^{2})X\rho_{0}\!X^{\dagger} \\
                  & + \int\limits_{r=-p}^{+p}\mathrm{d}r\,(r\sqrt{1-r^{2}})\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) \\
             (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} \end{array}\right]
  \end{aligned}
\end{equation}
$$
The terms $Z\rho\!Z^{\dagger}$ and $X\rho\!X^{\dagger}$, and the last matrix are independent of "$r$" and hence donot contribute to the integral, and the final state $\rho_{s}$ can be written as:
$$
\begin{equation}
  \begin{aligned}
    \rho_{s} = \int\limits_{r=-p}^{+p}\rho^{\prime}\,\mathrm{d}r = {} & \left.\left(\dfrac{r^{3}}{3}\right)\right|_{-p}^{+p}Z\rho_{0}\!Z^{\dagger} + \left.\left(r -\dfrac{r^{3}}{3}\right)\right|_{-p}^{+p}X\rho_{0}\!X^{\dagger} \\
                  & + \left.\left(-\dfrac{1}{3}\right)\left(1-r^{2}\right)^{\frac{3}{2}}\right|_{-p}^{+p}\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) \\
             (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} \end{array}\right]
    \end{aligned}
\end{equation}
$$
If we let $r$ range from $-1$ to $+1$, the last term gets cancelled out, and the final form looks:
$$
\begin{equation}
  %\begin{aligned}
    \rho_{s} = \left(\dfrac{2}{3}\right)Z\rho_{0}\!Z^{\dagger} + \left(\dfrac{4}{3}\right)X\rho_{0}\!X^{\dagger}
    %\end{aligned}
\end{equation}
$$

However, the earlier working assumption was that $r$ ranges from $0$ to $1$, and under this assumption, eq.\eqref{eq:Krausform-integral} changes to:
$$
\begin{equation}\label{eq:Krausform-integral-actual}
  \begin{aligned}
    \rho_{s} = \int\limits_{r=-0}^{1}\rho^{\prime}\,\mathrm{d}r = {} & \int\limits_{r=0}^{1}\mathrm{d}r\,r^{2}Z\rho_{0}\!Z^{\dagger} + \int\limits_{r=0}^{1}\mathrm{d}r\,(1-r^{2})X\rho_{0}\!X^{\dagger} \\
                  & + \int\limits_{r=0}^{1}\mathrm{d}r\,(r\sqrt{1-r^{2}})\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) \\
             (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} \end{array}\right]
  \end{aligned}
\end{equation}
$$
which can simplified to the form:
$$
\begin{equation}\label{eq:actual-decomp}
  \begin{aligned}
    \rho_{s} = {} & \left(\dfrac{1}{3}\right)Z\rho_{0}\!Z^{\dagger} + \left(\dfrac{2}{3}\right)X\rho_{0}\!X^{\dagger} \\
                  & + \left(\dfrac{1}{3}\right)\left[\begin{array}{cc}a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*} & (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) \\
             (\lvert a_{\uparrow}\rvert^{2}-\lvert a_{\downarrow}\rvert^{2}) & -(a_{\uparrow}a_{\downarrow}^{*}+a_{\downarrow}a_{\uparrow}^{*}) \end{array}\right]
  \end{aligned}
\end{equation}
$$


##### Future Work
The last matrix in the previous equation does not vanish easily, and it is not straight-forward to express the new density matrix in Kraus operator representation.

<sub><sup><br/><br/>Note: If you can't see the equations, please wait for a few seconds and check your internet connection. This site uses the cloud version of MathJax, and it may take time to render on slow connetions. If you still see errors in rendering, please let me know.<br/></sup></sub>