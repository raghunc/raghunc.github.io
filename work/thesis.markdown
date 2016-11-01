\documentclass[12pt,letterpaper,oneside]{report}


%\usepackage[english]{babel}
%\usepackage{fullpage}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsfonts}
\usepackage{graphicx}
%\usepackage{bibtex}
\usepackage[sort&compress,square,numbers]{natbib}
%\usepackage[square, sort, comma, numbers]{natbib}      %bibliography style
\usepackage{hyperref}
\usepackage[titletoc,page]{appendix}
\usepackage[margin=0.75in]{geometry}   %narrow margin, 1.5+ inches otherwise
\usepackage{caption}
\usepackage{subcaption}
%\usepackage{subfig}    %subcaption is preferred to subfig.
\usepackage{setspace}


\usepackage{placeins}

\usepackage[T1]{fontenc}


\bibliographystyle{ieeetr}     %APS style bibliography, can be acm, ieetr, plain etc.

\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

%\textwidth=6.5in
%\topmargin=0in
%\oddsidemargin=0in
%\textheight=9.3in

%Windows Path for the plots
%\graphicspath{{C:/Users/Raghu/Dropbox/drafts/MS\string_data/}}

%Linux Path for plots
\graphicspath{{/home/raghu/Documents/thesis/MS\string_data/ms\string_data/}}

\title{Spin Diffusion associated with a quantum random walk on a one-dimensional lattice}

\author{Raghu Nandan Chilukuri \\
         \\
        University of Cincinnati }
        
%\department{Electrical and Computer Engineering}
%\degree{Master of Science - Electrical Engineering}

\begin{document}

\maketitle
\tableofcontents
\listoffigures

\doublespacing

\begin{abstract}
%Quantum random walks have gained attention in recent times for their applications in diverse fields of physics, ranging from quantum computation, quantum algorithms to polymer and biological physics. Quantum random walks have also been shown to the basis of a universal model of quantum computation.

%Here, the focus is on the random walk properties of a quantum particle with spin which acts as an `\textit{internal coin}' that decides the next step in the walk. 

Classical random walks have found applications in explaining Brownian motion, diffusion of particles, designing computer algorithms, and even biological systems. Random walks can be used to explain the process of diffusion, and the concept of diffusion is used in the drift-diffusion model of motion of charge carriers in semiconductors. In these classical treatments of transport phenomena, the charge carrier transport can be characterized by a well defined diffusion coefficient $D$. Recently, there have been numerous proposals to make electronic and logic devices based on spin like the Datta-Das spinFET, which do not depend on charge transport. In such devices, the transport is non-classical (ballistic). This document examines the possibility of defining an analogous diffusion coefficient for non-classical transport, using quantum random walks as a starting point. In addition, this document also analyzes the effect of uniform noise on the behavior of quantum random walks and driving the quantum walk into classical regime.


%This document studies the effect of uniform noise on quantum random walks.
\end{abstract}

\chapter{Introduction}

The theory of random walk has been of interest in several fields like physics, computer science, biology and economics. In a random walk, a `\textbf{walker}' (or a particle) starts from a given point (the origin), and with a non-zero finite probability, takes a step in an arbitrary direction at each instant. The `steps' and `time instants' can be continuous variables (continuous-time random walk) or discrete (discrete-time random walk). Classically, random walk models have been used to describe a wide range of phenomena such as diffusion of particles (Brownian motion), motion of vacancies in a crystal, atoms on a crystalline surface, polymer chains and also biological systems. \newline

The random walk phenomenon can occur in one dimension (line), two dimensions (plane) or three dimensions (space). The following discussion focuses exclusively on the random walk in one dimension, which is termed `discrete quantum walk on the line'. In this kind of discrete random walk, the line (space) and time are divided into discrete units. During each timestep, the walker/particle takes one step in a random direction. This idea of walker taking discrete steps can naturally be extended to a random walk on a graph, giving rise to graph theoretic and computer science applications. Indeed, there are applications of Monte-Carlo Markov Chain (MCMC) algorithms, search algorithms and various other schemes to apply random walk models in computer science. \newline 

There exist classical random walk models useful in several algorithmic applications and it is natural to seek a quantum version of the random walk that is useful in building quantum algorithms. A good quantum algorithm should be faster/more efficient than a currently existing classical algorithm and should be robust to noise (the problem of decoherence). Although quantum computing has been thought of a very promising field, building good quantum algorithms had proved to be quite difficult. One proposed candidate to build good quantum algorithms is the \textit{\textbf{Quantum random walk}}. \newline 

Quantum random walk was first proposed by Y. Aharanov, L. Davidovich and N. Zagury~\citep{PhysRevA.48.1687} as a quantum mechanical analog of the classical random walk, with applications in quantum optics. As with classical random walk, quantum (random) walks belong to one of the two cases: continuous-time and discrete-time. Various applications of quantum random walks have been proposed in quantum optics~\citep{PhysRevA.48.1687}, quantum information and quantum computing, especially in quantum search algorithms~\citep{doi:10.1137/090745854}. Similar to classical random walks, quantum walks can occur in one (quantum walk on the line), two and three dimensions or on graphs (important for algorithmic applications). The information theoretic and computational properties and applications have been explored by several people; one such early exploration is by A. Vishwanath, A. Nayak, and A. Ambainis ~\citep{Ambainis:2001:OQW:380752.380757}. Simultaneously, various proposals have been made for the practical implementation of quantum walks in ion traps, optical lattices~\citep{PhysRevA.78.022314}, NMR and Bose-Einstein condensates(BEC)~\citep{PhysRevA.74.032307}. \newline

Historically, before the advent of graph algorithms, an important use of classical random walks was to explain Brownian motion~\citep{RevModPhys.15.1} which is used to explain the process of diffusion. Diffusion occurs in a wide variety of systems, including the movement of charge carriers in semiconductors. The movement of charge carriers in semiconductors is explained by the drift-diffusion model. The drift-diffusion model is based on the semi-classical Boltzmann Transport Equation, which is a semiclassical description of carrier transport. Modern semiconductor device sizes have become so small that the drift-diffusion model itself can no longer accurately predict the transport properties in such devices.

Now with the advent of proposals to make spin-based logic devices and transistors (such as the Datta-Das spinFET~\citep{datta-das-spinfet}), there have been attempts to use a diffusion coefficient for spin in such devices. This necessitates the need to examine if the ideas used in charge transport can be applied to spin transport equally well. There are questions regarding the validity of assuming the same diffusion coefficient ``$D$'' to describe spin and charge transport~\citep{spin-charge-inequality}. The current work examines the characterization of spin transport in different regimes, and proceeds to check if a unique spin diffusion coefficient can be defined, which can be used to characterize spin transport in different conditions. \newline

To understand the concept of a ``\textbf{diffusion coefficient} ($D$)'', consider a walker performing classical random walk, discrete or continuous. In the discussion in section~\ref{classical-diffusion-coeff}, the discrete classical random walk in one dimension is considered. Upon taking the continuum limit, one arrives at a coefficient which characterizes the diffusion phenomenon. This model doesn't take into account the kind of fields (or potentials) a charge carrier particle might experience, but the resulting parameter from this idealized model (diffusion coefficient) can be extended to describe the concept of diffusion in matter with external forces and fields, such as semiconductors. \newline

Similarly, to understand spin transport and examine if a `spin diffusion coefficient' can be analogously defined, consider an idealized model with no external interactions. In this model introduced shortly in Section~\ref{DTQW-model}, the walker performs discrete time quantum random walk in one dimension (on the line). The problem of mapping discrete time quantum walks(DTQW) to continuous time quantum walks(CTQW) has already been dealt with by several authors~\citep{PhysRevA.74.030301}, and wouldn't be discussed here. Among the possible types of discrete-time quantum random walks(DTQW) on the line, one variant of discrete-time quantum random walk known as the ``Hadamard walk'' is of special interest. It can be observed that quantum random walk has remarkably different properties compared to the classical random walk, such as a characteristic variance $\sigma^{2} \propto t^{2}$ or $\sigma \propto t$ where $\sigma(t)$ is the standard deviation of the probability distribution $P_{t}(n)$ of the particle. Compared to this, the classical random walk has a standard deviation $\sigma \propto \sqrt{t}$. Later, noise is added to model non-idealities and decoherence; and the noise was modelled by a uniform random number generator. \newline

In the simulations shown in later chapters, it can be seen that the observable asymptotic trend is $\sigma^{2} \propto t^{\gamma}$, where $\gamma = 2$ for the ideal, noiseless case (ballistic transport) and as the noise increases,$\gamma$ tends to $1$ (diffusive transport). In Chapter~\ref{chap: formalism}, Section~\ref{classical-diffusion-coeff} describes the classical random walk on the line. It shows how, in the continuous limit, a diffusion coefficient can be defined. Next, Section~\ref{DTQW-model} describes Hadamard walk and derives the equations of the discrete quantum random walk. It attempts to define a paramter analogous to the diffusion coefficient which was defined for the classical random walk. Later uniform random noise is added to the quantum random walk to model non-idealities in behavior. It can be observed that in the case of large noise, the random walk exhibits classical-like characteristics.


\chapter{Formalism} \label{chap: formalism}

\section{Classical Random walks}\label{classical-diffusion-coeff}

In a classical random walk, a particle (or `\textit{walker}') is located on the origin, and at each time step, the walker hops from the current site to one of the nearest neighbors with a certain probability. In the unbiased random walk, if a site has $c$ nearest neighboring sites, the probability of hopping to any of those sites is $\left(1/c\right)$. In one dimension, i.e. for the classical random walk on the line, the nearest neighbors are the lattice sites one step to the left or right of the walker. If the probability of moving to the left is, say, $p$, then the probability of moving to the right would be $(1 - p)$.  In the case of unbiased random walk on the line, the the probability of moving to the left (or right) is one-half, \textit{i.e.} $ p = (1 - p) = \frac{1}{2}$. The terms \textit{biased} or \textit{unbiased} are used analogous to coin-flipping experiments since the walker, like the coin, has two possibilities at each step. \newline

Consider a walker on a one dimensional lattice (line) in which a unit step length is $l$, and time for a single step is $\tau$. Let the probability to find it at a site labelled by number $n$ at time $t$ be $P_{t}(n)$. The time evolution at time $t+1$, by conservation of probability, requires that: 
\begin{equation}
P_{t+1}(n) = \dfrac{1}{2}[P_{t}(n-1) + P_{t}(n+1)] ,     \mbox{  where    } n = 0, \pm1, \pm2, \ldots
\end{equation}
 
which, on subtracting $P_{t}(n)$ on both sides becomes: 
\begin{equation} \label{eq:difference-eqn}
P_{t+1}(n) - P_{t}(n) = \dfrac{1}{2}[P_{t}(n-1) + P_{t}(n+1) - 2P_{t}(n)]
\end{equation}

Now the term on the left hand side of equation~\eqref{eq:difference-eqn} varies only with time $t$, and it can be written as:
\[ P_{t+1}(n) - P_{t}(n) = \tau \dfrac{\Delta P}{\Delta t} \mbox{where} \Delta\! t = (t+1) - t = \tau \]
Also, the term of the right hand side of equation~\eqref{eq:difference-eqn} varies with $n$, and can be seen as the second derivative of $P$ with respect to the spatial variable $n$, which can be written as:
\[ \dfrac{1}{2} (P_{t}(n-1) + P_{t}(n+1) - 2P_{t}(n) = l^{2} \dfrac{\Delta^{2} P}{\Delta n ^{2}}  \]
 
A continuous time description of the above random walk can be obtained by setting the lattice spacing $\Delta n \rightarrow 0$ and time step $\Delta t \rightarrow 0$. Under these limit conditions, the parameter $D$ defined by
\[ D = \lim_{l \to 0} \lim_{\tau \to 0} \dfrac{l^{2}}{2 \tau} \] is called the ``\textit{diffusion coefficient}'', which characterizes the rate at which the walker moves. In this continuous limit, the walk can be approximated to the diffusion equation: 
\begin{equation}
\dfrac{\partial P_{t}(n)}{\partial t} - D \dfrac{\partial^{2}P_{t}(n)}{\partial n^{2}} = 0     
\end{equation}

where $n$ and $t$ are now continuous variables. The solution of this differential equation is the well-known Gaussian probability distribution
\begin{equation}
P_{t}(n) =  \dfrac{1}{\sqrt{4 \pi Dt}} e^{-\frac{n^{2}}{4Dt}}
\end{equation}

Comparing this to the standard form of Gaussian distribution, the standard deviation is seen to be $\sigma(t) = \sqrt{2Dt}$ \textit{i.e.} $\sigma(t) \propto \sqrt{t}$. This equation in the continuous limit describes the well known diffusion phenomena in one dimension.

\section{Quantum Random walks}\label{DTQW-model}

The basis states of the walker for the quantum random walk on the line are position states, denoted by $\mid\! n\rangle$ for $n = 0, \pm1, \pm2, \ldots$. In classical random walk, specifying the position of the walker is sufficient to complete the description of the state of walker. However, the quantum random walk needs one more degree of freedom, called the ``\textit{chirality}'' or ``\textit{internal coin}'' which together with the position state, completes the description of the quantum walker. The term ``coin'' comes from classical random walk, where the movement of the walker at each step is decided by a `coin flip'. For an unbiased coin, the probabilities of the walker moving a step to the left or right are equal. In quantum random walk, the `coin' (or chirality) is usually some (discrete) property of the system, like the spin of a particle or the polarization of a photon. \newline

The basis states of chirality (internal coin) are $\mid\! L \rangle$ and $\mid\! R \rangle$, which usually represent a two level system. After each time-step, the walker moves one site to the left or right depending on whether the internal coin state is $\mid\! L \rangle$ or $\mid\! R \rangle$ respectively. In systems where the spin of the particle plays the role of internal coin, the basis states are also represented by $\mid \downarrow \rangle$ and $\mid \uparrow \rangle$ respectively. In this discussion, the coin states $\mid\! R\rangle$ and $\mid\!\uparrow\rangle$ are used interchangeably, and so are the symbols $\mid\! L\rangle$ and $\mid\!\downarrow\rangle$. The complete description of the walker is specified by the direct product of position and coin states as shown in Equation~\ref{eq: complete state}

\begin{equation} \label{eq: complete state}
\mid\!\psi\rangle = \mid\! n\rangle \mid\! s\rangle
\end{equation}

where $n = 0, \pm1, \pm2, \ldots$ and  $s$ can be $R (\equiv \uparrow)$ or  $L (\equiv \downarrow)$. \newline

In this representation, the walker moves to the right when the internal coin (spin) state is $R$ ($\uparrow$) and to the left for $L$ ($\downarrow$). An imporant property of quantum mechanical systems is the superposition of states. In the most general state, a quantum walker can be a superposition of the basis position and coin states. To see how the walker moves, let the walker start at origin ($n = 0$) and an arbitrary coin (spin) state ($R$ or $L$ or any superposition of $R$ and $L$) and then let the state of the walker evolve with time, as shown in Section~\ref{sec:qrw-evolution}. 

\subsection{Time evolution}
\subsubsection{Ideal Hadamard Walk} \label{sec:qrw-evolution}

To describe time evolution, consider a walker/particle performing a discrete-time quantum random walk on a one-dimensional lattice (line). The mathematical description used in this section closely follows that of Shapira et. al.~\citep{shapira2003one}, with a few changes. To see the time evolution of the walker, let the state of the walker at time $t$ be:
\begin{equation} \label{eq:initial-state}
\mid\! \psi(t)\rangle = \displaystyle \sum\limits_{n = -\infty}^{\infty}\sum\limits_{s=\downarrow}^{\uparrow} a_{n,s}(t)\mid\! n\rangle\mid\! s\rangle
\end{equation}

The state vector $\mid\! n\rangle$ ($n = 0, \pm1, \pm2, \ldots$) represents the spatial part of wavefunction at site $n$, and $\mid\! s\rangle$ ($s$ can be $L (\downarrow)$ or $R (\uparrow)$) is the spin (coin) part. The complex coefficients $a_{n,s}$ are probability amplitudes of the basis states $\mid\! n\rangle \mid\! s\rangle$. The complex numbers $a_{n,s}(t)$ are probability amplitudes, and hence obey the relation $\sum\limits_{n = -\infty}^{\infty} \mid\! a_{n,s}(t)\!\mid^{2} = 1$.  The coefficients $a_{n,s}(t+1)$ at the next time step depends on the spin/chirality state at time $t$. The time evolution of the quantum random walk is given by:

\begin{equation} \label{eq:time-evolution}
\mid\! \psi(t+1)\rangle = \hat{Q}\mid\! \psi(t)\rangle
\end{equation} 

The time evolution operator $\hat{Q}$ is a product of two components $\hat{Q} = \hat{T}\hat{U_{0}}$. The order of operation is from right to left, so the operator $\hat{U_{0}}$ acts first, followed by $\hat{T}$. Now, $\hat{U_{0}} = \hat{I} \otimes \hat{w_{0}}$ is an operator which acts only on the chiral(spin) state space(by $\hat{w_{0}}$) and leaves the position state unchanged (by acting with $\hat{I}$, the identity operator). In the next step, the $\hat{T}$ operator acts on the position space (moving the particle one step), leaving the coin state unchanged. This sequence of operations corresponds to the idea of flipping a coin, and then allowing the walker to take a step left or right depending on the outcome of coin-flip. \newline

The requirement on $\hat{w_{0}}$ is that it should be a unitary operator, to conserve probability. So out of several possible choices, consider the standard ``\textit{Hadamard}'' walk, in which a `Hadamard operator' acts on the chiral (coin) state. The choice of the Hadamard matix as the coin operator is not unique. There are other possible coin operators, such as the Grover coin opeator. Several other possible approaches to control the quantum random walks by choices of initial states and different coin operators are described the works of Tregenna~\citep{1367-2630-5-1-383}, Brun~\citep{PhysRevA.67.032304}~\citep{PhysRevLett.91.130602} etc. \newline

Physically, the Hadamard operator takes a spin-up or spin-down state and produces an equal superposition state and the identity acting on the position ket leaves it unchanged. The action of the Hadamard operator can be represented as:

\begin{equation} \label{eq:hadamard-op}
\begin{aligned}
\hat{w_{0}}\mid\! \uparrow\rangle = \dfrac{\mid\! \uparrow\rangle + \mid\! \downarrow\rangle}{\sqrt{2}} \\
 \hat{w_{0}}\mid\! \downarrow\rangle = \dfrac{\mid\! \uparrow\rangle - \mid\! \downarrow\rangle}{\sqrt{2}}
\end{aligned}
\end{equation}

\textit{i.e.}, the Hadamard operator (matrix) acting on a pure spin state sends them into one of two mutually orthogonal states (new-basis) in which the probabilities of $\mid\! \uparrow\rangle$ and $\mid\! \downarrow\rangle$ are equal. \newline

 For the quantum walk on the line, the coin state can take one of two possible values, physically implemented by two level systems like a spin-$\frac{1}{2}$ system or plane-polarized photons. The basis states of the 2-level coin state can be represented as a 2-level spin system in which 

\begin{equation}\label{eq:spin-basis}
\mid\! R\rangle = \mid\! \uparrow\rangle \equiv \left[ \! \begin{array}{c}
1 \\
0\\
\end{array} \!\right] \mbox{               and               } 
\mid\! L\rangle = \mid\! \downarrow\rangle \equiv \left[ \! \begin{array}{c}
0 \\
1\\
\end{array} \!\right]
\end{equation}

The Hadamard operator which converts these basis states into two (orthogonal) equal-superposition new basis states, in the above representation, is given by:
\begin{equation}
\hat{w_{0}} = \dfrac{1}{\sqrt{2}} \left[ \!\begin{array}{cc}
                       1 &  1 \\
                       1 & -1 \\
         \end{array} \!\right]
\end{equation} 

After acting with $U_{0}$, the position shift operator on the left \textit{i.e.} $\hat{T}$ acts on the position part of the state. The $\hat{T}$ operator is a translation operator which shifts the position of the particle one step to the left or right depending on whether the chirality is $\downarrow$ or $\uparrow$ respectively. 
\begin{equation}\label{eq:translation-op}
\begin{aligned}
\hat{T}\mid\! n\rangle \mid\! \uparrow\rangle = \mid\! n+1\rangle \mid\! \uparrow\rangle \\
\hat{T}\mid\! n\rangle \mid\! \downarrow\rangle = \mid\! n-1\rangle \mid\! \downarrow\rangle 
\end{aligned}
\end{equation}  

To see the time-evolution operator in action, start with the initial state specified in equation ~\eqref{eq:initial-state} and the time evolution as considered in equation ~\eqref{eq:time-evolution}.
\[ \mid\!\Psi(t+1)\rangle = \hat{T}\hat{U_{0}} \mid\! \Psi(t)\rangle =  \displaystyle \sum\limits_{n = -\infty}^{\infty} \hat{T}\hat{U_{0}} a_{n,\uparrow}(t) \mid\! n\rangle \mid\!\uparrow\rangle +  \displaystyle \sum\limits_{n = -\infty}^{\infty} \hat{T}\hat{U_{0}} a_{n,\downarrow}(t) \mid\! n\rangle \mid\!\downarrow\rangle \]

which, according to the action of Hadamard and time evolutions operators described in equations ~\eqref{eq:hadamard-op} and ~\eqref{eq:translation-op} respectively becomes:
\[ \hat{Q}\mid\! \Psi(t)\rangle =  \displaystyle \sum\limits_{n = -\infty}^{\infty}  \dfrac{a_{n,\uparrow}(t)}{\sqrt{2}} \left( \mid\! n+1\rangle \mid\!\uparrow\rangle + \mid\! n-1\rangle \mid\!\downarrow\rangle  \right)    +  \displaystyle \sum\limits_{n = -\infty}^{\infty} \dfrac{a_{n,\downarrow}(t)}{\sqrt{2}} \left(  \mid\! n+1\rangle \mid\!\uparrow\rangle - \mid\! n-1\rangle \mid\!\downarrow\rangle   \right)    \]

Collect the terms with identical kets:
\begin{equation}
\mid\!\Psi(t+1)\rangle =  \displaystyle \sum\limits_{n = -\infty}^{\infty} \left\lbrace \left( \dfrac{ a_{n,\uparrow}(t) + a_{n,\downarrow}(t)  }{\sqrt{2}} \right) \mid\! n+1\rangle \mid\!\uparrow\rangle + \left( \dfrac{ a_{n,\uparrow}(t) - a_{n,\downarrow}(t) }{\sqrt{2}} \right) \mid\! n-1\rangle \mid\!\uparrow\rangle   \right\rbrace
\end{equation} 

Comparing it to the standard form the state vector at time $t+1$ as described in equation~\eqref{eq:initial-state}, the relation between the coefficients $a_{n,s}$ at different time instants can be summarized as:
\begin{subequations} \label{eq:ideal-coeff}
\begin{align}
a_{n,\uparrow}(t+1) = \dfrac{1}{\sqrt{2}}[a_{n-1,\uparrow}(t) + a_{n-1,\downarrow}(t)] \\
a_{n,\downarrow}(t+1) = \dfrac{1}{\sqrt{2}}[a_{n+1,\uparrow}(t) - a_{n+1,\downarrow}(t)]
\end{align}
\end{subequations}

To solve these recursive relations, assume the walker starts at the origin ($n = 0$), and fix the chirality of the walker in a known state, say, $\mid\uparrow\rangle$ or $\mid\downarrow\rangle$ or any known superposition of  $\mid\uparrow\rangle$ and $\mid\downarrow\rangle$. This way, once the initial state is known, the coefficients $a_{n,s}(t)$ at any finite time $t$ can be calculated and the state of the walker at any time is known. \newline

The probability of finding the walker at position $n$ at time $t$ is $P_{t}(n)$. The quantity of interest is the standard deviation $\sigma(t)$ of the position of the walker performing the Hadamard walk, given by:

$$\sigma^{2}(t) \equiv \overline{\langle n^{2}(t) \rangle} - \overline{\langle n(t) \rangle}^{2} $$ 

where $\langle n \rangle$ and $\langle n^{2} \rangle$ are first and second moments of the position, and are defined by: 

\begin{eqnarray}
\overline{\langle n^{2}(t) \rangle} = \displaystyle \sum\limits_{n = -t}^{t} n^{2} \langle P_{t}(n)\rangle \\
\overline{\langle n(t) \rangle} = \displaystyle \sum\limits_{n = -t}^{t} n \langle P_{t}(n)\rangle
\end{eqnarray}

These relations, derived in the work of Shapira et. al.~\citep{shapira2003one} among others, are valid for the ideal Hadamard walk (without noise, scattering or reflections). \newline

\subsubsection{Noisy Hadamard Walk} \label{sec: noisy hadamard walk}

In the formalism described above, one way to introduce noise is to `bias' the ideal coin, that is, change the Hadamard operator (matrix). Physically, instead of sending a pure spin into one of the two orthogonal equal-superposition states, this biased coin operator sends a spin into a superposition in which the probabilities of $\mid\! \uparrow\rangle$ and $\mid\! \downarrow\rangle$ are not equal; and hence the `bias'. \newline

There were several works which study the effect of noise on quantum random walks. One work is the paper by SHapira et. al.~\citep{shapira2003one} already discussed. Further related publications which deal with the effect of noise of quantum random walks are by 

This current work tries to model the influence of uniform random noise on the walk. For this, a random variable `$r$' is introduced, and can take any values in the range $[0,1]$ (both inclusive). For the modified Hadamard operator to give the bias, the new, more general ``\textit{coin}'' operator is defined to be:
\begin{equation}
\hat{w} = \left[ \begin{array}{cc}
r                 &  \sqrt{1 - r^{2}} \\
\sqrt{1 - r^{2}}  &   -r \\
\end{array} \right]
\end{equation}


It can be seen that the Hadamard matrix $\hat{w_{0}}$ is special case of $\hat{w}$ shown above, when $r = \frac{1}{\sqrt{2}}$. Also, the matrix $\hat{w}$ can give rise to other important matrices for various values of $r$:

\begin{equation}
\hat{w}(r=0) = \left[ \begin{array}{cc}
 0     &     1  \\
 1     &     0  \\
\end{array}  \right] \equiv \sigma_{x} \mbox{ , and  }
\hat{w}(r=1) = \left[ \begin{array}{cc}
1     &    0  \\
0     &   -1  \\
\end{array} \right] \equiv \sigma_{z}
\end{equation} 


The action of the noisy Hadamard operator on the basis states in ~\eqref{eq:spin-basis} is given by:
\begin{subequations} \label{eq:noisy-H-action}
\begin{align}
\hat{w} \mid\! \uparrow\rangle =  r\mid\!\uparrow\rangle + \sqrt{1 - r^{2}} \mid\!\downarrow\rangle   \equiv \left[ \begin{array}{c}
                                            r  \\
                                         \sqrt{1 - r^{2}}
                                        \end{array} \right]        \\                                                                  
\hat{w} \mid\! \downarrow\rangle =  \sqrt{1 - r^{2}} \mid\!\uparrow\rangle - r \mid\!\downarrow\rangle  \equiv   \left[ \begin{array}{c}
                                         \sqrt{1 - r^{2}}  \\
                                               -r
                                        \end{array} \right] 
\end{align}                                                                               
\end{subequations}

Substitute these relations in the time evolution equation~\eqref{eq:time-evolution}, where $Q = \hat{T}\hat{U_{0}}$. The operator $\hat{U_{0}}$ acts first, and its action is given by:
\begin{eqnarray}
\begin{aligned}
\hat{U_{0}}\mid\! n\rangle \mid\!\uparrow\rangle = (\hat{I}\otimes \hat{w}(r)) \mid\! n\rangle \otimes \mid\!\uparrow\rangle \\
\hat{U_{0}}\mid\! n\rangle \mid\!\uparrow\rangle = [\hat{I}\mid\! n\rangle] \otimes [\hat{w}(r)\mid\!\uparrow\rangle]
\end{aligned}
\end{eqnarray} 

The action of the Hadamard operator $\hat{w}(r)$ is given by equation~\eqref{eq:noisy-H-action}, so that the above equation becomes
\begin{equation}
\hat{U_{0}}\mid\! n\rangle \mid\!\uparrow\rangle = \mid\! n\rangle \otimes \{r\mid\!\uparrow\rangle + \sqrt{1 - r^{2}} \mid\! \downarrow\rangle\}
\end{equation}

Instead, if the initial state of the walker is $\mid\! n\rangle \mid\!\downarrow\rangle$,  the resulting state after the operator $U_{0}$ acts becomes:
\begin{equation}
\hat{U_{0}}\mid\! n\rangle \mid\!\downarrow\rangle = \mid\! n\rangle \otimes \{\sqrt{1 - r^{2}}\mid\!\uparrow\rangle - r\mid\!\downarrow\rangle \}
\end{equation} 

Now the translation operator $\hat{T}$ acts on the resulting state $\hat{U_{0}}\mid\!n\rangle\mid\!s\rangle$ (where $s = \uparrow$ or $\downarrow$) to complete the time evolution described in equation~\eqref{eq:time-evolution}. The state of the walker at the next time step would be:
\begin{equation}
\mid\! \psi(t+1)\rangle = \hat{T} \left[ \displaystyle \sum\limits_{n = -\infty}^{\infty} a_{n,\uparrow}(t)\mid\! n\rangle \left( r \mid\!\uparrow\rangle + \sqrt{1 - r^{2}} \mid\!\downarrow\rangle\right)  \\
+  \displaystyle \sum\limits_{n = -\infty}^{\infty} a_{n,\downarrow}(t)\mid\! n\rangle \left(\sqrt{1 - r^{2}} \mid\!\uparrow\rangle - r \mid\!\downarrow\rangle\right)    \right]
\end{equation}

Now using the properties of time-evolution operator $\hat{T}$ described in~\eqref{eq:translation-op}, the above equation becomes:
\begin{equation}
\mid\! \psi(t+1)\rangle = \displaystyle \sum\limits_{n = -\infty}^{\infty} a_{n,\uparrow}(t) \left( r\mid\! n+1\rangle \mid\!\uparrow\rangle + \sqrt{1 - r^{2}} \mid\! n-1\rangle \mid\!\downarrow\rangle  \right) +   \\
\displaystyle \sum\limits_{n = -\infty}^{\infty} a_{n,\downarrow}(t) \left( \sqrt{1 - r^{2}} \mid\! n+1\rangle \mid\!\uparrow\rangle - r \mid\! n-1\rangle \mid\!\downarrow\rangle \right)
\end{equation}

 and rearranging the terms in a summation of single infinite series in terms of the corresponding state vectors:
\begin{equation}
%\begin{multline}
\mid\! \psi(t+1)\rangle = \displaystyle \sum\limits_{n = -\infty}^{\infty} \left[  \left( a_{n,\uparrow}(t) r + a_{n,\downarrow}(t) \sqrt{1 - r^{2}} \right) \mid\! n+1\rangle \mid\!\uparrow\rangle  +  \\
\left( a_{n,\uparrow}(t) \sqrt{1 - r^{2}} - a_{n,\downarrow}(t) r \right) \mid\! n-1\rangle  \mid\!\downarrow\rangle  \right]
%\end{multline}
\end{equation} 

To find out the relations between various coefficients $a_{n,\uparrow}$ and $a_{n,\downarrow}$, the above equation needs to be rewritten. Since the state vector is the sum of an infinite series, $\mid\! n+1\rangle$  can be rewplaced with $\mid\!n\rangle$ and replace $a_{x}$ with $a_{x-1}$ in the corresponding coefficient term. This amounts to 'adjusting the window' to look at the term one step to the left. Similarly, replace the coefficients $a_{n}$ with $a_{n+1}$ and $\mid\! n-1\rangle$ with $\mid\! n\rangle$ in the second set of brackets, looking at one term to the right in the infinite sum. The resulting equation can be rewritten as:
\begin{equation}
\mid\! \psi(t+1)\rangle = \displaystyle \sum\limits_{n = -\infty}^{\infty} \left[ \left( a_{n-1,\uparrow}(t)r + a_{n-1,\downarrow}(t) \sqrt{1 - r^{2}} \right) \mid\! n\rangle \mid\!\uparrow\rangle + \\ 
\left( a_{n+1,\uparrow}(t) \sqrt{1 - r^{2}} - a_{n+1,\downarrow}(t)r \right) \mid\! n\rangle \mid\!\downarrow\rangle      \right]
\end{equation}

These terms in parentheses are the coefficients of state vectors at time $t+1$ \textit{i.e.} $a_{n,\uparrow}(t+1)$ and $a_{n,\downarrow}(t+1)$. Comparing the coefficients of basis vectors in the above equation to those in the time evolution equation ~\eqref{eq:time-evolution}, the following recursive relations are obtained:
\begin{subequations}\label{eq:noisy-coeff}
\begin{align}
a_{n, \uparrow}(t + 1) = a_{n-1, \uparrow}(t).r + a_{n-1, \downarrow}(t).\sqrt{1 - r^{2}} \\
a_{n, \downarrow}(t + 1) = a_{n+1, \uparrow}(t).\sqrt{1 - r^{2}} - a_{n+1, \downarrow}(t).r
\end{align}
\end{subequations}

However, unlike the ideal Hadamard walk described in section~\ref{sec:qrw-evolution}, in our work, the standard deviations of spin-up (related $a_{n,\uparrow}$) and spin-down (related to $a_{n,\downarrow}$) are considered seperately. This is in line with our goal, outlined in the introduction, of finding difference in diffusion coefficients for different spin orientations. In the following chapters, it is shown that under ideal conditions (very low noise/decoherence), the values of $\sigma_{\uparrow}^{2}(t)$ and $\sigma_{\downarrow}^{2}(t)$ are significantly different for different initial spin injection conditions. To examine the effect of noise, $r$ is aloowed to vary as a uniformly distributed random variable. In order to calculate values of $\sigma^{2}$, the first and second moments of spatial distributions of particle/walker and the corresponding probability amplitudes. \newline

The probability of finding a spin-up particle at time $t$ at position $n$ is given by:
\begin{equation}
P_{t}^{\uparrow}(n) = \mid\! a_{n,\uparrow}(t)\!\mid^{2}
\end{equation}
 and similarly, the correspoding probability for a spin-down particle is:
\begin{equation}
P_{t}^{\downarrow}(n) = \mid\! a_{n,\downarrow}(t)\!\mid^{2}
\end{equation}

With these definitions, the first and second moments of spatial distribution of a particle starting in spin-up state ($\uparrow$) are:
\begin{subequations}
\begin{align}
\overline{\langle n_{\uparrow}(t)\rangle} = \displaystyle \sum\limits_{n = -t}^{t} n \langle P_{t}^{\uparrow}(n)\rangle \\
\overline{\langle n_{\uparrow}^{2}(t)\rangle} = \displaystyle \sum\limits_{n = -t}^{t} n^{2} \langle P_{t}^{\uparrow}(n)\rangle
\end{align}
\end{subequations}

Similarly, for a particle starting in the spin-down state ($\downarrow$), the first and second moments of spatial distribution are:
\begin{subequations}
\begin{align}
\overline{\langle n_{\downarrow}(t)\rangle} = \displaystyle \sum\limits_{n = -t}^{t} n \langle P_{t}^{\downarrow}(n)\rangle \\
\overline{\langle n_{\downarrow}^{2}(t)\rangle} = \displaystyle \sum\limits_{n = -t}^{t} n^{2} \langle P_{t}^{\downarrow}(n)\rangle
\end{align}
\end{subequations}

Using these definitions of first and second moments, the standard deviation of spatial distribution for spin-up and spin-down is written as:
\begin{eqnarray}
\sigma_{\uparrow}(t)    =  \sqrt{\left( \overline{\langle n_{\uparrow}(t) \rangle} - \overline{\langle n_{\uparrow}(t) \rangle}^{2} \right) } \\
\sigma_{\downarrow}(t)  =  \sqrt{\left( \overline{\langle n_{\downarrow}(t) \rangle } - \overline{\langle n_{\downarrow}(t) \rangle}^{2}   \right) }
\end{eqnarray}

The plots of the outcomes of simulation show the spatial distribution probabilities of the walker ($P(n) vs n$) and $\sigma_{\uparrow}^{2}$ and $\sigma_{\downarrow}^{2}$ as a function of time $t$.

%In our work, we have considered the variance for the cases of spin-up (\textit{i.e.} $\sigma_{\uparrow}^{2}(t)$) and spin-down(\textit{i.e.} $\sigma_{\downarrow}^{2}(t)$) separately. We aim to show that under ideal conditions (very low noise or decoherence), the values of $\sigma_{\uparrow}^{2}(t)$ and $\sigma_{\downarrow}^{2}(t)$ are different for different initial spin injection conditions. This work also examines the effect random noise on the convergence of variance for both spin orientations. \newline

%The first and second moments of the spatial distribution at time $t$, averaged over several iteration and other noise is given as~\citet{shapira2003one}: 

%The standard deviation $\sigma_{\alpha}(t)$ of the Hadamard walk with noise $\alpha$ is given by:

%$$\sigma_{\alpha}^{2}(t) \equiv \overline{\langle n^{2}(t) \rangle}_{\alpha} - \overline{\langle n(t) \rangle}^{2}_{\alpha} $$

%In the work reported by Shapira \textit{et. al.}~\citet{shapira2003one}, the standard deviation is calculated for the overall walk of the particle (both spin orientations). In our work, we have considered the variance for the cases of spin-up (\textit{i.e.} $\sigma_{\uparrow}^{2}(t)$) and spin-down(\textit{i.e.} $\sigma_{\downarrow}^{2}(t)$) separately. We aim to show that under ideal conditions (very low noise or decoherence), the values of $\sigma_{\uparrow}^{2}(t)$ and $\sigma_{\downarrow}^{2}(t)$ are different for different initial spin injection conditions. This work also examines the effect random noise on the convergence of variance for both spin orientations. \newline



\chapter{Simulations} \label{chap: simulations}

\section{Design of Simulations} \label{sec: main simulations}

The ideal quantum walk described in section ~\ref{sec:qrw-evolution} has been subject to several investigations by several authors, both analytically~\citep{konno2008quantum}~\citep{PhysRevA.78.052309} and numerically~\citep{shapira2003one}~\citep{PhysRevA.76.022316}. It has been already observed in section ~\ref{classical-diffusion-coeff} that classical random walk gives rise to a Gaussian probability distribution. Numerical simulations of quantum random walk show a different behavior, with a probability distribution which is symmetric about the origin. For a given number of time-steps, quantum random walk spreads much further on the lattice than its classical counterpart, with two peaks at each end of the line and is rather flat at the center (origin) as show in Figure ~\ref{fig: Ideal quantum walk P(n)}.

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_eqsup/noise0\string_3"}
\caption{Spatial probability distribution of quantum random walk with the Hadamard coin, in the ideal case starting with equal superposition}
\label{fig: Ideal quantum walk P(n)}
\end{figure}

For the ideal quantum walk whose spatial probability distribution is given in Figure~\ref{fig: Ideal quantum walk P(n)}, the variance of the walk $\sigma^{2}$ varies as $t^{2}$ (or for the standard deviation, $\sigma \propto t$), which is shown in Figure~\ref{fig: Ideal quantum walk variance}. \newline

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_eqsup/noise0\string_1"}
\caption{Variance of quantum random walk with the Hadamard coin, in the ideal case}
\label{fig: Ideal quantum walk variance}
\end{figure}

However, the quantity of interest is the ratio $\left(\frac{\sigma^{2}}{t}\right)$, which in the classical scenario is related to the diffusion coefficient (upto some constant). Hence the plot of ratio $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for the ideal quantum random walk is show in Figure~\ref{fig: Ideal quantum walk ratio equalsup}.

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_eqsup/noise0\string_2"}
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ as a function of $t$ for ideal quantum random walk with the Hadamard coin, with the walker starting in an equal superposition of spin-up and spin-down}
\label{fig: Ideal quantum walk ratio equalsup}
\end{figure} 

This is the standard behavior of quantum random walk which is explained in several works. However, most works do not explore the sensitivity of $\sigma$ to the initial injection condition; even if it is, it is explored only in the ideal (noiseless Hadamard) case. If the initial condition of the walker is right chirality (R) or spin-up ($\uparrow$), the probability distribution is skewed to the right as shown in Figure \ref{fig: Ideal quantum walk P spinup} . Similarly, if the walker starts with left chirality (L) or spin-down ($\downarrow$), the probability distribution is skewed to the left as shown in Figure \ref{fig: Ideal quantum walk P spindown}.

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_spinup/noise0\string_3"}
\caption{Spatial probability distribution of quantum random walk with the Hadamard coin, in the ideal case starting with spin-up}
\label{fig: Ideal quantum walk P spinup}
\end{figure}

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_spindown/noise0\string_3"}
\caption{Spatial probability distribution of quantum random walk with the Hadamard coin, in the ideal case starting with spin-down}
\label{fig: Ideal quantum walk P spindown}
\end{figure}

\FloatBarrier

The corresponding plots of $\frac{\sigma^{2}}{t}$ vs $t$ are shown in Figure~\ref{fig: Ideal quantum walk P spinup} (for the walker starting in spin-up state) and Figure~\ref{fig: Ideal quantum walk P spindown} (for the walker starting in spin-down state) respectively. It can be clearly seen that the value of the quantity $\left(\frac{\sigma^{2}}{t}\right)$ exhibits a strong dependence on the initial configuration of the walker. For a walker starting with a spin-up configuration (Figure~\ref{fig: Ideal quantum walk P spinup}), the ratio ($\frac{\sigma_{\uparrow}^{2}}{t}$), which is proportional to the classical diffusion coefficient for spin-up configuration is clearly significantly greater than ($\dfrac{\sigma_{\downarrow}^{2}}{t}$) for spin-down. Similarly, if the walker starts with spin-down, the value of $\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ is significantly greater than $\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ as shown in Figure~\ref{fig: Ideal quantum walk P spindown}. 

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_spinup/noise0\string_2"}
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ as a function of $t$ for ideal quantum random walk with the Hadamard coin, with the walker starting in spin-up state}
\label{fig: Ideal quantum walk ratio spinup}
\end{figure} 

\begin{figure}[h]
\centering
\includegraphics[keepaspectratio,height=0.4\textheight]{"plots\string_spindown/noise0\string_2"}
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ as a function of $t$ for ideal quantum random walk with the Hadamard coin, with the walker starting in spin-down state}
\label{fig: Ideal quantum walk ratio spindown}
\end{figure}

\FloatBarrier

It can be seen in Figure~\ref{fig: Ideal quantum walk ratio equalsup}, the quantity $\left(\frac{\sigma^{2}}{t}\right)$ is exactly linear in $t$, as expected in the case of quantum random walk with no noise. To find the variation of the quantities $\sigma^{2}$ and $\left(\frac{\sigma^{2}}{t}\right)$ as a function of time $t$, numerical simulations were done in MATLAB. Each simulation was averaged over 1,000 iterations, with each iteration having 1,000 simulation steps. \newline

In the simulations described in this work, uniform random noise is used to modify the Hadamard coin operator. The noise is modelled by a random number $r$ generated by the random number generating function ``\textbf{\textit{rand}}'' in MATLAB. This modified Hadamard operator sends each spin state into a superposition of spin-up and spin-down basis states such that total overall probability is 1 (so the coin operator is unitary). This modified Hadamard operator $\hat{w}$ is unitary with the property $\hat{w} = \hat{w}^{\dagger}$, such that the probability is conserved after each step. The unitarity is also verified in the simulations as well, the sum of probabilities being equal to $1$ after each step. \newline

In the work of Shapira \textit{et. al.}~\citep{shapira2003one}, the noise is modelled as an unitary operator of the form $e^{i\hat{a}(t)}$, where $\hat{a}(t) = \alpha_{1}(t)\sigma_{x} + \alpha_{2}(t)\sigma_{y} + \alpha_{3}(t)\sigma_{z}$, where $\sigma_{x}$, $\sigma_{y}$ and $\sigma_{z}$ are Pauli matrices. The noise is characterized by $\alpha$ which is the standard deviation of $\alpha_{1}, \alpha_{2}, \alpha_{3}$. The paper~\citep{shapira2003one} also specifies that the classical behavior overtakes the quantum behavior in case of strong noise \textit{i.e.} $\alpha \geq 0.07$. There are two ways to induce noise/decoherence in the quantum walk: decoherence in the position space or noise in the coin (chiral/spin)space (also called 'dephasing the coin'). In the model described above, the coin is `dephased' while retaining the position space noise-free. There have been several other works which deal with the problem of quantum random walks with dephased coins~\citep{PhysRevA.67.032304} and the quantum to classical transition in quantum random walks~\citep{PhysRevLett.91.130602}.\newline

The decoherence is modelled by a single parameter $r$ which modifies the Hadamard operator as seen from the equations~\eqref{eq:noisy-coeff} in Section~\ref{sec:qrw-evolution}, while maintaining unitarity. In Section~\ref{sec: noisy hadamard walk}, the generalized coin operator was represented by $\hat{w}(r)$. It reduces to the standard Hadamard operator $\hat{w_{0}}$ when $r = \frac{1}{\sqrt{2}}$ (represented by $a_{0}$ in this discussion).  The simulations were done with different `coin' operators by varying the parameter $r$ of the generalized Hadamard coin operator. The plots described in Figures~\ref{fig: Ideal quantum walk ratio equalsup},~\ref{fig: Ideal quantum walk ratio spinup}, and~\ref{fig: Ideal quantum walk ratio spindown} correspond to the ideal case when $r = a_{0} = \frac{1}{\sqrt{2}}$. The parameter $r$ is varied as a uniform random number, and at each time step, $r$ is allowed to lie in a certain range of values. Starting with $r = a_{0}$ for the ideal case, the range of $r$ is increased in steps, $a_{0} - 0.01 \le r \le a_{0} + 0.01$, $a_{0} - 0.02 \le r \le a_{0} + 0.02$, $a_{0} - 0.05 \le r \le a_{0} + 0.05$, $a_{0} - 0.07 \le r \le a_{0} + 0.07$, and so on, expanding the range to $0 \le r \le 1$ which corresponds to maximum decoherence. \newline

This chapter shows the plots of $\frac{\sigma^{2}}{t}$ vs $t$ and the corresponding spatial probability distribution $P(n)$ of the walker vs the spatial variable $n$.  For each value of the parameter $r$, three initial conditions were considered; the walker performing quantum walk starting in (a) equal superposition of up($\uparrow$) and down($\downarrow$) spins, (b) spin up ($\uparrow$), and (c) spin down ($\downarrow$).\newline

One important observation is the seperation of the ratios $\frac{\sigma^{2}}{t}$ for different spin injection conditions. Clearly, a walk starting in an initial spin-up state would have $\frac{\sigma_{\uparrow}^{2}}{t}$ significantly greater than $\frac{\sigma_{\downarrow}^{2}}{t}$. Similarly, a walk starting in spin-down state initially would have $\frac{\sigma_{\downarrow}^{2}}{t}$ significantly greater than $\frac{\sigma_{\uparrow}^{2}}{t}$. Thus, the concept of a ``diffusion coefficient'' itself depends on the initial state of the system. \newline

\begin{figure}
\centering
%\begin{subfigure}{\textwidth}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise01\string_2"} \label{fig:noise01_2 equalsup} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in an equal superposition of spin-up and spin-down}
%\end{subfigure}
%\begin{subfigure}{\textwidth}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise01\string_3"} \label{fig:noise01_2 equalsup P(n)} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in an equal superposition of spin-up and spin-down}
%\end{subfigure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise01\string_2"} \label{fig:noise01_2 spinup} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in spin-up configuration}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise01\string_3"} \label{fig:noise01_2 spinup P(n)} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in an equal superposition of spin-up and spin-down}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise01\string_2"} \label{fig:noise01_2 spindown} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in spin-down configuration}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise01\string_3"} \label{fig:noise01_2 spindown P(n)} }
%\caption{Plot of $\left(\dfrac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in an equal superposition of spin-up and spin-down}
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise01} 
\end{figure}

%%%%%% To be used only with 'subfig' package. But 'subcaption' package is superior.
%\begin{figure}
%\centering
%\subfloat[1][]{\includegraphics[width=0.4\textwidth]{"plots\string_eqsup/noise01\string_2"}} \label{fig:noise01_2 equalsup}
%\subfloat[][]{\includegraphics[width=0.4\textwidth]{"plots\string_eqsup/noise01\string_3"}} \label{fig:noise01_3 equalsup P(n)}
%\subfloat[2][]{\includegraphics[width=0.4\textwidth]{"plots\string_spinup/noise01\string_2"}} \label{fig:noise01_2 spinup}
%\subfloat[][]{\includegraphics[width=0.4\textwidth]{"plots\string_spinup/noise01\string_3"}} \label{fig:noise01_3 spinup P(n)}
%\subfloat[3][]{\includegraphics[width=0.4\textwidth]{"plots\string_spindown/noise01\string_2"}} \label{fig:noise01_2 spindown}
%\subfloat[][]{\includegraphics[width=0.4\textwidth]{"plots\string_spindown/noise01\string_3"}} \label{fig:noise01_3 spindown P(n)}
%\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk with the Hadamard parameter $r$ in the range $a_{0}-0.01 \le r \le a_{0}+0.01$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up configuration, and (e)spin-down configuration. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig: noise01}
%\end{figure}

\FloatBarrier

As it can be seen in Figure~\ref{fig:noise01}, adding a very small amount of noise doesn't alter the nature of quantum walk significantly. The following pages show further simulations for various allowed ranges of the parameter $r$. In the work of Shapira~\cite{shapira2003one}, a significant change in the probability distribution happens when $\alpha = 0.07$. Hence the curves are plotted for low noise in small increments, and once the quantum to classical tranistion is apparent, the increments can be made large. \newline

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise02\string_2"} \label{fig:noise02_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise02\string_3"} \label{fig:noise02_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise02\string_2"} \label{fig:noise02_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise02\string_3"} \label{fig:noise02_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise02\string_2"} \label{fig:noise02_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise02\string_3"} \label{fig:noise02_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.02 \le r \le a_{0}+0.02$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise02} 
\end{figure}

\FloatBarrier


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise05\string_2"} \label{fig:noise05_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise05\string_3"} \label{fig:noise05_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise05\string_2"} \label{fig:noise05_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise05\string_3"} \label{fig:noise05_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise05\string_2"} \label{fig:noise05_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise05\string_3"} \label{fig:noise05_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.05 \le r \le a_{0}+0.05$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise05} 
\end{figure}

\FloatBarrier


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise07\string_2"} \label{fig:noise07_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise07\string_3"} \label{fig:noise07_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise07\string_2"} \label{fig:noise07_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise07\string_3"} \label{fig:noise07_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise07\string_2"} \label{fig:noise07_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise07\string_3"} \label{fig:noise07_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.07 \le r \le a_{0}+0.07$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise07} 
\end{figure}

\FloatBarrier

It is clearly observed that classical behavior becomes significant when allowed deviation is $\pm 0.07$ from the value of $a_{0}$, \textit{i.e.} the value of $r$ lies in the range $a_{0} - 0.07 \le r \le a_{0} + 0.07$. It can be conjectured that the classical nature becomes stronger as the range of $r$ is allowed to increase. The next few pages show the transition of the walk from quantum to classical regime. \newline


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise1\string_2"} \label{fig:noise1_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise1\string_3"} \label{fig:noise1_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise1\string_2"} \label{fig:noise1_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise1\string_3"} \label{fig:noise1_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise1\string_2"} \label{fig:noise1_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise1\string_3"} \label{fig:noise1_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.1 \le r \le a_{0}+0.1$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise1} 
\end{figure}

\FloatBarrier

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise125\string_2"} \label{fig:noise125_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise125\string_3"} \label{fig:noise125_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise125\string_2"} \label{fig:noise125_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise125\string_3"} \label{fig:noise125_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise125\string_2"} \label{fig:noise125_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise125\string_3"} \label{fig:noise125_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.125 \le r \le a_{0}+0.125$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise125} 
\end{figure}
 
 
\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise15\string_2"} \label{fig:noise15_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise15\string_3"} \label{fig:noise15_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise15\string_2"} \label{fig:noise15_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise15\string_3"} \label{fig:noise15_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise15\string_2"} \label{fig:noise15_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise15\string_3"} \label{fig:noise15_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.15 \le r \le a_{0}+0.15$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise15} 
\end{figure}

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise2\string_2"} \label{fig:noise2_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise2\string_3"} \label{fig:noise2_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise2\string_2"} \label{fig:noise2_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise2\string_3"} \label{fig:noise2_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise2\string_2"} \label{fig:noise2_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise2\string_3"} \label{fig:noise2_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.2 \le r \le a_{0}+0.2$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise2} 
\end{figure}

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise25\string_2"} \label{fig:noise25_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noise25\string_3"} \label{fig:noise25_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise25\string_2"} \label{fig:noise25_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noise25\string_3"} \label{fig:noise25_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise25\string_2"} \label{fig:noise25_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noise25\string_3"} \label{fig:noise25_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.25 \le r \le a_{0}+0.25$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noise25} 
\end{figure}


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noiseall\string_2"} \label{fig:noiseall_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_eqsup/noiseall\string_3"} \label{fig:noiseall_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noiseall\string_2"} \label{fig:noiseall_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spinup/noiseall\string_3"} \label{fig:noiseall_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noiseall\string_2"} \label{fig:noiseall_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"plots\string_spindown/noiseall\string_3"} \label{fig:noiseall_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $0 \le r \le 1$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:noiseall} 
\end{figure}

\FloatBarrier


\section{Existence of Power law}
For the ideal quantum random walk discussed in Section~\ref{sec:qrw-evolution} and verified by numerical simulations in Figures~\ref{fig: Ideal quantum walk ratio equalsup},~\ref{fig: Ideal quantum walk ratio spinup}, and~\ref{fig: Ideal quantum walk ratio spindown}, $\left(\sigma_{\uparrow}^{2}/t\right) \propto t$ or $t^{1}$ and $\left(\sigma_{\downarrow}^{2}/t\right) \propto t$; and in general, $\left(\sigma_{\uparrow}^{2}/t\right) \neq \left(\sigma_{\downarrow}^{2}/t\right) $ except in the case when the walker starts in an equal superposition of spin basis states $\left((\mid\uparrow\rangle + \mid\downarrow)\rangle /\sqrt{2} \right)$ such as the case highlighted in Figure~\ref{fig: Ideal quantum walk ratio equalsup}. In the limit of large noise (classical limit), the expected behavior is  $\left(\sigma_{\uparrow}^{2}/t\right) \propto t^{0}$ or constant and similarly, $\left(\sigma_{\uparrow}^{2}/t\right) \propto t^{0}$. This naturally brings forth the question if there exists a simple relation between the quantities $\left(\sigma_{\uparrow}^{2}/t\right)$ vs $t$ and $\left(\sigma_{\downarrow}^{2}/t\right)$ vs $t$ which can be characterized by a power law of the form
\begin{equation} \label{eq: power law}
y = at^{\gamma}
\end{equation}

where $y$  is one of the quantities $\left(\sigma_{\uparrow}^{2}/t\right)$ and $\left(\sigma_{\downarrow}^{2}/t\right)$; $t$ is the time (discrete) and $\gamma$ is an exponent whose value is $1$ for the ideal quantum walk and $0$ for the classical random walk.


\subsection{Curve Fitting} \label{sec: fitting}
To check if a power law of the form shown in Equation~\ref{eq: power law} exists, a sample from the simulations shown in Section~\ref{sec: main simulations} were considered , and attempted to fit the $\left(\sigma_{s}^{2}/t\right)$ vs $t$ (where $s = \uparrow$ or $\downarrow$) curves against the power law equation $ y = at^{\gamma}$. The following Figures show the fitted curve plotted aganist the results obtained by numerical simulations. For each fit, the values of coefficients $a$, $\gamma$, and the goodness of the fit (measured by the value of $R^{2}$ are given.  


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise0up\string_fit"} \label{fig:noise0_up equalsup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise0down\string_fit"} \label{fig:noise0_down equalsup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r = a_{0}$ with the walker starting in equal superposition of spin-up and spin-down. The coefficients and $R^{2}$ values are: (a) $a = 0.1226$, $\gamma = 1.003$ and $R^{2} = 1$  (b) $a = 0.1226$, $\gamma = 1.003$ and $R^{2} = 1$ } \label{fig:noise0 eqsup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise0up\string_fit"} \label{fig:noise0_up spinup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise0down\string_fit"} \label{fig:noise0_down spinup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r = a_{0}$ with the walker starting in spin-up state. The coefficients and $R^{2}$ values are: (a) $a = 0.1183$, $\gamma = 1.001$ and $R^{2} = 1$  (b) $a = 0.08556$, $\gamma = 1.004$ and $R^{2} = 1$ } \label{fig:noise0 spinup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise0up\string_fit"} \label{fig:noise0_up spindown fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise0down\string_fit"} \label{fig:noise0_down spindown fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r = a_{0}$ with the walker starting in spin-down state. The coefficients and $R^{2}$ values are: (a) $a = 0.08567$, $\gamma = 1.004$ and $R^{2} = 1$  (b) $a = 0.1117$, $\gamma = 1.008$ and $R^{2} = 1$ } \label{fig:noise0 spindown fit}
\end{figure}

%-------------------------------------------------------------------------------------

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise07up\string_fit"} \label{fig:noise07_up equalsup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise07down\string_fit"} \label{fig:noise07_down equalsup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.07 \leq r \leq a_{0}+0.07 $ with the walker starting in equal superposition of spin-up and spin-down. The coefficients and $R^{2}$ values are: (a) $a = 1.004$, $\gamma = 0.5668$ and $R^{2} = 0.9845$  (b) $a = 0.9672$, $\gamma = 0.5722$ and $R^{2} = 0.9839$ } \label{fig:noise07 eqsup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise07up\string_fit"} \label{fig:noise07_up spinup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise07down\string_fit"} \label{fig:noise07_down spinup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.07 \leq r \leq a_{0}+0.07$ with the walker starting in spin-up state. The coefficients and $R^{2}$ values are: (a) $a = 1.073$, $\gamma = 0.5619$ and $R^{2} = 0.9811$  (b) $a = 0.5168$, $\gamma = 0.6526$ and $R^{2} = 0.9894$ } \label{fig:noise07 spinup fit}
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise07up\string_fit"} \label{fig:noise07_up spindown fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise07down\string_fit"} \label{fig:noise07_down spindown fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.07 \leq r \leq a_{0}+0.07$ with the walker starting in spin-down state. The coefficients and $R^{2}$ values are: (a) $a = 0.5186$, $\gamma = 0.6518$ and $R^{2} = 0.9896$  (b) $a = 1.019$, $\gamma = 0.5692$ and $R^{2} = 0.9799$ } \label{fig:noise07 spindown fit}
\end{figure}

%----------------------------------------------------------------------------------

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise125up\string_fit"} \label{fig:noise125_up equalsup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise125down\string_fit"} \label{fig:noise125_down equalsup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.125 \leq r \leq a_{0}+0.125 $ with the walker starting in equal superposition of spin-up and spin-down. The coefficients and $R^{2}$ values are: (a) $a = 2.015$, $\gamma = 0.3337$ and $R^{2} = 0.9408$  (b) $a = 1.933$, $\gamma = 0.3397$ and $R^{2} = 0.9385$ } \label{fig:noise125 eqsup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise125up\string_fit"} \label{fig:noise125_up spinup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise125down\string_fit"} \label{fig:noise125_down spinup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.125 \leq r \leq a_{0}+0.125$ with the walker starting in spin-up state. The coefficients and $R^{2}$ values are: (a) $a = 2.232$, $\gamma = 0.3198$ and $R^{2} = 0.9329$  (b) $a = 1.396$, $\gamma = 0.3843$ and $R^{2} = 0.9843$ } \label{fig:noise125 spinup fit}
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise125up\string_fit"} \label{fig:noise125_up spindown fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise125down\string_fit"} \label{fig:noise125_down spindown fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.125 \leq r \leq a_{0}+0.125$ with the walker starting in spin-down state. The coefficients and $R^{2}$ values are: (a) $a = 0.1.409$, $\gamma = 0.3829$ and $R^{2} = 0.9483$  (b) $a = 2.086$, $\gamma = 0.3296$ and $R^{2} = 0.9301$ } \label{fig:noise125 spindown fit}
\end{figure}

%-------------------------------------------------------------------------------------------

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise2up\string_fit"} \label{fig:noise2_up equalsup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noise2down\string_fit"} \label{fig:noise2_down equalsup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.2 \leq r \leq a_{0}+0.2 $ with the walker starting in equal superposition of spin-up and spin-down. The coefficients and $R^{2}$ values are: (a) $a = 1.986$, $\gamma = 0.2065$ and $R^{2} = 0.8842$  (b) $a = 1.887$, $\gamma = 0.2141$ and $R^{2} = 0.8777$ } \label{fig:noise2 eqsup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise2up\string_fit"} \label{fig:noise2_up spinup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noise2down\string_fit"} \label{fig:noise2_down spinup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.2 \leq r \leq a_{0}+0.2$ with the walker starting in spin-up state. The coefficients and $R^{2}$ values are: (a) $a = 2.169$, $\gamma = 0.1935$ and $R^{2} = 0.8712$  (b) $a = 1.641$, $\gamma = 0.2337$ and $R^{2} = 0.8843$ } \label{fig:noise2 spinup fit}
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise2up\string_fit"} \label{fig:noise2_up spindown fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noise2down\string_fit"} \label{fig:noise2_down spindown fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0}-0.2 \leq r \leq a_{0}+0.2$ with the walker starting in spin-down state. The coefficients and $R^{2}$ values are: (a) $a = 1.634$, $\gamma = 0.2345$ and $R^{2} = 0.8951$  (b) $a = 1.96$, $\gamma = 0.2089$ and $R^{2} = 0.8739$ } \label{fig:noise2 spindown fit}
\end{figure}

%---------------------------------------------------------------------------------------

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noiseallup\string_fit"} \label{fig:noiseall_up equalsup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_eqsup/noisealldown\string_fit"} \label{fig:noiseall_down equalsup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $0 \leq r \leq 1 $ with the walker starting in equal superposition of spin-up and spin-down. The coefficients and $R^{2}$ values are: (a) $a = 0.523$, $\gamma = 0.09166$ and $R^{2} = 0.8367$  (b) $a = 0.4655$, $\gamma = 0.1096$ and $R^{2} = 0.788$ } \label{fig:noiseall eqsup fit} 
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noiseallup\string_fit"} \label{fig:noiseall_up spinup fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spinup/noisealldown\string_fit"} \label{fig:noiseall_down spinup fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $0 \leq r \leq 1$ with the walker starting in spin-up state. The coefficients and $R^{2}$ values are: (a) $a = 0.5553$, $\gamma = 0.08116$ and $R^{2} = 0.7767$  (b) $a = 0.4846$, $\gamma = 0.1019$ and $R^{2} = 0.7475$ } \label{fig:noiseall spinup fit}
\end{figure}

\begin{figure}
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noiseallup\string_fit"} \label{fig:noiseall_up spindown fit} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"fits/epsfit\string_spindown/noisealldown\string_fit"} \label{fig:noiseall_down spindown fit} }
\caption{Fitted curves of (a)$\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ vs $t$ and (b)$\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$ vs $t$ for quantum random walk for 1000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $0 \leq r \leq 1$ with the walker starting in spin-down state. The coefficients and $R^{2}$ values are: (a) $a = 0.5236$, $\gamma = 0.090311$ and $R^{2} = 0.818$  (b) $a = 0.4805$, $\gamma = 0.1038$ and $R^{2} = 0.7475$ } \label{fig:noiseall spindown fit}
\end{figure}

%------------------------------------------------------------------------------------

\FloatBarrier
\section{Observations} \label{sec: observations}
\subsection{Sensitivity to initial state}
As the range of allowed values of $r$ (and hence the noise) increases, the quantum to classical transition of the random walk on the lattice is apparent. The spatial probability distribution of the walker $P(n)$ is predominantly Gaussian-like, and the spread of $P(n)$ decreases. Also, as the range of noise increases, the values of $\left(\sigma_{\uparrow}^{2}/t\right)$ and  $\left(\sigma_{\downarrow}^{2}/t\right)$ converge asymptotically regardless of the initial state of the walker. \newline

For the ideal quantum walk shown in Figures~\ref{fig: Ideal quantum walk ratio equalsup},~\ref{fig: Ideal quantum walk ratio spinup}, and~\ref{fig: Ideal quantum walk ratio spindown}, the ratios $\left(\sigma_{\uparrow}^{2}/t\right)$ and  $\left(\sigma_{\downarrow}^{2}/t\right)$ depend on the initial state (bias) of the walker and are generally quite different except when the walker starts in an equal superposition of the two states. It is also clear that $\left(\sigma_{\uparrow}^{2}/t\right)$ and $\left(\sigma_{\downarrow}^{2}/t\right)$ are linear in $t$, \textit{i.e.} $\left(\sigma^{2}/t\right) \propto t$ for both possible spin orientations ($\uparrow$ or $\downarrow$), which is characteristic of ballistic transport. \newline

The value of $\left(\sigma_{\uparrow}^{2}/t\right) > \left(\sigma_{\downarrow}^{2}/t\right)$ when the walker starts in an up($\uparrow$) spin state initially. Similarly,  $\left(\sigma_{\uparrow}^{2}/t\right) < \left(\sigma_{\downarrow}^{2}/t\right)$ when the walker starts in a down($\downarrow$) spin state initially. These trends continue as the noise increases, although the difference between  $\left(\sigma_{\uparrow}^{2}/t\right)$ and $\left(\sigma_{\downarrow}^{2}/t\right)$ sharply decreases, and both the values become nearly equal when the noise is maximum. \newline

\subsection{Quantum to classical transition}
The random walk with noisy Hadamard coin shows a gradual transition from purely quantum to (almost) classicalbehavior. In the work dealing with quantum walk with unitary noise~\citep{shapira2003one}, the classical behavior overtakes the quantum nature of the walk when the parameter characterizing the noise $\alpha$, has a standard deviation $\alpha = 0.07$. It can observed that the classical behavior becomes more prominent in the simulations in Section~\ref{sec: main simulations} when the noisy Hadamard parameter $r$ grows to $r = 0.07$ and beyond.\newline

The quantum walk with the modified Hadamard coin shown in Figures~\ref{fig:noise01} to \ref{fig:noiseall} shows a gradual transition from a fully quantum behavior to almost classical-like behavior. For very low deviations of $r$ from the ideal value $a_{0} ( = 1/\sqrt{2} = 0.7071)$, especially in the range $\lvert r - a_{0} \rvert < 0.07$ (\textit{i.e.} $0.7001 < r < 0.7141$), the walk mostly resembles the quantum walk, with the peaks on either ends bigger than the Gaussian envelope in the center. When the range of allowed values of $r$ is increased to $0 \leq r \leq 1$ as shown in Figures~\ref{fig:noise07} to \ref{fig:noiseall}, the random walk transitions from quantum walk-like to strongly classical-like, as seen from the probability distribution $P(n)$ of the walker. \newline 

\subsection{Seperation of $\left(\frac{\sigma_{\uparrow}^{2}}{t}\right)$ and  $\left(\frac{\sigma_{\downarrow}^{2}}{t}\right)$}
It is very evident that the values of $\left(\sigma_{\uparrow}^{2}/t\right)$ and  $\left(\sigma_{\downarrow}^{2}/t\right)$ are very different for the ideal quantum walk as shown in Figures~\ref{fig: Ideal quantum walk ratio equalsup},~\ref{fig: Ideal quantum walk ratio spinup}, and~\ref{fig: Ideal quantum walk ratio spindown}. The seperation of  $\left(\sigma_{\uparrow}^{2}/t\right)$ and  $\left(\sigma_{\downarrow}^{2}/t\right)$ becomes less distinct with increasing noise, as evidenced in plots from Figure~\ref{fig:noise01} to Figure~\ref{fig:noiseall}(maximum noise) when both the quantities are nearly equal as the random walk shows classical behavior. \newline

Asymptotically, the ratios $\left(\sigma_{\uparrow}^{2}/t\right)$ and $\left(\sigma_{\downarrow}^{2}/t\right)$ converge, and for both spin states ($\uparrow$ and $\downarrow$), it is clear that $\left(\sigma^{2}/t\right)$  approaches a constant value asymptotically, as observed in the Section~\ref{sec: fitting}. However, as observed by the change in the exponent $\gamma$, allowing the Hadamard paramter $r$ to take all possible allowed values ($0 \leq r \leq 1$) doesn't result in a perfectly classical behavior (within 1000 timesteps) which is described a power-law of the form described by Equation~\ref{eq: power law}. However, as some further simulations show in Chapter~\ref{chap: future work}, allowing the walker to perform the walk longer might result in making the walk more classical in the high noise scenario. \newline



\chapter{Future work} \label{chap: future work}

Also, although the walk doesn't become exactly classical, it is asymptotically classical with increasing time-steps, as seen in Figures~\ref{fig:2ksteps noise07},~\ref{fig:2ksteps noise20}, and~\ref{fig:2ksteps noiseall}. In this classical limit, the expected behavior is  $\left(\sigma_{\uparrow}^{2}/t\right) \propto t^{0}$ or constant and similarly, $\left(\sigma_{\downarrow}^{2}/t\right) \propto t^{0}$. \newline


\section{Classical limit}
In the work of Brun et. al.~\citep{PhysRevA.67.032304}, the authors observe that ``The usual classical solution is recovered in the limit where the coin decoheres completely every step''. The MATLAB code used for the simulations is characterized by a parameter $r$, a random number generated at every instant of time and before taking each step. It can be observed that the random walk behaves in a classical manner when the allowed range of $r$ becomes maximum, \textit{i.e.} $0 \leq r \leq 1$. However, the ratios $\left(\sigma_{\uparrow}^{2}/t\right)$ and $\left(\sigma_{\downarrow}^{2}/t\right)$ tends to a constant in the asymptotic limit; however, it is not exactly a constant for 1,000 steps. Hence the a sampling of the simulations were repeated with 2,000 steps averaged over a 1,000 iterations and the plots are presented in Figures~\ref{fig:2ksteps noise07},~\ref{fig:2ksteps noise20}, and~\ref{fig:2ksteps noiseall}. 
 

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noise07\string_2"} \label{fig:2ksteps noise07_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noise07\string_3"} \label{fig:2ksteps noise07_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noise07\string_2"} \label{fig:2ksteps noise07_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noise07\string_3"} \label{fig:2ksteps noise07_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noise07\string_2"} \label{fig:2ksteps noise07_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noise07\string_3"} \label{fig:2ksteps noise07_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 2000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0} - 0.07 \le r \le a-{0} + 0.07$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:2ksteps noise07} 
\end{figure}


\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noise20\string_2"} \label{fig:2ksteps noise20_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noise20\string_3"} \label{fig:2ksteps noise20_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noise20\string_2"} \label{fig:2ksteps noise20_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noise20\string_3"} \label{fig:2ksteps noise20_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noise20\string_2"} \label{fig:2ksteps noise20_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noise20\string_3"} \label{fig:2ksteps noise20_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 2000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $a_{0} - 0.20 \le r \le a-{0} + 0.20$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:2ksteps noise20} 
\end{figure}

\begin{figure}
\centering
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noiseall\string_2"} \label{fig:2ksteps noiseall_2 equalsup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_eqsup/noiseall\string_3"} \label{fig:2ksteps noiseall_3 equalsup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noiseall\string_2"} \label{fig:2ksteps noiseall_2 spinup} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spinup/noiseall\string_3"} \label{fig:2ksteps noiseall_3 spinup P(n)} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noiseall\string_2"} \label{fig:2ksteps noiseall_2 spindown} }
\subcaptionbox{} {\includegraphics[width=0.45\textwidth]{"2ksteps/2k\string_spindown/noiseall\string_3"} \label{fig:2ksteps noiseall_3 spindown P(n)} }
\caption{Plot of $\left(\frac{\sigma^{2}}{t}\right)$ vs $t$ for quantum random walk for 2000 steps averaged over 1000 iterations with the Hadamard parameter $r$ in the range $0 \le r \le 1$, with the walker starting in (a)equal superposition of spin-up and spin-down, (c)spin-up state, and (e)spin-down state. The plots (b), (d), and (f) show the probability distribution of the walker for the corresponding initial conditions.} \label{fig:2ksteps noiseall} 
\end{figure}

\FloatBarrier



It can be observed that the walk becomes flatter (more classical-like) with increasing noise when the walker is allowed to take a large number of steps. Other properties of the quantum walk as observed in Section~\ref{sec: observations} hold good for the walk with higher number of timesteps. The problem of deciding when a random walk can be seen ``\textit{classical enough}'' is not well-defined and open to definition. Also, the simulated values of the ratio $\left(\sigma^{2}/t\right)$ don't fit the equation $y = a.t^{\gamma}$ perfectly. There might be other possible forms of power-laws, for example $y = a.t^{\gamma} + b$, which mitght fit the simulations better, the exploration of which is yet to be done.


\newpage

\begin{appendices}

\chapter{MATLAB Code}
This appendix contains the core MATLAB code used to generate the plots shown in Chapter~\ref{chap: simulations}. The ``basic recurrence relations'' pointed out in the code refer to Equations~\eqref{eq:noisy-coeff}. The curve-fitting was mostly done with the ``Curve Fitting Toolbox'' (cftool) in MATLAB. The code for curve fitting and rendering the plots into `pdf' format is not included here.

\begin{verbatim}

 
a0 = 1/sqrt(2);
j = 2000;
iter = 1000;

%--- astar and bstar are the lower and upper limits of Hadamard parameter r ---
astar = 0;  %a0-0.07; 
bstar = 1;  %a0+0.07;
%--------------------------------------------------------------------------------

theta1 = asind(astar);
theta2 = asind(bstar);
    
pup = zeros(j,1,'double');
pdown = zeros(j,1,'double');
pupsq = zeros(j,1,'double');
pdownsq = zeros(j,1, 'double'); 
p = zeros((2*j)+1,iter,'double');
check = zeros(j,1,'double'); 
ratio_up = zeros(j,1, 'double');
ratio_down = zeros(j,1,'double');
    
sum_prob = 0;
    
for k = 1:1:iter
    aup = zeros((2*j)+1, j, 'double');
    adown = zeros((2*j)+1, j, 'double');
        
    %--------inital conditions (corresponding to equal superpositions)---------
    aup(j+1, 1) =  1/sqrt(2);
    adown(j+1, 1) = 1i/sqrt(2);
    %--------------------------------------------------------------------------
    
    for t = 1:1:j-1  %timesteps
        r = astar + ((bstar - astar) * rand(1,1));
        
        for x = 1:1:(2*j)  % steps in space, -j to j i.e. 2j+1 steps
            
            if x == 1     
%-------------Startting point, doesn't depend on lower values of x--------------
                
            else
                %basic recurrence relations--------------------------------------
                
                aup(x,t+1) = double((aup(x-1,t) * r) + (sqrt(1 - (r^2)) * adown(x-1,t)));
                adown(x,t+1) = double((aup(x+1,t) * (sqrt(1 - (r^2)))) - (r * adown(x+1,t)) );
            end
        end
    end

    %matlab starts indexing from 1(no negative x). So for us, the origin is at 'j' (=500); we vary x
    %from 1 to 2*j+1 instead of -j to +j and hence we multiply with (x-j)
    %rather than x.
    
    for t = 1:1:j
        for x = 1:1:(2*j)
            if t == 1
                
            else
                pup(t) =  pup(t) + ((x-j).*(abs(aup(x,t)) ^2));
                pupsq(t) =  pupsq(t) + (((x-j)^2).*(abs(aup(x,t)) ^2));
                pdown(t) =  pdown(t) + ((x-j) .* (abs(adown(x,t)) ^2));
                pdownsq(t) =  pdownsq(t) + (((x-j)^2) .* (abs(adown(x,t)) ^2));
            end
        end
    end

%----- This loop is just to check the sum of probabilities "p" remains 1 ----------------------        
    for t = 1:1:j
        for x = 1:1:2*j
            if x == 1
                
            else
                p(x,k) =  ((abs(aup(x,t)) ^2) + (abs(adown(x,t)) ^2));
            end
        end
    end
    
    
end
    
sigmasq_up = abs((pupsq./iter) - (((pup./iter)).^2));
sigmasq_down = abs((pdownsq./iter) - (((pdown./iter)).^2));

for t = 1:j
    ratio_up(t) =  sigmasq_up(t) ./t;
    ratio_down(t) =  sigmasq_down(t) ./t;
end

%------------------Code for the plots--------------------------
    
figure
plot(1:j, sigmasq_up ,'g', 1:j, sigmasq_down, 'r' );
legend('spin-up', 'spin-down' )
title(strcat('\sigma^2 vs t for a_{1} = ', num2str(astar), ' (\theta_{1}=', num2str(theta1), ') and a_{2} = ', num2str(bstar), '(\theta_{2}=', num2str(theta2) , ')' ) )
ylabel('\sigma^2')
xlabel('t')

figure
plot(1:j, ratio_up ,'g', 1:j, ratio_down, 'r' );
legend('spin-up', 'spin-down' )
ylabel('\sigma^2 / t')
xlabel('t')

figure
plot( -j:j, sum(p.')./iter );
title(strcat('P(n) vs n for n = ',' ', num2str(j) ) )
ylabel('P(n)')
xlabel('n')


\end{verbatim}

\end{appendices}







\bibliography{references}


\end{document}


