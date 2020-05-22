## Values and Patterns
A _literal_ is a base value which is not made of component values. These include: Number (`123`), String (`"hello"`), Nothing (`nothing`), Bool (`true`), and Atom (`:to_str`).

A _value_ $v$ is defined as a literal or a list of values:
$$
\begin{array}{lcl}
v &::=& \langle literal \rangle \\
  &\mid& [v_1, v_2, ..., v_n]
\end{array}
$$

A _pattern_ $p$ is defined as:
$$
\begin{array}{lcl}
p &::=& \langle literal \rangle\\
  &\mid&  \_  & \textrm{(Ignore pattern)}\\
  &\mid&  x   & \textrm{(Variable pattern)}\\
  &\mid& [p1, p2, ..., pn] & \textrm{(List of patterns)}
\end{array}
$$

A _pattern vector_ is a sequence of patterns, denoted $p = (p_1 p_2 ... p_n)$.

## Pattern Matching
Let $p$ be a pattern and $v$ be a value. $v$ _is an instance of_ $p$, or $p$ _matches_ $v$, written $p \leq v$, according to the following rules:
$$
\begin{align}
\_ &\leq v\\
x &\leq v\\
[p_1, ..., p_k] &\leq [v_1, ..., v_k] &\textrm{iff } (p_1 ... p_k) \leq (v_1 ... v_k)\\
(p_1 ... p_k) &\leq (v_1 ... v_k) &\textrm{iff } p_i \leq v_i \forall i \in \{1, ..., k\}\\
p &\leq v &\textrm{iff $p$ and $v$ are both literals and $p = v$}
\end{align}
$$

## Matchboxes
A _matchbox_ $M_{P,L}$ is a matrix of pattern vectors $P = (p^1 ... p^m)^\top$ that map to a vector of lambda expressions $L = (l^1 ... l^m)^\top$:
$$
M_{P,L} = \begin{pmatrix}
  p^1_1 & p^1_2 & \ldots & p^1_n & \rightarrow & l^1\\
  p^2_1 & p^2_2 & \ldots & p^2_n & \rightarrow & l^2\\
  \vdots & \vdots & \ddots & \vdots & \vdots & \vdots\\
  p^m_1 & p^m_2 & \ldots & p^m_n & \rightarrow & l^m
\end{pmatrix}
$$

### Matching with a Matchbox
Let $M_{P,L}$ be a matchbox and $v = (v_1 ... v_n)$ be a value vector. $v$ _matches_ row $i$ in $P$ iff it is the first row that matches $v$:
$$
\begin{align}
(p^i_1 ... p^i_n) &\leq (v_1 ... v_n)\\
\forall i < j, (p^j_1, ... p^j_n) &\not\leq (v_1 ... v_n)
\end{align}
$$

If $v$ is not matched by any row in $P$, we say that the pattern matching on $P$ _fails_. If no such $v$ exists, then we say $P$ is _exhaustive_.

## Compiling
A pattern matching compiler $C$ takes a vector of variables $x = (x_1 ... x_n)$ and a matchbox $M_{P,L}$ of width $n$ and height $m$.

 1. If $n$ is zero, then output the first element of $L$:
    $$
    C((), \begin{pmatrix}
      & \rightarrow & l^1\\
      & \rightarrow & l^2\\
      & \vdots & \\
      & \rightarrow & l^m
    \end{pmatrix}) = l^1
    $$
 2. If $n$ is non-zero, then follow these rules:

    a. _The Variable Rule_: If all the patterns in the first column of $P$ are variables, $y^1$, $y^2$, ..., $y^m$, then:
    $$C(x, M_{P,L}) = C((x_2 ... x_n), M^{'}_{P,L})$$
    where
    $$
    M^{'}_{P,L} = \begin{pmatrix}
      p^1_2 & ... & p^1_n & \rightarrow & \texttt{let}\ (y^1 := x_1)\ l^1\\
      p^2_2 & ... & p^2_n & \rightarrow & \texttt{let}\ (y^2 := x_1)\ l^2\\
      & & & \vdots & \\
      p^m_2 & ... & p^m_n & \rightarrow & \texttt{let}\ (y^m := x_1)\ l^m
    \end{pmatrix}.
    $$
    We bind the first variable of each row with the first element of the input vector, in their respective lambdas. In this case, the ignore pattern is treated the same as the variable pattern.

    b. If none of the previous rules apply, the matchbox $M_{P,L}$ is cut into two matchboxes
