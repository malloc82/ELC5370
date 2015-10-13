\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{assignment}[2014/01/28 Assignment Class]
% \LoadClass{scrreprt}
% \LoadClass{report}
% \LoadClass{scrartcl}
\LoadClass{article}

\usepackage
[
% a4paper,% other options: a3paper, a5paper, etc
% left=1cm,
% right=2cm,
% top=3cm,
% bottom=4cm,
hmargin=.75in,
vmargin=1in
% use vmargin=2cm to make vertical margins equal to 2cm.
% us  hmargin=3cm to make horizontal margins equal to 3cm.
% use margin=3cm to make all margins  equal to 3cm.
]{geometry}

\usepackage{titling}
\makeatletter

\usepackage{fancyhdr}
\pagestyle{fancy} % enable fancy page style
\renewcommand{\headrulewidth}{0.2pt} % comment if you want the rule
\fancyhf{} % clear header and footer
\fancyhead[L]{\@title} % section title on the left
\fancyhead[R]{\@author}   % page number
% \fancyhead[C]{Header}
\fancyfoot[C]{\thepage}

\usepackage{graphicx} % new way of doing eps files
\usepackage{listings} % nice code layout
\usepackage{amsmath}
\usepackage{placeins} % mainly for \FloatBarrier
\usepackage{mathtools} % for rcases*, dcases etc
\usepackage[normalem]{ulem} % for underlining text, e.g. \uline{...}, \uwave{}, \sout{}, \xout{}, etc.
\usepackage{marginnote} % for margin notes
\usepackage{amssymb}    % for \mathbb
\usepackage{mathrsfs}   % for fonts \mathcal{}, \mathscr{}, etc
\usepackage[makeroom]{cancel} % used for generate "cross out" effect
\usepackage{hyperref}
% \usepackage{hypcap}
\usepackage{enumerate} % enumerate with different letter style

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Custom Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand*\rfrac[2]{{}^{#1}\!/_{#2}}
\newcommand{\eqnote}[1]{\mbox{\hspace{12pt}#1\hspace{12pt}}}
% \newcommand{\eqtext}[1]{\mbox{\hspace{12pt}#1\hspace{12pt}}}
\newcommand{\eqtextbegin}[1]{\mbox{#1\hspace{12pt}}}
\newcommand{\eqtextend}[1]{\mbox{\hspace{12pt}#1\hspace{3pt}}}

\newcommand\numberthis{\addtocounter{equation}{1}\tag{\theequation}} % add equation number in align*

\newcommand{\floor}[1]{\left \lfloor #1 \right \rfloor}
\newcommand{\ceil}[1]{\left \lceil #1 \right \rceil}
\newcommand{\abs}[1]{\left| #1 \right|}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Shortcuts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\p}{\partial}
\newcommand{\df}{\dfrac}
\newcommand{\f}{\frac}
\newcommand{\ol}{\overline}
\newcommand{\OP}{\operatorname}
\newcommand\eqtext[1]{\stackrel{\mathllap{\normalfont\tiny\mbox{#1}}}{=}}

%% ==========================================
%% Indentation:
%% ==========================================

% Make the first line of all sections etc., be indented by the usual paragraph indentation.
% \usepackage{indentfirst}

% used to remove global indent at beginning of paragraph
% parskip will also add a small spacing between paragraphs
% \setlength{\parindent}{0pt}
\usepackage{parskip}

%% ==========================================
%% Color setting:
%% ==========================================

\usepackage[usenames]{color} % color
\definecolor{listinggray}{gray}{0.9}
\definecolor{graphgray}{gray}{0.7}
\definecolor{blue}{rgb}{0,0,1}
% \MatLab{title}{label}{file}

%% ==========================================
%% Source code listing:
%% ==========================================

\newcommand{\MatLab}[3]{
  \lstset{language=Matlab}
  \lstset{backgroundcolor=\color{listinggray},rulecolor=\color{blue}}
  \lstset{linewidth=\textwidth}
  \lstset{commentstyle=\textit, stringstyle=\upshape,showspaces=false}
  \lstset{frame=tb}
  \lstinputlisting[caption={#1},label={#2}]{#3}
}
% \SciLab{title}{label}{file}
\newcommand{\SciLab}[3]{
  \lstset{language=Scilab}
  \lstset{backgroundcolor=\color{listinggray},rulecolor=\color{blue}}
  \lstset{linewidth=\textwidth}
  \lstset{commentstyle=\textit, stringstyle=\upshape,showspaces=false}
  \lstset{frame=tb}
  \lstinputlisting[caption={#1},label={#2}]{#3}
}
% \Ccode{title}{label}{file}
\newcommand{\Ccode}[3]{
  \lstset{language=c}
  \lstset{backgroundcolor=\color{listinggray},rulecolor=\color{blue}}
  \lstset{linewidth=\textwidth}
  \lstset{commentstyle=\textit, stringstyle=\upshape,showspaces=false}
  \lstset{frame=tb}
  \lstinputlisting[caption={#1},label={#2}]{#3}
}
\newcommand{\Java}[3]{
  \lstset{language=Java}
  \lstset{backgroundcolor=\color{listinggray},rulecolor=\color{blue}}
  \lstset{linewidth=\textwidth}
  \lstset{commentstyle=\textit, stringstyle=\upshape,showspaces=false}
  \lstset{frame=tb}
  \lstinputlisting[caption={#1},label={#2}]{#3}
}

\definecolor{orange}{rgb}{1,0.5,0}%Lowercase breaks on \documentclass[jou]{apa}!

% source: http://stackoverflow.com/questions/741985/latex-source-code-listing-like-in-professional-books
% \SourceCode{language}{caption}{label}{file_path}
\newcommand{\SourceCode}[4]{
  % #1 = language, usually first letter is in upper case
  % #2 = caption
  % #3 = label
  % #4 = path
  \lstset{language=#1}
  \lstset{
    commentstyle=\color{orange}\textit,
    basicstyle=\footnotesize\ttfamily, % Standardschrift
    % numbers=left,               % Ort der Zeilennummern
    numberstyle=\tiny,          % Stil der Zeilennummern
    % stepnumber=2,               % Abstand zwischen den Zeilennummern
    numbersep=5pt,              % Abstand der Nummern zum Text
    tabsize=2,                  % Groesse von Tabs
    extendedchars=true,         %
    breaklines=true,            % Zeilen werden Umgebrochen
    keywordstyle=\color{red},
    frame=b,
    % keywordstyle=[1]\textbf,    % Stil der Keywords
    % keywordstyle=[2]\textbf,    %
    % keywordstyle=[3]\textbf,    %
    % keywordstyle=[4]\textbf,   \sqrt{\sqrt{}} %
    stringstyle=\color{blue}\ttfamily, % Farbe der String
    showspaces=false,           % Leerzeichen anzeigen ?
    showtabs=false,             % Tabs anzeigen ?
    xleftmargin=17pt,
    framexleftmargin=17pt,
    framexrightmargin=5pt,
    framexbottommargin=4pt,
    backgroundcolor=\color{listinggray},
    % backgroundcolor=\color{lightgray},
    showstringspaces=false      % Leerzeichen in Strings anzeigen ?
  }
  \lstinputlisting[caption={#2},label={#3}]{#4}
}

\newcommand{\CodeSnippet}[6]{
  % #1 = language, usually first letter is in upper case
  % #2 = caption
  % #3 = label
  % #4 = path
  \lstset{language=#1}
  \lstset{
    commentstyle=\color{orange}\textit,
    basicstyle=\footnotesize\ttfamily, % Standardschrift
    % numbers=left,               % Ort der Zeilennummern
    numberstyle=\tiny,          % Stil der Zeilennummern
    % stepnumber=2,               % Abstand zwischen den Zeilennummern
    numbersep=5pt,              % Abstand der Nummern zum Text
    tabsize=2,                  % Groesse von Tabs
    extendedchars=true,         %
    breaklines=true,            % Zeilen werden Umgebrochen
    keywordstyle=\color{red},
    frame=b,
    % keywordstyle=[1]\textbf,    % Stil der Keywords
    % keywordstyle=[2]\textbf,    %
    % keywordstyle=[3]\textbf,    %
    % keywordstyle=[4]\textbf,   \sqrt{\sqrt{}} %
    stringstyle=\color{blue}\ttfamily, % Farbe der String
    showspaces=false,           % Leerzeichen anzeigen ?
    showtabs=false,             % Tabs anzeigen ?
    xleftmargin=17pt,
    framexleftmargin=17pt,
    framexrightmargin=5pt,
    framexbottommargin=4pt,
    backgroundcolor=\color{listinggray},
    % backgroundcolor=\color{lightgray},
    showstringspaces=false      % Leerzeichen in Strings anzeigen ?
  }
  \lstinputlisting[caption={#2},label={#3}, firstline={#5}, lastline={#6}]{#4}
}

