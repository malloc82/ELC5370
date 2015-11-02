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


\lstdefinelanguage{clojure}%
{morekeywords={*,*1,*2,*3,*agent*,*allow-unresolved-vars*,*assert*,*clojure-version*,*command-line-args*,%
*compile-files*,*compile-path*,*e,*err*,*file*,*flush-on-newline*,*in*,*macro-meta*,%
*math-context*,*ns*,*out*,*print-dup*,*print-length*,*print-level*,*print-meta*,*print-readably*,%
*read-eval*,*source-path*,*use-context-classloader*,*warn-on-reflection*,+,-,->,->>,..,/,:else,%
<,<=,=,==,>,>=,@,accessor,aclone,add-classpath,add-watch,agent,agent-errors,aget,alength,alias,%
all-ns,alter,alter-meta!,alter-var-root,amap,ancestors,and,apply,areduce,array-map,aset,%
aset-boolean,aset-byte,aset-char,aset-double,aset-float,aset-int,aset-long,aset-short,assert,%
assoc,assoc!,assoc-in,associative?,atom,await,await-for,await1,bases,bean,bigdec,bigint,binding,%
bit-and,bit-and-not,bit-clear,bit-flip,bit-not,bit-or,bit-set,bit-shift-left,bit-shift-right,%
bit-test,bit-xor,boolean,boolean-array,booleans,bound-fn,bound-fn*,butlast,byte,byte-array,%
bytes,cast,char,char-array,char-escape-string,char-name-string,char?,chars,chunk,chunk-append,%
chunk-buffer,chunk-cons,chunk-first,chunk-next,chunk-rest,chunked-seq?,class,class?,%
clear-agent-errors,clojure-version,coll?,comment,commute,comp,comparator,compare,compare-and-set!,%
compile,complement,concat,cond,condp,conj,conj!,cons,constantly,construct-proxy,contains?,count,%
counted?,create-ns,create-struct,cycle,dec,decimal?,declare,def,definline,defmacro,defmethod,%
defmulti,defn,defn-,defonce,defprotocol,defstruct,deftype,delay,delay?,deliver,deref,derive,%
descendants,destructure,disj,disj!,dissoc,dissoc!,distinct,distinct?,do,do-template,doall,doc,%
dorun,doseq,dosync,dotimes,doto,double,double-array,doubles,drop,drop-last,drop-while,empty,empty?,%
ensure,enumeration-seq,eval,even?,every?,false,false?,ffirst,file-seq,filter,finally,find,find-doc,%
find-ns,find-var,first,float,float-array,float?,floats,flush,fn,fn?,fnext,for,force,format,future,%
future-call,future-cancel,future-cancelled?,future-done?,future?,gen-class,gen-interface,gensym,%
get,get-in,get-method,get-proxy-class,get-thread-bindings,get-validator,hash,hash-map,hash-set,%
identical?,identity,if,if-let,if-not,ifn?,import,in-ns,inc,init-proxy,instance?,int,int-array,%
integer?,interleave,intern,interpose,into,into-array,ints,io!,isa?,iterate,iterator-seq,juxt,%
key,keys,keyword,keyword?,last,lazy-cat,lazy-seq,let,letfn,line-seq,list,list*,list?,load,load-file,%
load-reader,load-string,loaded-libs,locking,long,long-array,longs,loop,macroexpand,macroexpand-1,%
make-array,make-hierarchy,map,map?,mapcat,max,max-key,memfn,memoize,merge,merge-with,meta,%
method-sig,methods,min,min-key,mod,monitor-enter,monitor-exit,name,namespace,neg?,new,newline,%
next,nfirst,nil,nil?,nnext,not,not-any?,not-empty,not-every?,not=,ns,ns-aliases,ns-imports,%
ns-interns,ns-map,ns-name,ns-publics,ns-refers,ns-resolve,ns-unalias,ns-unmap,nth,nthnext,num,%
number?,odd?,or,parents,partial,partition,pcalls,peek,persistent!,pmap,pop,pop!,pop-thread-bindings,%
pos?,pr,pr-str,prefer-method,prefers,primitives-classnames,print,print-ctor,print-doc,print-dup,%
print-method,print-namespace-doc,print-simple,print-special-doc,print-str,printf,println,println-str,%
prn,prn-str,promise,proxy,proxy-call-with-super,proxy-mappings,proxy-name,proxy-super,%
push-thread-bindings,pvalues,quot,rand,rand-int,range,ratio?,rational?,rationalize,re-find,%
re-groups,re-matcher,re-matches,re-pattern,re-seq,read,read-line,read-string,recur,reduce,ref,%
ref-history-count,ref-max-history,ref-min-history,ref-set,refer,refer-clojure,reify,%
release-pending-sends,rem,remove,remove-method,remove-ns,remove-watch,repeat,repeatedly,%
replace,replicate,require,reset!,reset-meta!,resolve,rest,resultset-seq,reverse,reversible?,%
rseq,rsubseq,second,select-keys,send,send-off,seq,seq?,seque,sequence,sequential?,set,set!,%
set-validator!,set?,short,short-array,shorts,shutdown-agents,slurp,some,sort,sort-by,sorted-map,%
sorted-map-by,sorted-set,sorted-set-by,sorted?,special-form-anchor,special-symbol?,split-at,%
split-with,str,stream?,string?,struct,struct-map,subs,subseq,subvec,supers,swap!,symbol,symbol?,%
sync,syntax-symbol-anchor,take,take-last,take-nth,take-while,test,the-ns,throw,time,to-array,%
to-array-2d,trampoline,transient,tree-seq,true,true?,try,type,unchecked-add,unchecked-dec,%
unchecked-divide,unchecked-inc,unchecked-multiply,unchecked-negate,unchecked-remainder,%
unchecked-subtract,underive,unquote,unquote-splicing,update-in,update-proxy,use,val,vals,%
var,var-get,var-set,var?,vary-meta,vec,vector,vector?,when,when-first,when-let,when-not,%
while,with-bindings,with-bindings*,with-in-str,with-loading-context,with-local-vars,%
with-meta,with-open,with-out-str,with-precision,xml-seq,zero?,zipmap
},%
   sensitive,% ???
   alsodigit=-,%
   morecomment=[l];,%
   morestring=[b]"%
  }[keywords,comments,strings]%

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
    keywordstyle=\color{blue},
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
    keywordstyle=\color{blue},
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

