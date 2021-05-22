# locksynth

Generate code for concurrent data structure from their sequential knowledge and structural definitions


Requirements to build and run locksynth:

1) Working bash shell, for Windows 10 WSL works
2) Have SWI-Prolog in PATH : SWI-Prolog is available in package managers of Ubuntu and similar Linux Distros
   SWI-Prolog Download page : https://www.swi-prolog.org/download/stable
3) Have CLINGO ASP Solver in PATH
    CLINGO must be Python enabled. Best way to setup clingo with python support is using Anaconda package distribution
    CLINGO is installed using : conda install -c potassco clingo
    More information on CLINGO installation can be found here: https://potassco.org/clingo/
4) Run ./build.sh in the same folder (with appropriate permissions)  

Usage: 

1) Input sequential data structure knowledge should be provided in seq.pl
seq.pl contains the sequential data structure knowledge: see sample seq.pl in this repo
seq.pl contains code/7, operation/3, primitive_write_step/2, modifies/3,  causes/3, time_dependent/3 predicates that capture the sequential data structure
       information

Each of these predicates in seq.pl are indexed by the data structure name: list for Linked List, ext_tree for External BST in the sample seq.pl

2) Data structure theory should be provided in a separate .lp file
    For example list.lp contains the data structure theory for Linked Lists. 
                ext_tree.lp contains the theory for External BSTs
    Note: if the theory file name is DS.lp then the data structure knowledge in seq.pl should be indexed by DS
  
3) Finally, running locksynth is easy:
     
     ./locksynth Data_Structure
   
    For example, given sample list.lp and seq.pl 
   
     ./locksynth list 
     
     produces the concurrent versions of sequential code provided in seq.pl
