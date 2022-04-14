# HaddockIssue1472

This project is just for investigation of tools that have problems with 
    processing the files in here, in particular Haddock.

It is not intended to be used for any other puposes and not to be distributed for any other reason. See the license file.

Background:
In Haddock Issue #1472 is has been reported that Haddock stops processing without any message. It actually hangs.
The problem seems to be related with UTF-8 encoding.
If you remove the character © in line 589 of file src/Code.sh then Haddock works as expected.

On windows / mingw systems type sh to execute sh.bat.

On unix-like systems execute the bash skript "sh".
