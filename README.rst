===================
 Subtitle Workshop
===================

The initial commit to this repository contains the entire source code of the latest official released version of **Subtitle Workshop**, 2.51, my subtitle editing tool for Microsoft Windows.
It also includes the source code of **SubtitleAPI**, a library for adding subtitle support that may be used for any program and powers Subtitle Workshop's support of a vast number of formats.


Compiling
=========
The source code is old (written between 2002 and 2004). It was originally made with **Delphi 7**, and may need some tweaking in order to compile it with the  latest versions of the tools/components.

If you want to get it to compile, these are (hopefully all) the additional components you may need:

* `RemObjects Pascal Script <http://www.remobjects.com/ps.aspx>`_
* `VirtualTreeView <http://www.delphi-gems.com/VirtualTreeview/>`_

The installer file was made for NSIS http://nsis.sourceforge.net/, and as it is old it will most probably not work at all with the latest NSIS version.


Disclaimer
==========

* I started writing this tool when I was 14 years old and did not have any formal training.
* I never used a Version Control System when I wrote this; in fact I didn't even know they existed.
* A paid programmer producing code like this for a living should probably be fired.
* Expect lack of comments, bad programming style, bad code organization, misuse of the Object-Oriented programming paradigm.
* Expect really complicated ways of doing really simple stuff (like subtitle parsing). I didn't know about regular expressions at the time. I was just really persistent and had the time to experiment with many approaches to handle any problem I came up with.
* Fortunately, I have improved with the years... ;-)

With all that said, I hope this is useful for someone :-)


License
=======
Subtitle Workshop and SubtitleAPI source code are both released under the GNU/GPL 3 license.