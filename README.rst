===================
 Subtitle Workshop
===================

The initial commit to this repository contains the entire source code of the latest official released version of **Subtitle Workshop**, 2.51, my subtitle editing tool for Microsoft Windows.
It also includes the source code of **SubtitleAPI**, a library for adding subtitle support that may be used for any program and powers Subtitle Workshop's support of a vast number of formats.

Description
===========

Subtitle Workshop is the most complete, efficient and convenient freeware subtitle editing tool. It supports all the subtitle formats you need and has all the features you would want from a subtitle editing program. Subtitle Workshop makes subtitle creating/editing/converting tasks almost a pleasure, the amicable and intuitive interface mixes easy to access menus & must have features with advanced functions and a remarkable speed and stability, drastically reducing subtitle editing time. It includes spell check function and an advanced video preview feature which will ease the task even more. The best choice for the beginner, expert or the fansubber. Have a try, and you'll forget the rest!

Complete list of features
=========================

* Reading and writing engine
    * Based in SubtitleAPI, so new formats can be easily added by downloading an updated dll file.
    * Currently (version 1.05 of SubtitleAPI) supports around 56 subtitle formats.
    * Can handle both time and frame based subtitles.
    * Can load files in plain text format so that you can set the timings.
    * Supports reading and writing style tags (bold, italic, underline) and color tags in format which support it.
    * Supports Output Settings in DVDSubtitle, SAMI, Sonic Scenarist, SubStation Alpha, SubViewer 1 & 2 and TMPlayer formats.
    * Supports saving a subtitle in your custom format.
    
* Interface
    * User friendly.
    * Configurable shortcuts.
    * Supports multi-language (currently over 35 languages are available!).
    * Can switch between interfaces to adapt to the user's need, you can turn Video Preview Mode on and off and Translator Mode on and off.
    * User friendly "Translator mode".

* Video preview
    * Integrated in the main window.
    * Shows subtitles over the video.
    * Can show information about the video you are watching.
    * You are able to customize foreground color, background color (or transparent), show border or not, show shadow or not, border and shadow's width of the subtitles.
    * NOT based in Windows Media Player, this results in a highly improved performance and visual quality.

* Subtitle fixing
    * Highly enhanced functions strictly designed to get speed and a high grade of efficiency.
    * Advanced & easy to customize text scripts for OCR errors repair, offering the possibility of using regular expressions.
    * Powerful and totally customizable error fixing engine.
    * Possibility to mark errors in the main list with a different font style and color so that it is easy to identify them visually.
    * Optional automated checks/fixes on load subtitle.
    * Checks and fixes (all are optional, you may configure it): empty subtitles, lines without letters, overlapping, bad values, hearing impaired subtitles, text before colon (":") (optional - "Only if text is in capital letters"), unnecessary dots, "-" in subtitles with one line, subtitles over two lines, prohibited characters (configurable characters), repeated characters (configurable characters), repeated subtitles (configurable time tolerance), OCR errors, spaces before custom characters, spaces after custom characters, unnecessary spaces, too long/short durations and too long lines (only check).

* General
    * Extremely customizable.
    * Multi-level Undo-Redo.
    * Spell check using Microsoft Word's engine, so it supports any installed language.
    * Can create new subtitles from scratch.
    * Easily add and delete subtitles with "Insert" and "Delete" keys.
    * Can split subtitle (at selected item, at given item, at given time, at given frame, or at the end of video), or in an indefinite number of parts (equal in time, in lines or at the end of multiple videos).
    * Can join an indefinite number of subtitles, and those subtitles may be in different formats with different FPS (you can select a different FPS for each file).
    * Two methods of getting FPS from video: without using DirectX (only supports AVI) and using DirectX (supports all video formats, but it is slower).
    * You are able to choose which subtitle extensions to associate.
    * Supports drag & drop (subtitle files and video files).
    * Supports command line.
    * Recent files list (customizable number of recent files from 0 to 20).

* Timing operations
    * Set delay (positive or negative, time or frames).
    * Set duration limits (maximum duration and minimum duration).
    * Adjust subtitles using four possible methods: 1- first and last dialogs, 2- synchronize using two points (linear algorithm), 3- Adjust to synchronized subtitles and 4- Advanced system to synchronize subtitles using an indefinite number of points.
    * "Time expander/reducer" to expand/reduce the final time of certain subtitles under certain conditions.
    * "Automatic durations" to calculate the duration of subtitles using a simple formula.
    * FPS Conversion with one click.
    * "Extend length" to extend the length of selected subtitles to the start time of the next one.
    * "Shift subtitle" forward or backwards a configurable amount of time.
    * "Read times from file" feature, to fix a subtitle using another subtitle's times.
    
* Text-related operations
    * Search & Replace text with "Match whole words", "Case sensitive" and "Preserve case" options.
    * "Smart line adjust" to constrain subtitles bigger than three lines into two and adjust length of lines.
    * "Convert case" with 6 different conversion modes.
    * "Unbreak subtitles" to make all selected subtitles be in one line.
    * "Divide lines" to easily divide a subtitle with more than one line (or one big line) into two subtitles with proper time recalculation.
    * "Reverse text" keeping lines order or not.
    * "Fix punctuation", very useful for right-to-left languages.
    * "Delete unnecessary links" to delete the unnecessary "..." if they are present the end of one line and at the beginning of the next one.
    * "Read texts from file" feature, to fix a subtitle using another subtitle's text.

* Text and times related
    * "Sort subtitles" to sort all the subtitles according to their start time.
    * "Combine subtitles" to make all the selected subtitles become only one (for eg. "- Hi!" and "- Hey!!!" to "- Hi!|- Hey!!!").
    * Visual effects with the texts and times of the subtitles.
    * Possibility to mark subtitles for later review (and of course unmark them) and read Subtitle Report Files (*.srf) generated by ViPlay.

Current list of supported formats:

* Adobe Encore DVD (*.txt)
* Advanced SubStation Alpha (*.ass)
* AQTitle (*.aqt)
* Captions 32 (*.txt)
* Captions DAT (*.dat)
* Captions DAT Text (*.dat)
* Captions Inc. (*.txt)
* Cheetah (*.asc)
* CPC-600 (*.txt)
* DKS Subtitle Format (*.dks)
* DVD Junior (*.txt)
* DVD Subtitle System (*.txt)
* DVDSubtitle (*.sub)
* FAB Subtitler (*.txt)
* IAuthor Script (*.txt)
* Inscriber CG (*.txt)
* JACOSub 2.7+ (*.jss; *.js)
* Karaoke Lyrics LRC (*.lrc)
* Karaoke Lyrics VKT (*.vkt)
* KoalaPlayer (*.txt) (equal to one of the variations of TMPlayer)
* MAC DVD Studio Pro (*.txt)
* MacSUB (*.scr)
* MicroDVD (*.sub)
* MPlayer (*.mpl)
* MPlayer2 (*.mpl)
* MPSub (*.sub)
* OVR Script (*.ovr)
* Panimator (*.pan)
* Philips SVCD Designer (*.sub)
* Phoenix Japanimation Society (*.pjs)
* Pinnacle Impression (*.txt)
* PowerDivX (*.psb)
* PowerPixel (*.txt)
* QuickTime Text (*.txt)
* RealTime (*.rt)
* SAMI Captioning (*.smi)
* Sasami Script (*.s2k)
* SBT (*.sbt)
* Sofni (*.sub)
* Softitler RTF (*.rtf)
* SonicDVD Creator (*.sub)
* Sonic Scenarist (*.sst)
* Spruce DVDMaestro (*.son)
* Spruce Subtitle File (*.stl)
* Stream SubText Player (*.sst)
* Stream SubText Script (*.ssts)
* SubCreator 1.x (*.txt)
* SubRip (*.srt)
* SubSonic (*.sub)
* SubStation Alpha (*.ssa)
* SubViewer 1.0 (*.sub)
* SubViewer 2.0 (*.sub)
* TMPlayer (*.txt) (five different variations)
* Turbo Titler (*.txt)
* Ulead DVD Workshop 2.0 (*.txt)
* ViPlay Subtitle File (*.vsf)
* ZeroG (*.zeg)


Compiling
=========
The source code is old (written between 2002 and 2004). It was originally made with **Delphi 7**, and may need some tweaking in order to compile it with the  latest versions of the tools/components.

If you want to get it to compile, these are (hopefully all) the additional components you may need:

* `RemObjects Pascal Script <http://www.remobjects.com/ps.aspx>`_
* `VirtualTreeView <http://www.delphi-gems.com/VirtualTreeview/>`_

The installer file was made for `NSIS <http://nsis.sourceforge.net/>`_, and as it is old it will most probably not work at all with the latest NSIS version.

I have included two *.bat* scripts that I used to invoke the commands to compile the project; paths are hardcoded so you will need to change that.

Disclaimer
==========

* I started writing this tool when I was 14 years old and did not have any formal training.
* I never used a Version Control System when I wrote this; in fact I didn't even know they existed.
* Expect lack of comments, bad programming style, bad code organization, misuse of the Object-Oriented paradigm.
* Expect no MVC pattern: that is for *pussies* (?). User interface and code are only one, and they love each other so much they are completely glued together.
* Expect really complicated ways of doing really simple stuff (like subtitle parsing). I didn't even know about regular expressions at the time I started this. I was just a really persistent kid and had enough free time to experiment with many approaches to handle any problem I came up with.
* A paid programmer producing code like this for a living should probably be fired.
* Fortunately, I have improved with the years... ;-)

With all that said, I hope this is useful for someone :-)


License
=======
Subtitle Workshop and SubtitleAPI source code are both released under the GNU/GPL 3 license.