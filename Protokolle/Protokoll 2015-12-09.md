2nd meeting of the 'Code Clinic' programming club - 9 december 2015
----

Attendance: 9

While waiting for me to connect unsuccessfully my computer to the projector from the seminar room, Maren gave a bunch of tips on "how to find and get help in R". Here is a short version of what she said:

	From Inside R: if function name known, here function x, ?x or help(x) [ex: ?mean]; if unknown, function search is ?? (or help.search). Works with fuzzy matching, can be used for expression [ex: ??"pattern matching"]. Finally, library sos have a set of functions (findFn) to seek in a similar manner funcitons from package that are not installed on your computer.
	On the web: R mailing list (https://stat.ethz.ch/mailman/listinfo/r-help), Stackoverflow (http://stackoverflow.com/) or any other more specific site of the stackexchange network [ex: gis.stackexchange.com for GIS]. With one major caveat: learn to ask correctly. Questions should explain clearly the problem, show minimal code for the error to be reproducible, give a sample of data (using dput(your_df)).
	To change the language in which the error are displayed to english, use: Sys.setenv(LANG="en")
	Googling error: it usually works (as long as error in english) but if not rseek.org and duckduckgo.com works usually better in edge cases.
	Syntax highlighting can help avoiding basic errors. In Mac, native to the R GUI. On PC, one can use Notepad++ (it can be used inside the GUI using options(editor="path/to/notepad++.exe"))

Then I tried to explain some basics on web scraping for data mining with a case study: pangaea.de. In this simple case, the difficulty reside in two points: being able to reproduce the API the website use to connect to their database (in this case, in a way similar to google, using a URL of the form search?q=query&count=500&offset=0&...) and being able to read in HTML pages, in particular HTML tables. Basic knowledge of HTML is necessary to be able to perform this kind of tasks.
Code attached.

At the end of the meeting, we gathered ideas for future meeting themes, and decided on the Thurday 7th of January at 4pm in room 3330 (Nordbau) for the next meeting.

