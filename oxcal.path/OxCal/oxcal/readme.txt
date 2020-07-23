OxCal
=====

This is a stand-alone distribution of OxCal which is configured to run using nodejs software.  You can use it with any of the latest browsers on Windows, Mac or Linux.

Install NodeJs
--------------
Install the nodejs software available from:

https://nodejs.org/

Installation on a PC
--------------------
Copy the OxCal directory in which this file is found to the to a suitable directory:

C:\Program Files

Firefox Installation on a Mac
-----------------------------
Copy the OxCal directory in which this file is found to the to a suitable directory:

/Applications

Running the program using NodeJs
---------------------------------
Use a command line terminal and change to the oxcal directory.  From there run the node software using the NodeServer script by typing:

node NodeServer.js

on the command line.  If you prefer you can set this up in a startup batch file or script.  This will start the server on which OxCal will now run.

In your browser you then need to enter the url:

http://localhost:8080/

Normally this will take you straight to OxCal but the first time you do this you will be taken to a page which allows you to set up the directory area for files and also the location of the oxcal web files.  You can also change the port on which the server runs.  As a default it will run on a localhost port not accessible to other computers.

If you ever wish to reset these values you can either navigate to:

http://localhost:8080/setup.html

or press [File > Reset] from within OxCal.  

Group configurations
--------------------
If you wish you could set this up on a group server rather than your own computer this would give the group all access to the same user area.  In this case you may with to use port 80 but make sure your firewall prevents access from outside your institution.  Alternatively you should be able to set it up on different computers as above and share a drive for your files - this will enable you to use more different computers for processing.

(c) Christopher Bronk Ramsey
