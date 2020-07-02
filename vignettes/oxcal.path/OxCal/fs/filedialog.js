var path="";
var filename="";
var fileExt="";
var directory="";
var generics=new Array("","Home","Up");
var drives=new Array();
var fileTypes=new Array("All","Curves","Documents","Graphics","Plots","Priors","Projects");
var fileFormats=new Array("pdf","png","svg");
var fileFormat="svg";
var fileFormatNo=2;
var fileType="Projects";
var fileTypeNo=5;
var filecontent="";
var readonly=false;
var action;
var sourceWindow;
var sorton="name";sortrev=false;
var allowdelete=false;
function setGenerics(drive)
{
 var i,j;
 m=document.getElementById("Directory");
 while(m.length > generics.length)
 {
  m.remove(m.length-1);
 };
 m.selectedIndex=0;
 for(i=0;i<drives.length;i++)
 {
  try
  {
   m.add(new Option(drives[i]),null); // standards compliant
  }
  catch(ex)
  {
    m.add(new Option(drives[i]));
  }
  if(drives[i]==drive)
  {
   m.selectedIndex=i+generics.length;
   document.title=drive;
   if(getArgs().action)
   {
    document.title+=":"+getArgs().action;
   };
  };
 };
 if(m.selectedIndex==0)
 {
  document.title="OxCal";
  if(getArgs().action)
  {
   document.title+=":"+getArgs().action;
  };
 };
};
function refresh(commands)
{
 var prevpath;
 commands=encodeURI(commands);
 document.getElementById("lister").src="";
 document.getElementById('Directory').selectedIndex=0;
 fileType=fileTypes[document.getElementById('FileType').selectedIndex];
 if(fileType=="OxCal")
 {
  document.getElementById("lister").width=600;
  document.getElementById("lister").height=480;
 };
 if((fileType=="Graphics")&&(onAServer()))
 {
  document.getElementById("formatArea").style.display="block";
 }
 else
 {
  fileFormat="svg";
  document.getElementById("formatArea").style.display="none";
 };
 if(onAServer())
 {
  if((path=="undefined")||(!path)){path="/";filename="";};
  if((commands=='undefined')||!commands){commands="";}else{commands="&"+commands;};
  if(fileExt){commands+="&extension="+fileExt;};
  switch(fileType)
  {
  case "Projects":
   if(allowdelete){commands+="&detail=true";};
   break;
  case "OxCal":
   break;
  default:
   commands+="&detail=true";
   break;
  };
  if(getArgs().contains)
  {
   commands+="&contains="+getArgs().contains;
   if(getArgs().within)
   {
    commands+="&within="+getArgs().contains;
   };
  };
  document.getElementById('PathName').value=path;
  directory=new Object();
  directory.files=new Array();
  directory.dirs=new Array();
  directory.conditions=false;
  document.getElementById("lister").src="filelist.php?path="+path+commands;
 }
 else
 {
  switch(fileType)
  {
  case "Projects":
   if(allowdelete)
   {
    localDirRead(true,fileExt,getArgs().contains,getArgs().within);
    break;
   };
  case "OxCal":
   localDirRead(false,"",getArgs().contains,getArgs().within);
   break;
  default:
   localDirRead(true,fileExt,getArgs().contains,getArgs().within);
   break;
  };
 };
};
function localUpdateFileView(d)
{
 directory=d;
 path=d.path;
 document.getElementById('PathName').value=path;
 document.getElementById("lister").src="filelist.html";
};
function initialise()
{
 sourceWindow=window.opener;
 if(!window.opener)
 {
  if(window.parent.document.getElementById('fileDialog'))
  {
   sourceWindow=window.parent;
  };
 };
 var opPath,i,chA;
 if(getArgs().path)
 {
  path=getArgs().path;
 }
 else
 {
  path=getCookie("OxCalWorkingPath");
 };
 if(sourceWindow)
 {
  if(sourceWindow.getFilename)
  {
   opPath=sourceWindow.getFilename();
   if(opPath)
   {
    fileChoose(pathToFilename(opPath));
    opPath=pathToPath(opPath);
    if(opPath)
    {
     path=opPath.replace("_current/","");
    };
   };
  };
 };
 switch(action)
 {
 case "Save":case "Run":
  if(!filename)
  {
   fileChoose("Untitled");
  };
  checkNewName();
  break;
 };
 if(getArgs().createIfNeeded)
 {
  refresh("createIfNeeded=true");
 }
 else
 {
  refresh();
 };
 showDir();
 if((!onAServer()) && (path.indexOf("\\")!=-1))
 {
  chA=("A").charCodeAt(0);
  for(i=0;i<26;i++)
  {
   drives[i]=String.fromCharCode(i+chA);
  };
  setGenerics();
 };
};
function doDelete(filename)
{
 if(onAServer())
 {
  refresh("filename="+filename+"&action=delete");
 }
 else
 {
  localFileDelete(filename);
  if(filename.match(".oxcal"))
  {
   localFileDelete(filename.replace(".oxcal",".js"));
   localFileDelete(filename.replace(".oxcal",".txt"));
   localFileDelete(filename.replace(".oxcal",".log"));
   localFileDelete(filename.replace(".oxcal",".work"));
   localFileDelete(filename.replace(".oxcal",".bat"));
  };
  refresh();
 };
};
function setServerSub(dirname)
{
 var els,drvs,drv="";
 drvs=dirname.split(":");
 if(drvs.length==2)
 {
  drv=drvs[0];
  dirname=drvs[1];
  path="";
 };
 if(dirname=="..")
 {
  els=path.split("/");
  els.pop();els.pop();
  if(els.length)
  {
   path=els.join("/")+"/";
  }
  else
  {
   path="";
  };
 }
 else
 {
  path+=dirname+"/";
 };
 if(drv)
 {
  path=drv+":"+path;
 };
 filename="";
 document.getElementById('PathName').value=path;
 setCookie("OxCalWorkingPath",path);
};
function doNewFolder(filename)
{
 if(onAServer())
 {
  refresh("filename="+filename+"&action=mkdir");
 }
 else
 {
  createDirectory(filename);
 };
};
function doRename(filename,newFilename)
{
 if(onAServer())
 {
  refresh("filename="+filename+"&newFilename="+newFilename+"&action=rename");
 }
 else
 {
  fileMove(filename,path,newFilename);
  if(filename.match(".oxcal"))
  {
   fileMove(filename.replace(".oxcal",".js"),path,newFilename.replace(".oxcal",".js"));
   fileMove(filename.replace(".oxcal",".txt"),path,newFilename.replace(".oxcal",".txt"));
   fileMove(filename.replace(".oxcal",".log"),path,newFilename.replace(".oxcal",".log"));
   fileMove(filename.replace(".oxcal",".work"),path,newFilename.replace(".oxcal",".work"));
   fileMove(filename.replace(".oxcal",".bat"),path,newFilename.replace(".oxcal",".bat"));
  };
  refresh();
 };
};
function goPath()
{
 if(onAServer())
 {
  path=document.getElementById('PathName').value;
  filename="";
  setCookie("OxCalWorkingPath",path);
  refresh();
 }
 else
 {
  setWorkingPath(document.getElementById('PathName').value);
  refresh();
 };
};
function dirOpen(dirname)
{
 if(onAServer())
 {
  setServerSub(dirname);
  refresh();
 }
 else
 {
  path=setLocalFileSub(path,dirname);
  filename="";
  refresh();
 };
};
function goGeneric(name)
{
 var sel;
 sel=document.getElementById('Directory').selectedIndex;
 if(!name)
 {
  if(sel<generics.length)
  {
   switch(generics[sel])
   {
   case "Home":
    name="Home";
    break;
   case "Up":
    dirOpen("..");
    return;
   default:
    return;
   };
  }
  else
  {
   dirOpen(drives[sel-generics.length]+":");
  };
 };
 if(onAServer())
 {
  switch(name)
  {
  case "Home":
   if(getArgs().path)
   {
    document.getElementById('PathName').value=getArgs().path;
   }
   else
   {
    document.getElementById('PathName').value="/";
   };
   goPath();
   break;
  };
 }
 else
 {
  if(name || (sel<generics.length))
  {
   setWorkingPathGeneric(name);
   refresh();
  };
 };
};
function pathToFilename(path)
{
 var parts=path.split(/[/\\]/);
 return parts.pop();
};
function pathToPath(path)
{
 if(!path){return "/";};
 var parts=path.split(/[/]/);
 if(parts.length>1)
 {
  parts.pop();
  return parts.join("/")+"/";
 };
 return "/";
};
function addFile(filename)
{
 directory.files.push(filename);
};
function addCondition(cond)
{
 if(!directory.files.length){return;};
 if(typeof(directory.files[directory.files.length-1])=='object')
 {
  directory.files[directory.files.length-1]['condition']=cond;
  directory.conditions=true;
 };
};
function addDir(filename)
{
 directory.dirs.push(filename);
};
function checkName(name)
{
 name=name.replace(/^\s*/,"").replace(/\s*$/,"");
 name=name.replace(/[^0-9a-zA-Z]/g,"_");
 name=name.replace(/[_]+/g,"_");
 return name;
};
function showDir()
{
 document.getElementById('PathName').value=path;
};
function showFilename()
{
 switch(action)
 {
 case "Save":
 case "Run":
  document.writeln('<input name="FileName" id="FileName" type="text" size="55" onchange="checkNewName()"><hr\/>');
  break;
 };
};
function showAction()
{
 if(onAServer())
 {
  switch(action)
  {
  case "Open":
   document.writeln('<button type="button" id="local" name="local" onclick="fileUpload()">Upload<\/button><\/td><td>');
  case "Save":
   document.writeln('<button type="button" id="local" name="local" onclick="fileDownload()">Download<\/button><\/td><td>');
   break;
  };
 };
 document.writeln('<button type="submit" id="ok" name="ok" onclick="doAction()">'+action+'<\/button>');
};
function getFullPath(filename,wr)
{
 if(directory.conditions)
 {
  if(wr)
  {
   return path+"_draft/"+filename;
  }
  else
  {
   return path+"_current/"+filename;
  };
 };
 return path+filename;
};
function fileChoose(filename)
{
 window.filename=filename;
 switch(action)
 {
 case "Save":
 case "Run":
  document.getElementById('FileName').value=filename;
  checkNewName();
  break;
 default:
  document.getElementById('PathName').value=getFullPath(filename,false);
  break;
 };
};
function fileNew(ext)
{
 switch(ext)
 {
 case "oxcal":
  newFile();
  finishAction();
  break;
 case "prior":
  editFile("","prior");
  break;
 case "14c":
  editFile("","14c");
  break;
 };
};
function fileDelete(filename)
{
 if(confirm("Do you wish to delete: "+filename+"?"))
 {
  doDelete(filename);
 };
};
function fileRename(filename)
{
 var newFilename;
 var namebits,ext;
 namebits=filename.split(".");
 ext=namebits[1];
 newFilename=checkName(prompt("Rename file:",namebits[0]));
 if(newFilename)
 {
  if(ext){newFilename+="."+ext;};
  doRename(filename,newFilename);
 };
};
function fileUpload()
{
 refresh("action=upload");
};
function fileDownload()
{
 fileFormat=fileFormats[document.getElementById('FileFormat').selectedIndex];
 if(sourceWindow && sourceWindow.getFileContent)
 {
  filecontent=sourceWindow.getFileContent();
 }
 else
 {
  filecontent="";
 };
 refresh("action=download");
};
function getMimeType(filename)
{
 var ext=filename.split(".").pop().toLowerCase();
 switch(ext)
 {
 case "log": case "txt":
 case "csv": case "14i":
 case "prior": case "oxcal": case "plot":
  return "text/"+ext;
 case "js": 
  return "text/javascript";
 case "svg":
  return "image/svg+xml";
 case "pdf":
  return "application/pdf";
 case "png":
  return "image/png";
 case "tex":
  return "application/x-tex";
 case "json":
  return "application/json";
 };
 return "application/force-download";
};
function fileView()
{
 if(onAServer())
 {
  window.open("ocp_open_server.php?type="+getMimeType(filename)+"&filename="+encodeURIComponent(getFullPath(filename,false)),"", "toolbar=yes,location=yes,menubar=no,resizable=yes,status=yes");
 }
 else
 {
  window.open("../mydata"+getFullPath(filename,false),"", "toolbar=yes,location=yes,menubar=no,resizable=yes,status=yes");
 };
};
function newFolder()
{
 var folderName;
 if(readonly){alert("New folder not allowed here");return;};
 folderName=prompt("New folder","");
 if(folderName)
 {
  folderName=checkName(folderName);
  doNewFolder(folderName);
 };
};
function checkNewName()
{
 var namebits,ext;
 filename=document.getElementById('FileName').value;
 namebits=filename.split(".");
 ext=fileExt;
 if(!ext && namebits.length>1)
 {
  ext=checkName(namebits.pop());
 };
 filename=checkName(namebits[0]);
 if(!filename){return false;};
 if(ext)
 {
  filename+="."+ext;
 };
 switch(ext)
 {
 case "tex":
  fileFormat="pdf";
  document.getElementById('FileName').value=filename.replace(".tex",".pdf");
  break;
 case "svg":
  fileFormat=fileFormats[document.getElementById('FileFormat').selectedIndex];
  document.getElementById('FileName').value=filename.replace(".svg","."+fileFormat);
  break;
 default:
  document.getElementById('FileName').value=filename;
  break;
 };
 return true;
};
function doAction()
{
 var namebits,ext;
 namebits=filename.split(".");
 fileFormat=fileFormats[document.getElementById('FileFormat').selectedIndex];
 ext=fileExt;
 if(!ext && namebits.length>1)
 {
  ext=checkName(namebits.pop());
 };
 switch(action)
 {
 case "Run":
  if(!checkNewName()){alert("No filename");return;};
  if(sourceWindow)
  {
   sourceWindow.setFilename(getFullPath(filename,false));
   sourceWindow.runFile();
  };
  break;
 case "Insert":
  if(sourceWindow)
  {
   sourceWindow.insertFile(path,filename);
  };
  break;  
 case "Save":
  if(!checkNewName()){alert("No filename");return;};
  if(sourceWindow)
  {
   filecontent=sourceWindow.getFileContent();
   sourceWindow.setFilename(getFullPath(filename,true));
   if(onAServer())
   {
    switch(fileFormat)
    {
    case "pdf":
    case "svg":
     path=getFullPath("",true);
     refresh("filename="+filename+"&action=save");
     break;
    default:
     fileDownload();
     break;
    };
    return;
   }
   else
   {
    localFileWrite(path+filename,filecontent,function ()
    {
     if(window.parent.document.getElementById('fileDialog'))
     {
      window.parent.hideArea("fileDialog");
     }
     else
     {
      window.close();
     };
    });
    return;
   };
  };
  break;
 case "Open":
  switch(ext)
  {
  case "work":
    window.open("../oxcal/OxCal.html?view=status&source="+
     encodeURIComponent(getFullPath(filename.replace(".work",".js"),false)),"", "toolbar=yes,location=yes,menubar=yes,scrollbars=yes,resizable=yes,status=yes");
    break;
  case "js":
   if((filename.indexOf("data.js")!=-1)&&onAServer())
   {
    if(window.location.pathname.indexOf("/html/")==-1)
    {
     window.open("../jsobj/ObjectEdit.php?filename="+encodeURIComponent(getFullPath(filename,false))+"&ext="+ext, "EditWindow","width=650,height=650,toolbar=no,location=no,menubar=no,scrollbars=yes,resizable=yes,status=no");
    }
    else
    {
     window.open("../jsobj/ObjectEdit.php?filename="+encodeURIComponent(getFullPath(filename,false))+"&ext="+ext, "EditWindow","width=650,height=650,toolbar=no,location=no,menubar=no,scrollbars=yes,resizable=yes,status=no");
    };
    break;
   };
   if(fileType=="All")
   {
    if(parent.document.getElementById('fileDialog'))
    {
     window.location="get_file_content.php?filename="+encodeURIComponent(getFullPath(filename,false));
    }
    else
    {
     editFile(getFullPath(filename,false),ext);
    };
   }
   else
   {
    window.open("../oxcal/OxCal.html?source="+encodeURIComponent(getFullPath(filename,false)),"", "toolbar=yes,location=yes,menubar=yes,scrollbars=yes,resizable=yes,status=yes");
   };
   break;
  case "json":
   if(parent.document.getElementById('fileDialog'))
   {
    window.location="get_file_content.php?filename="+encodeURIComponent(getFullPath(filename,false));
   }
   else
   {
    editFile(getFullPath(filename,false),ext);
   };
   break;
  case "oxcal":
   window.open("../oxcal/OxCal.html?Mode=Input&source="+encodeURIComponent(getFullPath(filename,false)),"", "toolbar=yes,location=yes,menubar=yes,scrollbars=yes,resizable=yes,status=yes");
   break;
  case "png":
  case "pdf":
   fileView();
   return;
  case "svg":
   if(onAServer())
   {
    switch(fileFormat)
    {
    case "svg":
     fileView();
     break;
    default:
     fileDownload();
     return;
    };
   }
   else
   {
    switch(fileFormat)
    {
    case "svg":
     fileView();
     break;
    default:
     alert("Not yet implemented");
     return;
    };
    return;
   };
   break;
  case "14c":
  case "html":
  case "txt":
  case "log":
  case "prior":
   editFile(getFullPath(filename,false),ext);
   break;
  case "plot":
   if(parent.document.getElementById('fileDialog'))
   {
    parent.setFilename(getFullPath(filename,true));
//    parent.hideArea('fileDialog');
    if(onAServer())
    {
     window.location="../oxplot/oxplot_load.html";
    }
    else
    {
     window.location="../oxplot/oxplot_load.html";
    };
   }
   else
   {
    window.open("../oxplot/OxPlot.html?source="+encodeURIComponent(getFullPath(filename,false)),"", "toolbar=yes,location=yes,menubar=yes,scrollbars=yes,resizable=yes,status=yes");
   };
   break;
  default:
   if(onAServer())
   {
    fileDownload();
    return;
   }
   else
   {
    launchFile(filename);
    return;
   };
   break;
  };
  break;
 };
 finishAction();
};
function finishAction()
{
 if(getArgs().action)
 {
  if(window.parent.document.getElementById('fileDialog'))
  {
   window.parent.hideArea("fileDialog");
  }
  else
  {
   window.close();
  };
 }
 else
 {
  refresh();
 };
};
function showOptions(ar,sel)
{
 var i;
 for(i=0;i<ar.length;i++)
 {
  if(i==sel)
  {
   document.writeln('<option label="'+ar[i]+'" selected>'+ar[i]+'<\/option>');
  }
  else
  {
   document.writeln('<option label="'+ar[i]+'">'+ar[i]+'<\/option>');
  };
 };
};
function showActionTitle()
{
 action=getArgs().action;
 fileExt=getArgs().ext;
 if(fileExt && fileExt!="undefined")
 {
  fileExt=fileExt.replace(/[.]/g,"");
  switch(fileExt)
  {
  case "prior":
   fileTypes=new Array("Priors");
   fileType="Priors";
   fileTypeNo=0;
   break;
  case "json":
   fileTypes=new Array("JSON");
   fileType="JSON";
   fileTypeNo=0;
   break;
  case "plot":
   fileTypes=new Array("Plots");
   fileType="Plots";
   fileTypeNo=0;
   break;
  case "oxcal":
   fileTypes=new Array("Projects");
   fileType="Projects";
   fileTypeNo=0;
   break;
  case "svg":
   fileTypes=new Array("Graphics");
   fileType="Graphics";
   fileTypeNo=0;
   break;
  case "14c":
   fileTypes=new Array("Curves");
   fileType="Curves";
   fileTypeNo=0;
   break;
  case "tex":
   fileTypes=new Array("Documents");
   fileType="Documents";
   fileTypeNo=0;
   break;
  case "*":
   fileExt="";   
  default:
   fileTypes=new Array("All");
   fileType="All";
   fileTypeNo=0;
   break;
  };
  if(!action)
  {
   action="Open";
   allowdelete=true;
  };
 }
 else
 {
  fileTypes=new Array("All","Curves","Documents","Graphics","OxCal","Plots","Priors","Projects");
  if(!action)
  {
   action="Open";
   allowdelete=true;
   fileType="OxCal";
   fileTypeNo=4;
  }
  else
  {
   fileType="Projects";
   fileTypeNo=7;
  };
  fileExt="";
 };
 if(action=="Insert")
 {
  fileTypes=new Array("Curves","Priors");
  fileType="Curves";
  fileTypeNo=0;
 };
 document.writeln('<title>OxCal:'+action+'<\/title>');
};
showActionTitle();