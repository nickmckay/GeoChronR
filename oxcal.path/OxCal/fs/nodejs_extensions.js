//nodejs_extensions.js
//OxCal extensions for Node js

// This script sets OSName variable as follows:
// "Windows"    for all versions of Windows
// "MacOS"      for all versions of Macintosh OS
// "Linux"      for all versions of Linux
// "UNIX"       for all other UNIX flavors 
// "Unknown OS" indicates failure to detect the OS
var nodejs_path="/";

function checkAddon()
{
 getFromServer("../testNodeJs",function (txt) { 
  switch(txt)
  {
  case "true":
   nodeJS=true;
   break;
  case "false":
   nodeJS=true;
   window.location="../setup.html";
   break;
  default:
   setCookie("NodeJs",false);
   nodeJS=false;
   break;
  };
 },function (){
  setCookie("NodeJs",false);
  nodeJS=false;
 });
 return true;
};
function nodeJsPath(filename)
{
 if(filename.indexOf("/")==-1){return nodejs_path+filename;};
 return filename;
};
function createDirectory(name)
{
 getFromServer("../mydata"+nodeJsPath(name)+"?action=createDir",function (txt) { 
   if(typeof(localUpdateFileView)!="undefined")
   {
    localDirRead();
   };
 });
 return true;
};
function setLocalFileSub(path,dirname)
{
 if(dirname=="..")
 {
  let els=nodejs_path.split("/");
  els.pop();els.pop();els.push("");
  nodejs_path=els.join("/");
 }
 else
 {
  nodejs_path=path+dirname+"/";
 };
 if(typeof(localUpdateFileView)!="undefined")
 {
  localDirRead();
 };
 return true;
};
function setWorkingPathGeneric(name)
{
 if(name=="Home")
 {
  nodejs_path="/";
  localDirRead();
 };
 return true;
};
function localFileDelete(filename)
{
 getFromServer("../mydata"+nodejs_path+filename+"?action=delete",function (txt) { 
   if(typeof(localUpdateFileView)!="undefined")
   {
    localDirRead();
   };
 });
 return true;
};
function fileMove(oldname,newpath,newname)
{
 getFromServer("../mydata"+nodejs_path+oldname+"?action=rename&to=mydata"
 	+newpath+newname,function (txt) { 
   if(typeof(localUpdateFileView)!="undefined")
   {
    localDirRead();
   };
 });
 nodejs_path=newpath;
 return true;
};
function localFileWrite(filename,content,andthen)
{
 putToServer("../mydata"+nodeJsPath(filename),content,function (txt) { 
   if(typeof(andthen)!="undefined"){andthen(txt);};
  },function (err) {alert(err)});
 return true;
};
function localFileRead(filename,andthen)
{
 getFromServer("../mydata"+nodeJsPath(filename),function (txt) { 
   if(typeof(andthen)!="undefined")
   {
    andthen(txt);
   };
  },function (err) {alert(err)});
 return true;
};
function launchFile(filename)
{
 document.location="../mydata"+nodeJsPath(filename)+"?action=download";
};
function localOxCalLaunch(filename,lock,callback)
{
 getFromServer("../mydata"+nodeJsPath(filename)+"?action=oxcal&lock="+lock,function (txt) { 
   if((typeof(callback)!="undefined")&&(lock))
   {
    callback();
   };
  },function(txt){alert(txt);});
 if((typeof(callback)!="undefined")&&(!lock))
 {
  callback();
 };
 return true;
};
function localDirRead(detail,ext,contains,within)
{
// detail: include size and date; ext: extension; contains: must have string; within: limit of string search
 var q="?action=readDir";
 if(detail){q+="&detail=true";};
 if(ext){q+="&ext="+ext;};
 if(contains){q+="&contains="+contains;};
 if(within){q+="&within="+within;};
 getFromServer("../mydata"+nodejs_path.slice(0,-1)+q,function (txt) { 
   if(typeof(localUpdateFileView)!="undefined")
   {
    localUpdateFileView(JSON.parse(txt));;
   };
  });
 return true;
};
function fullPath(filename)
{
 return nodeJsPath(filename);
};
function localOpenOxCal(filename,reason)
{
 getFromServer("../mydata"+nodeJsPath(filename),function (txt) { 
   onLocalOpenOxCal({"content":txt,"reason":reason});
  },function (txt) {onLocalOpenOxCal({"content":false,"reason":reason})});
 return true;
};
function resetAddon()
{
 document.location="../setup.html";
 return true;
};
checkAddon();
