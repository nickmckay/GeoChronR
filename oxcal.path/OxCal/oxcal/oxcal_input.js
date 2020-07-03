var defaultcommand="R_Date";
var command="Plot()\n{\n};";
var clipboard="";
var modelView=0;
var modelViewMode="edit";
var reversed=false;
var needsReload=false;
var plotWindow;

function enableMenu(ok)
{
 if(window.left.enableMenu)
 {
  window.left.enableMenu(ok);
 };
};
function checkRight(reloadOnly)
{
 if(needsReload)
 {
  window.right.location.replace("oci_right.html");
  needsReload=false;
  return(false);
 };
 if(reloadOnly)
 {
  return(true);
 };
 return modelView!=2;
};
function addCommand(addin)
{
 if(checkRight()){window.right.insert(addin+";");};
};
function applyCommand(addin)
{
 if(checkRight()){window.right.apply(addin+";");};
};
function addComment(txt)
{
 cut();
 clipboard="\/\/"+txt+"\n"+clipboard;
 paste();
 setInView(modelView);
};
function setInView(value,mode)
{
 if(!mode)
 {
  modelViewMode="edit";
 }
 else
 {
  modelViewMode=mode;
 };
 modelView=parseFloat(value);
 if(checkRight(true))
 {
  window.right.readModel();
  window.right.writeModel(true);
 };
};
function setCommand(cmd)
{
 if(checkRight(true))
 {
  window.right.readModel();
  command=cmd;
  window.right.writeModel(true);
 };
};
function undo()
{
 window.right.undo();
};
function redo()
{
 window.right.redo();
};
function cut()
{
 if(checkRight()){window.right.cut();};
};
function copy()
{
 if(checkRight()){window.right.copy();};
};
function paste()
{
 if(checkRight()){window.right.paste();};
};
function deleteCode()
{
 if(checkRight()){window.right.deleteCode();};
};
function edit()
{
 window.left.edit();
};
function saveFileDialog()
{
 window.right.readModel();
 saveFileAs("oxcal");
};
function saveFile()
{
  if(!filename)
  {
   saveFileDialog();
   return false;
  };
  if(!checkRight(true)){return false;};
  if(onAServer())
  {
   window.right.readModel();
   window.right.document.getElementById("saveFileCommand").value=parent.command;
   window.right.document.getElementById("saveFileFilename").value=filename;
   window.right.document.getElementById("saveFile").submit();
   enableMenu(false);
   return true;
  }
  else
  {
   if(localFilePossible())
   {
    window.right.readModel();
    localFileWrite(filename,command);
   }
   else
   {
    setInView(2,"save");
    return false;
   };
  };
  return false;
};
function saveLocalFile()
{
  if(!checkRight(true)){return false;};
  if(onAServer())
  {
   window.right.readModel();
   window.right.document.getElementById("saveLocalFileCommand").value=parent.command;
   window.right.document.getElementById("saveLocalFileFilename").value=filename;
   window.right.document.getElementById("saveLocalFile").submit();
   enableMenu(false);
   return true;
  }
  else
  {
   setInView(2,"save");
   return false;
  };
  return false;
};
function insertFile(path,filename)
{
 var name;
 if(onAServer()){path="";};
 if(filename.match(".14c"))
 {
  name=filename.replace(".14c","");
  addCommand('Curve("'+name+'","'+path+filename+'")');
 };
 if(filename.match(".prior"))
 {
  name=filename.replace(".prior","");
  addCommand('Prior("'+name+'","'+path+filename+'")');
 };
};
function openFile(filename)
{
  if(onAServer() || localFilePossible())
  {
   fileDialog("Open");
   return true;
  }
  else
  {
   setInView(2,"open");
   return false;
  };
  return false;
};
