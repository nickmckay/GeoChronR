var separatorArray=new Array("\t",",",";",":","|",/\s+/);
var outlierModels=new Array();
outlierModels[0]={name:"SSimple", dist:"N(0,2)",      scale:"0",     typ:"s"};
outlierModels[1]={name:"RSimple", dist:"N(0,100)",    scale:"0",     typ:"r"};
outlierModels[2]={name:"TSimple", dist:"N(0,100)",    scale:"0",     typ:"t"};
outlierModels[3]={name:"RScaled", dist:"T(5)",        scale:"U(0,4)",typ:"r"};
outlierModels[4]={name:"General", dist:"T(5)",        scale:"U(0,4)",typ:"t"};
outlierModels[5]={name:"Charcoal",dist:"Exp(1,-10,0)",scale:"U(0,3)",typ:"t"};

function importData()
{
 var str=document.getElementById("import").value;
 var sep=separatorArray[document.getElementById("importSeparator").selectedIndex];
 var cmds=str.split("\n");
 var parms,maxparms;
 var i,j;
 var maxparms=1;
 for(i=0;i<cmds.length;i++)
 {
  parms=cmds[i].split(sep);
  if(parms.length>=maxparms)
  {
   if(parms.length>maxparms){maxparms=parms.length;};
   for(j=0;j<parms.length;j++)
   {
    document.getElementById("Param"+(j+1)).value=parms[j];
   };
   add_command();
  };
 };
};
function add_command()
{
 var ind=document.getElementById("Verb").selectedIndex;
 var no=parent.commands[ind].split(",").length;
 var cmd=parent.commands[ind].split(",")[0];
 var params=0;
 var group=false;
 var modifier=false;
 var attribute=false;
 var pmt="";
 var str="";
 var p;
 if(no>1)
 {
  if(cmd=="Comment")
  {
   parent.addComment(document.getElementById("Param1").value);
   return;
  };
  cmd=cmd+"(";
  if(parent.commands[ind].split(",")[no-1]=="{")
  {
   no--;group=true;
  };
  if(parent.commands[ind].split(",")[no-1]=="@")
  {
   no--;modifier=true;
  };
  if(parent.commands[ind].split(",")[no-1]=="=")
  {
   pmt=parent.commands[ind].split(",")[no-2];
   str=document.getElementById("Param1").value;
   switch(pmt)
   {
   case 'color':
    parent.applyCommand(pmt+"=\""+str+"\"");
    break;
   default:
    parent.applyCommand(pmt+"="+str);
    break;
   };
   return;
  };
  for(p=1;p<6;p++)
  {
   if(p<no)
   {
    pmt=parent.commands[ind].split(",")[p];
    str=document.getElementById("Param"+p).value;
    if((str.length>0)||(p<(no-1)))
    {
     if(params>0){cmd=cmd+", ";};
     switch(pmt)
     {
     case "[Name]":
      if(str)
      {
	   cmd=cmd+'"'+str+'"';
      }
      else
      {
       params--;
      };
      break;
     case "Name":
     case "Parameter1":
     case "Parameter2":
     case "Curve1":
     case "Curve2":
     case "Text":
     case "Filename":
      cmd=cmd+'"'+str+'"';
      break;
     default:
      cmd=cmd+str;
      break;
     };
     params++;
    };
   };
  };
  cmd=cmd+")";
  if(parent.commands[ind].indexOf("{")!=-1)
  {
   cmd=cmd+"{}";
  };
 };
 if(modifier)
 {
  parent.applyCommand(cmd);
 }
 else
 {
  parent.addCommand(cmd);
 };
};
function edit()
{
 parent.copy();
 hideAllMenus();
 document.getElementById('editText').value=parent.clipboard;
 makeActive("main",6);
};
function editOk()
{
 parent.cut();
 parent.addCommand(document.getElementById('editText').value);
 makeActive("main",-1);
};
function editCancel()
{
 makeActive("main",-1);
};
function enableMenu(ok)
{
 if(!document.getElementById("menuSystem")){return;};
 if(ok)
 {
  document.getElementById("menuSystem").style.display="block";
 }
 else
 {
  document.getElementById("menuSystem").style.display="none";
 };
};
function prompt()
{
 var ind=document.getElementById("Verb").selectedIndex;
 var no=parent.commands[ind].split(",").length;
 var prmt,str,p;
 prmt=document.getElementById("Prompt0");
 prmt.replaceChild(document.createTextNode(parent.commands[ind].split(",")[0]),prmt.firstChild);
 prmt=document.getElementById("Prompt0T");
 prmt.replaceChild(document.createTextNode(parent.commands[ind].split(",")[0]),prmt.firstChild);
 for(p=1;p<6;p++)
 {
  str=" ";
  if(p<no)
  {
   str=parent.commands[ind].split(",")[p];
   if((str.indexOf("{")!=-1)||(str.indexOf("@")!=-1)||(str.indexOf("=")!=-1))
   {
    str=" ";
   };
  };
  prmt=document.getElementById("Prompt"+p);
  prmt.replaceChild(document.createTextNode(str),prmt.firstChild);
  if(p<6)
  {
   prmt=document.getElementById("Prompt"+p+"T");
   prmt.replaceChild(document.createTextNode(str),prmt.firstChild);
  };
  if(str==" ")
  {
   document.getElementById("PromptR"+p).style.display="none";
  }
  else
  {
   document.getElementById("PromptR"+p).style.display="block";
  };
 };
};
function setCommand(vrb)
{
 for(i=0;i<parent.commands.length;i++)
 {
  if(vrb==parent.commands[i].split(",")[0])
  {
   document.getElementById("Verb").selectedIndex=i;
   prompt();
   return;
  };
 };
};
function hideAllMenus()
{
 makeActive('main',-1);
};
function fileClick(i)
{
// New|Open||Close|Save|Save as|Run|Insert||Manager
 switch(i)
 {
 case 0:
  parent.newFile();
  break;
 case 1:
  parent.openFile();
  break;
 case 3:
  if(confirm("Close window"))
  {
   parent.window.close();
  };
  break;
 case 4:
  parent.saveFile();
  break;
 case 5:
  parent.saveFileDialog();
  break;
 case 6:
  parent.runFile();
  break;
 case 7:
  parent.fileDialog("Insert","");
  break;
 case 9:
  parent.fileDialog();
  break;
 };
};
function viewClick(i)
{
 switch(i)
 {
 case 0: parent.reversed=false;parent.setInView(0);return;
 case 1: parent.reversed=true;parent.setInView(0);return;
 case 2: parent.setInView(1);return;
 case 3: parent.setInView(2);return;
 };
};
function editClick(i)
{
 switch(i)
 {
 case 0: parent.undo();return;
 case 1: parent.redo();return;
 case 3: parent.cut();return;
 case 4: parent.copy();return;
 case 5: parent.paste();return;
 case 6: parent.deleteCode();return;
 case 7: parent.edit();return;
 };
};
function commandAClick(vrb)
{
 setCommand(vrb);
};
function commandBClick(vrb)
{
 setCommand(vrb);
};
function commandCClick(vrb)
{
 setCommand(vrb);
};
function commandDClick(vrb)
{
 setCommand(vrb);
};
function commandEClick(vrb)
{
 setCommand(vrb);
};
function modelhelpClick(id)
{
 makeActive('modelhelpTable',id)
};
function trapCode(trap,name)
{
 if(!trap){return( 'Boundary("'+name+'");');};
 return( 'Boundary("'+name+'"){ Start("Start of '+name+'");Transition("Period of '+name+'");End("End of '+name+'");};');
};
function createSequence(id,str)
{
 var type,no,i,code,radios;
 no=Number(document.getElementById(id+"No").value);
 radios=document.getElementsByName(id+"Type");
 trap=(document.getElementById("phaseTrapezium").checked) && (str=='Phase');
 for(i=0;i<radios.length;i++)
 {
  if(radios[i].checked){type=radios[i].value;};
 };
 if(!no || (no<1)){alert("Phase number must be greater than 1");return;};
 code="";
 switch(type)
 {
 case "contiguous":
  code+='Sequence(){'+trapCode(trap,'Start 1');
  for(i=0;i<no;i++)
  {
   code+=str+'("'+(i+1)+'"){};';
   if(i==no-1)
   {
    code+=trapCode(trap,'End '+(i+1));
   }
   else
   {
    code+=trapCode(trap,'Transition '+(i+1)+'/'+(i+2));
   };
  };
  code+='}';
  parent.addCommand(code);
  break;
 case "sequential":
  code+='Sequence(){';
  for(i=0;i<no;i++)
  {
   code+=trapCode(trap,'Start '+(i+1));
   code+=str+'("'+(i+1)+'"){};';
   code+=trapCode(trap,'End '+(i+1));
  };
  code+='}';
  parent.addCommand(code);
  break;
 case "overlapping":
  code+='Phase(){';
  for(i=0;i<no;i++)
  {
   code+='Sequence(){'+trapCode(trap,'Start '+(i+1));
   code+=str+'("'+(i+1)+'"){};';
   code+=trapCode(trap,'End '+(i+1))+'};';
  };
  code+='};';
  parent.addCommand(code);
  break;
 };
};
function createRiseOrFall(id,str)
{
 var type,ordered,i,code,radios;
 ordered=document.getElementById(id+"Ordered").checked;
 radios=document.getElementsByName(id+"Direction");
 for(i=0;i<radios.length;i++)
 {
  if(radios[i].checked){type=radios[i].value;};
 };
 code="Sequence(){";
 switch(type)
 {
 case "rise":
  code+=str+"{};";
  if(ordered){code+="Sequence(){};";}else{code+="Phase(){};";};
  code+="Boundary();";
  break;
 case "fall":
  code+="Boundary();";
  if(ordered){code+="Sequence(){};";}else{code+="Phase(){};";};
  code+=str+"{};";
  break;
 };
 code+="};";
 parent.addCommand(code);
};
function createNormal()
{
 var ordered,i,code;
 ordered=document.getElementById("normalOrdered").checked;
 code="Sequence(){";
 code+="Sigma_Boundary();";
 if(ordered){code+="Sequence(){};";}else{code+="Phase(){};";};
 code+="Sigma_Boundary();";
 code+="};";
 parent.addCommand(code);
};
function createOutlierModel()
{
 var name,dist,scale,typ,code;
 name=document.getElementById("outlierName").value;
 dist=document.getElementById("outlierDist").value;
 scale=document.getElementById("outlierScale").value;
 typ=document.getElementById("outlierType").value;
 code='Outlier_Model("'+name+'",'+dist+','+scale+',"'+typ+'");';
 parent.addCommand(code);
};
function stockOutlierModel()
{
 var om;
 om=document.getElementById("outlierStock").selectedIndex;
 document.getElementById("outlierName").value=outlierModels[om].name;
 document.getElementById("outlierDist").value=outlierModels[om].dist;
 document.getElementById("outlierScale").value=outlierModels[om].scale;
 document.getElementById("outlierType").value=outlierModels[om].typ;
};
function depositionOutlierModel()
{
 var om;
 om=document.getElementById("outlierDeposition").selectedIndex;
 if(om>0)
 {
  document.getElementById("outlierStock").selectedIndex=om-1;
  stockOutlierModel();
 };  
};
function setupDeposition()
{
 var i,j,code,parms,minz,maxz,interp,first;
 var str=document.getElementById("depositionImport").value;
 var sep=separatorArray[document.getElementById("depositionSeparator").selectedIndex];
 var cmds=str.split("\n");
 parms=new Array();
 first=true;
 if(cmds.length>1)
 {
  for(i=0;i<cmds.length;i++)
  {
   parms[i]=cmds[i].split(sep);
   if(parms[i].length<4){continue;};
   if(first)
   {
    minz=parms[i][3];maxz=parms[i][3];
    first=false;
    continue;
   };
   if(Number(parms[i][3])>maxz){maxz=Number(parms[i][3]);};
   if(Number(parms[i][3])<minz){minz=Number(parms[i][3]);};
  };
  if((maxz>minz))
  {
   interp=(100/(maxz-minz)).toPrecision(1);
   document.getElementById("interpolationRate").value=interp;
  };
 }
 else
 {
  document.getElementById("interpolationRate").value="";
 };
 if(document.getElementById("poisson").checked)
 {
  switch(document.getElementById("poissonUnits").selectedIndex)
  {
  case 0: document.getElementById("poissonK").value=0.1; break; //mm
  case 1: document.getElementById("poissonK").value=1; break; //cm
  case 2: document.getElementById("poissonK").value=100; break; //m
  };
  document.getElementById("poissonV").value="U(-2,2)";
 }
 else
 {
  document.getElementById("poissonK").value="";
  document.getElementById("poissonV").value="";
 };
};
function createDeposition()
{
 var i,j,code,parms;
 var str=document.getElementById("depositionImport").value;
 var typ=document.getElementById("depositionDateType").options[
 	document.getElementById("depositionDateType").selectedIndex].value;
 var name=document.getElementById("depositionName").value;
 var sep=separatorArray[document.getElementById("depositionSeparator").selectedIndex];
 var cmds=str.split("\n");
 var poisson=document.getElementById("poisson").checked;
 var k=document.getElementById("poissonK").value;
 var out=document.getElementById("outlierDepVal").value;
 if(document.getElementById("outlierDeposition").selectedIndex>0)
 {
  createOutlierModel();
 };
 if(poisson && k!="")
 { 
  if(k==0)
  {
   code="Sequence(\""+name+"\"){";
  }
  else
  {
   code="P_Sequence(\""+name+"\","+k;
   if(document.getElementById("interpolationRate").value || document.getElementById("poissonV").value)
   {
    code+=","+Number(document.getElementById("interpolationRate").value);
    if(document.getElementById("poissonV").value)
    {
     code+=","+document.getElementById("poissonV").value;
    };
   };
   code+="){";
  };
 }
 else
 {
  code="U_Sequence(\""+name+"\"";
  if(document.getElementById("interpolationRate").value)
  {
   code+=","+Number(document.getElementById("interpolationRate").value);
  };
  code+="){";
 };
 if(typ!='14C')
 {
  code+='timescale="'+typ+'";';
 };
 code+="Boundary();";
 parms=new Array();
 for(i=0;i<cmds.length;i++)
 {
  parms[i]=cmds[i].split(sep);
 };
 i=parms.length-1;
 // strip off any trailing blank lines
 while(i>0 && ((!(parms[i].length>3))||(parms[i][3]==''))){i--;};
 for(;i>=0;i--)
 {
  if((!(parms[i].length>3))||(parms[i][3]==''))
  {
   if(i>0)
   {
    if(parms[i-1].length>3)
    {
     code+="Boundary();";
    }
    else
    {
     if((i<parms.length-1)&&(parms[i+1].length>3));
     {
      code+="Boundary();";
     };
    };
   };
   continue;
  };
  if((i>0) && (Number(parms[i-1][3])==Number(parms[i][3])))
  {
   switch(typ)
   {
   case "14C":
    code+='R_Combine("'+parms[i][0];
    break;
   default:
    code+='Combine("'+parms[i][0];
    break;
   };
   for(j=i-1;(j>=0) && (Number(parms[j][3])==Number(parms[i][3]));j--)
   {
    code+="/"+parms[j][0];
   };
   code+='"){';
   if(out && (outlierModels[document.getElementById("outlierStock").selectedIndex].typ=="r") && (typ=="14C"))
   {
    for(j=i;(j>=0) && (Number(parms[j][3])==Number(parms[i][3]));j--)
    {
     code+='R_Date("'+parms[j][0]+'",'+Number(parms[j][1])+','
      +Number(parms[j][2])+'){Outlier('+out+');};';
     i=j;
    };
    code+='z='+Number(parms[i][3])+';};';
   }
   else
   {
    for(j=i;(j>=0) && (Number(parms[j][3])==Number(parms[i][3]));j--)
    {
     switch(typ)
     {
     case "14C":
      code+='R_Date("'+parms[j][0]+'",'+Number(parms[j][1])+','+Number(parms[j][2])+');';
      break;
     default:
      code+='Date("'+parms[j][0]+'",N(calBP('+Number(parms[j][1])+'),'+Number(parms[j][2])+'));';
      break;
     };
     i=j;
    };
    if(out){code+='Outlier('+out+');';};
    code+='z='+Number(parms[i][3])+';};';
   };
  }
  else
  {
   switch(typ)
   {
   case "14C":
    code+='R_Date("'+parms[i][0]+'",'+Number(parms[i][1])+','+Number(parms[i][2])+'){';
    break;
   default:
    code+='Date("'+parms[i][0]+'",N(calBP('+Number(parms[i][1])+'),'+Number(parms[i][2])+')){';
    break;
   };
   if(out){code+='Outlier('+out+');';};
   code+='z='+Number(parms[i][3])+';};';
  };
 };
 code+="Boundary();";
 code+="};";
 parent.addCommand(code);
};
function createRCombine()
{
 var i,code,parms;
 var str=document.getElementById("rCombineImport").value;
 var sep=separatorArray[document.getElementById("rCombineSeparator").selectedIndex];
 var name=document.getElementById("rCombineName").value;
 var extra=document.getElementById("rCombineExtra").value;
 var cmds=str.split("\n");
 code='R_Combine("'+name+'"';
 if(extra){code+=", "+Number(extra);};
 code+="){";
 //strip off empty line at the end
 if(cmds[cmds.length-1].split(sep).length<3){cmds.length--;};
 for(i=0;i<cmds.length;i++)
 {
  parms=cmds[i].split(sep);
  if(parms.length>=3)
  {
   code+='R_Date("'+parms[0]+'",'+Number(parms[1])+','+Number(parms[2])+');';
  };
 };
 code+="};";
 parent.addCommand(code);
};
function createWiggle()
{
 var i,code,parms;
 var str=document.getElementById("wiggleImport").value;
 var sep=separatorArray[document.getElementById("wiggleSeparator").selectedIndex];
 var cmds=str.split("\n");
 code="D_Sequence(){";
 //strip off empty line at the end
 if(cmds[cmds.length-1].split(sep).length<3){cmds.length--;};
 for(i=0;i<cmds.length;i++)
 {
  parms=cmds[i].split(sep);
  if((parms.length==4)&&(parms[3]))
  {
   code+='R_Date("'+parms[0]+'",'+Number(parms[1])+','+Number(parms[2])+');Gap('+Number(parms[3])+');';
  }
  else
  {
   if(parms.length>=3)
   {
    code+='R_Date("'+parms[0]+'",'+Number(parms[1])+','+Number(parms[2])+');';
   };
  };
 };
 code+="};";
 parent.addCommand(code);
};
function outlierOptions(id,onchange)
{
  var i;
  document.writeln('<select id="'+id+'" onchange="'+onchange+'">');
  if(id=='outlierDeposition')
  {
   document.write("<option value='' selected='true'><\/option>");
  };
  for(i=0;i<outlierModels.length;i++)
  {
   document.write("<option value=");
   document.write(outlierModels[i].name);
   if((outlierModels[i].name=="General")&&(id!='outlierDeposition'))
   {
    document.write(" selected='true'");
   };
   document.write(">");
   document.write(outlierModels[i].name);
   document.writeln("<\/option>");
  };
  document.writeln("<\/select>");
};
function commandOptions()
{
  var cmd,i;
  document.writeln('<select id="Verb" onchange="prompt()">');
  for(i=0;i<parent.commands.length;i++)
  {
   cmd=parent.commands[i].split(",")[0];
   document.write("<option value=");
   document.write(i);
   if(cmd==parent.defaultcommand)
   {
    document.write(" selected='true'");
   };
   document.write(">");
   document.write(cmd);
   document.writeln("<\/option>");
  };
  document.writeln("<\/select>");
};
function initialise()
{
 prompt();
 if(!parent.onAServer())
 {
  enableMenu(true);
  if(!parent.localFilePossible())
  {
   document.getElementById("filenameDisplay").style.display="none";
  };
 }
 else
 {
  enableMenu(true);
 };
 makeActive('tools','0',true);
};
