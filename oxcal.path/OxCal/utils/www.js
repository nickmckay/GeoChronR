 var main=0;
 var sub=-1;
 var webinfo=false;
 var nerc=false;
 var dataSaved;
 function checkFile(file)
 {
  var i,j,k;
  for(i=0;i<data.length;i++)
  {
   if(data[i].page==file)
   {
    main=i;
    return true;
   };
   for(j=0;j<data[i].sub.length;j++)
   {
    if(data[i].sub[j].page==file)
    {
     main=i;sub=j;
     return true;
    };
    for(k=0;k<data[i].sub[j].sub.length;k++)
    {
     if(data[i].sub[j].sub[k].page==file)
     {
      main=i;sub=j;
      return true;
     };
    };
   };
  };
  return false;
 };
 function subMenuString()
 {
  var str="";
  var i;
  for(i=0;i<data[main].sub.length;i++)
  {
   if(i!=0){str+="|";};
   str+=data[main].sub[i].menu;
  };
  return str;
 };
 function mainMenuString()
 {
  var str="";
  var i;
  for(i=0;i<data.length;i++)
  {
   if(i!=0){str+="|";};
   str+=data[i].menu;
  };
  return str;
 };
 function subClick(op)
 {
  if(data[main].sub[op].page.indexOf("/")==-1)
  {
   window.location="embed.php?File="+data[main].sub[op].page;
  }
  else
  {
   window.location=data[main].sub[op].page;
  };
 };
 function mainClick(op)
 {
  if(data[op].page.indexOf("/")==-1)
  {
   window.location="embed.php?File="+data[op].page;
  }
  else
  {
   window.location=data[op].page;
  };   
 };
 function locator()
 {
  var str=data[main].menu;
  if(sub<0){if(main==0){return "";};return str;};
  str+=" &gt; "+data[main].sub[sub].menu;
  return str;
 };
 function writeHeader()
 {
  if(data[main].sub.length)
  {
   document.write('<div class="menu">');
   clickmenu("sub",subMenuString());
   document.write('<div style="position:absolute;top:300px">');
   if(webinfo)
   {
    document.write('<h6><a href="embed.php?File=index.html">Oxford Radiocarbon Accelerator Unit<\/a><\/h6>');
    document.write('<h6><a href="http://www.radiocarbondating.com/">Waikato Radiocarbon Dating Laboratory<\/a><\/h6>');
   }
   else
   {
    if(nerc)
    {
     document.write('<h6><a href="http://www.gla.ac.uk/nercrcl/">NERC Radiocarbon Laboratory<\/a><\/h6>');
     document.write('<h6><a href="http://c14.arch.ox.ac.uk/">Oxford Radiocarbon Accelerator Unit<\/a><\/h6>');
    }
    else
    {
     document.write('<h6><a href="http://www.rlaha.ox.ac.uk">Research Laboratory for        Archaeology<br>and the History of Art<\/a><\/h6>');
    };
   };
   document.write('<h6>');
   document.writeln("Version "+header.version);
   document.writeln("Issued "+
            header.issued.getDate().toString()+"/"+
            (header.issued.getMonth()+1).toString()+"/"+
            header.issued.getFullYear()); 
   document.write('<\/h6><\/div>');
   document.write('<\/div>');
   document.write('<div class="menuBody">');
  }
  else
  {
   document.write('<div class="body">');
  };
  dataSaved=data;
  data=false;
 };
 function writeFooter()
 {
  data=dataSaved;
  dataSaved=false;
  document.write('<\/div>');
 };
