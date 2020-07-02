// (c) CB Ramsey 2005
 function isSimple(it)
 {
  switch(typeof(it))
  {
  case "number":
  case "string":
  case "boolean":
   return true;
  case "object":
   if(it.constructor==Date)
   {
    return true;
   };
  };
  return false;
 };
 function displaySimple(it)
 {
  switch(typeof(it))
  {
  case "number":
   return it.toString();
  case "string":
   return '"'+it.toString().replace(/\"/g,'\\"').replace(/\n/g,'\\n')+'"';
  case "boolean":
   return it.toString();
  case "object":
   if(it.constructor==Date)
   {
    return "new Date("+it.getTime()+")";
   };
  };
  return false;
 };
 function checkForArrayConstructorForObject(it)
 {
  if(typeof(it)=="object")
  {
   if(it.constructor==Array)
   {
    if(it.length<1)
    {
     for(el in it)
     {
      return true;
     };
    };
   };
  };
  return false;
 };
 function JSONdisplay(it,name)
 {
  return name+"="+JSON.stringify(it)+";";
 };
 function displayItem(it,name,noheader)
 {
  var disp="";
  var el;
  var i;
  if(typeof(it)=="undefined"){return "";};
  if(!(noheader || checkForArrayConstructorForObject(it)))
  {
   return JSONdisplay(it,name);
  };
  if(isSimple(it))
  {
   disp += name + "=" + displaySimple(it) + ";\n";
  }
  else
  {
   if(typeof(it)!="object"){return "";};
// check if it is an array
   if((it.constructor==Array)&&(!checkForArrayConstructorForObject(it)))
   {
    if((it.length>1) && isSimple(it[it.length-1]))
    {
     disp += name + "= new Array(" + displaySimple(it[0]);
     for(i=1;i<it.length;i++)
     {
      disp+= ", " + displaySimple(it[i]);
     };
     disp += ");\n";
    }
    else
    {
     if(!noheader)
     {
      disp += name + "= new Array();\n";
     };
     for(el in it)
     {
      if(isNaN(el))
      {
       disp+=displayItem(it[el],name + "['" + el + "']");
      }
      else
      {
       disp+=displayItem(it[el],name + "[" + el + "]");
      };
     };
    };
   }
   else
   {
    if(!noheader)
    {
     disp += name + "= new Object();\n";
    };
    for(el in it)
    {
     if(isNaN(el))
     {
      disp+=displayItem(it[el],name + "." + el);
     }
     else
     {
      disp+=displayItem(it[el],name + "[" + el + "]");
     };
    };
   };
  };
  return disp;
 };
 function duplSimple(it)
 {
  switch(typeof(it))
  {
  case "number":
  case "string":
  case "boolean":
   return it;
  case "object":
   if(it.constructor==Date)
   {
    return new Date(it.getTime());
   };
  };
  return false;
 };
 function duplItem(it)
 {
  var rtn,el;
  if(typeof(it)=="undefined"){return false;};
  if(isSimple(it))
  {
   return duplSimple(it);
  }
  else
  {
   if(typeof(it)!="object"){return false;};
// check if it is an array
   if(it.constructor==Array)
   {
    rtn = new Array();
    for(el in it)
    {
     rtn[el]=duplItem(it[el]);
    };
   }
   else
   {
    rtn = new Object();
    for(el in it)
    {
     rtn[el]=duplItem(it[el]);
    };
   };
  };
  return rtn;
 };
 function duplRemoteItem(win,it)
 {
  return JSON.parse(win.JSON.stringify(it));
 };
