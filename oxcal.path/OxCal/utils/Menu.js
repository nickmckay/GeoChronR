// (c) CB Ramsey 2006
 var menuControl=new Object;
 menuControl.helpActive=false;
 menuControl.lastId="";
 menuControl.lastI=0;
 menuControl.lastTab=false;
 menuControl.menus=new Object;
 var menuBars=new Object;
 var clickMenus=new Object;

 function toggleHelp()
 {
  menuControl.helpActive=!menuControl.helpActive;
  makeActive(menuControl.lastId,menuControl.lastI,menuControl.lastTab);
 };
 function overHelp()
 {
  if(!menuControl.helpActive)
  {
   document.getElementById("helpButton").src="../img/HelpOver.gif"
  };
 };
 function outHelp()
 {
  if(!menuControl.helpActive)
  {
   document.getElementById("helpButton").src="../img/Help.gif"
  };
 };
/* function outButton(b)
 {
  b.firstChild.src="../img/Button.gif";
 };
 function overButton(b)
 {
  b.firstChild.src="../img/ButtonMid.gif";
 };
 function downButton(b)
 {
  b.firstChild.src="../img/ButtonDeep.gif";
 };
 function upButton(b)
 {
  b.firstChild.src="../img/Button.gif";
 };*/
 function outTButton(b)
 {
  b.className="button";
 };
 function downTButton(b)
 {
  b.className="buttonDown";
 };
 function upTButton(b)
 {
  b.className="button";
 };
 function Button(action,image)
 {
  document.writeln("<div class='button' onclick='"+action+"'>"+
   "<img style='position:relative;top:-1px;left:-1px' src='"+image+"'\/><\/div>");
/*  document.writeln("<div class='button' onmouseover='overButton(this)' "+
   "onmouseout='outButton(this)' onmousedown='downButton(this)' onmouseup='upButton(this);"+action+"'>"+
   "<img src='../img/Button.gif' \/><img style='position:relative;top:-23px' src='"+image+"'\/><\/div>");*/
/*  document.writeln("<button style='height:25px;width:32px;background-color:rgba(255,255,255,0.5);border-width:1px;-moz-border-radius: 4px;-webkit-border-radius: 4px' onclick='"+action+"'>"+
   "<img style='position:relative;top:-1px;left:-4px;height:22px;width:26px' src='"+image+"'\/>"+
   "</button>");*/
 };
 function dynOverButton(){overButton(this);};
 function dynOutButton(){outButton(this);};
 function dynDownButton(){downButton(this);};
 function dynUpButton(){upButton(this);eval(this.action);};
 function dynamicButton(action,image)
 {
  var dv,im;
  dv=document.createElement("DIV");
  dv.classname='button';
  dv.onmouseover=dynOverButton;
  dv.onmouseout=dynOutButton;
  dv.onmousedown=dynDownButton;
  dv.onmouseup=dynUpButton;
  dv.action=action;
  im=document.createElement("IMG");
  im.src='../img/Button.gif';
  dv.appendChild(im);
  im=document.createElement("IMG");
  im.src=image;
  im.style.position='relative';
  im.style.left='-28px';
  dv.appendChild(im);
  return dv;  
 };
 function downRocker(b)
 {
  b.firstChild.src="../img/RockerDown.gif";
 };
 function upRocker(b)
 {
  b.firstChild.src="../img/Rocker.gif";
 };
 function Rocker(map)
 {
  document.writeln("<div class='rocker'>"+
   "<img class='button' style='padding:0px; margin :0px;border:0px' src='../img/Rocker.gif' usemap='#"+map+"'\/><\/div>");
 };
 function IntegralTableButton(action,text)
 {
  if(text.indexOf(".gif")>0)
  {
   document.writeln("<td class='button' style='padding:0px; margin :0px'"+
    "onmousedown='downTButton(this)' onmouseup='upTButton(this);"+action+"'>"+
    "<img class='button' src='"+text+"'\/><\/td>");
  }
  else
  {
   document.writeln("<td class='button' style='padding:2px'"+
    "onmousedown='downTButton(this)' onmouseup='upTButton(this);"+action+"'>"+
    text+"<\/td>");
  };
 };
 function TableButton(action,text)
 {
  if(text.indexOf(".gif")>0)
  {
   document.write("<td>");
   Button(action,text);
   document.writeln("<\/td>");
  }
  else
  {
   document.writeln("<td><input type='button' onclick='"+action+"' value='"+text+"'\/><\/td>");
  };
 };
 function TableInput(action,id)
 {
  document.writeln("<td style='padding:0px;text-align: center;' ><input type='text' id='"+id+"' size='5' onchange='"+action+"'><\/td>");
 };
 function helpButton()
 {
  document.writeln("<div class='helpButton'>");
  Button("toggleHelp()","../img/Help.gif");
  document.writeln("<\/div>");
 };
 function brightLight(td)
 {
  if(td.active){return;};
  td.className="menuBright";
 };
 function highLight(td)
 {
  if(td.active){return;};
  td.className="menuHigh";
 };
 function lowLight(td)
 {
  if(td.active){return;};
  td.className="";
 };
 function forwardLight(td)
 {
  if(td.active){return;};
  td.className="menuForward";
 };
 function touchLight(td)
 {
  if(td.active){return;};
  td.className="menuTouch";
 };
 function backLight(td)
 {
  if(td.active){return;};
  td.className="menuBack";
 };
 function makeHighlight(id,i)
 {
  menuControl.lastId=id;
  menuControl.lastI=i;
  var men;
  var j;
  any=false;
  for(j=0;men||(j<1);j++)
  {
   men=document.getElementById(id+"Menu"+j);
   if(men)
   {
    if(menuControl.menus[id]==j)
    {
     // active item - leave alone
    }
    else
    {
     if(j==i)
     {
      men.className="menuHigh";
      any=true;
     }
     else
     {
      men.className="";
     };
    };
   };
  };
 };
 function makeActive(id,i,tab)
 {
  menuControl.lastId=id;
  menuControl.lastI=i;
  menuControl.lastTab=tab;
  var men,opt,any,extr,hlp;
  var j;
  any=false;
  menuControl.menus[id]=i;
  for(j=0;men||opt||extr||hlp||(j<1);j++)
  {
   men=document.getElementById(id+"Menu"+j);
   opt=document.getElementById(id+j);
   extr=document.getElementById(id+"Extra"+j);
   hlp=document.getElementById(id+"Help"+j);
   if(men)
   {
    if(j==i)
    {
     if(tab)
     {
	  men.className="menuForward";
     }
     else
     {
	  men.className="menuBright";
	 };
	 men.active=true;
     any=true;
    }
    else
    {
     if(tab)
     {
	  men.className="menuBack";
     }
     else
     {
	  men.className="";
	 };
	 men.active=false;
    };
   };
   if(opt)
   {
    if(j==i)
    {
     opt.style.display="block";
    }
    else
    {
     opt.style.display="none";
    };
   };
   if(hlp)
   {
    if((j==i) && menuControl.helpActive)
    {
     switch(hlp.tagName)
     {
     case "SPAN":
      hlp.style.display="inline";
      break;
     default:
      hlp.style.display="block";
      break;
     };
    }
    else
    {
     hlp.style.display="none";
    };
   };
   if(extr)
   {
    if(j==i)
    {
     extr.style.display="block";
    }
    else
    {
     extr.style.display="none";
    };
   };
  };
  men=document.getElementById(id+"MenuLine");
  if(men)
  {
   men.style.height="1px";
   if(tab)
   {
    if(any)
    {
     men.className="menuForward";
    }
    else
    {
     men.className="";
    };
   }
   else
   {
    if(any)
    {
     men.className="menuBright";
    }
    else
    {
     men.className="";
    };
   };
  };
 };
 function menu(id,str,tab,internal)
 {
  var items=str.split("|");
  var i;
  var len=items.length;
  menuControl.menus[id]=-1;
  if(tab)
  {
   document.writeln("<table class='menutab'><tr>");
  }
  else
  {
   if(typeof(internal)!="undefined")
   {
    document.writeln("<table class='menubar'><tr style='height:30px'>");
   }
   else
   {
    document.writeln("<table class='menubar'><tr>");
   };
  };
  for(i=0;i<len;i++)
  {
   if(items[i].length>0)
   {
    if(tab)
    {
     document.writeln("<th id='"+id+"Menu"+i+"' onmouseover='touchLight(this)' onmouseout='backLight(this)' onclick='makeActive(\""+id+"\","+i+",true)'>"+items[i]+"<\/th>");
    }
    else
    {
     document.writeln("<th id='"+id+"Menu"+i+"' onmouseover='highLight(this)' onmouseout='lowLight(this)' onclick='makeActive(\""+id+"\","+i+")'>"+items[i]+"<\/th>");
    };
   }
   else
   {
    document.writeln("<th style='text-align:center' id='"+id+"Menu"+i+"'>|</th>");
   };
  };
  if(!tab)
  {
   document.writeln("<th style='width:90%'>&nbsp;<\/th>");
  };
  if(typeof(internal)!="undefined")
  {
   document.writeln("<\/tr>");
   document.writeln("<tr>");
   for(i=0;i<len;i++)
   {
    document.write("<td style='padding:0px;height:0px'>");
    document.write("<div id='main"+i+"' class='optionInternal'>");
    internal(i);
    document.write("<\/div><\/td>");
   };
   document.writeln("<\/tr>");
  }
  else
  {
   document.writeln("<\/tr><tr><td id='"+id+"MenuLine' colspan="+i+" style='height:1px'> <\/td><\/tr>");
  };
  document.writeln("<\/table>");
  makeActive(id,-1,tab);
 };
 function clickmenu(id,str,uselabel,barstyle)
 {
  var items=str.split("|");
  var i;
  var len=items.length;
  menuControl.menus[id]=-1;
  if(barstyle)
  {
   document.writeln("<table class='menubar'><tr>");
   for(i=0;i<len;i++)
   {
    if(items[i].length>0)
    {
     if(uselabel)
     {
      document.writeln("<th id='"+id+"Menu"+i+"' "+ 
	   "onmouseover='highLight(this)' "+
	   "onmouseout='lowLight(this)' "+
       "onclick='lowLight(this);"+id+"Click(\""+items[i]+"\")'>"+items[i]+"<\/th>");
     }
     else
     {
      document.writeln("<th id='"+id+"Menu"+i+"' "+
 	   "onmouseover='highLight(this)' "+
	   "onmouseout='lowLight(this)' "+
       "onclick='lowLight(this);"+id+"Click("+i+")'>"+items[i]+"<\/th>");
     };
    }
    else
    {
     document.writeln("<th id='"+id+"Menu"+i+"'>|<\/th>");
    };
   };
   document.writeln("<\/tr><tr><td id='"+id+"MenuLine' colspan="+i+" style='height:1px'> </td></tr>");
  }
  else
  {
   document.writeln("<table class='menuDrop'>");
   for(i=0;i<len;i++)
   {
    if(items[i].length>0)
    {
     if(uselabel)
     {
      document.writeln("<tr><td id='"+id+"Menu"+i+"' "+ 
	   "onmouseover='highLight(this)' "+
	   "onmouseout='lowLight(this)' "+
       "onclick='lowLight(this);"+id+"Click(\""+items[i]+"\")'>"+items[i]+"<\/td><\/tr>");
     }
     else
     {
      document.writeln("<tr><td id='"+id+"Menu"+i+"' "+
 	   "onmouseover='highLight(this)' "+
	   "onmouseout='lowLight(this)' "+
       "onclick='lowLight(this);"+id+"Click("+i+")'>"+items[i]+"<\/td><\/tr>");
     };
    }
    else
    {
     document.writeln("<tr><td id='"+id+"Menu"+i+"' class='menu'><hr/><\/td><\/tr>");
    };
   };
  };
  document.writeln("<\/table>");
  makeActive(id,-1);
 };
