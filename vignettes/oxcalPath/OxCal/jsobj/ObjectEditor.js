// (c) CB Ramsey 2007

var actionSpec;
var sortKey;
var sortDirn;
var exchangeSpec;
var newExchange=false;
var defaultBritish=false;
var exchangeWindow;
var timerNote;
var editColorSpec;

// code for backward compatability to deal with deprecated escape

function encode(str)
{
 return encodeURIComponent(str).replace(/\!/g,'%21').replace(/\~/g,'%7E').replace(/\'/g,'%27').replace(/\(/g,'%28').replace(/\)/g,'%29');
};

function findJSONDate(str)
{
 var s,d,t,dt;
 s=str.split("T");
 if(s.length!=2){return str;};
 d=s[0].split("-");
 if(d.length!=3){return str;};
 if(str.indexOf("Z")==-1){return str;};
 t=s[1].replace("Z","").split(":");
 if(t.length!=3){return str;};
 dt=new Date(str);
 return dt;
};

function findJSONDates(obj)
{
 switch(typeof(obj))
 {
 case "string":
  return findJSONDate(obj); // catches Dates in JSON
 case "object":
  for(el in obj)
  {
   obj[el]=findJSONDates(obj[el]);
  };
  return obj;
 };
 return obj;
};

function myParseJSON(str)
{
 var obj=JSON.parse(str);
 return findJSONDates(obj);
};

function decode(str)
{
 try
 { 
  return decodeURIComponent(str);
 }
 catch(err)
 {
  return unescape(str);
 };
};

function decodeObject(obj)
{
 switch(typeof(obj))
 {
 case "string":
  return findJSONDate(decode(obj)); // catches Dates in JSON
 case "object":
  for(el in obj)
  {
   obj[el]=decodeObject(obj[el]);
  };
  return obj;
 };
 return obj;
};

// main itemspec object types

function itemSpec(id,prmpt,typ,reqd)
{
 this.name=id;
 this.prompt=prmpt;
 this.required=reqd;
 this.type=typ;
 this.width=null;
 this.height=null;
 this.options=new Array();
 this.children=new Array();
 this.elements=new Array();
 this.expand=true;
 this.noheader=false;
 this.dialog=false;
 this.edit=false;
 this.className="objObject";
 this.repeat=20;
};

function copyFunction(f,to_data)
{
 if(to_data && (typeof(f)=='function'))
 {
 }
 else
 {
  return f;
 };
};

function copyItemSpec(to,from,to_data,from_data)
{
 var i;
 /* main required items */
 to.name=from.name;
 to.prompt=from.prompt;
 to.required=from.required;
 to.type=from.type;
 to.className=from.className;
 /* main required items */
 to.expand=from.expand; // default true
 to.repeat=from.repeat;
 if(from.width){to.width=from.width};
 if(from.height){to.height=from.height};
 if(from.inline){to.inline=from.inline;};
 if(from.tabbed){to.tabbed=from.tabbed;};
 if(from.readonly){to.readonly=from.readonly;};
 if(from.changed){to.changed=from.changed;};
 if(from.special){to.special=from.special;};
 if(from.hidden){to.hidden=from.hidden;};
 if(from.popup){to.popup=from.popup;};
 if(from.database){to.database=from.database;};
 if(from.outline){to.outline=from.outline;};
 if(from.hint){to.hint=from.hint;};
 /* optional arrays */
 if(from.options && from.options.length){ to.options=from.options; };
 if(from.optionsDisplay && from.optionsDisplay.length){to.optionsDisplay=from.optionsDisplay;};
 if(from.optionsIndex && from.optionsIndex){to.optionsIndex=from.optionsIndex;}; 
 /* functions */
 if(from.checker){to.checker=copyFunction(from.checker,to_data);};
 if(from.changer){to.changer=copyFunction(from.changer,to_data);};
 if(from.editor){to.editor=copyFunction(from.editor,to_data);};
 if(from.action){to.action=copyFunction(from.action,to_data);};
 if(from.calculator){to.calculator=copyFunction(from.calculator,to_data);};
 /* children */
 to.children=new Array();
 if(from.children && from.children.length)
 {
  for(i=0;i<from.children.length;i++)
  {
   if(to_data)
   {
    to.children[i]=new Object();
    if(from_data)
    {
     copyItemSpec(to.children[i],from.children[i],true,true);
    }
    else
    {
     copyItemSpec(to.children[i],from.children[i],true,false);
    };
   }
   else
   {
    if(from_data)
    {
     to.children[i]=itemSpecFromData(from.children[i]);
    }
    else
    {
     to.children[i]=from.children[i].duplItemSpec();
    };
   }
  };
 };
 return to;
};

function itemSpecFromData(d)
{
 var s=new itemSpec("","","",true);
 copyItemSpec(s,d,false,true);
 return s;
};

itemSpec.prototype.toData=function()
{
 var o=new Object();
 copyItemSpec(o,this,true,false);
 return o;
};

itemSpec.prototype.toJSON=function()
{
 return JSON.stringify(this.toData());
};

itemSpec.prototype.duplItemSpec=function()
{
 var s=new itemSpec("","","",true);
 var i;
 copyItemSpec(s,this,false,false);
 return s;
};

function zeropad(num)  // zero-pad single-digit numbers (00...09)
{
 if (num < 10){return ("0" + num);};return num;
};

function myWriteDate(dt,tm,british)
{
 var d,m,y,h,n,s,str;
 d=zeropad(dt.getDate());
 m=zeropad(dt.getMonth()+1);
 y=dt.getFullYear();
 h=zeropad(dt.getHours());
 n=zeropad(dt.getMinutes());
 s=zeropad(dt.getSeconds());
 if((y==-1)&&(m==11)&&(d==30)){return "";};
 if(!y)
 {
  if((((m=='12')||(m=='01')) && (d='31'))||(!m || !d))
  {
   return "";
  };
 };
 if(british || (defaultBritish && (typeof(british)=='undefined')))
 {
  str=d+"/"+m+"/"+y;
 }
 else
 {
  str=y+"-"+m+"-"+d;
 };
 if(tm)
 str+=" "+h+":"+n+":"+s;
 return str;
};

function checkYear(yr)
{
 var y=Number(yr);
 if(y>=100){return y;};
 if((y>0)&&(y<70)){return y+2000;};
 if(y>=70){return y+1900;};
 return y;
};

function myReadDate(str,tm)
{
 var i,j,b;
 var dt=new Date();
 dt.getTime();
 dt.setHours(0);dt.setMinutes(0);dt.setSeconds(0);
 if(!str)
 { str="0000-00-00";};
 var s=str.split(/\s+/);
 for(i=0;i<s.length;i++)
 {
  b=s[i].split(":");
  if(b.length>1)
  {
   if(tm)
   {
    dt.setHours(b[0]);
    dt.setMinutes(b[1]);
   };
  };
  if(b.length>2)
  {
   if(tm)
   {
    dt.setSeconds(b[2]);
   };
  };
  b=s[i].split("/");
  if(b.length>2)
  {
   dt.setFullYear(checkYear(b[2]));
  };
  if(b.length>1)
  {
   dt.setDate(1);
   dt.setMonth(b[1]-1);
   dt.setDate(b[0]);
  };
  b=s[i].split("-");
  if(b.length>2)
  {
   dt.setDate(1);
   dt.setFullYear(checkYear(b[0]));
   dt.setMonth(b[1]-1);
   dt.setDate(b[2]);
  };
 };
 return dt;
};

var n=new itemSpec("","","",true);
itemSpec.prototype.createObject=function()
{
 switch(this.type)
 {
 case "Time":
 case "Date":
  return 0;
 case "Boolean":
  return false;
 case "Number":
  return 0;
 case "Text":case "TextArea":case "Pre":case "Color":
  return "";
 case "Object":
  return new Object();
 case "Array":
  return new Array();
 case "Action":case "Button":case "Page":
  return undefined;
 };
 return false;
};

itemSpec.prototype.appendChild=function(id,prmpt,typ,reqd)
{
 return this.children[this.children.length]=new itemSpec(id,prmpt,typ,reqd);
};

itemSpec.prototype.appendChildSpec=function(spec)
{
 return this.children[this.children.length]=spec;
};

itemSpec.prototype.appendComplexText=function(el,str)
{
   var sty,ar,i,j,pts,levs,lev,ctrl,ln;
   ar=str.toString().split("{");
   if(ar.length==1)
   {
    el.appendChild(document.createTextNode(str));
   }
   else
   {
    ln=0;levs=[];levs[0]=el;lev=0;
    for(i=0;i<ar.length;i++)
    {
     pts=ar[i].split("}");
     for(j=0;j<pts.length-1;j++)
     {
      if(lev<0){return el;};
      levs[lev].appendChild(document.createTextNode(pts[j]));
      lev--;
     };
     cntrl='';
     if(pts[j].length!=0)
     {
      switch(pts[j].charAt(pts[j].length-1))
      {
      case '_': 
      case '^':
       cntrl=pts[j].charAt(pts[j].length-1);
       pts[j]=pts[j].substr(0,pts[j].length-1);
       break;
      default :
       if(pts[j].lastIndexOf('\\')!=-1)
       {
        cntrl=pts[j].substr(pts[j].lastIndexOf('\\'))
        pts[j]=pts[j].substr(0,pts[j].lastIndexOf('\\'));
       };
       break;
      };
     };
     levs[lev].appendChild(document.createTextNode(pts[j]));
     if(i==ar.length-1){continue;};
     lev++;
     switch(cntrl)
     {
     case '^':
      levs[lev]=document.createElement("sup");
      break;
     case '_':
      levs[lev]=document.createElement("sub");
      break;
     case '\\textit': case '\\it':
      levs[lev]=document.createElement("i");
      break;
     case '\\textbf': case '\\bf':
      levs[lev]=document.createElement("b");
      break;
     default:
      levs[lev]=document.createElement("span");
      break;
     };
     levs[lev-1].appendChild(levs[lev]);
    };
   };
   return el;
};

itemSpec.prototype.complexTextNode=function(str)
{
 var el=document.createElement("span");
 this.appendComplexText(el,str);
 return el;
};

itemSpec.prototype.setDbRowStyle=function()
{
 var cls;
 if(this.parent)
 {
  if(this.index%2==0)
  {
   cls="objOdd";
  }
  else
  {
   cls="objEven";
  };
  if(this.object.created)
  {
   cls+="New";
  } 
  else
  {
   if(this.object.deleted)
   {
    cls+="Deleted"
   }
   else
   {
    if(this.object.changed)
    {
     cls+="Changed";
    };
   };
  };
  this.XHTML.className=cls;
 };
};

itemSpec.prototype.appendHint=function(el)
{
 var d;
 if(!this.hint){return;};
 el.title=this.hint;
};

itemSpec.prototype.createXHTML=function(par,edit,ind)
{
 var s,h,d,b,m,r,i,col_h,col_p,col_i,inline,outline,dl,div;
 inline=false;outline=false;
 this.childArea=false;
 this.popupContainer=false;
 if(par)
 {
  this.parent=par;
  switch(par.type)
  {
  case "Object":
   if(!par.object[this.name])
   {
    par.object[this.name]=this.createObject();
	if(this.options.length)
	{
	 if(this.type=="Number")
	 {
	  par.object[this.name]=0;
	 }
	 else
	 {
	  par.object[this.name]=decode(this.options[0]);
	 };
	};
   };
   this.object=par.object[this.name];
   break;
  case "Array":
   this.index=ind;
   if(!par.object[this.index])
   {
    par.object[this.index]=this.createObject();
	if(this.options.length)
	{
	 if(this.type=="Number")
	 {
	  par.object[this.index]=0;
	 }
	 else
	 {
	  par.object[this.index]=decode(this.options[0]);
	 };
	};
   };
   this.object=par.object[this.index];
   break;
  };
  inline=(par.inline && (!this.popup));
  outline=par.outline;
 };
 if(inline)
 {
  this.XHTML=document.createElement("TD");
 }
 else
 {
  this.XHTML=document.createElement("TR");
 };
 switch(this.type)
 {
 case "Time":
 case "Date":
 case "Boolean":
 case "Number":
 case "Text":case "TextArea":case "Pre":case "Action":case "Button":case "Page":case "Color":
  if(inline)
  {
   this.container=this.XHTML;
  }
  else
  {
   if(this.parent && (this.parent.type=="Array"))
   {
    if(edit)
	{
     dl=document.createElement("TD");
     this.deleter=this.control("deleter");
	 this.deleter.spec=this.parent;
	 this.deleter.index=this.index;
	 this.deleter.onclick=delArray;
     this.sorter=this.control("sorter");
     this.sorter.spec=this.parent;
     this.sorter.index=this.index;
     this.sorter.onmousedown=sortFromHere;
     this.sorter.onmouseup=sortToHere
     dl.className="objControl";
     dl.appendChild(this.deleter);
     dl.appendChild(document.createTextNode(" "));
     dl.appendChild(this.sorter);
	 this.XHTML.appendChild(dl);
	};
    d=document.createElement("TD");
	this.container=d;
	this.XHTML.appendChild(d);
   }
   else
   {
    h=document.createElement("TH");
    s=document.createElement("TH");
    switch(this.type)
    {
    case "Page":
      s.appendChild(document.createElement("HR"));
      s.colSpan=3;
      this.XHTML.appendChild(s);
      break;
    case "Button":
      this.XHTML.appendChild(s);
      break;
   default:
     if(this.type=="Action")
     {
	  switch(this.name)
	  {
	  case "delete":
       this.expander=this.control("deleter");
	   break;
	  case "edit":
       this.expander=this.control("edit");
	   break;
	  default:
       this.expander=this.control("action");
	   break;
	  };
      this.expander.spec=this;
      this.expander.onclick=doAction;
     }
     else
     {
      if((!this.object)&&(this.required))
      {
       this.expander=this.control("required");
      }
      else
      {
       this.expander=this.control("optional");
      };
     };
     if(!this.popup && this.parent && this.parent.tabbed)
     {
      h.appendChild(this.expander);
      this.index=ind;
      h.spec=this.parent;
      h.index=this.index;
      h.onclick=raiseTab;
      this.tab=h;
      if(this.index==0)
      {
       h.className="objTabLeftActive";
      }
      else
      {
       h.className="objTabLeft";
      };
     }
     else
     {
      s.appendChild(this.expander);
      this.XHTML.appendChild(s);
     };
     if(this.type=="Action")
     {
	  if(!this.object)
	  {
	   this.appendComplexText(h," "+this.prompt);
       h.className="objLink";
       h.spec=this;
       h.onclick=doAction;
	   h.onmouseover=makeUL;
	   h.onmouseout=unMakeUL;
	  };
	 }
	 else
	 {
	  this.appendComplexText(h," "+this.prompt);
      this.appendHint(h);
	 };
	 if(this.parent && (this.parent.type=="Array"))
	 {
      h.appendChild(document.createTextNode(" "+(this.index+1).toString()));
	 };
	 break;
	};
    d=document.createElement("TD");
    this.container=d;
    if(this.type!="Page"){this.XHTML.appendChild(h);};
    if(!this.popup && this.parent && this.parent.tabbed)
    {
     d.rowSpan=this.parent.children.length;
     this.parent.childArea.style.borderCollapse='separate';
     this.parent.tabindex=0;
     if(this.index==0)
     {
      this.XHTML.appendChild(d);
     };
    }
    else
    {
     if(this.type!="Page"){this.XHTML.appendChild(d);};
    };
   };
  };
  break;
 case "Object":
 case "Array":
  if(!inline)
  {
   if(this.inline && this.parent && (this.parent.type=="Array"))
   {
    this.container=this.XHTML;
	break;
   };
   s=document.createElement("TH");
  };
  if(this.parent && (this.parent.type=="Array") && edit && (!this.readonly))
  {
   this.deleter=this.control("deleter");
   this.deleter.spec=this.parent;
   this.deleter.index=this.index;
   this.deleter.onclick=delArray;
   this.sorter=this.control("sorter");
   this.sorter.spec=this.parent;
   this.sorter.index=this.index;
   this.sorter.onmousedown=sortFromHere;
   this.sorter.onmouseup=sortToHere
  };
  this.expander=false;
  if((!this.noheader && this.parent && !this.parent.tabbed)||(!this.expand))
  {
   if(this.expand)
   {
    this.expander=this.control('contract');
   }
   else
   {
    this.expander=this.control('expander');
   };
   this.expander.spec=this;
   this.expander.onclick=toggleExpand;
  };
  if(inline)
  {
   if(this.parent.tabbed)
   {
    this.className="objObjectInTabTop";
    this.XHTML.style.padding="0px";
   }
   else
   {
    if(this.parent && (this.parent.type=="Array"))
    {
     h=this.complexTextNode(this.prompt+" "+(this.index+1).toString());
    }
    else
    {
     h=this.complexTextNode(this.prompt);
    };
    this.appendHint(h);
   };
  }
  else
  {
   s.vAlign="top";
   h=document.createElement("TH");
   if(this.parent && this.parent.tabbed)
   {
    this.className="objObjectInTabLeft";
    h.style.padding="0px";
   };
   h.colSpan=3;
   div=document.createElement("DIV");
   this.appendComplexText(div,this.prompt);
   if(this.parent && (this.parent.type=="Array"))
   {
    div.appendChild(document.createTextNode(" "+(this.index+1).toString()));
   };
   this.appendHint(div);
   if(!this.popup && this.parent && this.parent.tabbed && (this.parent.type=="Object"))
   {
    s.appendChild(div);
    h.rowSpan=this.parent.children.length;
    this.index=ind;
    s.spec=this.parent;
    s.index=this.index;
    s.onclick=raiseTab;
    this.tab=s;
    if(this.index==0)
    {
     s.className="objTabLeftActive";
    }
    else
    {
     s.className="objTabLeft";
    };
    this.parent.tabindex=0;
    this.parent.childArea.style.borderCollapse='separate';
   }
   else
   {
    if(!this.parent && !this.readonly && !this.dialog)
    {
     this.edswitch=this.control('edit');
     this.edswitch.onclick=this.toggleEdit.bind(this);
     this.edswitch.ondblclick=this.makeEdit.bind(this,true);
     s.appendChild(this.edswitch);
    };
    if(this.expander){s.appendChild(this.expander);};
    h.appendChild(div);
   };
  };
  d=document.createElement("TABLE");
  d.className=this.className;
  if(this.outline)
  {
   d.className="objOutline";
  };
  if(this.dialog)
  {
   d.className="objDialog";
  };
  if((d.className=="objObject") && (this.noheader || !this.parent))
  {
   d.className="objBlank";
  };
  if(this.expand)
  {
   d.style.display="inline-block";
  }
  else
  {
   d.style.display="none";
  };
  if(this.width){d.style.width=this.width;};
  if(this.height){d.style.height=this.height;};
  this.childArea=d;
  b=document.createElement("TBODY");
  if(this.inline || (this.type=="Array"))
  {
   if(this.inline)
   {
    if((this.type=="Object")  && (!this.noheader))
	{
     r=document.createElement("TR");
     if(this.tabbed){d.style.borderCollapse='separate';this.tabindex=0;};
     for(i=0;i<this.children.length;i++)
     {
      if(this.children[i].hidden || this.children[i].popup){continue;};
      col_h=document.createElement("TH");
      if(this.tabbed)
      {
       col_h.spec=this;
       col_h.index=i;
       col_h.onclick=raiseTab;
       this.children[i].tab=col_h;
       if(i==0)
       {
        col_h.className="objTabTopActive";
       }
       else
       {
        col_h.className="objTabTop";
       };
      };
      if(this.children[i].type!="Button")
      {
	   col_i=false;
	   if(this.children[i].required && (!this.object[this.children[i].name]))
	   {
	    col_i=true;
	   };
	   if(col_i)
	   {
	    col_i=this.control('required');
	   }
	   else
	   {
	    col_i=this.control('optional');
	   };
	   col_i.spec=this.children[i];
	   col_h.appendChild(col_i);
	   this.appendComplexText(col_h," "+this.children[i].prompt);
	  };
	  this.children[i].appendHint(col_h);
	  r.appendChild(col_h);
     };
     b.appendChild(r);
	};
    r=document.createElement("TR");
    b.appendChild(r);
    this.container=r;
   }
   else
   {
    this.container=b;
   };
  }
  else
  {
   this.container=b;
  };
  this.childArea.appendChild(b);
  if(inline)
  {
   if(this.deleter)
   {
    this.XHTML.appendChild(this.deleter);
    this.XHTML.appendChild(document.createTextNode(" "));
    this.XHTML.appendChild(this.sorter);
    this.XHTML.appendChild(document.createTextNode(" "));
   };
   if(this.expander)
   {
    this.XHTML.appendChild(this.expander);
    this.XHTML.vAlign="top";
    this.XHTML.appendChild(document.createTextNode(" "));
    this.XHTML.appendChild(h);
    this.XHTML.appendChild(document.createElement("BR"));
   };
   this.XHTML.appendChild(d);
  }
  else
  {
   h.appendChild(d);
   if(this.deleter)
   {
    dl=document.createElement("TD");
	dl.appendChild(this.deleter);
	dl.appendChild(this.sorter);
	this.XHTML.appendChild(dl);
   };
   if((!this.outline)&&(!outline))
   {
    this.XHTML.appendChild(s);
   };
   if(!this.popup && this.parent && this.parent.tabbed && (this.parent.type=="Object"))
   {
    if(ind==0){this.XHTML.appendChild(h);};
   }
   else
   {
    this.XHTML.appendChild(h);
   };
  };
  break;
 };
 this.edit=edit;
 this.fillContainer();
 return this.XHTML;
};

itemSpec.prototype.showTop=function(show)
{
 if(this.parent){return;};
 if(!this.XHTML){return;};
 if(show)
 {
  this.XHTML.style.display='inline-block';
 }
 else
 {
  this.XHTML.style.display='none';
 };
};

itemSpec.prototype.setParent=function()
{
 switch(this.parent.type)
 {
 case "Object":
  this.parent.object[this.name]=this.object;
  break;
 case "Array":
  this.parent.object[this.index]=this.object;
  break;
 };
};

itemSpec.prototype.fillContainer=function()
{
 var i,j,m,r,d,col_h,col_p,col_i,dl,s,note,noteList,preList,li,t;
 var edit=this.edit;
 var action=this.action;
 if(this.readonly)
 {
  if(this.name=="burn_corr"){alert("a");};
  if(this.type!="Array"){edit=false;};
 };
 if(this.calculator)
 {
  eval(this.calculator.toString());
  this.setParent();
 };
 if(!this.container)
 {
  if(!this.parent.object[this.name])
  {
   this.parent.object[this.name]=this.createObject();
  };
  this.object=this.parent.object[this.name];
  return;
 };
 if((!this.edit || this.readonly)&&(this.special))
 {
  eval(this.special.toString());
  return;
 };
 this.showTop(false);
 if(edit && (!this.readonly) && this.editor)
 {
  this.edswitch=this.control('edit');
  this.edswitch.spec=this;
  this.edswitch.index=this.index;
  this.edswitch.type="Edit";
  this.edswitch.onclick=doEdit;
  this.container.appendChild(this.edswitch);
  this.container.appendChild(document.createTextNode(" "));
  action=false;
  edit=false;
  if(this.special)
  {
   eval(this.special.toString());
   return;
  };
 };
 if(this.options.length>0)
 {
  if(edit && (!this.readonly))
  {
   m=document.createElement("SELECT");
   if(this.parent.inline){m.style.width='auto';};
   if(this.width){m.style.width=(this.width*8)+"px";};
   for(i=0;i<this.options.length;i++)
   {
    if(this.optionsDisplay)
    {
     m.options[i]=new Option(decode(this.optionsDisplay[i]));
    }
    else
    {
     m.options[i]=new Option(decode(this.options[i]));
    };
	if(this.type=="Number")
	{
	 if(typeof(this.options[0])=='number')
	 {
	  if(this.options[i]==this.object)
	  {
	   m.selectedIndex=i;
	  };
	 }
	 else
	 {
	  if(i==this.object)
	  {
	   m.selectedIndex=i;
	  };
	 };
	}
	else
	{
	 if(decode(this.options[i])==this.object)
	 {
	  m.selectedIndex=i;
	 };
	};
   };
   m.spec=this;
   m.onchange=getSelect;
  }
  else
  {
   if(this.type=="Number")
   {
    m=this.complexTextNode(decode(this.options[Number(this.object)]));
   }
   else
   {
    if(this.optionsIndex && this.optionsDisplay && !isNaN(this.optionsIndex[this.object]))
    {
     m=this.complexTextNode(this.optionsDisplay[this.optionsIndex[this.object]]);
    }
    else
    {
     m=this.complexTextNode(this.object);
    };
   };
   if(action)
   {
    this.container.className="objLink";     
    this.container.onclick=doAction;
    this.container.spec=this;
   };
  };
  this.container.appendChild(m);
 }
 else
 {
  switch(this.type)
  {
  case "TextArea" :case "Pre":
   if(edit && (!this.readonly))
   {
    m=document.createElement("TEXTAREA");
    if(this.width)
    {
     m.style.width=this.width*8+"px";
    };
	if(this.height)
	{
	 m.style.height=this.height*16+"px";
	};
    m.value=decode(this.object);
    m.spec=this;
    m.onchange=getText;
    this.container.appendChild(m);
   }
   else
   {
    if(this.type=="Pre")
    {
     m=document.createElement("PRE");
     // use windows line endings to pander to IE
     m.appendChild(document.createTextNode(decode(this.object).replace(/\n/g,"\n\r")));
     this.container.appendChild(m);
    }
    else
    {
     preList=decode(this.object).split("\n\n");
     for(i=0;i<preList.length;i++)
     {
      m=document.createElement("P");
	  m.appendChild(this.complexTextNode(preList[i]));
      this.container.appendChild(m);
	 };
    };
   };
   break;
  case "Action":
   if((this.parent.type=="Array")||(this.parent.inline))
   {
    switch(this.name)
    {
	case "delete":
     this.expander=this.control('deleter');
	 break;
	case "edit":
     this.expander=this.control('edit');
	 break;
	default:
     this.expander=this.control('action');
	 break;
	};
    this.expander.spec=this;
    this.expander.onclick=doAction;
    this.container.appendChild(this.expander);
	this.container.appendChild(document.createTextNode(" "));
   };
  case "Button": // and Action continued
   if(edit && (!this.readonly))
   {
    m=document.createElement("INPUT");
    m.type="text";
    if(this.width)
    {
     m.style.width=this.width*8+"px";
    };
    if(!this.object){this.object="";};
    m.value=decode(this.object.toString());
    m.spec=this;
    m.onchange=getText;
   }
   else
   {
    if(this.type=="Button")
    {
     m=document.createElement("INPUT");
     m.style.width="100%";
     if(this.width)
     {
      m.style.width=this.width*8+"px";
     };
     if(this.submit)
     {
      m.type="Submit";
     }
     else
     {
      m.type="Button";
     };
     m.spec=this;
     m.onclick=doAction;
     if((!this.object)||(this.readonly))
     {
      m.value=decode(this.prompt.toString());
     }
     else
     {
      m.value=decode(this.object.toString());
     };
    }
    else
    {
	 m=document.createElement("SPAN");
     if(this.object)
	 {
       m.className="objLink";
       m.spec=this;
       m.onclick=doAction;
	   m.onmouseover=makeUL;
	   m.onmouseout=unMakeUL
       s=this.complexTextNode(decode(this.object.toString()));
	   m.appendChild(s);
	 };
	};
   };
   this.container.appendChild(m);
   break;
  case "Number":
  case "Text":
   if(edit && (!this.readonly))
   {
    m=document.createElement("INPUT");
    m.type="text";
    if(this.width)
    {
     m.style.width=this.width*8+"px";
    }
    else
    {
     if(this.type=="Number"){m.style.width="40px";};
    };
    m.value=decode(this.object.toString());
    m.spec=this;
    m.onchange=getText;
   }
   else
   {
    m=this.complexTextNode(decode(this.object.toString()));
    if(action)
    {
     this.container.className="objLink";
     this.container.onclick=doAction;
     this.container.spec=this;
    };
   };
   this.container.appendChild(m);
   break;
  case "Time":
  case "Date":
   if(this.object.constructor==Date)
   {
    s=myWriteDate(this.object,this.type=="Time");
   }
   else
   {
    s="";
   };
   if(edit && (!this.readonly))
   {
    m=document.createElement("INPUT");
    m.type="text";
    if(this.width)
    {
     m.style.width=this.width*8+"px";
    }
    else
    {
     m.style.width="80px";
    };
    m.value=s;
    m.spec=this;
    m.onchange=getText;
   }
   else
   {
    m=document.createTextNode(s);
    if(action)
    {
     this.container.className="objLink";
     this.container.onclick=doAction;
     this.container.spec=this;
    };
   };
   this.container.appendChild(m);
   break;
  case "Boolean":
   if(edit && (!this.readonly))
   {
    m=document.createElement("INPUT");
    m.type="checkbox";
	m.checked=false;
    if(this.object)
	{
	 m.defaultChecked=true;
	 m.checked=true;
	}
	else
	{
	 m.defaultChecked=false;
	 m.checked=false;
	};
    m.spec=this;
    m.onclick=getCheckBox;
   }
   else
   {
    m=this.elementWithClass("div","objIcon");
    if(this.object==true)
	{
 	 m.appendChild(this.elementWithClass("div","true"));
	}
	else
	{
 	 m.appendChild(this.elementWithClass("div","false"));
	};
   };
   this.container.appendChild(m);
   break;
  case "Color":
   this.container.style.textAlign="center";
   this.container.style.verticalAlign="middle";
   d=document.createElement("DIV");
   d.style.backgroundImage="url('../img/Check.png')";
   d.style.width="32px";
   d.style.height="16px";
   this.container.appendChild(d);
   m=document.createElement("DIV");
   m.style.backgroundColor=this.object;
   m.style.width="100%";
   m.style.height="100%";
   if(edit && (!this.readonly))
   {
    d.style.borderStyle="inset";
    this.container.onclick=changeColor;
    this.container.spec=this;
   };
   d.appendChild(m);
   break;
  case "Page":
//   this.container.appendChild(document.createElement("HR"));
   break;
  case "Array":
   if(!this.inline && (edit ||(this.children.length>1)||this.database))
   {
    r=document.createElement("TR");
    if(this.database){r.className='objHeader';};
    if(this.database)
    {
     col_h=document.createElement("TH");
     col_h.className="objLink";
     col_h.spec=this;
     col_h.onclick=selectArray;
 	 col_p=document.createTextNode("All");
	 col_h.appendChild(col_p);
	 r.appendChild(col_h);
    };
    col_h=document.createElement("TH");
    if(edit)
	{
     if(!this.readonly && !this.outline)
     {
      m=this.control('import');
	  m.spec=this;
	  m.onclick=doExchange;
	  col_h.appendChild(m);
      m=this.control('edit');
	  m.spec=this;
	  m.onclick=function(){
	   this.propagateEdit=!this.propagateEdit;
	   this.refillContainer();
	  }.bind(this,true);
	  col_h.appendChild(m);
	  col_h.style.minWidth="48px";
	 };
	}
	else
	{
	 if(!this.outline)
	 {
	  this.noteshow=this.control('expander');
	  col_h.appendChild(this.noteshow);
	  this.noteshow.spec=this;
	  this.noteshow.onclick=showNotes;
	  this.noteshow.title='Show all notes';
	  this.noteshow.style.display="none";
	 };
	};
	r.appendChild(col_h);
    for(i=0;i<this.children.length;i++)
    {
     if(this.children[i].hidden || this.children[i].popup){continue;};
     col_h=document.createElement("TH");
	 col_i=false;
 	 if(this.children[i].required)
	 {
	  if(this.object.length)
	  {
	   for(j=0;j<this.object.length;j++)
	   {
	    if(!this.object[j])
		{
	     col_i=true;
		}
		else
		{
		 if(!this.object[j][this.children[i].name] 
		    && (this.children.length!=1))
	     {
	      col_i=true;
	     };
		};
	   };
	  }
	  else
	  {
	   if(this.required)
	   {
	    col_i=true;
	   };
	  };
	 };
	 if(this.edit)
	 {
	  if(col_i)
	  {
	   col_i=this.control('required');
	  }
	  else
	  {
	   col_i=this.control('sorter');
	  };
	  col_i.spec=this;
	  col_i.usekey=(this.children.length>1)||this.database;
	  col_i.key=this.children[i];
	  col_i.onclick=sortArray;
	 }
 	 else
 	 {
	  if(col_i)
	  {
	   col_i=this.control('required');
	  }
	  else
	  {
	   col_i=this.control('optional');
	  };
 	  if(this.database)
 	  {
 	   col_i=this.control('sorter');
 	   col_i.spec=this;
 	   col_i.usekey=true;
 	   col_i.key=this.children[i];
 	   col_i.type="Sort";
	   col_i.onclick=sortArray;
 	  };
 	 };
	 col_h.appendChild(col_i);
	 col_h.appendChild(this.control('space'));
     this.appendComplexText(col_h,this.children[i].prompt);
//	 col_h.appendChild(col_p);
	 this.children[i].appendHint(col_h);
	 switch(this.children[i].type)
	 {
	 case "Array": case "Object":
	 };
	 r.appendChild(col_h);
    };
	this.container.appendChild(r);
   };
   if(this.parent)
   {
    if(this.inline && this.parent && (this.parent.type=="Array") && edit && !(this.parent.readonly))
    {
     dl=document.createElement("TD");
     this.deleter=this.control("deleter");
     this.deleter.spec=this.parent;
     this.deleter.index=this.index;
	 this.deleter.onclick=delArray;
     this.sorter=this.control("sorter");
     this.sorter.spec=this.parent;
     this.sorter.index=this.index;
     this.sorter.onmousedown=sortFromHere;
     this.sorter.onmouseup=sortToHere
     dl.className="objControl";
     dl.appendChild(this.deleter);
	 dl.appendChild(this.control('space'));
     dl.appendChild(this.sorter);
 	 this.container.appendChild(dl);
	};
   };
   for(i=0;i<this.object.length;i++)
   {
    //repeat header
    if(!this.inline && (edit ||(this.children.length>1)||this.database) 
    	&& i && this.repeat && (i % this.repeat==0))
    {
     r=document.createElement("TR");
     if(this.database){r.className='objHeader';};
     if(this.database)
     {
      col_h=document.createElement("TH");
	  r.appendChild(col_h);
     };
     col_h=document.createElement("TH");
	 r.appendChild(col_h);
     for(j=0;j<this.children.length;j++)
     {
      if(this.children[j].hidden || this.children[j].popup){continue;};
      col_h=document.createElement("TH");
	  col_i=this.control('optional');
	  col_h.appendChild(col_i);
	  col_h.appendChild(this.control('space'));
	  this.appendComplexText(col_h,this.children[j].prompt);
	  this.children[j].appendHint(col_h);
 	  r.appendChild(col_h);
     };
	 this.container.appendChild(r);
    };// end repeat here
    if((this.children.length==1) && !this.database)
    {
     this.elements[i]=this.children[0].duplItemSpec();
    }
    else
    {
     if(!this.object[i]){continue;};
     this.elements[i]=new itemSpec("",this.prompt,"Object",true);
	 this.elements[i].inline=true;
     for(j=0;j<this.children.length;j++)
     {
      this.elements[i].children[j]=this.children[j].duplItemSpec();
     };
	};
    this.container.appendChild(this.elements[i].createXHTML(this,edit,i));
    if(this.elements[i].popupContainer)
    {
     this.container.appendChild(this.elements[i].popupContainer);
    };
   };
   if(edit && !this.readonly)
   {
    d=document.createElement("TD");
    m=this.control('adder');
    m.onclick=incrArray;
    m.spec=this;
    d.appendChild(m);
	if(this.inline)
	{
     this.container.appendChild(d);
	}
	else
	{
     t=document.createElement("TR");
     if(this.database)
     {
      t.appendChild(document.createElement("TH"));
     };
     t.appendChild(d);
     d=document.createElement("TH");
     d.className="objAdd";
	 d.onmouseover=makeUL;
	 d.onmouseout=unMakeUL
     if(this.children.length==1)
     {
      this.appendComplexText(d,"New "+this.children[0].prompt);
     }
     else
     {
      this.appendComplexText(d,"Add to "+this.prompt);
     };
     d.spec=this;
     d.onclick=incrArray;
     t.appendChild(d);
     this.container.appendChild(t);
	};
   };
   break;
  case "Object":
   if(this.parent)
   {
    if(this.inline && this.parent && (this.parent.type=="Array"))
    {
     this.setDbRowStyle();
    };
    if(this.inline && this.parent && this.parent.database)
    {
     dl=document.createElement("TD");
     dl.className='objControl';
     m=document.createElement("INPUT");
     m.type="checkbox";
	 m.checked=false;
     if(this.object.selected)
	 {
	  m.defaultChecked=true;
	  m.checked=true;
	 }
	 else
	 {
	  m.defaultChecked=false;
	  m.checked=false;
	 };
     m.spec=this;
     m.onclick=getCheckBox;
     dl.appendChild(m);
     this.container.appendChild(dl);
    };
    if(this.inline && this.parent && (this.parent.type=="Array"))
    {
     dl=document.createElement("TD");
     if(edit)
     {
      if(this.parent.propagateEdit){this.propagateEdit=true;};
	  if(!this.parent.readonly)
	  {
       this.deleter=this.control("deleter");
       this.deleter.spec=this.parent;
       this.deleter.index=this.index;
	   this.deleter.onclick=delArray;
       this.sorter=this.control("sorter");
       this.sorter.spec=this.parent;
       this.sorter.index=this.index;
       this.sorter.onmousedown=sortFromHere;
       this.sorter.onmouseup=sortToHere
       dl.appendChild(this.deleter);
	   dl.appendChild(this.control('space'));
       dl.appendChild(this.sorter);
	   dl.appendChild(this.control('space'));
	  };
	  if(this.propagateEdit)
	  {
       this.edswitch=this.control('editPropagate');
	  }
	  else
	  {
       this.edswitch=this.control('edit');
	   edit=false;
	  };
      this.edswitch.spec=this;
      this.edswitch.index=this.index;
      this.edswitch.type="Edit";
      this.edswitch.onclick=edArrayElement;
	  dl.width=25;
      dl.className="objControl";
      dl.appendChild(this.edswitch);
     }
     else
     {
      this.noteshow=this.control('expander');
      this.noteshow.spec=this;
	  this.noteshow.onclick=showNote;
      this.noteshow.style.display="none";
      this.noteshow.title="Show notes";
	  dl.appendChild(this.noteshow);
     };
 	 this.container.appendChild(dl);
	};
   };
   dl=false;note=false;
   for(i=0;i<this.children.length;i++)
   {
    if(this.children[i].hidden)
    {
     this.children[i].parent=this;
     this.children[i].fillContainer();
     continue;
    };
    if(this.children[i].popup)
    {
     if(((this.object[this.children[i].name] 
      && (this.object[this.children[i].name]!="#")/*&&(!this.parent.edit)*/))
      ||(edit && (!this.children[i].readonly)))
     {
      if(!note)
      {
       note=document.createElement("div");
       m=document.createElement("table");
       m.className='objPopup';
       note.appendChild(m);
       noteList=document.createElement("tbody");
       m.appendChild(noteList);
      };
      m=this.children[i].createXHTML(this,edit,i);
      noteList.appendChild(m);
      if(this.children[i].popupContainer)
      {
       noteList.appendChild(this.children[i].popupContainer);
      };
     }
     else
     {
      this.children[i].parent=this;
      this.children[i].fillContainer();
      continue;
     };
    }
    else
    {
     this.children[i].createXHTML(this,edit,i);
     dl=this.children[i].container;
     if(this.tabbed && this.inline)
     {
      this.children[i].XHTML.colSpan=this.children.length;
      if(i==this.tabindex)
      {
       this.container.appendChild(this.children[i].XHTML);
      };
     }
     else
     {
      this.container.appendChild(this.children[i].XHTML);
      if(this.children[i].popupContainer)
      {
       this.container.appendChild(this.children[i].popupContainer);
      };
     };
    };
    if(this.database && this.changed)
    {
     this.children[i].XHTML.className="objChanged";
    };
    if(this.database && this.object.created)
    {
     this.children[i].XHTML.className="objNew";
    };
    if(this.database && this.object.deleted)
    {
     this.children[i].XHTML.className="objDeleted";
    };
   };
   if(dl && note)
   {
    this.popupContainer=document.createElement(this.XHTML.nodeName);
    if(this.noteshow)
    {
     this.noteshow.style.display="block";
     this.parent.noteshow.style.display="block";
    };
    if(!edit)
    {
     this.popupContainer.style.display="none";
    };
    this.popupContainer.className="objPopup";
    if(this.inline) // ie will be TR
    {
     this.popupContainer.appendChild(document.createElement('td'));
     dl=document.createElement('td');
     dl.colSpan=(this.XHTML.childNodes.length-1).toString();
     dl.appendChild(note);
     this.popupContainer.appendChild(dl);
    }
    else
    {
     this.popupContainer.appendChild(note);
    };
    this.container.popupNote=note;
    if(this.XHTML.parentNode)
    {
     if(this.XHTML.nextSibling)
     {
      this.XHTML.parentNode.insertBefore(this.popupContainer,this.XHTML.nextSibling);
     }
     else
     {
      this.XHTML.parentNode.appendChild(this.popupContainer);
     };
    };
   };
   break;
  };
 };
 this.showTop(true);
};


itemSpec.prototype.emptyContainer=function()
{
 var i,val,disp;
 this.showTop(false);
 switch(this.type)
 {
 case "Array":
  for(i=0;i<this.elements.length;i++)
  {
   if(!this.elements[i]){continue;};
   this.elements[i].emptyContainer();
  };
  if(this.checker)
  {
   this.doCheck();
  };
  this.elements.length=0;
  if(this.parent)
  {
   if(!this.unChanged())
   {
    this.parent.changed=true;
   };
  };
  break;
 case "Object":
  for(i=0;i<this.children.length;i++)
  {
   this.children[i].emptyContainer();
  };
  if(this.checker)
  {
   this.doCheck();
  };
  if(this.parent)
  {
   if(!this.unChanged())
   {
    this.parent.changed=true;
    if(this.parent.database)
    {
     this.object.changed=true;
    }
    else
    {
     if(this.database)
     {
      this.object.changed=true;
     };
    };
   };
  };
  break;
 default:
  if(this.checker)
  {
   this.doCheck();
  };
  if(this.parent && this.parent.object)
  {
   if(!this.unChanged())
   {
    this.parent.changed=true;
   };
   switch(this.parent.type)
   {
   case "Array":
    this.parent.object[this.index]=this.object;
    break;
   case "Object":
    this.parent.object[this.name]=this.object;
    break;
   };
  };
 };
 if(!this.container)
 {
  this.showTop(true);
  return;
 };
 while(this.container.firstChild)
 {
  this.container.removeChild(this.container.firstChild);
 };
 if(this.popupContainer)
 {
  this.popupContainer.parentNode.removeChild(this.popupContainer);
  this.popupContainer=false;
 };
 this.showTop(true);
};

itemSpec.prototype.doCheck=function()
{
 var tst=this.object;
 switch(typeof(this.checker))
 {
 case "function":
  this.checker();
  break;
 case "string":
  eval(this.checker.toString());
  break;
 };
 if(tst!=this.object)
 {
  this.changed=true;
 };
};

itemSpec.prototype.doChange=function()
{
 var tst=this.object;
 switch(typeof(this.changer))
 {
 case "function":
  this.changer();
  break;
 case "string":
  eval(this.changer.toString());
  break;
 };
 if(tst!=this.object)
 {
  this.changed=true;
 };
};

itemSpec.prototype.doEdit=function()
{
 var tst=this.object;
 switch(typeof(this.editor))
 {
 case "function":
  this.editor();
  break;
 case "string":
  eval(this.editor.toString());
  break;
 };
 if(tst!=this.object)
 {
  this.changed=true;
 };
};

itemSpec.prototype.doAction=function()
{
 switch(typeof(this.action))
 {
 case "function":
  this.action();
  return;
 case "string":
  eval(this.action.toString());
  return;
 };
};

itemSpec.prototype.refillContainer=function(noupdate)
{
 var i,tmp;
 if(noupdate)
 {
  tmp=this.object;
  this.object=false;
  this.emptyContainer();
  this.object=tmp;
  this.fillContainer();
 }
 else
 {
  this.emptyContainer();
  this.fillContainer();
 };
};

itemSpec.prototype.checkRequired=function(silent)
{
 var i,str,p;
 if(!silent){this.checking=true;};
 if((!this.required) && (this.type!="Array"))
 {
  return true;
 };
 switch(this.type)
 {
 case "Action":case "Button":
  return true;
 case "Boolean":
 case "Text": case "TextArea": case "Pre": case "Action": case "Button":case "Color":
 case "Number": case "Date": case "Time":
  if(this.object)
  {
   return true;
  };
  break;
 case "Array":
  for(i=0;i<this.elements.length;i++)
  {
   if(!this.elements[i]){continue;};
   if(!this.elements[i].checkRequired(silent)){return false;};
  };
  if((!this.elements.length) && this.required)
  {
   break;
  };
  return true;
 case "Object":
  for(i=0;i<this.children.length;i++)
  {
   if(!this.children[i].checkRequired(silent)){return false;};
  };
  return true;
 };
 str=this.prompt;
 p=this;
 while(p)
 {
  if(p.parent && p.parent.checking)
  {
   if(!isNaN(p.index))
   {
    str= p.parent.prompt + "["+(p.index+1).toString()+"] > " + str;
   }
   else
   {
    str= p.parent.prompt + " > " + str;
   };
  };
  p=p.parent;
 };
 str="Required entries missing\n"+str;
 if(!silent)
 {
  alert(str);
 };
 return false;
};

itemSpec.prototype.unChanged=function(force)
{
 var i;
 if(force)
 {
  this.changed=false;
 };
 if(this.changed)
 {
  return false;
 };
 switch(this.type)
 {
 case "Array":
  for(i=0;i<this.elements.length;i++)
  {
   if(!this.elements[i]){continue;};
   if(!this.elements[i].unChanged(force)){return false;};
  };
  break;
 case "Object":
  for(i=0;i<this.children.length;i++)
  {
   if(!this.children[i].unChanged(force)){return false;};
  };
  break;
 };
 return true;
};

itemSpec.prototype.createDisplay=function(obj)
{
 var t,b;
 switch(this.type)
 {
 case "Object":
  if(!obj)
  {
   obj=new Object();
  };
  break;
 case "Array":
  if(!obj)
  {
   obj=new Array();
  };
  break;
 };
 this.object=obj;
 t=document.createElement("table");
 if(this.dialog)
 {
  t.className="objMenuDrop";
 }
 else
 {
  if(this.outline)
  {
   t.className="objOutline";
  }
  else
  {
   t.className=this.className;
  };
 };
 if(!this.readonly)
 {
  t.ondblclick=ensureEdit;
 };
 if(this.width){t.style.width=this.width;};
 if(this.height){t.style.height=this.height;};
 t.spec=this;
 t.edit=this.edit;
// t.style.width="100%";
 b=document.createElement("tbody");
 b.appendChild(this.createXHTML(null,this.edit,0));
 if(this.popupContainer)
 {
  b.appendChild(this.popupContainer);
 };
 t.appendChild(b);
 if((!this.checkRequired(true)) && (!this.edit))
 {
  this.toggleEdit();
 };
 return t;
};


itemSpec.prototype.toggleExpand=function()
{
 while(this.expander.firstChild)
 {
  this.expander.removeChild(this.expander.firstChild);
 };
 if(this.expand)
 {
  this.expand=false;
  this.childArea.style.display="none";
  this.expander.title='Expand';
  this.expander.appendChild(this.elementWithClass('div','right'));
//  this.expander.appendChild(this.elementWithClass('div','circleGreen'));
//  this.expander.appendChild(this.elementWithClass('div','plus'));
 }
 else
 {
  this.expand=true;  
  this.childArea.style.display="inline-block";
  this.expander.title='Contract';
  this.expander.appendChild(this.elementWithClass('div','down'));
//  this.expander.appendChild(this.elementWithClass('div','circleOrange'));
//  this.expander.appendChild(this.elementWithClass('div','minus'));
 };
};

itemSpec.prototype.toggleNote=function(multi,force,status)
{
 var i,container=this.noteshow;
 if(multi)
 {
 }
 else
 {
  if(!this.popupContainer){return;};
 };
 while(container.firstChild)
 {
  container.removeChild(container.firstChild);
 };
 if(this.shownote || (force && !status))
 {
  this.shownote=false;
  if(multi)
  {
   container.title='Show all notes';
   for(i=0;i<this.elements.length;i++)
   {
    this.elements[i].toggleNote(false,true,false);
   };
  }
  else
  {
   container.title='Show notes';
   this.popupContainer.style.display="none"
  };
  container.appendChild(this.elementWithClass('div','right'));
 }
 else
 {
  this.shownote=true;  
  if(multi)
  {
   container.title='Hide all notes';
   for(i=0;i<this.elements.length;i++)
   {
    this.elements[i].toggleNote(false,true,true);
   };
  }
  else
  {
   container.title='Hide notes';
   this.popupContainer.style.display="table-row"
  };
  container.appendChild(this.elementWithClass('div','down'));
 };
};

itemSpec.prototype.toggleEdit=function()
{
 if(this.edit)
 {
  if(!this.unChanged())
  {
   this.changed=true;
  };
  if(this.parent && this.parent.database)
  {
   if(this.changed)
   {
    this.object.changed=true;
   };
  };
  this.propagateEdit=false;
 };
 this.edit=!this.edit;
 if(this.readonly){this.edit=false;};
 this.refillContainer();
 if(this.edit && !this.expand)
 {
  this.toggleExpand();
 };
};

itemSpec.prototype.makeEdit=function(state)
{
 if(this.readonly){state=false;};
 if(this.edit)
 {
  if(!this.unChanged())
  {
   this.changed=true;
  };
  if(state){this.propagateEdit=true;};
 };
 if(!state){this.propagateEdit=false;};
 this.edit=state;
 this.refillContainer();
 if(this.edit && !this.expand)
 {
  this.toggleExpand();
 };
};

itemSpec.prototype.exportRecord=function(header,format)
{
 var s;
 if(header)
 {
  switch(this.type)
  {
   case "Action": case "Button":
    return "";
  };
  switch(format)
  {
  case "csv (display)":
  case "csv":
   return '"'+this.prompt+'"';
  case "TeX":
   if(this.type=="Button"){return "";};
   return TeXclean(this.prompt);
  default:
   return this.name;
  };
 }
 else
 {
  if((!this.container)&&(this.special)) // check if there is a specialTeX for an undisplayed item
  {
   this.container=document.createElement("TD");
   eval(this.special.toString());
  };
  if(((format=="TeX")||(format=="tabTeX")) && this.specialTeX)
  {
   return this.specialTeX;
  };
  switch(this.type)
  {
  case "Action": case "Button":
   return "";
  case "Text": case "Color":
  case "TextArea": case "Pre": 
   switch(format)
   {
   case "csv (display)":
    if(this.optionsIndex && this.optionsDisplay && !isNaN(this.optionsIndex[this.object]))
    {
     return '"'+this.optionsDisplay[this.optionsIndex[this.object]]+'"';   
    };
   case "csv":
    return '"'+decode(this.object).replace(/\n/g,"[LF]")+'"';   
   case "tabTeX":
    if(this.optionsIndex && this.optionsDisplay && !isNaN(this.optionsIndex[this.object]))
    {
     return TeXclean(this.optionsDisplay[this.optionsIndex[this.object]]);   
    };
    s=TeXclean(this.object).replace(/\n/g,"[LF]");
    return s;
   case "TeX":
    if(this.optionsIndex && this.optionsDisplay && !isNaN(this.optionsIndex[this.object]))
    {
     return TeXclean(this.optionsDisplay[this.optionsIndex[this.object]]);   
    };
    s=TeXclean(this.object);
    if(this.type=="Pre")
    {
     if(this.width)
     {
      return "\\begin{minipage}{"+this.width+"ex}\\begin{verbatim}\n"+
       s+"\n\\end{verbatim}\\end{minipage}";
     }
     else
     {
      return "\\begin{minipage}{8cm}\\begin{verbatim}\n\n"+s+"\n\n\\end{verbatim}\\end{minipage}";
     };
    };
    return s;
   default:
    return decode(this.object).replace(/\n/g,"[LF]");
   };
  case "Number":
   if(isNaN(this.object)){return "NaN";};
   if(((format=="TeX")||(format=="tabTeX")||(format=="csv (display)")) && this.options.length)
   {
    return TeXclean(decode(this.options[this.object]));
   }
   else
   {
    if(((format=="TeX")||(format=="tabTeX")||(format=="csv (display)")) && this.specialTeX)
    {
     return this.specialTeX;
    }
    else
    {
     return this.object.toString();
    };
   };
  case "Date":
   if(this.object)
   {
    if(this.object.constructor==Date)
    {
     return myWriteDate(this.object);
    }
   };
   return "";
  case "Time":
   return myWriteDate(this.object,true);
  case "Boolean":
   if((format=="TeX")||(format=="tabTeX"))
   {
    if(this.object)
    {
     return "$\\checkmark$";
    }
    else
    {
     return "$-$";
    };
   };
   if(this.object)
   {
    return "TRUE";
   }
   else
   {
    return "FALSE";
   };
   break;
  default:
   return "";
  };
 };
 return "";
};

itemSpec.prototype.outlineLevel=function()
{
 var lev,spec;
 spec=this;
 lev=0;
 while(spec.outline)
 {
  lev++;
  if(!spec.parent){break;};
  spec=spec.parent;  
 };
 return lev;
};

itemSpec.prototype.parentOutline=function()
{
 if(this.parent)
 {
  if(this.parent.outline){return true;};
 };
 return false;
};

itemSpec.prototype.exportData=function(withHeader,format)
{
 var i,j,str,note,width,first,thisWidth,lastCol,outline;
 str="";
 sep="\t";
 end="\n";
 outline=this.outline && (format=='TeX');
 if((!this.expand)&&(this.database)){return "";};
 if((format=="csv")||(format=='csv (display)')){sep=",";};
 if(format=="TeX")
 {
  if(outline)
  {
   sep="\n";end="\n\n";
  }
  else
  {
   sep=" & ";end="\\\\\n";
   land=(this.type=="Array") && (this.children.length>3);
   if(this.parentOutline()){land=false;};
   if(land)
   {
    str+="\\begin{landscape}\n\\small\\sf\\begin{center}\n";
   };
   width=0;
   str+="\\begin{longtable}{";
   switch(this.type)
   {
   case "Object":
    if(!this.inline)
    {
     str+="|>{\\raggedright}p{0.30\\textwidth}|>{\\raggedright}p{0.60\\textwidth}l|";
     break;
    };
   case "Array":
    if(this.database && (this.elements.length==0) && (this.type=="Array")){return "";};
    for(i=0;i<this.children.length;i++)
    {
     if(this.children[i].hidden || this.children[i].popup){continue;};
     if(this.children[i].type=="Button"){continue;};
     width++;
     lastCol=i;
     thisWidth=this.children[i].width;
     if(thisWidth)
     {
      str+="|>{\\raggedright}p{"+thisWidth+"ex}";
     }
     else
     {
      switch(this.children[i].type)
      {
      case "Number": str+="|r";break;
      default :str+="|l";break;
      };
     };
    };
    if(thisWidth){str+="l";width++;};
    str+="|";
    break;
   };
   str+="}\n\\hline\n";
  };
 };
 if(withHeader && (!outline))
 {
  switch(this.type)
  {
  case "Object":
   if((format=="TeX")&&(!this.inline)){break;};
  case "Array":
   first=true;
   for(i=0;i<this.children.length;i++)
   {
    if((format=="TeX") && (this.children[i].hidden || this.children[i].popup))
    {
     continue;
    };
    if(this.children[i].type=="Button"){continue;};
    if(!first){str+=sep;};
    if(format=="TeX")
    {
     if(thisWidth && (i==lastCol))
     {
      str+="\\multicolumn{2}{";
     }
     else
     {
      str+="\\multicolumn{1}{";
     };
     if(first){str+="|";};
     str+="c|}{";
    };
    first=false;
    if((format=="TeX")&&this.children[i].width)
    {
     str+="\\parbox[t]{"+this.children[i].width+"ex}{\\centering "+this.children[i].exportRecord(true,format)+"\\vspace{2pt}}";
    }
    else
    {
     str+=this.children[i].exportRecord(true,format);
    };
    if(format=="TeX"){str+="}";};
   };
   str+=end;
   break;
  };
 };
 if((format=="TeX")&&(!outline))
 {
  str+="\\hline\\endhead\\hline\\endfoot\\hline\\hline\\endlastfoot\n";
 };
 if(outline)
 {
  switch(this.outlineLevel())
  {
  case 1:
   str+="\\section*{"+this.exportRecord(true,format).toUpperCase()+"}\n";
   break;
  };
  switch(this.type)
  {
  case "Array":
   for(j=0;j<this.elements.length;j++)
   {
    switch(this.elements[j].type)
    {
    case 'Page':
     str+="\\newpage\n";
     break;
    case 'Array': case 'Object':
     if(this.elements[j].prompt)
     {
      switch(this.outlineLevel())
      {
      case 1:
       str+="\\section*{"+this.elements[j].exportRecord(true,format)+" "+(j+1)+"}\n";
       break;
      case 2:
       str+="\\subsection*{"+this.elements[j].exportRecord(true,format)+" "+(j+1)+"}\n"
       break;
      case 3:
       str+="\\subsubsection*{"+this.elements[j].exportRecord(true,format)+" "+(j+1)+"}\n"
       break;
      };
     };
     str+=this.elements[j].exportData(true,format);
     str+="\\goodbreak\n";
     break;
    };
   };
   break;
  case "Object":
   for(i=0;i<this.children.length;i++)
   {
    switch(this.children[i].type)
    {
    case 'Page':
     str+="\\newpage\n";
     break;
    case 'Array': case 'Object':
     if(this.children[i].prompt)
     {
      switch(this.outlineLevel())
      {
      case 1:
       str+="\\section*{"+this.children[i].exportRecord(true,format)+"}\n";
       break;
      case 2:
       str+="\\subsection*{"+this.children[i].exportRecord(true,format)+"}\n"
       break;
      case 3:
       str+="\\subsubsection*{"+this.children[i].exportRecord(true,format)+"}\n"
       break;
      };
     };
     str+=this.children[i].exportData(true,format);
     str+="\\goodbreak\n";
     break;
    };
   };
   break;
  };
 }
 else
 {
  switch(this.type)
  {
  case "Array":
   for(j=0;j<this.elements.length;j++)
   {
    note="";
    if(!this.elements[j]){continue;};
    if(this.children.length==1 && !this.database)
    {
      str+=this.elements[j].exportRecord(false,format);
    }
    else
    {
     first=true;
     for(i=0;i<this.elements[j].children.length;i++)
     {
      if(format=="TeX")
      {
       if(this.children[i].type=="Button"){continue;};
       if(this.children[i].popup)
       {
        if(this.elements[j].children[i].object && (this.elements[j].children[i].object!="#"))
        {
         if(this.children[i].type=="TextArea"){note+="\n\n";};
         note+=this.children[i].exportRecord(true,format)+": "+this.elements[j].children[i].exportRecord(false,format)+"; ";
         if(this.children[i].type=="TextArea"){note+="\n\n";};
        };
        continue;
       };
       if(this.children[i].hidden)
       {
        continue;
       };
      };
      if(!first){str+=sep;};
      str+=this.elements[j].children[i].exportRecord(false,format);
      first=false;
     };
    };
    if((format=="TeX") && thisWidth){str+=sep;};
    str+=end;
    if(note)
    {
     str+=sep+"\\multicolumn{"+(width-2)+"}{l}{\\parbox{10cm}{\\raggedright "
       +note+"}}"+sep+end+"\\hline\n";
    };
   };
   break;
  case "Object":
   if((format=="TeX")&&(!this.inline))
   {
    for(i=0;i<this.children.length;i++)
    {
     if(this.children[i].hidden){continue;};
     if(this.children[i].type=="Button"){continue;};
     str+=this.children[i].exportRecord(true,format)+sep;
     str+=this.children[i].exportRecord(false,format);
     str+=sep;
     str+=end;
    };
   }
   else
   {
    first=true;
    for(i=0;i<this.children.length;i++)
    {
     if(this.children[i].type=="Button"){continue;};
     if(!first){str+=sep;};
     first=false;
     str+=this.children[i].exportRecord(false,format);
    };
    if((format=="TeX") && thisWidth){str+=sep;};
    str+=end;
   };
  };
 };
 if(format=="TeX" && (!this.outline))
 {
  str+="\\end{longtable}\n";
  if(land)
  {
   str+="\\end{center}\n\\end{landscape}\n";
  };
 };
 return str;
};

function stringToType(val,type)
{
 var dt;
 switch(type)
 {
 case "Number":
  return parseFloat(val);
 case "Date":
  dt=myReadDate(val,false);
  return dt
 case "Time":
  dt=myReadDate(val,true);
  return dt
 case "Boolean":
  switch(val)
  {
  case "true": case "TRUE": case "ok": case "1": case "OK": case "True":
   return true;
  };
  return false;
 default:
 };
 return decode(val);
};

itemSpec.prototype.importData=function(str,withHeader)
{
 var header,record,i,j;
 str=str.replace(/\r/g,"");
 var lines=str.split("\n");
 this.emptyContainer();
 if(withHeader)
 {
  if(lines.length<2){return;};
  header=lines[0].split("\t");
  switch(this.type)
  {
  case "Object":
   record=lines[1].split("\t");
   for(j=0;j<header.length;j++)
   {
    if(header[j])
	{
     this.object[header[j]]=record[j];
	};
   };
   for(j=0;j<this.children.length;j++)
   {
    if(record[j])
	{
	 this.object[this.children[j].name]=stringToType(this.object[this.children[j].name],this.children[j].type);
	};
   };
   break;
  case "Array":
   this.object.length=0;
   for(i=1;i<lines.length;i++)
   {
    record=lines[i].split("\t");
	if(((record.length>1)||(this.database))&&lines[i])
	{
	 this.object[i-1]=new Object;
     for(j=0;j<header.length;j++)
     {
	  if(header[j])
	  {
       this.object[i-1][header[j]]=record[j];
	  };
	 };
     for(j=0;j<this.children.length;j++)
     {
	  this.object[i-1][this.children[j].name]=stringToType(this.object[i-1][this.children[j].name],this.children[j].type);
	 };
	 if(this.database)
	 {
	  this.object[i-1].created=true;
	 };
	}
	else
	{
	 if(lines[i])
	 {
	  this.object[i-1]=stringToType(lines[i],this.children[0].type);
	 };
	};
   };
   break;
  };
 }
 else
 {
  if(lines.length<1){return;};
  switch(this.type)
  {
  case "Object":
   record=lines[0].split("\t");
   for(j=0;j<this.children.length;j++)
   {
    if(record[j])
	{
     this.object[this.children[j].name]=stringToType(record[j],this.children[j].type);
	};
   };
   break;
  case "Array":
   this.object.length=0;
   for(i=0;i<lines.length;i++)
   {
    record=lines[i].split("\t");
	if(record.length>1)
	{
	 this.object[i]=new Object;
     for(j=0;j<this.children.length;j++)
     {
	  if(record[j])
	  {
       this.object[i][this.children[j].name]=stringToType(record[j],this.children[j].type);
	  };
	 };
	 if(this.database)
	 {
	  this.object[i].created=true;
	 };
	}
	else
	{
	 if(lines[i])
	 {
	  this.object[i]=stringToType(lines[i],this.children[0].type);
	 };
	};
   };
   break;
  };
 };
 this.changed=true;
 this.fillContainer();
};

itemSpec.prototype.incrArray=function()
{
 var l;
 l=this.object.length;
 if(!l){l=0;this.object.length=0;};
 this.emptyContainer();
 this.object.length++;
 if((this.children.length==1)&&!this.database)
 {
  /* simple array */
  this.object[l]=this.children[0].createObject();
 }
 else
 {
  /* multiple field array */
  this.object[l]=new Object();
  if(this.database)
  {
   this.object[l].created=true;
  };
 };
 this.fillContainer();
 if(this.elements.length)
 {
  this.elements[this.elements.length-1].edit=this.edit;
  this.elements[this.elements.length-1].propagateEdit=true;
  if(!this.elements[this.elements.length-1].expand)
  {
   this.elements[this.elements.length-1].toggleExpand();
  };
  this.elements[this.elements.length-1].refillContainer();
 };
 this.changed=true;
 if(this.changer)
 {
  this.doChange();
 };
 return this.elements[this.elements.length-1];
};

itemSpec.prototype.sortArray=function(key,asc)
{
 var i;
 if(!((this.edit)||(this.database))){return;};
 sortKey=key;
 if(asc){sortDirn=1;}else{sortDirn=-1;}; 
 this.emptyContainer();
 if(this.database)
 {
  for(i=0;i<this.object.length;i++)
  {
   this.object[i].db_sort_line=i+1;
  };
 };
 this.object.sort(compareArray);
 this.fillContainer();
 this.changed=true;
 if(this.changer)
 {
  this.doChange();
 };
};

itemSpec.prototype.indexOptions=function()
{
 var i;
 this.optionsIndex=new Object();
 for(i=0;i<this.options.length;i++)
 {
  if(this.options[i])
  {
   this.optionsIndex[this.options[i]]=i;
  };
 };
};

itemSpec.prototype.elementWithClass=function(typ,cl)
{
 var el=document.createElement(typ);
 el.className=cl;
 return el;
};

itemSpec.prototype.control=function(typ)
{
 var d;
 if(typ=='space'){return document.createTextNode('');};
 d=this.elementWithClass('div','objIcon');
 switch(typ)
 {
 case 'deleter':
  d.appendChild(this.elementWithClass('div','circleRed'));
  d.appendChild(this.elementWithClass('div','cross'));
  d.title="Delete";
  break;
 case 'edit':
  d.appendChild(this.elementWithClass('div','circleCyan'));
  d.appendChild(this.elementWithClass('div','pencil'));
  d.title="Edit";
  break;
 case 'editPropagate':
  d.appendChild(this.elementWithClass('div','circleCyanOn'));
  d.appendChild(this.elementWithClass('div','pencil'));
  d.title="Edit";
  break;
 case 'adder':
  d.appendChild(this.elementWithClass('div','circleGreen'));
  d.appendChild(this.elementWithClass('div','plus'));
  break;
 case 'expander':
//  d.appendChild(this.elementWithClass('div','circleGreen'));
//  d.appendChild(this.elementWithClass('div','plus'));
  d.appendChild(this.elementWithClass('div','right'));
  d.title="Expand";
  break;
 case 'contract':
//  d.appendChild(this.elementWithClass('div','circleOrange'));
//  d.appendChild(this.elementWithClass('div','minus'));
  d.appendChild(this.elementWithClass('div','down'));
  d.title="Contract";
  break;
 case 'sorter':
  d.appendChild(this.elementWithClass('div','circleBlue'));
  d.appendChild(this.elementWithClass('div','sort'));
  d.style.cursor="move";
  d.title="Sort";
  break;
 case 'optional':
  d.appendChild(this.elementWithClass('div','circle'));
  break;
 case 'required':
  d.appendChild(this.elementWithClass('div','circle'));
  d.appendChild(this.elementWithClass('div','spot'));
  d.title="Required";
  break;
 case 'action':
  d.appendChild(this.elementWithClass('div','circleBlue'));
  d.appendChild(this.elementWithClass('div','go'));
  d.title="Action";
  break;
 case 'import':
  d=this.elementWithClass('div','objIconImport');
  d.appendChild(this.elementWithClass('div','sheet'));
  d.appendChild(this.elementWithClass('div','in'));
  d.appendChild(this.elementWithClass('div','out'));
  d.title="Import/Export";
 case 'space':
  break;
 };
 return d;
};

function lightButton()
{
 switch(this.type)
 {
 case "Minus":
  this.src="../img/OrangeMinus.gif";
  break;
 case "Plus":
  this.src="../img/GreenPlus.gif";
  break;
 case "Cross":
  this.src="../img/RedCross.gif";
  break;
 case "Edit":
  this.src="../img/AquaGrey.gif";
  break;
 case "Right":
  this.src="../img/BlueRight.gif";
  break;
 case "Sort":
  this.src="../img/BlueUpDown.gif";
  break;
 };
};


function dimButton()
{
 switch(this.type)
 {
 case "Minus":
  this.src="../img/Orange.gif";
  break;
 case "Plus":
  this.src="../img/Green.gif";
  break;
 case "Cross":
  this.src="../img/Red.gif";
  break;
 case "Edit":
  this.src="../img/Aqua.gif";
  break;
 case "Right":
  this.src="../img/Blue.gif";
  break;
 case "Sort":
  this.src="../img/Blue.gif";
  break;
 };
};

function makeUL()
{
 this.style.textDecoration="underline";
};

function unMakeUL()
{
 this.style.textDecoration="none";
};

function toggleExpand()
{
 this.spec.toggleExpand();
};


function getSelect()
{
 switch(this.spec.type)
 {
 case "Number":
  this.spec.object=this.selectedIndex;
  if(this.spec.options.length && typeof(this.spec.options[0])=='number')
  {
   this.spec.object=this.spec.options[this.spec.object];
  };
  break;
 case "Text":
  this.spec.object=decode(this.spec.options[this.selectedIndex]);
  break;
 };
 this.spec.changed=true;
 if(this.spec.parent)
 {
  if(this.spec.parent.type=="Object")
  {
   this.spec.parent.object[this.spec.name]=this.spec.object;
  };
 };
 if(this.spec.changer)
 {
  this.spec.doChange();
 };
};


function getCheckBox()
{
 switch(this.spec.type)
 {
 case "Boolean":
  if(this.checked)
  {
   this.spec.object=true;
  }
  else
  {
   this.spec.object=false;
  };
  this.spec.changed=true;
  if(this.spec.parent)
  {
   if(this.spec.parent.type=="Object")
   {
    this.spec.parent.object[this.spec.name]=this.spec.object;
   };
  };
  break;
 case "Object":
  if(this.checked)
  {
   this.spec.object.selected=true;
  }
  else
  {
   this.spec.object.selected=false;
  };
  break;
 };
 if(this.spec.changer)
 {
  this.spec.doChange();
 };
};


function getText()
{
 var mem;
 switch(this.spec.type)
 {
 case "Number":
  if(isNaN(this.value))
  {
   this.spec.object="NaN";
   break;
  };
  this.spec.object=Number(this.value);
  break;
 case "Action": case "Button":
 case "Text": case "TextArea": case "Pre": case "Color":
//  this.value=this.value.replace(/[^\x21-\x7e\xa2-\xa5\xb0-\xb1\xb5\xc0-\xff\s]/g,"?");
  this.spec.object=this.value;
  this.spec.object=this.spec.object.replace(/\r/g,"");
  break;
 case "Date":
  if((!this.spec.object) || (this.spec.object.constructor!=Date))
  {
   this.spec.object=new Date();
  };
  if(this.value && (this.spec.object.constructor==Date))
  {
   this.spec.object=myReadDate(this.value,false);
   this.value=myWriteDate(this.spec.object,false);
  }
  else
  {
   if(defaultBritish)
   {
    this.value="dd/mm/yyyy";
   }
   else
   {
    this.value="yyyy-mm-dd";
   };
   this.spec.object=false;
  };
  break;
 case "Time":
  if((!this.spec.object) || (this.spec.object.constructor!=Date))
  {
   this.spec.object=new Date();
  };
  if(this.value && (this.spec.object.constructor==Date))
  {
   this.spec.object=myReadDate(this.value,true);
   this.value=myWriteDate(this.spec.object,true);
  }
  else
  {
   if(defaultBritish)
   {
    this.value="dd/mm/yyyy hh:mm:ss";
   }
   else
   {
    this.value="yyyy-mm-dd hh:mm:ss";
   };
   this.spec.object=false;
  };
  break;
 };
 this.spec.changed=true;
 if(this.spec.parent)
 {
  if(this.spec.parent.type=="Object")
  {
   this.spec.parent.object[this.spec.name]=this.spec.object;
  };
 };
 if(this.spec.changer)
 {
  this.spec.doChange();
 };
};


function toggleEdit()
{
 this.spec.toggleEdit();
};

function ensureEdit()
{
 if(!this.spec.edit)
 {
  this.spec.makeEdit(true);
 }
 else
 {
  if(!this.spec.propagateEdit)
  {
   this.spec.makeEdit(true);
  };
 };
};

function incrArray()
{
 var l;
 l=this.spec.object.length;
 if(!l){l=0;this.spec.object.length=0;};
 this.spec.emptyContainer();
 this.spec.object.length++;
 if((this.spec.children.length==1) && !this.spec.database)
 {
  /* simple array */
  this.spec.object[l]=this.spec.children[0].createObject();
 }
 else
 {
  /* multiple field array */
  this.spec.object[l]=new Object();
  if(this.spec.database)
  {
   this.spec.object[l].created=true;
  };
 };
 this.spec.fillContainer();
 if(this.spec.elements.length)
 {
  this.spec.elements[this.spec.elements.length-1].edit=true;
  this.spec.elements[this.spec.elements.length-1].propagateEdit=true;
  if(!this.spec.elements[this.spec.elements.length-1].expand)
  {
   this.spec.elements[this.spec.elements.length-1].toggleExpand();
  };
  this.spec.elements[this.spec.elements.length-1].refillContainer();
 };
 this.spec.changed=true;
 if(this.spec.changer)
 {
  this.spec.doChange();
 };
 return this.spec.elements[this.spec.elements.length-1];
};

function delArray()
{
 var i;
 if(this.spec.database && !this.spec.object[this.index].created)
 {
  this.spec.object[this.index].deleted=!this.spec.object[this.index].deleted;
  this.spec.elements[this.index].setDbRowStyle();
  this.spec.changed=true;
  if(this.spec.changer)
  {
   this.spec.doChange();
  };
  return;
 };
 if(!this.spec.database)
 {
  if(this.spec.children.length==1)
  {
   if(!confirm("Delete "+this.spec.children[0].prompt+"?")){return;};
  }
  else
  {
   if(!confirm("Delete from "+this.spec.prompt+"?")){return;};
  };
 };
 this.spec.emptyContainer();
 for(i=this.index;i<this.spec.object.length-1;i++)
 {
  this.spec.object[i]=this.spec.object[i+1];
 };
 this.spec.object.length--;
 this.spec.fillContainer();
 this.spec.changed=true;
 if(this.spec.changer)
 {
  this.spec.doChange();
 };
};

function sortFromHere()
{
 this.spec.sortFromIndex=this.index;
};

function sortToHere()
{
 var tmp,i;
 if(typeof(this.spec.sortFromIndex)=="undefined" || isNaN(this.spec.sortFromIndex)){return;};
 if((this.spec.sortFromIndex<0) || (this.spec.sortFromIndex>=this.spec.object.length)){return;};
 if(this.spec.sortFromIndex==this.index){return;};
 this.spec.emptyContainer();
 if(this.spec.sortFromIndex>this.index)
 {
  tmp=this.spec.object[this.spec.sortFromIndex];
  for(i=this.spec.sortFromIndex;i>this.index;i--)
  {
   this.spec.object[i]=this.spec.object[i-1];
  };
  this.spec.object[this.index]=tmp;
 }
 else
 {
  tmp=this.spec.object[this.spec.sortFromIndex];
  for(i=this.spec.sortFromIndex;i<this.index;i++)
  {
   this.spec.object[i]=this.spec.object[i+1];
  };
  this.spec.object[this.index]=tmp;
 };
 this.spec.fillContainer();
};

function edArrayElement()
{
 this.spec.emptyContainer();
 this.spec.propagateEdit=!this.spec.propagateEdit;
 if(this.spec.parent.database && !this.spec.unChanged())
 {
  this.spec.object.changed=true;
 };
 this.spec.fillContainer();
};

var tstDate=new Date;

function compareObjectValue(a)
{
 var el;
 if(a.constructor==tstDate.constructor){return a.getTime();};
 for(el in a)
 {
  switch(typeof(a[el]))
  {
  case "number":
  case "string":
  case "boolean":
   return a[el];
   break;
  case "object":
   return compareObjectValue(a[el]);
  };
 };
 return false;
};

function compareArray(a,b)
{
 var aa,bb,lca,lcb,el;
 if(sortKey)
 {
  aa=a[sortKey];
  bb=b[sortKey];
 }
 else
 {
  aa=a;
  bb=b;
 };
 switch(typeof(aa))
 {
 case "number":
  break;
 case "string":
  lca=aa;
  aa=aa.toUpperCase();
  if(!isNaN(aa))
  {
   aa=parseFloat(aa);
   if(isNaN(bb)){return -1*sortDirn;};
  };
  break;
 case "boolean":
  if(aa){aa=1;}else{aa=0;};
  break;
 case "object":
  if(aa==null){return 1*sortDirn;};
  aa=compareObjectValue(aa);
  break;
 default:
  return 1*sortDirn;
 };
 switch(typeof(bb))
 {
 case "number":
  break;
 case "string":
  lcb=bb;
  bb=bb.toUpperCase();
  if(!isNaN(bb))
  {
   bb=parseFloat(bb);
   if(isNaN(aa)){return 1*sortDirn;};
  };
  break;
 case "boolean":
  if(bb){bb=1;}else{bb=0;};
  break;
 case "object":
  if(bb==null){return -1*sortDirn;};
  bb=compareObjectValue(bb);
  break;
 default:
  return -1*sortDirn;
 };
 if(aa>bb){return 1*sortDirn;};
 if(aa<bb){return -1*sortDirn;};
 if(lca && lcb) // sort by case if strings equal
 {
  aa=lca;bb=lcb;
  if(aa>bb){return 1*sortDirn;};
  if(aa<bb){return -1*sortDirn;};
 };
 if(typeof(a.db_sort_line)!="undefined") //sort by last sort if db
 {
  aa=a.db_sort_line;bb=b.db_sort_line;
  if(aa>bb){return 1;};
  if(aa<bb){return -1;};
 };
 return 0;
};

function sortArray()
{
 this.key.ascending=!this.key.ascending;
 if(this.usekey)
 {
  this.spec.sortArray(this.key.name,this.key.ascending);
 }
 else
 {
  this.spec.sortArray("",this.key.ascending);
 };
};

function selectArray()
{
 var i,ok;
 if(!this.spec.database){return;};
 if(!this.spec.object.length){return;};
 ok=!this.spec.object[0].selected;
 this.spec.emptyContainer();
 for(i=0;i<this.spec.object.length;i++)
 {
  this.spec.object[i].selected=ok;
 };
 this.spec.fillContainer();
};

function doExchange()
{
 var locat,url;
 exchangeSpec=this.spec;
 newExchange=false;
 exchangeWindow=window.open("../jsobj/exchange.html","Exchange",
   "width=500,height=500,toolbar=no,location=no,menubar=no,resizable=no,status=no");
 window.onfocus=updateExchange;
};
function updateExchange()
{
 var l,ll;  
 window.onfocus="";
 if(exchangeSpec && newExchange)
 {
  exchangeSpec.importData(newExchange,true);  
 };
 newExchange=false;
 if(exchangeWindow.open)
 {
  exchangeWindow.close();
 };
};


function doAction()
{
 actionSpec=this.spec;
 if(this.spec.action)
 {
  this.spec.doAction();
 }
 else
 {
  // use old method
  eval(this.spec.name+"()");
 };
};

function doEdit()
{
 if(this.spec.editor)
 {
  this.spec.doEdit();
 };
};

function TeXclean(str)
{
   function removeCntrCodes(txt)
   {
    return txt.replace(/\\/g,"\\textbackslash{}").replace(/_/g,"\\_").replace(/\^/g,"?").replace(/\$/g,"\\$").replace(/\#/g,"\\#").replace(/&/g,"\\&").replace(/%/g,"\\%").replace(/\//g,"/\\-").replace(/>/g,"$>$").replace(/</g,"$<$").replace(/\[/g,"$[$").replace(/\]/g,"$]$");
   };
   var sty,ar,i,j,pts,levs,lev,ctrl,ln,tex="";
   str=decode(str).replace(/[^\u0009\u000a\u000d\u0020-\uffff]/g,"?");
   str=str.replace(/[\u0009\u2008\u2009\u200a\u200b\u00a0]/g," ");
   ar=str.split("{");
   if(ar.length==1)
   {
    return removeCntrCodes(str);
   }
   else
   {
    ln=0;levs=[];levs[0]="";lev=0;
    for(i=0;i<ar.length;i++)
    {
     pts=ar[i].split("}");
     for(j=0;j<pts.length-1;j++)
     {
      if(lev<0){return lev[0];};
      levs[lev]+=removeCntrCodes(pts[j]);
      if(levs[lev].charAt(0)=="$"){levs[lev]+="}$";}else{levs[lev]+="}";};
      levs[lev-1]+=levs[lev];
      lev--;
     };
     cntrl='';
     if(pts[j].length!=0)
     {
      switch(pts[j].charAt(pts[j].length-1))
      {
      case '_': 
      case '^':
       cntrl=pts[j].charAt(pts[j].length-1);
       pts[j]=pts[j].substr(0,pts[j].length-1);
       break;
      default :
       if(pts[j].lastIndexOf('\\')!=-1)
       {
        cntrl=pts[j].substr(pts[j].lastIndexOf('\\'))
        pts[j]=pts[j].substr(0,pts[j].lastIndexOf('\\'));
       };
       break;
      };
     };
     levs[lev]+=removeCntrCodes(pts[j]);
     if(i==ar.length-1){continue;};
     lev++;
     switch(cntrl)
     {
     case '^':
      levs[lev]='$^{';
      break;
     case '_':
      levs[lev]='$_{';
      break;
     case '\\textit': case '\\it':
      levs[lev]='\\textit{';
      break;
     case '\\textbf': case '\\bf':
      levs[lev]='\\textbf{';;
      break;
     default:
      levs[lev]='{';
      break;
     };
    };
   };
   return levs[0];
};

function showNote()
{
 this.spec.toggleNote(false,false,false);
};

function showNotes()
{
 this.spec.toggleNote(true,false,false);
};

function raiseNote()
{
};

function lowerNote()
{
};

function raiseTab()
{
 var i;
 this.spec.children[this.spec.tabindex].refillContainer();
 for(i=0;i<this.spec.children.length;i++)
 {
  if(this.spec.children[i].tab)
  {
   if(i==this.index)
   {
    this.spec.tabindex=this.index;
    switch(this.spec.children[i].tab.className)
    {
    case 'objTabTop':
     this.spec.children[i].tab.className='objTabTopActive';
     this.spec.container.replaceChild(this.spec.children[i].XHTML,
     	this.spec.container.firstChild);
     break;
    case 'objTabLeft':
     this.spec.children[i].tab.className='objTabLeftActive';
     if(this.spec.children[0].childArea)
     {
      this.spec.children[0].childArea.replaceChild(this.spec.children[i].container,
     	this.spec.children[0].childArea.lastChild);
     }
     else
     {
      this.spec.children[0].XHTML.replaceChild(this.spec.children[i].container,
     	this.spec.children[0].XHTML.lastChild);
     };
     break;
    };
   }
   else
   {
    switch(this.spec.children[i].tab.className)
    {
    case 'objTabTopActive':
     this.spec.children[i].tab.className='objTabTop';
     break;
    case 'objTabLeftActive':
     this.spec.children[i].tab.className='objTabLeft';
     break;
    };
   };
  };
 };
};

function colorDialog()
{
 document.writeln("<div id='colorArea' class='blockArea'><iframe id='colorFrame' name='colorFrame' src='../utils/c_picker.html' scrolling='no' style='position:fixed;left:20%;top:100px;border:outset;overflow:hidden' height=370 width=500><\/iframe><\/div>");
};
function changeColor()
{
 editColorSpec=this.spec;
 window.colorFrame.document.getElementById("Color").value=editColorSpec.object;
 window.colorFrame.fromColor();
 document.getElementById('colorArea').style.display='block';
};
function cancelColor()
{
 document.getElementById('colorArea').style.display='none'; 
};
function setColor()
{
 editColorSpec.emptyContainer();
 editColorSpec.object=window.colorFrame.color;
 editColorSpec.changed=true;
 editColorSpec.fillContainer();
 editColorSpec.changed=true;
 if(editColorSpec.parent)
 {
  if(editColorSpec.parent.type=="Object")
  {
   editColorSpec.parent.object[editColorSpec.name]=editColorSpec.object;
  };
 };
 if(editColorSpec.changer)
 {
  editColorSpec.doChange();
 };
 document.getElementById('colorArea').style.display='none'; 
};

// JSON-LD

itemSpec.prototype.JSON_Context=function(cntx)
{
 var i,scheme;
 if(!cntx)
 {
  cntx={};
 };
 for(i=0;i<this.children.length;i++)
 {
  if(this.children[i].schema)
  {
   cntx[this.children[i].name]={"@id":this.children[i].schema,"@type":"@id"};
  }
  else
  {
   scheme="";
   switch(this.children[i].type)
   {
   case "Array": scheme="@graph";break;
   case "Number": scheme="Number";break;
   case "Boolean": scheme="Boolean";break;
   case "Object": scheme="StructuredValue";break;
   case "Date": scheme="Date";break;
   case "Time": scheme="DateTime";break;
   case "Color": scheme="color";break;
   case "Text": case "TextArea": case "Pre": scheme="Text";break;
   };
   switch(scheme)
   {
   case "":break;
//   case "@graph":
//    cntx[this.children[i].name]="@graph";
//    break;
   default:
    cntx[this.children[i].name]={"@id":"http://test.org/"+this.children[i].name,"@type":"http://schema.org/"+scheme};
    break;
   };
   if(scheme)
   {
   };
  };
  this.children[i].JSON_Context(cntx);
 };
 return cntx;
};