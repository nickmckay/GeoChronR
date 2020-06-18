function core(id)
{
 this.container=document.getElementById(id);
 this.type=false;
 this.id=id;
 this.segments=0;
 if(!this.container){return;};
 this.container.core=this;
 while(this.container.firstChild)
 {
  this.container.removeChild(this.container.firstChild);
 };
 var el;
 el=document.createElement("img");
 el.src="BrownCoreTop.gif";
 this.container.appendChild(el);
 el=document.createElement("br");
 this.container.appendChild(el);
 el=document.createElement("img");
 el.src="BrownCoreBottom.gif";
 this.container.appendChild(el);
};
new core("");
core.prototype.grow=function(size,type)
{
 this.container.removeChild(this.container.firstChild);
 var el;
 el=document.createElement("img");
 switch(this.type)
 {
 case "red":
  el.src="BrownCoreBarRed.gif";
  break;
 case "green":
  el.src="BrownCoreBarGreen.gif";
  break;
 case "black":
  el.src="BrownCoreBar.gif";
  break;
 default:
  el.src="BrownCore.gif";
  break;
 };
 this.container.insertBefore(el,this.container.firstChild);
 if(size>6)
 {
  el=document.createElement("br");
  this.container.insertBefore(el,this.container.firstChild);
  el=document.createElement("img");
  el.src="BrownCore.gif";
  el.height=size-6;
  el.width=30;
  this.container.insertBefore(el,this.container.firstChild);
 };
 el=document.createElement("br");
 this.container.insertBefore(el,this.container.firstChild);
 el=document.createElement("img");
 this.type=type;
 switch(type)
 {
 case "red":
  el.src="BrownCoreTopRed.gif";
  break;
 case "green":
  el.src="BrownCoreTopGreen.gif";
  break;
 default:
  el.src="BrownCoreTop.gif";
  break;
 };
 this.container.insertBefore(el,this.container.firstChild);
};
core.prototype.nextStep=function()
{
 var h;
 if(this.segments<60)
 {
  switch(this.id)
  {
  case "coreD":
   if(Math.random()<0.2)
   {
    this.grow(0,"red");
   }
   else
   {
    this.grow(0,"green");
   };
   this.segments++;
   break;
  case "coreS":
   if(Math.random()<0.4)
   {
    this.grow(Math.random()*6*5,"red");
   };
   this.segments+=1.25;
   break;
  case "coreP2":
   if(Math.random()<0.2)
   {
    if(Math.random()<0.2)
    {
     this.grow(0,"red");
    }
    else
    {
     this.grow(0,"black");
    };
    this.segments++;
   };
   break;
  case "coreP1":
   if(Math.random()<0.2)
   {
    if(Math.random()<0.4)
    {
     this.grow(12,"red");
    }
    else
    {
     this.grow(12,"black");
    };
    this.segments+=2;
   };
   break;
  case "coreU":
   if(Math.random()<0.2)
   {
    this.grow(0,"red");
   }
   else
   {
    this.grow(0,"");
   };
   this.segments++;
   break;
  };
  return this.segments<60;
 }
 else
 {
  new core(this.id);
  return false;
 };
};
function nextStep(id,incr)
{
 if(document.getElementById(id).core.nextStep())
 {
  window.setTimeout("nextStep(\""+id+"\","+incr+")",incr);
 };
};
function initCores()
{
 var cr;
 new core("coreD");
 new core("coreS");
 new core("coreP1");
 new core("coreP2");
 new core("coreU");
 cr=new core("keySample");
 cr.grow(0,"red");
 cr.grow(12);
 cr=new core("keyKnown");
 cr.grow(0,"green");
 cr.grow(0,"green");
 cr.grow(12);
 cr=new core("keyGranularity");
 cr.grow(0,"black");
 cr.grow(0,"black");
 cr.grow(12);
};
