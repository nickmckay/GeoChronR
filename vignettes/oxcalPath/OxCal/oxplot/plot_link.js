var canvas={};
var plotInfo={};
var plotData={};
var plotOptions={};
var multiPlots={};
var plotColor={};

function plotLink(frame)
{
 this.frame=frame;  // if unset will open new window
 this.reset();
};

plotLink.prototype.reset=function()
{
 this.canvas={};
 this.canvas.scale=1.0; // zoom scaling
 this.canvas.scaleFont=0.8; // font scaling
 this.canvas.scaleLine=1.3; // line scaling
 this.canvas.pxPerCm=35; // px per cm
 this.canvas.frameWidth=18.0; // cm
 this.canvas.frameHeight=13.5; // cm
 this.plotOptions={};
 this.plotOptions.plotPosX=3;
 this.plotOptions.plotPosY=1.5;
 this.plotOptions.plotWidth=12;
 this.plotOptions.plotHeight=12;
 this.plotOptions.multiPlot=false; 
 this.plotInfo={};
 this.plotInfo.minx=0;
 this.plotInfo.maxx=100;
 this.plotInfo.x_calc="";
 this.plotInfo.dx_calc="";
 this.plotInfo.xlabel="";
 this.plotInfo.reversex=false;
 this.plotInfo.autox=1;
 this.plotInfo.miny=0;
 this.plotInfo.maxy=100;
 this.plotInfo.y_calc="";
 this.plotInfo.dy_calc="";
 this.plotInfo.ylabel="";
 this.plotInfo.autoy=1;
 this.plotInfo.reversey=false;
 this.plotInfo.title="";
 this.plotInfo.keyTitle="Key";
 this.plotInfo.yright=false;
 this.plotInfo.xtop=false;
 this.plotInfo.hideRectangle=false;
 this.plotData=[];
 this.plotColor=new Object;
 this.plotColor.showKey=false;
 this.plotColor.z_calc="";
 this.plotColor.zlabel="";
 this.plotColor.minz=0;
 this.plotColor.maxz=100
 this.plotColor.autoz=1;
 this.plotColor.reversez=false;
 this.plotColor.min_col="rgb(0,0,255)";
 this.plotColor.max_col="rgb(255,0,0)";
 this.multiPlots={};
 this.multiPlots.columns=1;
 this.multiPlots.tiedXAxes=0;
 this.multiPlots.tiedYAxes=0;
 this.multiPlots.interXPlot=0;
 this.multiPlots.interYPlot=0;
 this.multiPlots.showKey=true;
 this.multiPlots.keyRow=0;
 this.multiPlots.keyColumn=0;
 this.multiPlots.plots=[];
};

plotLink.prototype.render=function()
{
 canvas=this.canvas;
 plotInfo=this.plotInfo;
 plotData=this.plotData;
 plotOptions=this.plotOptions;
 multiPlots=this.multiPlots;
 plotColor=this.plotColor;
 if(this.frame)
 {
  this.frame.src='../oxplot/OxPlot.html';
 }
 else
 {
  window.open('../oxplot/OxPlot.html');
 };
};

plotLink.prototype.appendData=function(obj)
{
 this.plotData.push(obj);
 return obj;
};

plotLink.prototype.appendPlot=function(obj)
{
 this.multiPlots.plots.push(obj);
 return obj;
};

plotLink.prototype.clearPlot=function()
{
 this.reset();
 this.render();
};

plotLink.prototype.hsvaToRgba=function(h,s,v,a)
{
 var r,g,b;
 if((0<=h) && (h<=60)){r=255;g=(255*h/60);b=0;};
 if((60<h) && (h<=120)){r=(255-255*(h-60)/60);g=255;b=0;};
 if((120<h) && (h<=180)){r=0;g=255;b=(255*(h-120)/60);};
 if((180<h) && (h<=240)){r=0;g=(255-255*(h-180)/60);b=255;};
 if((240<h) && (h<=300)){r=(255*(h-240)/60);g=0;b=255;};
 if((300<h) && (h<=360)){r=255;g=0;b=(255-255*(h-300)/60);}; 
 r=255-(s*(255-r)/100);
 g=255-(s*(255-g)/100);
 b=255-(s*(255-b)/100);
 r=Math.round(v*r/100);
 g=Math.round(v*g/100);
 b=Math.round(v*b/100);
 if(!a){return "rgb("+r+","+g+","+b+")";};
 return "rgba("+r+","+g+","+b+","+a+")";
};

plotLink.prototype.setTransparency=function(color,a)
{
 if(color.indexOf('rgb(')==0)
 {
  return color.replace('rgb(','rgba(').replace(')',','+a+')');
 };
 if(color.indexOf('rgba(')==0)
 {
  return color.substr(0,color.lastIndexOf(','))+','+a+')';
 };
 return color;
};

plotLink.prototype.reflect=function(ax)  // ax is x or y or null for diagonal
{
 var i;
 function swapObjSpecific(obj,ax)
 {
  var tmp;
  obj["reverse"+ax]=!obj["reverse"+ax];
  tmp=obj["min"+ax];
  obj["min"+ax]=obj["max"+ax];
  obj["max"+ax]=tmp;
  switch(obj["auto"+ax])
  {
  case 1:obj["auto"+ax]=2;break;
  case 2:obj["auto"+ax]=1;break;
  };
 };
 function swap(obj,a,b)
 {
  var mem;
  mem=obj[a];
  obj[a]=obj[b];
  obj[b]=mem;
 };
 function diagonalSwap(obj)
 {
  swap(obj,"row","column");
  swap(obj,"xlabel","ylabel");
  swap(obj,"x_calc","y_calc");
  swap(obj,"dx_calc","dy_calc");
  swap(obj,"minx","miny");
  swap(obj,"maxx","maxy");
  swap(obj,"xtop","yright");
  swap(obj,"autox","autoy");
  swap(obj,"nonlinx","nonliny");
  swap(obj,"rowspan","colspan");
 };
 if(ax)
 {
  swapObjSpecific(this.plotInfo,ax);
  for(i=0;i<this.multiPlots.plots.length;i++)
  {
   swapObjSpecific(this.multiPlots.plots[i],ax);
  };
 }
 else // diagonal copy
 {
  diagonalSwap(this.plotInfo);
  for(i=0;i<this.multiPlots.plots.length;i++)
  {
   diagonalSwap(this.multiPlots.plots[i]);
  };
  swap(this.plotOptions,"plotHeight","plotWidth");
  swap(this.multiPlots,"tiedXAxes","tiedYAxes");
  swap(this.multiPlots,"interXPlot","interYPlot");
  swap(this.multiPlots,"keyRow","keyColumn");
  this.multiPlots.columns=1;
  for(i=0;i<this.multiPlots.plots.length;i++)
  {
   if(this.multiPlots.plots[i].column>=this.multiPlots.columns)
   {
    this.multiPlots.columns=this.multiPlots.plots[i].column+1;
   };
   if(this.multiPlots.keyColumn>=this.multiPlots.columns)
   {
    this.multiPlots.columns=this.multiPlots.keyColumn+1;
   };
  };
 };
};
