// ocp_plot.js
// OxCal Plotting routines
// =======================

  var mover={active:false,frm_clientX:0,frm_clientY:0,
 	  to_clientX:0,to_clientY:0,tmr:false,relax:false};

  function stripPlot()
  {
   var el;
   el=document.getElementById("mainframe");
   while(el.firstChild)
   {
    el.removeChild(el.firstChild);
   };
  };


  function draw()
  {
    var item;
    var minx,maxx;
    var miny,maxy;
    var minz,maxz;
	var yparam;
	var xmargin,zmargin;
	var i,j;
    var indexOcd=new Array;
    var start,finish;
    start=1;finish=ocd.length;

	stripPlot();
	
	// reset all of the selections if ocd is in separate scope
	if(!ocdIndex){return;};
	if(ocdIndex.length)
	{
	 for(j=0;j<ocdIndex.length;j++)
     {
      ocd[ocdIndex[j].ind].selectNo=j+1;
     };
	};


	if(plotOptions.plotFrom<1){plotOptions.plotFrom=1;};
    if(plotOptions.viewType)
    {
     // sort out size and method
     switch(plotOptions.viewType)
     {
	 case "model":
	  model_schematic();
	  break;
     case "table":
     case "individual":
	  setupYValues();
      if(ocd[plotOptions.plotFrom] && ocd[plotOptions.plotFrom].correlation)
      {
       // use correlation view
       correlation();
      }
      else
      {
       // individual view
       individual();
      };
      break;
	 case "multiple":
	 case "select":
	 case "stack":
	 case "curve":
	 case "z":
	  // sort out how to plot
	  // redo this since ocd may not be in same scope as before

	  setupYValues();

      multiple(plotOptions.viewType);
      break;
     case "correlation":
      break;
     };
    };
    if(window.parent && window.parent.showingSequence)
    {
     window.parent.nextSlide();
    };
  };
  function tidyText(str)
  {
   return str.replace(/\x2b\x2f\x2d/g,String.fromCharCode(0x00b1));
  };
  function BCADLabeller(pos,gap)
  {
   if(plotOptions.BCAxis)
   {
    if(pos==0)
    {
     return plotOptions.BCADBoundary;
    };
    if(pos>0){return ViewPort.prototype.xlabeller(Math.abs(pos+1),gap);}
    return ViewPort.prototype.xlabeller(Math.abs(pos),gap);
   }
   else
   {
    return ViewPort.prototype.xlabeller(Math.abs(pos),gap);
   };
   return "";
  };
  // functions to conversion to and from F14C
  function F14C_Date(f14c)
  {
   return(-plotOptions.rcConst*Math.log(f14c));
  };
  function UnCal_F14C(cdate)
  {
   return Math.exp(-Math.log(2)*(plotOptions.BPDatum-cdate)/5730);
  };
  function Date_F14C(date,cdate)
  {
   if(!isNaN(cdate) && plotOptions.showDelta14C)
   {
    return 1000*Math.exp(-date/plotOptions.rcConst)/UnCal_F14C(cdate)-1000;
   }
   else
   {
    return(Math.exp(-date/plotOptions.rcConst));
   };
   return 0;
  };
  function F14CErr_DateErr(f14c,error)
  {
   return(plotOptions.rcConst*error/f14c);
  };
  function DateErr_F14CErr(date,error,bp)
  {
   if(!isNaN(bp) && plotOptions.showDelta14C)
   {
    return 1000*(Date_F14C(date)*error/plotOptions.rcConst)/UnCal_F14C(bp);
   }
   else
   {
    return(Date_F14C(date)*error/plotOptions.rcConst);
   };
   return 0;
  };
  
  // specific extension for OxCal x-axis
  ViewPort.prototype.drawXAxis=function(minx,maxx,str,pre,grid)
  {
    var labels=["BC","AD"];
    var style="";
    switch(plotOptions.viewType)
    {
	case "multiple":
	case "select":
	case "stack":
	case "curve":
	case "z":
     if(plotOptions.showPosterior && plotOptions.showItalics)
     {
      style="italic";
     };
     break;
    };
    this.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	switch(plotOptions.dataType)
	{
	case "interval":
 	 this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                          plotOptions.plotWidth,plotOptions.plotHeight,
	                          minx,0,maxx-minx,plotOptions.plotHeight);
	 this.xlabel=label.xAxisInt;
	 if(grid && plotOptions.showGrid){this.grid("x",minx,maxx,plotOptions.plotHeight,plotOptions.majorx);};
	 if(plotOptions.labelX!=""){this.xlabel=plotOptions.labelX;};
	 this.axis("x",minx,maxx,plotOptions.majorx,plotOptions.minorx,"",style);
     break;
	case "date":
	 switch(plotOptions.reportingStyle)
	 {
	 case 0:
 	  this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           plotOptions.BPDatum-minx,0,minx-maxx,plotOptions.plotHeight);
	  this.xlabel=str+" ("+pre+"BP)";
	  if(plotOptions.labelX!=""){this.xlabel=plotOptions.labelX;};
	  if(grid && plotOptions.showGrid){this.grid("x",plotOptions.BPDatum-maxx,plotOptions.BPDatum-minx,plotOptions.plotHeight,plotOptions.majorx);};
	  this.axis("x",plotOptions.BPDatum-maxx,plotOptions.BPDatum-minx,plotOptions.majorx,plotOptions.minorx,"",style);
	  break;
	 case 2:
	  labels=["BCE","CE"];
	 case 1: /* BC/AD */ case 2: /* BCE/CE */
	  this.xlabeller=BCADLabeller;
	  plotOptions.BCAxis=false;
	  plotOptions.BCADBoundary="1"+pre+labels[0]+"/1"+pre+labels[1];
	  if(maxx<1)
	  {
	   plotOptions.BCAxis=true;
	   this.xlabel=str+" ("+pre+labels[0]+")";
	  }
	  else
	  {
	   if(minx>1)
	   {
	    this.xlabel=str+" ("+pre+labels[1]+")";
	   } 
	   else
	   {
	    plotOptions.BCAxis=true;
	    this.xlabel=str+" ("+pre+labels[0]+"/"+pre+labels[1]+")";
	   };
	  };
	  if(plotOptions.labelX!=""){this.xlabel=plotOptions.labelX;};
	  if(plotOptions.BCAxis)
	  {
 	   this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           minx-1,0,maxx-minx,plotOptions.plotHeight);
 	   if(grid && plotOptions.showGrid){this.grid("x",minx-1,maxx-1,plotOptions.plotHeight,plotOptions.majorx);};
	   this.axis("x",minx-1,maxx-1,plotOptions.majorx,plotOptions.minorx,"",style);
	  }
	  else
	  {
 	    this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           minx,0,maxx-minx,plotOptions.plotHeight);
  	   if(grid && plotOptions.showGrid){this.grid("x",minx,maxx,plotOptions.plotHeight,plotOptions.majorx);};
	   this.axis("x",minx,maxx,plotOptions.majorx,plotOptions.minorx,"",style);
	  };
	  break;
	 case 3: /* ISO */ case 4: /* G */
 	  this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           minx,0,maxx-minx,plotOptions.plotHeight);
	  this.xlabel=str+" ("+pre+"CE)";
	  if(plotOptions.labelX!=""){this.xlabel=plotOptions.labelX;};
 	  if(grid && plotOptions.showGrid){this.grid("x",minx,maxx,plotOptions.plotHeight,plotOptions.majorx);};
	  this.axis("x",minx,maxx,plotOptions.majorx,plotOptions.minorx,"",style);
	  break;
	 };
	 break;
	default:
 	 this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                          plotOptions.plotWidth,plotOptions.plotHeight,
	                          minx,0,maxx-minx,plotOptions.plotHeight);
	 this.xlabel="";
	 if(plotOptions.labelX!=""){this.xlabel=plotOptions.labelX;};
	 if(grid && plotOptions.showGrid){this.grid("x",minx,maxx,plotOptions.plotHeight,plotOptions.majorx);};
	 this.axis("x",minx,maxx,plotOptions.majorx,plotOptions.minorx,"",style);
     break;
	};
  };
  
  // specific extension for OxCal y-axis (correlation only)
  ViewPort.prototype.drawYAxis=function(miny,maxy,str,pre,grid)
  {
   var labels=["BC","AD"];
   this.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	switch(plotOptions.dataType)
	{
	case "interval":
 	 this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                          plotOptions.plotWidth,plotOptions.plotHeight,
	                          0,miny,plotOptions.plotWidth,maxy-miny);
	 this.ylabel="Interval (yrs)";
	 if(plotOptions.labelY!=""){this.ylabel=plotOptions.labelY;};
	 if(grid && plotOptions.showGrid){this.grid("y",miny,maxy,plotOptions.plotWidth,plotOptions.majory);};
	 this.axis("y",miny,maxy,plotOptions.majory,plotOptions.minory);
     break;
	case "date":
	 switch(plotOptions.reportingStyle)
	 {
	 case 0:
 	  this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           0,plotOptions.BPDatum-miny,plotOptions.plotWidth,miny-maxy);
	  this.ylabel=str+" ("+pre+"BP)";
	  if(plotOptions.labelY!=""){this.ylabel=plotOptions.labelY;};
	  if(grid && plotOptions.showGrid){this.grid("y",plotOptions.BPDatum-maxy,plotOptions.BPDatum-miny,plotOptions.plotWidth,plotOptions.majory);};
	  this.axis("y",plotOptions.BPDatum-maxy,plotOptions.BPDatum-miny,plotOptions.majory,plotOptions.minory);
	  break;
	 case 2:
	  labels=["BCE","CE"];
	 case 1: /* BC/AD */ case 2: /* BCE/CE */
	  this.ylabeller=BCADLabeller;
	  plotOptions.BCAxis=false;
	  plotOptions.BCADBoundary="1"+pre+labels[0]+"/1"+pre+labels[1];
	  if(maxy<1)
	  {
	   plotOptions.BCAxis=true;
	   this.ylabel=str+" ("+pre+labels[0]+")";
	  }
	  else
	  {
	   if(miny>1)
	   {
	    this.ylabel=str+" ("+pre+labels[1]+")";
	   } 
	   else
	   {
	    plotOptions.BCAxis=true;
	    this.ylabel=str+" ("+pre+labels[0]+"/"+labels[1]+")";
	   };
	  };
	  if(plotOptions.labelY!=""){this.ylabel=plotOptions.labelY;};
	  if(plotOptions.BCAxis)
	  {
 	   this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           0,miny-1,plotOptions.plotWidth,maxy-miny);
	   if(grid && plotOptions.showGrid){this.grid("y",miny-1,maxy-1,plotOptions.plotWidth,plotOptions.majory);};
	   this.axis("y",miny-1,maxy-1,plotOptions.majory,plotOptions.minory);
	  }
	  else
	  {
 	   this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           0,miny,plotOptions.plotWidth,maxy-miny);
	   if(grid && plotOptions.showGrid){this.grid("y",miny,maxy,plotOptions.plotWidth,plotOptions.majory);};
	   this.axis("y",miny,maxy,plotOptions.majory,plotOptions.minory);
	  };
	  break;
	 case 3: /* ISO */ case 4: /* G */
 	  this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                           plotOptions.plotWidth,plotOptions.plotHeight,
	                           0,miny,plotOptions.plotWidth,maxy-miny);
	  this.ylabel=str+" ("+pre+"CE)";
	  if(plotOptions.labelY!=""){this.ylabel=plotOptions.labelY;};
	  if(grid && plotOptions.showGrid){this.grid("y",miny,maxy,plotOptions.plotWidth,plotOptions.majory);};
	  this.axis("y",miny,maxy,plotOptions.majory,plotOptions.minory);
	  break;
	 };
	 break;
	default:
 	 this.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	                          plotOptions.plotWidth,plotOptions.plotHeight,
	                          0,miny,plotOptions.plotWidth,maxy-miny);
	 this.ylabel="";
	 if(plotOptions.labelY!=""){this.ylabel=plotOptions.labelY;};
	 if(grid && plotOptions.showGrid){this.grid("y",miny,maxy,plotOptions.plotWidth,plotOptions.majory);};
	 this.axis("y",miny,maxy,plotOptions.majory,plotOptions.minory);
     break;
	};
  };

  // OxCal specific extension for displaying a distribution in a multi-plot
  
  ViewPort.prototype.drawLabel=function(ind)
  {
    var style;
    var item=ocd[ind];
    var type;
    var indices="";
	var label;
	var xmargin;
	this.map(0,-4,1,8);
	label="";
	if(!plotOptions.showTitle){return;};
	if((item.name=="depthModel")&&(item.data)&&(!isNaN(item.data.z)))
	{
	 if((plotOptions.viewType=="z")&&plotOptions.showInterpolation){return;};
	 label=item.data.z.toString();
	}
	else
	{
	 label=makeTitle(item);
	};
	if(item.question)
	{
	 label+="?";
	};
	if(item.op=="Axis"){return;};
	if(item.op=="Line")
	{
	  xmargin=0.0;
	  if(plotOptions.showStructure && ((plotOptions.viewType=="multiple")||(plotOptions.viewType=="z")))
	  {
       xmargin=item.level*plotOptions.levelMargin;
	  };
	  this.appendChild(this.line(xmargin,0,1.0-xmargin,0,"majorpip"));
	  return;
	};
	if(plotOptions.showIndices && plotOptions.showPosterior)
	{
	 if(plotOptions.showAgreement)
	 {
	  if(item.level==0 && ocd[0].posterior && ocd[ind+1] && (ocd[ind+1].level>0))
	  {
	   if(!isNaN(ocd[0].posterior.modelAgreement))
	   {
	    indices+="Amodel:"+toFixxed(ocd[0].posterior.modelAgreement,0);
	   };
	  };
	  if(item.likelihood && item.likelihood.overallAgreementText)
	  {
	   if(indices){indices+=" ";};
	   indices+=item.likelihood.overallAgreementText;
	  };
	  if(item.posterior && plotOptions.showPosterior)
	  {
	   if(!isNaN(item.posterior.agreement))
	   {
	    if(indices){indices+=" ";};
	    indices+="A:"+toFixxed(item.posterior.agreement,0);
	   }
	   else
	   {
	    if(!isNaN(item.posterior.probability))
	    {
	     if(indices){indices+=" ";};
	     indices+="P:"+toFixxed(item.posterior.probability,0);
	    };
	   };
	  };
	 };
	 if(plotOptions.showConvergence && item.posterior && item.posterior.convergence)
	 {
	  if(indices){indices+=" ";};
	  indices+="C:"+item.posterior.convergence.toFixed(0);
	 };
	 if(plotOptions.showOutliers && item.posterior && item.posterior.outlier_prior)
	 {
	  if(indices){indices+=" ";};
	  indices+="O:"+item.posterior.outlier_post.toFixed(0)+"/"+item.posterior.outlier_prior.toFixed(0);
	 };
	 if(indices)
	 {
	  label+=" ["+indices+"]";
	 };
	};
	if(plotOptions.showPosterior && item.posterior && item.posterior.prob && plotOptions.showItalics)
	{
	 // this is showing the output of a model
	 style="italic";
	}
	else
	{
	 // standard label
	 style="";
	};
	if(plotOptions.showStructure && ((plotOptions.viewType=="multiple")||(plotOptions.viewType=="z")) 
      && (item.level!=undefined))
	{
     this.appendChild(this.text((item.level+1.5)*plotOptions.levelMargin,0,label,10,"",-0.5,"",style));
	 if((item.gap||item.gerror))
	 {
	  label="Gap "+item.gap;
	  if(item.gerror){label+=" "+item.gerror;};
	  if(plotOptions.showReversed)
	  {
	   label=this.text((item.level+2.5)*plotOptions.levelMargin,+1.5,label,8,"",-0.5);
	  }
	  else
	  {
	   label=this.text((item.level+2.5)*plotOptions.levelMargin,-1.5,label,8,"",-0.5);
	  };
	  this.appendChild(label);
	 };
	}
	else
	{
     this.appendChild(this.text(plotOptions.levelMargin,0,label,10,"",-0.5,"",style));
	};
  };
  
  function getItemOpacity(ind)
  {
   if(ocd[ind].posterior && ocd[ind].posterior.outlier_possible && plotOptions.colorOutliers)
   {
    ol=ocd[ind].posterior.outlier_post;
    return (101-ol)/300;
   };
   return false;
  };
  
  function getItemColor(ind)
  {
   var ol;
   if(plotOptions.showBandW){return false;};
   if(ocd[ind].posterior && ocd[ind].posterior.outlier_possible && plotOptions.colorOutliers)
   {
    ol=ocd[ind].posterior.outlier_post;
    return "rgb("+Math.round(ol*255/100)+","+Math.round((100-ol)*255/100)+",0)";
   };
   if(ocd[ind].data && ocd[ind].data.color)
   {
    return ocd[ind].data.color;
   };
   if((typeof(ocd[ind].calib)!="undefined"))
   {
    if(calib[ocd[ind].calib])
    {
     if(calib[ocd[ind].calib].color)
     {
      return calib[ocd[ind].calib].color;
     }
     else
     {
      return false;
     };
	}
	else
	{
     if(ocd[ocd[ind].calib] && ocd[ocd[ind].calib].color)
     { 
	  return ocd[ocd[ind].calib].color;
 	 };
	};
   };
   return false;
  };
  
  function getItemFill(ind,default_fill)
  {
   if(ocd[ind].data && ocd[ind].data.fill)
   {
    return ocd[ind].data.fill;
   };
   return default_fill;
  };
  
  function getItemMarker(ind,median)
  {
   if(ocd[ind].data && ocd[ind].data.marker)
   {
    return ocd[ind].data.marker;
   };
   if(median)
   {
    return "cross";
   };
   return "circle";
  };

  function getItemMarkerSize(ind,median)
  {
   if(ocd[ind].data && ocd[ind].data.markerSize)
   {
    return ocd[ind].data.markerSize;
   };
   return 3;
  };
  
  function checkStrokeColor(poly,ind,div)
  {
   var col,opac;
   opac=getItemOpacity(ind);
   if(div){opac/=div;};
   col=getItemColor(ind);
   if(col)
   {
    if(opac)
    {
	 poly.setAttributeNS(null,"style","stroke:"+col+";stroke-opacity:"+(opac+0.25));
    }
    else
    {
	 poly.setAttributeNS(null,"style","stroke:"+col);
	};
   }
   else
   {
    if(opac)
    {
	 poly.setAttributeNS(null,"style","stroke:black;stroke-opacity:"+(opac+0.25));
	};
   };
  };

  function checkFillAndOpacity(poly,ind,div,default_fill)
  {
   var col,opac,fill;
   col=getItemColor(ind);
   if(!default_fill){default_fill=col;};
   opac=getItemOpacity(ind);
   fill=getItemFill(ind,default_fill);
   if(div){opac/=div;};
   if(col)
   {
    if(opac)
    {
	 poly.setAttributeNS(null,"style","fill:"+fill + "; stroke:"+col+"; fill-opacity:"+opac + "; stroke-opacity:"+(opac+0.33));
    }
    else
    {
	 poly.setAttributeNS(null,"style","fill:"+fill + "; stroke:"+col);
	};
   }
   else
   {
    if(opac)
    {
	 poly.setAttributeNS(null,"style","fill:black; stroke:black; fill-opacity:"+opac + "; stroke-opacity:"+(opac+0.33));
    };
   };
  };
  
  ViewPort.prototype.rugPlot=function(ind,mult)
  {
   var i,j,k,poly,plotter,col;
   if(!model||!model.element){return;};
   if((!ocd[ind])||(!model.element[ind])){return;};
   if(!model.element[ind].param){return;};
   if(!plotOptions.showRanges){return;};
   switch(ocd[ind].op)
   {
   case "Sum":case "KDE_Plot":case "KDE_Model": break;
   default: return;
   };
   for(i=0;i<model.element[ind].param.length;i++)
   {
    j=model.element[ind].param[i];
    for(k=0;k<2;k++)
    {
     col="black";
     plotter=ocd[j].posterior;
     if(k)
     {
      if(!plotOptions.showLikelihood){continue;};
      plotter=ocd[j].likelihood;col="lightgrey";
     }
     else
     {
      if(!plotOptions.showPosterior){continue;};
     };
	 if(plotter && plotOptions.showMean)
	 {
	  poly=this.symbol(plotter.mean,mult*(-k-1),getItemMarkerSize(ind),"circle");
	  poly.setAttributeNS(null,"style","stroke:"+col+";fill:white");
	  this.appendChild(poly);
	 };
	 if(plotter && plotOptions.showMedian)
	 {
	  poly=this.symbol(plotter.median,mult*(-k-1),getItemMarkerSize(ind),"cross");
	  poly.setAttributeNS(null,"style","stroke:"+col);
	  this.appendChild(poly);
	 };
	};
	if((plotOptions.showMedian || plotOptions.showMean) && ocd[j].data && ocd[j].data.sim_date)
	{
	 poly=this.symbol(ocd[j].data.sim_date,mult*(-3),getItemMarkerSize(ind),"diamond");
	 poly.setAttributeNS(null,"style","stroke:black;fill:none");
	 this.appendChild(poly);
	};
   };
  };

  ViewPort.prototype.rug14C=function(ind,test)
  {
   var i,j,k,poly,plotter,col,item;
   if(!(plotOptions.showMedian || plotOptions.showMean)){return false;};
   if(!model){return false;};
   if((!ocd[ind])||(!model.element[ind])){return false;};
   if(!model.element[ind].param){return false;};
   if(!plotOptions.showCurve){return false;};
   switch(ocd[ind].op)
   {
   case "Sum":case "KDE_Plot":case "KDE_Model": break;
   default: return false;
   };
   for(i=0;i<model.element[ind].param.length;i++)
   {
    j=model.element[ind].param[i];
    item=ocd[j];
    if(!isNaN(item.calib) && calib[item.calib] && plotOptions.showCurve && (plotOptions.dataType=="date") && item.date)
    {
     if(test)
     {
      ocd[ind].calib=item.calib;
      return true;
     };
    }
    else
    {
     continue;
    };
    if(plotOptions.showF14C)
    {
	 poly=this.symbol(0.2,Date_F14C(item.date),getItemMarkerSize(ind),"cross");
	}
	else
	{
	 poly=this.symbol(0.2,item.date,getItemMarkerSize(ind),"cross");
	};
	if(plotOptions.showBandW)
	{
	 poly.setAttributeNS(null,"style","stroke:black");
	}
	else
	{
	 poly.setAttributeNS(null,"style","stroke:red");
	};
	this.appendChild(poly);
   };
   return false;
  };

  ViewPort.prototype.drawDist=function(ind)
  {
    var x=new Array;
    var y=new Array;
	var poly;
    var item=ocd[ind];
    var type;
    var rect;
	var xmargin;
	var col,opac;
	var plotter;
	var i,j;
	if((item.name=="depthModel")&&(item.data)&&(!isNaN(item.data.z)))
	{
	 if((plotOptions.viewType=="z")&&plotOptions.showInterpolation){return;};
	};
    switch(item.op)
    {
    case "Before":
    case "After":
     if(plotOptions.showPosterior)
     {
      return;
     };
     break;
    case "P_Sequence":
     if(plotOptions.dataType!=item.type)
     {
      return;
     };
    };
    function make_arrays(plotter)
    {
      var len,i;
      x.length=0;y.length=0;
      len=plotter.prob.length;
      x[0]=plotter.start;
      y[0]=0;
      if(plotOptions.showNormalised)
      {
	   for(i=0;i<len;i++)
	   {
	    x[i+1]=plotter.start+i*plotter.resolution;
	    y[i+1]=plotter.prob[i]*plotter.probNorm/plotOptions.maxz;
	   };
      }
      else
      {
	   for(i=0;i<len;i++)
	   {
	    x[i+1]=plotter.start+i*plotter.resolution;
	    y[i+1]=plotter.prob[i];
	   };
	  };
	  if(!plotOptions.showRanges)
	  {
	   for(i=len;i<len*2;i++)
	   {
	    x[i+1]=x[len*2-1-i+1];
	    y[len*2-1-i+1]*=0.8;
	    y[i+1]=-y[len*2-1-i+1];	    
	   };
	  };
	  x[i+1]=x[i];
	  y[i+1]=0;
    };
	this.map(plotOptions.minx,-4,plotOptions.maxx-plotOptions.minx,8);
    col=getItemColor(ind);
    opac=getItemOpacity(ind);
    if(plotOptions.showDistribution)
    {
     // start with likelihood distribution
     if(plotOptions.showLikelihood && item.likelihood && item.likelihood.prob)
     {
      switch(item.op)
      {
      case "Sum":
      case "KDE_Model":
      case "KDE_Plot":
       if(plotOptions.showPosterior)
       {
        break;
       };
      default:
       if(plotOptions.showPosterior){type="shadow";}else{type="main";};
       make_arrays(item.likelihood);
	   poly=this.polygon("",x,y,type);
	   checkFillAndOpacity(poly,ind);
	   this.appendChild(poly);
       break;
      };
	 };
     // then posterior distribution
     if(plotOptions.showPosterior && item.posterior && item.posterior.prob)
     {
      type="main";
      make_arrays(item.posterior);
	  poly=this.polygon("",x,y,type);
	  checkFillAndOpacity(poly,ind);
	  this.appendChild(poly);
	 };
	};
	plotter=null;
	if(plotOptions.showPosterior){plotter=item.posterior;}
	else
	{
	 if(plotOptions.showLikelihood){plotter=item.likelihood;};
	};
	x.length=0;
	y.length=0;
	if(plotter && plotOptions.showRanges)
	{
	 if(plotOptions.showDistribution)
	 {
	  this.map(plotOptions.minx,-10,plotOptions.maxx-plotOptions.minx,20);
	  for(i=1;i<4;i++)
	  {
	   if(plotOptions.showRange[i] && plotter.range[i])
	   {
	    if(plotOptions.mergeRanges && plotter.range[i].length)
	    {
	 	 x.length=0;y.length=0;
	     if(plotter.range[i][0][0]=="...")
	     {
		  x[0]=plotter.range[i][plotter.range[i].length-1][1]-plotter.resolution*10;y[0]=-i;
		  x[1]=plotter.range[i][plotter.range[i].length-1][1];y[1]=-i;
		  x[2]=plotter.range[i][plotter.range[i].length-1][1];y[2]=-i+0.5;
	     }
	     else
	     {
	      if(plotter.range[i][plotter.range[i].length-1][1]=="...")
	      {
		   x[0]=plotter.range[i][0][0];y[0]=-i+0.5;
		   x[1]=plotter.range[i][0][0];y[1]=-i;
		   x[2]=plotter.range[i][0][0]+plotter.resolution*10;y[2]=-i;
	      }
	      else
	      {
		   x[0]=plotter.range[i][0][0];y[0]=-i+0.5;
	       x[1]=plotter.range[i][0][0];y[1]=-i;
		   x[2]=plotter.range[i][plotter.range[i].length-1][1];y[2]=-i;
		   x[3]=plotter.range[i][plotter.range[i].length-1][1];y[3]=-i+0.5;
	      };
	     };
		 if(x.length>0)
		 {
		  poly=this.polyline("",x,y,"range");
          checkStrokeColor(poly,ind);
	      this.appendChild(poly);
		 };
	     continue;
	    };
	    for(j=0;j<plotter.range[i].length;j++)
	    {
		 x.length=0;y.length=0;
	     if(plotter.range[i][j][0]=="...")
	     {
		   x[0]=plotter.range[i][j][1]-plotter.resolution*10;y[0]=-i;
		   x[1]=plotter.range[i][j][1];y[1]=-i;
		   x[2]=plotter.range[i][j][1];y[2]=-i+0.5;
	     }
	     else
	     {
	      if(plotter.range[i][j][1]=="...")
	      {
		   x[0]=plotter.range[i][j][0];y[0]=-i+0.5;
		   x[1]=plotter.range[i][j][0];y[1]=-i;
		   x[2]=plotter.range[i][j][0]+plotter.resolution*10;y[2]=-i;
	      }
	      else
	      {
		   x[0]=plotter.range[i][j][0];y[0]=-i+0.5;
		   x[1]=plotter.range[i][j][0];y[1]=-i;
		   x[2]=plotter.range[i][j][1];y[2]=-i;
		   x[3]=plotter.range[i][j][1];y[3]=-i+0.5;
	      };
	     };
		 if(x.length>0)
		 {
		   poly=this.polyline("",x,y,"range");
           checkStrokeColor(poly,ind);
	       this.appendChild(poly);
		 };
	    };
	   };
	  };
	 }
	 else
	 {
	  this.map(plotOptions.minx,-10,plotOptions.maxx-plotOptions.minx,20);
	  for(i=3;i>0;i--)
	  {
	   if(plotOptions.showRange[i] && plotter.range[i])
	   {
	    if(plotOptions.mergeRanges && plotter.range[i].length)
	    {
	     if(plotter.range[i][0][0]=="...")
	     {
		   x[0]=plotter.range[i][plotter.range[i].length-1][1]-plotter.resolution*10;y[0]=+i;
		   x[1]=plotter.range[i][plotter.range[i].length-1][1];y[1]=+i;
		   x[2]=plotter.range[i][plotter.range[i].length-1][1];y[2]=-i;
		   x[3]=plotter.range[i][plotter.range[i].length-1][1]-plotter.resolution*10;y[3]=-i;
		   poly=this.polyline("",x,y,"range");
           checkStrokeColor(poly,ind,i);
	       this.appendChild(poly);
	     }
	     else
	     {
	      if(plotter.range[i][plotter.range[i].length-1][1]=="...")
	      {
		   x[0]=plotter.range[i][0][0]+plotter.resolution*10;y[0]=+i;
		   x[1]=plotter.range[i][0][0];y[1]=+i;
		   x[2]=plotter.range[i][0][0];y[2]=-i;
		   x[3]=plotter.range[i][0][0]+plotter.resolution*10;y[3]=-i;
		   poly=this.polyline("",x,y,"range");
           checkStrokeColor(poly,ind,i);
	       this.appendChild(poly);
	      }
	      else
	      {
	       rect=this.rectangle(plotter.range[i][0][0],-i,
	        plotter.range[i][plotter.range[i].length-1][1]-plotter.range[i][0][0],i*2,"range"+i);
	       rect.setAttributeNS(null,"rx",3);
	       checkFillAndOpacity(rect,ind,i);
	       this.appendChild(rect);
	      };
	     };
	     continue;
	    };
	    for(j=0;j<plotter.range[i].length;j++)
	    {
	     if(plotter.range[i][j][0]=="...")
	     {
		   x[0]=plotter.range[i][j][1]-plotter.resolution*10;y[0]=+i;
		   x[1]=plotter.range[i][j][1];y[1]=+i;
		   x[2]=plotter.range[i][j][1];y[2]=-i;
		   x[3]=plotter.range[i][j][1]-plotter.resolution*10;y[3]=-i;
		   poly=this.polyline("",x,y,"range");
           checkStrokeColor(poly,ind,i);
	       this.appendChild(poly);
	     }
	     else
	     {
	      if(plotter.range[i][j][1]=="...")
	      {
		   x[0]=plotter.range[i][j][0]+plotter.resolution*10;y[0]=+i;
		   x[1]=plotter.range[i][j][0];y[1]=+i;
		   x[2]=plotter.range[i][j][0];y[2]=-i;
		   x[3]=plotter.range[i][j][0]+plotter.resolution*10;y[3]=-i;
		   poly=this.polyline("",x,y,"range");
           checkStrokeColor(poly,ind,i);
	       this.appendChild(poly);
	      }
	      else
	      {
	       rect=this.rectangle(plotter.range[i][j][0],-i,
	        plotter.range[i][j][1]-plotter.range[i][j][0],i*2,"range"+i);
	       rect.setAttributeNS(null,"rx",3);
	       checkFillAndOpacity(rect,ind,i);
	       this.appendChild(rect);
	      };
	     };
	    };
	   };
	  };
	 };
	};
	this.map(plotOptions.minx,-4,plotOptions.maxx-plotOptions.minx,8);
	if(plotter && plotOptions.showSigma && plotter.sigma)
	{
	 poly=this.errorbars(plotter.mean,0,plotter.sigma,0);
	 poly.setAttributeNS(null,"style","stroke:black");
     checkStrokeColor(poly,ind);
	 this.appendChild(poly);
	};	
	if((plotOptions.viewType=="curve") && plotter && plotOptions.showSigma)
	{
	 this.map(plotOptions.minx,-10,plotOptions.maxx-plotOptions.minx,20);
	 poly=this.errorbars(plotter.mean,0,0,1);
	 poly.setAttributeNS(null,"style","stroke:black");
     checkStrokeColor(poly,ind);
	 this.appendChild(poly);
	};
	if(plotter && plotOptions.showMean && plotter.mean)
	{
	 poly=this.symbol(plotter.mean,0,getItemMarkerSize(ind),getItemMarker(ind,false));
	 poly.setAttributeNS(null,"style","stroke:black;fill:white");
     checkFillAndOpacity(poly,ind,0,"white");
	 this.appendChild(poly);
	};
	if(plotter && plotOptions.showMedian && plotter.median)
	{
	 if(plotOptions.showMean)
	 {
	  poly=this.symbol(plotter.median,0,getItemMarkerSize(ind),"cross");
	  poly.setAttributeNS(null,"style","stroke:black");
      checkStrokeColor(poly,ind);
	 }
	 else
	 {
	  poly=this.symbol(plotter.median,0,getItemMarkerSize(ind),getItemMarker(ind,true));
 	  poly.setAttributeNS(null,"style","stroke:black;fill:white");
      checkFillAndOpacity(poly,ind,0,"white");
	 };
	 this.appendChild(poly);
	};
	if(plotter && (plotOptions.showMedian || plotOptions.showMean) && ocd[ind].data && ocd[ind].data.sim_date)
	{
	 poly=this.symbol(ocd[ind].data.sim_date,0,getItemMarkerSize(ind),"diamond");
	 poly.setAttributeNS(null,"style","stroke:black;fill:white");
     checkFillAndOpacity(poly,ind,0,"white");
	 this.appendChild(poly);
	};
	this.rugPlot(ind,0.4);
  };
    
  function individual()
  {
    var item,plotter,lhood;
    var mainViewport;
    var mainFrame;
    var plotarea;
    var distarea;
    var kdearea;
    var curvearea,curvegroup,shadowarea;
    var rangearea;
    var convergearea;
	var textarea;
    var rect;
    var norm;
    var xmargin;
    var minc,maxc,minbp=0,maxbp=0;
    var x=new Array;
    var y=new Array;
    var xc=new Array;
    var yc=new Array;
    var i,j,k;
    var el;
    var sig;
    var minx,maxx;
    var miny,maxy;
    var crv=0;
    var refstr;
    var smpl,smplbase;
	var poly;
	var rangetext;
	var temp
	var isCalib=false;
	var ind=plotOptions.plotFrom;
	var kde_e;
	var rug14C=false;
    	
	// sort out what to plot
    minx=plotOptions.minx;
    maxx=plotOptions.maxx;
    miny=plotOptions.miny;
    maxy=plotOptions.maxy;

	item=ocd[ind];
	if(!item){return;};

	if(plotOptions.showPosterior && item.posterior)
	{
	 plotter=item.posterior;
	}
	else
	{
	 if(plotOptions.showLikelihood && item.likelihood)
	 {
	  plotter=item.likelihood;
      isCalib=!isNaN(item.calib) && calib[item.calib] && plotOptions.showCurve && (plotOptions.dataType=="date") && item.date;
	 };
	};
	
    // link plot to SVG document
    mainFrame=attachSVGDocument(plotOptions.frameWidth,plotOptions.frameHeight,plotOptions.scale,plotOptions.scaleFont,plotOptions.scaleLine);
	mainFrame.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	
	//
	mainViewport=mainFrame.viewPort("mainviewport",0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainFrame.appendChild(mainViewport.element);
    
    // define area of plot
	mainViewport.appendChild(mainViewport.rectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight,"border"));
	plotarea=mainViewport.viewPort("plotarea",
	  plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight);
	textarea=mainViewport.viewPort("textarea",
	  plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight);
	// set clipping region
	plotarea.map(0,0,1,1);
	plotarea.clipPath(plotarea.rectangle(0,0,1,1,""));
	
	// if convergence plot then do that
	if(plotOptions.showConvergence && plotOptions.showIndices && plotOptions.showPosterior
	 && item.posterior && item.posterior.sample)
	{
	 plotarea.map(0,0,1,1);
	 if(plotOptions.showRanges && plotOptions.showDistribution)
	 {
	  smplbase=plotOptions.rangeMargin;
	 }
	 else
	 {
	  smplbase=0;
	 };
	 convergearea=plotarea.viewPort("convergearea",0,smplbase,1,0.9-smplbase);
	 convergearea.map(minx,item.posterior.sample.length,maxx-minx,-item.posterior.sample.length);
	 for(i=0;i<item.posterior.sample.length;i++)
	 {
	  smpl=item.posterior.sample[i];
	  if(item.posterior.sampleOk[i])
	  {
	   convergearea.appendChild(convergearea.line(smpl,i,smpl,i+1.5,"okline"));
	  }
	  else
	  {
	   convergearea.appendChild(convergearea.line(smpl,i,smpl,i+1.5,"errorline"));
	  };
	 };
	 plotarea.appendChild(convergearea.element);
	};
	// plot the calibration curve
	rug14C=plotarea.rug14C(ind,true);
	if((!isNaN(item.calib) && calib[item.calib] && plotOptions.showCurve && (plotOptions.dataType=="date") && item.date)||rug14C)
	{
     crv=item.calib;
	 minc=Math.floor((minx-calib[crv].start)/calib[crv].resolution);
	 maxc=Math.ceil((maxx-calib[crv].start)/calib[crv].resolution);
	 if(minc<0){minc=0;};
	 if(maxc>=calib[crv].bp.length){maxc=calib[crv].bp.length-1;};
	 if(maxc>=minc)
	 {
      minbp=(calib[crv].bp[minc]+calib[crv].bp[maxc])/2-(maxx-minx)/2;
      maxbp=minbp+(maxx-minx);
      if(plotOptions.showF14C)
      {
       temp=maxbp;
       maxbp=Date_F14C(minbp);
       minbp=Date_F14C(temp);
      };
      if(plotOptions.showDistribution)
      {
       minbp-=(maxbp-minbp)/10;
       if(plotOptions.showRanges)
       {
        minbp=minbp-(maxbp-minbp)*(plotOptions.rangeMargin/(1-plotOptions.rangeMargin));
       };
      };
      j=0;
      for(i=minc;i<=maxc;i++)
      {
       xc[j]=(i*calib[crv].resolution)+calib[crv].start;
       yc[j]=calib[crv].bp[i]-calib[crv].sigma[i];
       if(plotOptions.showF14C)
       { 
        yc[j]=Date_F14C(yc[j]);
        if(yc[j]>maxbp){maxbp=yc[j];};
       }
       else
       {
        if(yc[j]<minbp){minbp=yc[j];};
       };
       j++;
      };
      for(i=maxc;i>=minc;i--)
      {
       xc[j]=(i*calib[crv].resolution)+calib[crv].start;
       yc[j]=calib[crv].bp[i]+calib[crv].sigma[i];
       if(plotOptions.showF14C)
       { 
        yc[j]=Date_F14C(yc[j]);
        if(yc[j]<minbp){minbp=yc[j];};
       }
       else
       {
        if(yc[j]>maxbp){maxbp=yc[j];};
       };
       j++;
      };
 	  plotarea.map(0,0,1,1);
      curvearea=plotarea.viewPort("curvarea",0,0,1,1);
      curvearea.map(minx,minbp,maxx-minx,maxbp-minbp);
	  curvegroup=curvearea.group("curve");
	  curvegroup.appendChild(curvearea.polygon("curve",xc,yc));
	  if(calib[crv].rawcal  && plotOptions.showCurveRaw)
	  {
	   for(i=0;i<calib[crv].rawcal.length;i++)
	   {
	    if((calib[crv].rawcal[i]>minx)&&(calib[crv].rawcal[i]<maxx))
		{
		 if(plotOptions.showF14C)
		 {
		  curvegroup.appendChild(curvearea.line(calib[crv].rawcal[i],Date_F14C(calib[crv].rawbp[i]-calib[crv].rawsigma[i]),
		   calib[crv].rawcal[i],Date_F14C(calib[crv].rawbp[i]+calib[crv].rawsigma[i])));
		 }
		 else
		 {
		  curvegroup.appendChild(curvearea.line(calib[crv].rawcal[i],calib[crv].rawbp[i]-calib[crv].rawsigma[i],
		   calib[crv].rawcal[i],calib[crv].rawbp[i]+calib[crv].rawsigma[i]));
		 };
		};
	   };
	  }; 
 	  curvearea.appendChild(curvegroup);
	  plotarea.appendChild(curvearea.element);
	  xc.length=0;
	  yc.length=0;
	  // plot the normal distribution
	  if(item && item.date && item.error && plotOptions.showNormal)
	  {
	   distarea=plotarea.viewPort("distarea",0,0,0.3,1);
	   distarea.map(0,minbp,1.1,maxbp-minbp);
       for(i=0;i<40;i++)
	   {
	    sig=(i-20)/5;
	    xc[i]=Math.exp(-sig*sig/2);
	    if(plotOptions.showF14C)
	    {
	     yc[i]=Date_F14C(item.date)+DateErr_F14CErr(item.date,item.error)*sig;
	    }
	    else
	    {
	     yc[i]=item.date+item.error*sig;
	    };
	   };
       norm=distarea.polygon("normal",xc,yc,"normal");
       if(item.deltaR)
       {
        norm.setAttributeNS(null,"style","fill-opacity:0.1;stroke-opacity:0.5");
  	    distarea.appendChild(norm);
        for(i=0;i<40;i++)
	    {
	     sig=(i-20)/5;
	     xc[i]=Math.exp(-sig*sig/2);
	     if(plotOptions.showF14C)
	     {
	      yc[i]=Date_F14C(item.date-ocd[item.deltaR].date)
              +DateErr_F14CErr(item.date-ocd[item.deltaR].date,
              Math.sqrt(item.error*item.error + 
              ocd[item.deltaR].error*ocd[item.deltaR].error))*sig;
         }
         else
         {
	      yc[i]=item.date-ocd[item.deltaR].date
              +Math.sqrt(item.error*item.error + 
              ocd[item.deltaR].error*ocd[item.deltaR].error)*sig;
         };
	    };
        norm=distarea.polygon("normal",xc,yc,"normal");
       };
  	   distarea.appendChild(norm);
	   plotarea.appendChild(distarea.element);
	  };
	  // plot a rug plot if required
	  if(rug14C)
	  {
	   distarea=plotarea.viewPort("distarea",0,0,0.3,1);
	   distarea.map(0,minbp,1.1,maxbp-minbp);
	   distarea.rug14C(ind,false);
	   plotarea.appendChild(distarea.element);
	  };
	 };
	}
	else
	{
	 maxbp=0;
	 minbp=0;
	};
       

	// plot shadow distribution
	if((plotter == item.posterior) && plotOptions.showLikelihood && item.likelihood
	 && item.likelihood.prob && plotOptions.showDistribution)
	{
	 lhood=item.likelihood;
 	 plotarea.map(0,0,1,1);
	 if(plotOptions.showRanges)
	 {
 	  shadowarea=plotarea.viewPort("shadowarea",0,plotOptions.rangeMargin,1,plotOptions.distHeight);
 	 }
 	 else
 	 {
 	  shadowarea=plotarea.viewPort("shadowarea",0,0,1,plotOptions.distHeight);
 	 };
	 shadowarea.map(minx,miny,maxx-minx,maxy-miny);
	 x[0]=lhood.start;
	 for(i=0;i<lhood.prob.length;i++){x[i+1]=lhood.start+i*lhood.resolution;};
	 x[i+1]=x[i];
	 y[0]=0;
	 if(lhood.probNorm && plotter.probNorm)
	 {
	  for(i=0;i<lhood.prob.length;i++){y[i+1]=lhood.prob[i]*lhood.probNorm/plotter.probNorm;};
	 }
	 else
	 {
	  for(i=0;i<lhood.prob.length;i++){y[i+1]=lhood.prob[i];};
	 };
	 y[i+1]=0;
	 shadowarea.appendChild(shadowarea.polygon("likelihood",x,y,"shadow"));
	 x.length=0;
	 plotarea.appendChild(shadowarea.element);
	};

	// plot main distribution
	if(plotter && plotOptions.showDistribution && plotter.prob)
	{
 	 plotarea.map(0,0,1,1);
	 if(plotOptions.showRanges)
	 {
 	  distarea=plotarea.viewPort("distarea",0,plotOptions.rangeMargin,1,plotOptions.distHeight);
 	 }
 	 else
 	 {
 	  distarea=plotarea.viewPort("distarea",0,0,1,plotOptions.distHeight);
 	 };
	 distarea.map(minx,miny,maxx-minx,maxy-miny);
	 x[0]=plotter.start;
	 for(i=0;i<plotter.prob.length;i++){x[i+1]=plotter.start+i*plotter.resolution;};
	 x[i+1]=x[i];
	 y[0]=0;
	 for(i=0;i<plotter.prob.length;i++){y[i+1]=plotter.prob[i];};
	 y[i+1]=0;
	 distarea.appendChild(distarea.polygon("prob",x,y,"main"));
	 x.length=0;
	 plotarea.appendChild(distarea.element);
	};
	
	// plot KDE ghosts if required
	if(model && model.element[ind] && model.element[ind].kde_ensembles && plotOptions.showEnsembles && plotOptions.showPosterior)
	{
 	 plotarea.map(0,0,1,1);
	 if(plotOptions.showRanges)
	 {
 	  kdearea=plotarea.viewPort("kdearea",0,plotOptions.rangeMargin,1,plotOptions.distHeight);
 	 }
 	 else
 	 {
 	  kdearea=plotarea.viewPort("kdearea",0,0,1,plotOptions.distHeight);
 	 };
	 kdearea.map(minx,miny,maxx-minx,maxy-miny);
	 for(j=0;(j<model.element[ind].kde_ensembles.length)&&(j<plotOptions.showEnsembles);j++)
	 {
	  kde_e=model.element[ind].kde_ensembles[j];
	  x[0]=kde_e.start;
	  for(i=0;i<kde_e.prob.length;i++){x[i+1]=kde_e.start+i*kde_e.resolution;};
	  x[i+1]=x[i];
	  y[0]=0;
	  for(i=0;i<kde_e.prob.length;i++){y[i+1]=kde_e.prob[i]*kde_e.probNorm/plotter.probNorm;};
	  y[i+1]=0;
	  kdearea.appendChild(kdearea.polyline("",x,y,"ensemble"));
	  x.length=0;
	 };
	 plotarea.appendChild(kdearea.element);
	};
	// and/or KDE curve
	if(model && model.element[ind] && model.element[ind].kde_mean && (Number(plotOptions.showEnsembles)>-1) && plotOptions.showPosterior)
	{
 	 plotarea.map(0,0,1,1);
	 if(plotOptions.showRanges)
	 {
 	  kdearea=plotarea.viewPort("kdecurvearea",0,plotOptions.rangeMargin,1,plotOptions.distHeight);
 	 }
 	 else
 	 {
 	  kdearea=plotarea.viewPort("kdecurvearea",0,0,1,plotOptions.distHeight);
 	 };
	 kdearea.map(minx,miny,maxx-minx,maxy-miny);
	 kde_e=model.element[ind].kde_mean;
	 x.length=0;y.length=0;
	 x[0]=kde_e.start;
	 for(i=0;i<kde_e.prob_sigma.length;i++){x[i+1]=kde_e.start+i*kde_e.resolution;};
	 x[i+1]=x[i];
	 y[0]=0;
	 for(i=0;i<kde_e.prob_sigma.length;i++){y[i+1]=kde_e.prob_sigma[i][0]*kde_e.probNorm/plotter.probNorm;};
	 y[i+1]=0;
	 kdearea.appendChild(kdearea.polyline("",x,y,"kdemean"));
	 x.length=0;y.length=0;
	 for(i=0;i<kde_e.prob_sigma.length;i++){x.push(kde_e.start+i*kde_e.resolution);};
	 for(i=kde_e.prob_sigma.length-1;i>=0;i--){x.push(kde_e.start+i*kde_e.resolution);};
	 for(i=0;i<kde_e.prob_sigma.length;i++)
	 {
	  y.push((kde_e.prob_sigma[i][0]+kde_e.prob_sigma[i][1])*kde_e.probNorm/plotter.probNorm);
	 };
	 for(i=kde_e.prob_sigma.length-1;i>=0;i--)
	 {
	  sig=(kde_e.prob_sigma[i][0]-kde_e.prob_sigma[i][1])*kde_e.probNorm/plotter.probNorm
	  if(sig>0)
	  {
	   y.push(sig);
	  }
	  else
	  {
	   y.push(0);
	  };
	 };
	 kdearea.appendChild(kdearea.polygon("",x,y,"kdesigma"));
	 x.length=0;y.length=0;
	 plotarea.appendChild(kdearea.element);
	};

	// plot ranges
	if(plotter && plotOptions.showRanges)
	{
	 if(plotOptions.showDistribution || !(plotOptions.showCurve && item.date && item.error))
	 {
  	  plotarea.map(0,0,1,1);
	  rangearea=plotarea.viewPort("rangearea",0,0,1,plotOptions.rangeMargin);
	  rangearea.map(minx,-5,maxx-minx,5);
	  for(i=1;i<4;i++)
	  {
	   if(plotOptions.showRange[i] && plotter.range[i])
	   {
	    for(j=0;j<plotter.range[i].length;j++)
	    {
		 x.length=0;y.length=0;
	     if(plotter.range[i][j][0]=="...")
	     {
		   x[0]=plotter.range[i][j][1]-plotter.resolution*10;y[0]=-i;
		   x[1]=plotter.range[i][j][1];y[1]=-i;
		   x[2]=plotter.range[i][j][1];y[2]=-i+0.5;
	     }
	     else
	     {
	      if(plotter.range[i][j][1]=="...")
	      {
		   x[0]=plotter.range[i][j][0];y[0]=-i+0.5;
		   x[1]=plotter.range[i][j][0];y[1]=-i;
		   x[2]=plotter.range[i][j][0]+plotter.resolution*10;y[2]=-i;
	      }
	      else
	      {
		   x[0]=plotter.range[i][j][0];y[0]=-i+0.5;
		   x[1]=plotter.range[i][j][0];y[1]=-i;
		   x[2]=plotter.range[i][j][1];y[2]=-i;
		   x[3]=plotter.range[i][j][1];y[3]=-i+0.5;
	      };
	     };
		 if(x.length>0)
		 {
		   poly=rangearea.polyline("",x,y,"range");
	       rangearea.appendChild(poly);
		 };
	    };
	   };
	  };
	  plotarea.appendChild(rangearea.element);
	 }
	 else
	 {
	  if(1)
	  {
   	   plotarea.map(0,0,1,1);
	   rangearea=plotarea.viewPort("rangearea",0,0,1,1);
	   rangearea.map(minx,minbp,maxx-minx,maxbp-minbp);
	   for(i=3;i>0;i--)
	   {
	    if(plotOptions.showRange[i] && plotter.range[i])
	    {
	     for(j=0;j<plotter.range[i].length;j++)
	     {
 	      if(plotter.range[i][j][0]=="...")
	      {
		   x[0]=plotter.range[i][j][1]-plotter.resolution*10;y[0]=item.date+i*item.error;
		   x[1]=plotter.range[i][j][1];y[1]=item.date+i*item.error;
		   x[2]=plotter.range[i][j][1];y[2]=item.date-i*item.error;
		   x[3]=plotter.range[i][j][1]-plotter.resolution*10;y[3]=item.date-i*item.error;
		   poly=rangearea.polyline("",x,y,"range");
	       rangearea.appendChild(poly);
	      }
	      else
	      {
	       if(plotter.range[i][j][1]=="...")
	       {
		    x[0]=plotter.range[i][j][0]+plotter.resolution*10;y[0]=item.date+i*item.error;
		    x[1]=plotter.range[i][j][0];y[1]=item.date+i*item.error;
		    x[2]=plotter.range[i][j][0];y[2]=item.date-i*item.error;
		    x[3]=plotter.range[i][j][0]+plotter.resolution*10;y[3]=item.date-i*item.error;
		    poly=rangearea.polyline("",x,y,"range");
	        rangearea.appendChild(poly);
	       }
	       else
	       {
	        rect=rangearea.rectangle(plotter.range[i][j][0],item.date-i*item.error,
	         plotter.range[i][j][1]-plotter.range[i][j][0],i*2*item.error,"range"+i);
	        rect.setAttributeNS(null,"rx",3);
	        rangearea.appendChild(rect);
	       };
		  };
	     };
	    };
	   };
 	   plotarea.appendChild(rangearea.element);
	  };
	 };
	};
	
	// put on the summary statistics 
	
   	plotarea.map(0,0,1,1);
	rangearea=plotarea.viewPort("rangearea",0,0,1,plotOptions.rangeMargin);
 	rangearea.map(minx,-5,maxx-minx,5);
	if(plotter && plotOptions.showSigma && plotter.sigma)
	{
	 poly=rangearea.errorbars(plotter.mean,0,plotter.sigma,0);
	 poly.setAttributeNS(null,"style","stroke:black");
	 rangearea.appendChild(poly);
	};	
	if(plotter && plotOptions.showMean && plotter.mean)
	{
	 poly=rangearea.symbol(plotter.mean,0,3,'circle');
	 poly.setAttributeNS(null,"style","stroke:black;fill:white");
	 rangearea.appendChild(poly);
	};
	if(plotter && plotOptions.showMedian && plotter.median)
	{
	 poly=rangearea.symbol(plotter.median,0,3,'cross');
	 poly.setAttributeNS(null,"style","stroke:black");
	 rangearea.appendChild(poly);
	};
	if(plotter && (plotOptions.showMedian || plotOptions.showMean) && item.data && item.data.sim_date)
	{
	 poly=rangearea.symbol(item.data.sim_date,0,3,"diamond");
	 poly.setAttributeNS(null,"style","stroke:black;fill:white");
	 rangearea.appendChild(poly);
	};
	rangearea.rugPlot(ind,1);
	
	
 	plotarea.appendChild(rangearea.element);

	// put the title on the plot
	if(item && plotOptions.showTitle && (item.name || (plotter && (plotter.comment && plotter.comment.length))))
	{
	 textarea.map(0,0,1,1);
	 if(plotOptions.labelTitle=="")
	 {
	  if(item.likelihood && (plotter==item.likelihood))
	  {
		textarea.appendChild(plotarea.text(0.5,0.98,tidyText(makeTitle(item,true)),12,"middle",-1.25));
	  }
	  else
	  {
	   if(plotOptions.showItalics)
	   {
		textarea.appendChild(textarea.text(0.5,0.98,tidyText(makeTitle(item,true)),12,"middle",-1.25,"","italic"));
	   }
	   else
	   {
		textarea.appendChild(textarea.text(0.5,0.98,tidyText(makeTitle(item,true)),12,"middle",-1.25));
	   };
	  };
	 }
	 else
	 {
	  textarea.appendChild(textarea.text(0.5,1,plotOptions.labelTitle,12,"middle",-1.25));
	 };
	 if(plotter && plotter.comment && (plotter.comment.length > 1) && plotOptions.showText)
	 {
	  k=1;
	  rangetext=textRange(plotter,item.type,isCalib);
	  for(i=0;i<rangetext.length;i++)
	  {
	   j=0;
	   while(rangetext[i].charAt(j)==" "){j++;};
	   textarea.appendChild(textarea.text(0.5+j/50,0.98-k*0.075*plotOptions.scaleFont*plotOptions.scaleLine,tidyText(rangetext[i]),10,"",-1.25));
	   k++;
	  };
	  for(i=1;i<plotter.comment.length;i++)
	  {
	   j=0;
	   while(plotter.comment[i].charAt(j)==" "){j++;};
	   if(j<2)  // miss out all ranges as these are in automatically
	   {
	    textarea.appendChild(textarea.text(0.5+j/50,0.98-k*0.075*plotOptions.scaleFont*plotOptions.scaleLine,tidyText(plotter.comment[i]),10,"",-1.25));
	    k++;
	   };
	  };
	 };
	};
	
	// put all of the elements on the plot
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	mainViewport.appendChild(plotarea.element);
	mainViewport.appendChild(textarea.element);
	mainViewport.appendChild(mainViewport.rectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight,"border"));
	
	// draw x axis
	if(plotOptions.showPosterior && item.posterior)
	{
	 mainViewport.drawXAxis(minx,maxx,label.xAxisPost,label.axisPostPrefix,false);
	}
	else
	{
	 if(isCalib)
	 {
 	  mainViewport.drawXAxis(minx,maxx,label.xAxisCalib,label.axisCalibPrefix,false);
	 }
	 else
	 {
	  mainViewport.drawXAxis(minx,maxx,label.xAxisCalend,label.axisCalendPrefix,false);
	 };
	};

	if(plotOptions.showLikelihood && (plotOptions.showCurve || (item && item.date)) && (!(plotOptions.showPosterior && item.posterior)))
	{
	}
	else
	{
	};

	// draw y axis
	if(plotOptions.showCurve && (minbp!=maxbp))
	{
     mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
     if(plotOptions.showF14C)
     {
      mainViewport.ylabel=label.yAxisF14C;
     }
     else
     {
      mainViewport.ylabel=label.yAxisBP;
     };
	 if(plotOptions.labelY!=""){mainViewport.ylabel=plotOptions.labelY;};
  	 mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
  	  plotOptions.plotWidth,plotOptions.plotHeight,
  	  0,minbp,plotOptions.plotWidth,maxbp-minbp);
	 mainViewport.axis("y",minbp,maxbp,plotOptions.majory,plotOptions.minory);
	}
	else
	{
	 if(plotter)
	 {
      mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
      mainViewport.ylabel=label.yAxisProb;
	  if(plotOptions.labelY!=""){mainViewport.ylabel=plotOptions.labelY;};
	  if(plotOptions.showRanges)
	  {
	   mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.rangeMargin*plotOptions.plotHeight-miny*plotOptions.distHeight*plotOptions.plotHeight/(maxy-miny),
	    plotOptions.plotWidth,plotOptions.distHeight*plotOptions.plotHeight/(maxy-miny),0,0,plotOptions.plotWidth,plotter.probNorm);
	  }
	  else
	  {
	   mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY-miny*plotOptions.distHeight*plotOptions.plotHeight/(maxy-miny),
	    plotOptions.plotWidth,plotOptions.distHeight*plotOptions.plotHeight/(maxy-miny),0,0,plotOptions.plotWidth,plotter.probNorm);
	  };
	  mainViewport.maxYLabel=4;
	  if(plotter.probNorm)
	  {
	   mainViewport.axis("y",0,plotter.probNorm,0,0);
	  }
	  else
	  {
	   mainViewport.axis("y",0,1,0,0);
	  };
	 };
	};
	
	// put reference on
	if(item && item.ref && plotOptions.showReference)
	{
	 if((typeof(item.calib)!='undefined') && calib[item.calib])
	 {
	  refstr=item.ref+"; "+calib[item.calib].ref;
      if(item.deltaR)
      {
       refstr+=" "+makeTitle(ocd[item.deltaR],true);
       refstr=tidyText(refstr);           
      };
	 }
	 else
	 {
	  if((typeof(item.calib)!='undefined') && ocd[item.calib])
	  {
	   refstr=item.ref+"; "+makeTitle(ocd[item.calib],true);
       if(item.deltaR)
       {
        refstr+=" "+makeTitle(ocd[item.deltaR],true);
        refstr=tidyText(refstr);           
       };
	  }
	  else
	  {
	   refstr=item.ref;
	  };
	 };
     mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 mainViewport.appendChild(mainViewport.text(plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.plotHeight,
	  refstr,6,"",0.3));
	};
  };
  function sortDepth(a,b)
  {
   return a.y-b.y;
  };
  function multiple(opt)
  {
    var item,plotter;
    var minx,maxx;
    var minxt,maxxt;
    var miny,maxy;
    var minc,maxc;
    var minz,maxz;
    var minbp,maxbp,minbptest,maxbptest;
    var maxp;
    var start,finish;
    var xmargin,yspace;
    var mainViewport;
    var mainFrame;
    var plotarea,textarea;
    var curvearea,curvegroup;
	var curveElement;
    var structarea,tabarea;
	var grouparea;
	var refarea;
    var rect,path;
    var itemarea,itemlabelarea;
    var i,im,j,k,n,m;
    var xc=new Array;
    var yc=new Array;
    var reversed;
    var depthModel=new Array;
    var newPage;
    var d,prop;
	var curvelist=new Array;
	var refstr;
	var x_ref,y_ref;
	var item;
	var colr;
	var temp,temp2;
	var depth_info;
	
    start=1;finish=ocd.length;

    minx=plotOptions.minx;
    maxx=plotOptions.maxx;
    miny=plotOptions.miny;
    maxy=plotOptions.maxy;
    minz=plotOptions.minz;
    maxz=plotOptions.maxz;
    
	reversed=plotOptions.showReversed;
    switch(plotOptions.viewType)
    {
	case "multiple":
	case "select":
	case "stack":
	   reversed=!reversed;
	   break;
	};

    // link plot to SVG document
    mainFrame=attachSVGDocument(plotOptions.frameWidth,plotOptions.frameHeight,plotOptions.scale,plotOptions.scaleFont,plotOptions.scaleLine);
	mainFrame.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);

 
	// set up main viewport
	mainViewport=mainFrame.viewPort("mainviewport",0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainFrame.appendChild(mainViewport.element);

    // define area of plot
	mainViewport.appendChild(mainViewport.rectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight,"border"));
	plotarea=mainViewport.viewPort("plotarea",
	  plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight);
	textarea=mainViewport.viewPort("textarea",
	  plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight);
	tabarea=mainViewport.viewPort("tabarea",
	  plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight);

    // plot the structure
    if(plotOptions.showStructure && ((opt=="multiple")||(opt=="z")))
    {
 	 plotarea.map(0,0,1,1);
     structarea=plotarea.viewPort("structarea",0,0,1,1);
 	 // set clipping region
	 structarea.map(0,0,1,1);
	 structarea.clipPath(plotarea.rectangle(0,0,1,1,""));
 	 if(reversed)
	 {
      structarea.map(0,maxy,1,miny-maxy);
      tabarea.map(0,maxy,1,miny-maxy);
	 }
	 else
	 {
      structarea.map(0,miny,1,maxy-miny);
      tabarea.map(0,miny,1,maxy-miny);
     };
     for(i=0;i<ocd.length;i++)
     {
      if(typePlottable(i))
      {
       if(ocd[i+1] && ocd[i+1].level)
       {
        if((ocd[i+1].level>ocd[i].level)&&(ocd[i].selectNo>0)) // start of a group
        {
         var start_y,y_margin;
         start_y=ocd[i].yParam;
         y_margin=1;
         for(j=i+1;j<ocd.length;j++)
         {
          if(isNaN(start_y))
          {
           if(typePlottable(j) && (!isNaN(ocd[j].yParam)))
           {
            start_y=ocd[j].yParam;
           };
          };
          if(ocd[j] && (ocd[j].level!=undefined) && (ocd[j].level<=ocd[i].level)){break;};
         };
         // look back for last one that is ok and is not relative
         try
         {
          while((!typePlottable(j-1)||(isNaN(ocd[j-1].yParam))) && (j>i)){j--;};
          if(isNaN(ocd[i].yParam) && !isNaN(ocd[j-1].yParam) && !isNaN(start_y) && (ocd[i].op!="Depth_Model"))
          {
           y_margin=(miny-maxy)/(plotOptions.plotsPerPage+0.8);
           if(ocd[j-1].yParam>start_y)
           {
            ocd[i].yParam=start_y-Math.abs(y_margin);
           }
           else
           {
            ocd[i].yParam=start_y+Math.abs(y_margin);
           };
          };
         }
         catch(e){};
         if(!isNaN(ocd[i].yParam) && !isNaN(ocd[j-1].yParam) && (ocd[j-1].selectNo>0) && 
          (ocd[i].yParam < maxy) && (ocd[j-1].yParam > miny))
         {
          xmargin=(ocd[i].level+1)*plotOptions.levelMargin;
          if(plotOptions.showBrackets)
          {
		   xc.length=0;yc.length=0;
           xc[0]=xmargin+0.01;yc[0]=ocd[i].yParam-0.4*y_margin;
           xc[1]=xmargin;yc[1]=ocd[i].yParam-0.4*y_margin;
           xc[2]=xmargin;yc[2]=ocd[j-1].yParam+0.4*y_margin;
           xc[3]=xmargin+0.01;yc[3]=ocd[j-1].yParam+0.4*y_margin;
		   rect=structarea.polyline("",xc,yc,"range");
	       structarea.appendChild(rect);
          }
          else
          {
		   rect=structarea.rectangle(xmargin,ocd[i].yParam-0.4*y_margin,
            1-2*xmargin,ocd[j-1].yParam-ocd[i].yParam+0.8*y_margin,"structure");
		   if(plotOptions.show3DEffects){rect.setAttributeNS(null,"style","filter:url(#Shadow)");} ;
		   rect.setAttributeNS(null,"rx",5);
		   structarea.appendChild(rect);
		   if(ocd[i].yParam > miny)
		   {
		    grouparea=tabarea.viewPort("group",xmargin,ocd[i].yParam-0.4*y_margin,
              1-2*xmargin,ocd[j-1].yParam-ocd[i].yParam+0.8*y_margin);
		    grouparea.map(xmargin,ocd[i].yParam-0.4*y_margin,
              1-2*xmargin,ocd[j-1].yParam-ocd[i].yParam+0.8*y_margin);
		    rect=grouparea.rectangle(xmargin,ocd[i].yParam-0.4*y_margin,
              1-2*xmargin,ocd[j-1].yParam-ocd[i].yParam+0.8*y_margin,"structure");
		    rect.setAttributeNS(null,"rx",5);
		    grouparea.clipPath(rect);
		    rect=grouparea.rectangle(xmargin,ocd[i].yParam-0.4*y_margin,
              1-2*xmargin,0.9*y_margin,"structtab");
		    grouparea.appendChild(rect);
		    tabarea.appendChild(grouparea.element);
		   };
	       rect=structarea.rectangle(xmargin,ocd[i].yParam-0.4*y_margin,
            1-2*xmargin,ocd[j-1].yParam-ocd[i].yParam+0.8*y_margin,"structborder");
	       rect.setAttributeNS(null,"rx",5);
	       structarea.appendChild(rect);
	      };
         };
        };
       };
      };
     };
     plotarea.appendChild(structarea.element);
    };

	// set clipping region
	plotarea.map(0,0,1,1);
	plotarea.clipPath(plotarea.rectangle(0,0,1,1,""));
	
	// plot the calibration curve
	if(calib[0] && opt=="curve")
	{
	 minbp=false;
	 maxbp=false;
	 for(n=0;n<calib.length;n++)
	 {
	  if(n==0 && !plotOptions.showCurve && !plotOptions.showCurveRaw){continue;};
	  if(!calib[n]){continue;};
//	  if(ocd[n].selectNo>0){}else{continue;};
	  minc=Math.floor((minx-calib[n].start)/calib[n].resolution);
	  maxc=Math.ceil((maxx-calib[n].start)/calib[n].resolution);
	  if(minc<0){minc=0;};
	  if(maxc>=calib[n].bp.length){maxc=calib[n].bp.length-1;};
	  if((maxc>=minc)&&(Math.ceil((maxc-minc)/20)>=1))
	  {
	   for(i=minc;i<=maxc;i+=Math.ceil((maxc-minc)/20))
	   {
        minbptest=calib[n].bp[i]-2*calib[n].sigma[i];
        maxbptest=minbptest+4*calib[n].sigma[i];
        if(plotOptions.showF14C || plotOptions.showDelta14C)
        {
         temp=minbptest;
         minbptest=Date_F14C(maxbptest,(i*calib[n].resolution)+calib[n].start);
         maxbptest=Date_F14C(temp,(i*calib[n].resolution)+calib[n].start);
        };
	    if((!minbp)||(minbptest<minbp)){minbp=minbptest;};
	    if((!maxbp)||(maxbptest>maxbp)){maxbp=maxbptest;};
	   };
	  };
	 };
	 miny=minbp;
	 maxy=maxbp;
	 plotarea.map(0,0,1,1);
	 curvearea=plotarea.viewPort("curvarea",0,0,1,1);
	 if(reversed)
	 {
	  curvearea.map(minx,maxy,maxx-minx,miny-maxy);
	 }
	 else
	 {
	  curvearea.map(minx,miny,maxx-minx,maxy-miny);
	 };
	 for(n=calib.length-1;n>=0;n--)
	 {
	  if(n==0 && !plotOptions.showCurve && !plotOptions.showCurveRaw){continue;};
	  if(!calib[n]){continue;};
	  if(ocd[n].selectNo>0){}else{continue;};
	  // recheck min and max for this curve
	  minc=Math.floor((minx-calib[n].start)/calib[n].resolution);
	  maxc=Math.ceil((maxx-calib[n].start)/calib[n].resolution);
	  if(minc<0){minc=0;};
	  if(maxc>=calib[n].bp.length){maxc=calib[n].bp.length-1;};
	  j=0;
	  xc.length=0;yc.length=0;
	  for(i=minc;i<=maxc;i++)
	  {
	   xc[j]=(i*calib[n].resolution)+calib[n].start;
	   yc[j]=calib[n].bp[i]-calib[n].sigma[i];
	   if(plotOptions.showF14C||plotOptions.showDelta14C){yc[j]=Date_F14C(yc[j],xc[j]);};
       j++;
	  };
      for(i=maxc;i>=minc;i--)
	  {
	   xc[j]=(i*calib[n].resolution)+calib[n].start;
	   yc[j]=calib[n].bp[i]+calib[n].sigma[i];
	   if(plotOptions.showF14C||plotOptions.showDelta14C){yc[j]=Date_F14C(yc[j],xc[j]);};
	   j++;
	  };
	  curvegroup=curvearea.group("curve");
	  if((calib[n].color)&&(!plotOptions.showBandW))
	  {
	   curvegroup.setAttributeNS(null,"style","fill:"+calib[n].color
	    + "; stroke:"+calib[n].color);
	  };
	  if(calib[n].showCurve)
	  {
	   curvegroup.appendChild(curvearea.polygon("curve",xc,yc));
	  };
	  if(calib[n].rawcal && calib[n].showCurveRaw)
	  {
	   for(i=0;i<calib[n].rawcal.length;i++)
	   {
	    if((calib[n].rawcal[i]>minx)&&(calib[n].rawcal[i]<maxx))
		{
		 if(calib[n].showCurve)
		 {
		  if(plotOptions.showF14C||plotOptions.showDelta14C)
		  {
		   curvegroup.appendChild(curvearea.line(calib[n].rawcal[i],Date_F14C(calib[n].rawbp[i]-calib[n].rawsigma[i],calib[n].rawcal[i]),
		    calib[n].rawcal[i],Date_F14C(calib[n].rawbp[i]+calib[n].rawsigma[i],calib[n].rawcal[i])));
		  }
		  else
		  {
		   curvegroup.appendChild(curvearea.line(calib[n].rawcal[i],calib[n].rawbp[i]-calib[n].rawsigma[i],
		    calib[n].rawcal[i],calib[n].rawbp[i]+calib[n].rawsigma[i]));
		  };
		 }
		 else
		 {
		  if(plotOptions.showF14C||plotOptions.showDelta14C)
		  {
		   if(typeof(calib[n].rawcalsig)=='undefined')
		   {
		    curvegroup.appendChild(curvearea.errorbars(calib[n].rawcal[i],Date_F14C(calib[n].rawbp[i],calib[n].rawcal[i]),0,DateErr_F14CErr(calib[n].rawbp[i],calib[n].rawsigma[i],calib[n].rawcal[i])));
		   }
		   else
		   {
			curvegroup.appendChild(curvearea.errorbars(calib[n].rawcal[i],Date_F14C(calib[n].rawbp[i],calib[n].rawcal[i]),calib[n].rawcalsig[i],DateErr_F14CErr(calib[n].rawbp[i],calib[n].rawsigma[i],calib[n].rawcal[i])));
		   };
		   temp=curvearea.symbol(calib[n].rawcal[i],Date_F14C(calib[n].rawbp[i],calib[n].rawcal[i]),calib[n].markerSize,calib[n].marker);
		  }
		  else
		  {
		   if(typeof(calib[n].rawcalsig)=='undefined')
		   {
		    curvegroup.appendChild(curvearea.errorbars(calib[n].rawcal[i],calib[n].rawbp[i],0,calib[n].rawsigma[i]));
		   }
		   else
		   {
		    curvegroup.appendChild(curvearea.errorbars(calib[n].rawcal[i],calib[n].rawbp[i],calib[n].rawcalsig[i],calib[n].rawsigma[i]));
		   };
		   temp=curvearea.symbol(calib[n].rawcal[i],calib[n].rawbp[i],calib[n].markerSize,calib[n].marker);
		  };
	      temp.setAttributeNS(null,"style","stroke:"+calib[n].color+";fill:"+calib[n].fill+";fill-opacity:1");
	      curvegroup.appendChild(temp);
		 };
		};
	   };
	  }; 
 	  curvearea.appendChild(curvegroup);
	  plotarea.appendChild(curvearea.element);
	  xc.length=0;
	  yc.length=0;
	 };
	};

	// plot a depth model etc
	switch(plotOptions.viewType)
	{
	case "z":
	 if(!plotOptions.showInterpolation){break;};
 	 plotarea.map(0,0,1,1);
     curvearea=plotarea.viewPort("curvarea",0,0,1,1);
 	 if(reversed)
	 {
      curvearea.map(minx,maxy,maxx-minx,miny-maxy);
	 }
	 else
	 {
      curvearea.map(minx,miny,maxx-minx,maxy-miny);
     };
	 if(model && model.group && model.group.length)
	 {
	  for(m=0;m<model.group.length;m++)
	  {
	   colr=getColor(m);
	   if((model.group[m].type!="U_Sequence")&&(model.group[m].type!="P_Sequence"))
	   {
	    continue;
	   };
       for(k=3;k>0;k--)
       {
		  if(plotOptions.showRange[k])
		  {
		   j=0;
		   if(model.group[m].depth_list)
		   {
			depth_info=model.group[m].depth_list;
		   }
		   else
		   {
			depth_info=model.group[m].contents;
		   };
		   if(depth_info.length==0){continue;}; // don't plot sections with no internal points
		   for(im=-1;im<depth_info.length+1;im++)
		   {
			if(im==-1){i=model.group[m].start;}
			else
			{
			 if(im==depth_info.length){i=model.group[m].end;}
			 else
			 {
			  i=depth_info[im];
			 };
			};
			if(!ocd[i]){continue;};
			if(!isNaN(ocd[i].yParam) && (ocd[i].selectNo>0))
			{
			 if(plotOptions.showPosterior)
			 {
			  if(ocd[i].posterior && ocd[i].posterior.range && ocd[i].posterior.range[k] && ocd[i].posterior.range[k].length)
			  {
			   depthModel[j]=new Object;
			   depthModel[j].x1=ocd[i].posterior.range[k][0][0];
			   depthModel[j].x2=ocd[i].posterior.range[k][ocd[i].posterior.range[k].length-1][1];
			   depthModel[j].y=ocd[i].yParam;
			   j++;
			  };
			 }
			 else
			 {
			  if(plotOptions.showLikelihood && ocd[i].likelihood && ocd[i].likelihood.range && ocd[i].likelihood.range[k])
			  {
			   depthModel[j]=new Object;
			   depthModel[j].x1=ocd[i].likelihood.range[k][0][0];
			   depthModel[j].x2=ocd[i].likelihood.range[k][ocd[i].likelihood.range[k].length-1][1];
			   depthModel[j].y=ocd[i].yParam;
			   j++;
			  };
			 };
			};
		   };
		   if(depthModel.length)
		   {
			depthModel.sort(sortDepth);
			d="M";
			for(i=0;i<depthModel.length;i++)
			{
			 if(i==1){d+=" L";};
			 d+=curvearea.ux(depthModel[i].x1)+","+curvearea.uy(depthModel[i].y)+" ";
			};
			d+=" L";
			for(i=depthModel.length-1;i>=0;i--)
			{
			 if(i==depthModel.length-2){d+=" L";};
			 d+=curvearea.ux(depthModel[i].x2)+","+curvearea.uy(depthModel[i].y)+" ";
			};
			depthModel.length=0;
			d+=" z";
			path=curvearea.createElement("path");
			path.setAttributeNS(null,"d",d);
			path.setAttributeNS(null,"class","model"+k);
			if(plotOptions.showBandW)
			{
			 path.setAttributeNS(null,"style","fill:none; stroke:"+colr);
			}
			else
			{
			 path.setAttributeNS(null,"style","fill:"+colr
			  + "; stroke:"+colr);
			};
			curvearea.appendChild(path);
		   };
		  };
		 };
	  };
	  for(m=0;m<model.element.length;m++)
	  {
	   if(plotOptions.showEnsembles && model.element[m] && typeof(model.element[m].age_depth_ensembles)!='undefined')
	   {
	    for(k=0;(k<model.element[m].age_depth_ensembles.length)&&(k<plotOptions.showEnsembles);k++)
	    {
	     curvearea.appendChild(curvearea.polyline("ensemble_"+m+"_"+k,model.element[m].age_depth_ensembles[k],
	     	model.element[m].age_depth_z,"ensemble"));
	    };
	   };
	  };
	  plotarea.appendChild(curvearea.element);
	 };
	 break;
	};

	// plot the distributions
 	if(reversed)
	{
	 yspace=-2*(maxy-miny)/(1.7*plotOptions.plotsPerPage);
 	 plotarea.map(minx,maxy,maxx-minx,miny-maxy);
 	 textarea.map(minx,maxy,maxx-minx,miny-maxy);
	}
	else
	{
	 yspace=2*(maxy-miny)/(1.7*plotOptions.plotsPerPage);
 	 plotarea.map(minx,miny,maxx-minx,maxy-miny);
 	 textarea.map(minx,miny,maxx-minx,maxy-miny);
	};
	yspace*=plotOptions.plotHeightMulti/plotOptions.plotHeight;
	for(i=start;i<finish;i++)
	{
	 if(ocd[i])
	 {
	  temp=ocd[i].yParam;
	  if((opt=="curve")&&(plotOptions.showF14C||plotOptions.showDelta14C)){temp=Date_F14C(temp,ocd[i].xParam);};
	 }
	 else
	 {
	  continue;
	 };
	 if(!isNaN(temp) && (ocd[i].selectNo>0) && (temp) > miny && (temp < maxy))
	 {
	  if((opt=="curve") && (plotOptions.showRanges || plotOptions.showSigma) && !plotOptions.showDistribution)
	  {
	   if(plotOptions.showF14C||plotOptions.showDelta14C)
	   {
	    if(reversed)
	    {
	     temp=temp-DateErr_F14CErr(ocd[i].yParam,ocd[i].error,ocd[i].xParam)*10;
	     temp2=DateErr_F14CErr(ocd[i].yParam,ocd[i].error,ocd[i].xParam)*20;
	    }
	    else
	    {
	     temp=temp+DateErr_F14CErr(ocd[i].yParam,ocd[i].error,ocd[i].xParam)*10;
	     temp2=-DateErr_F14CErr(ocd[i].yParam,ocd[i].error,ocd[i].xParam)*20;
	    };
	   }
	   else
	   {
	    if(reversed)
	    {
	     temp=temp-ocd[i].error*10;
	     temp2=ocd[i].error*20;
	    }
	    else
	    {
	     temp=temp+ocd[i].error*10;
	     temp2=-ocd[i].error*20;
	    };
	   };
	   itemarea=plotarea.viewPort("itemarea"+i,minx,temp,maxx-minx,temp2);
	   itemlabelarea=textarea.viewPort("itemarea"+i,minx,temp,maxx-minx,temp2);
	  }
	  else
	  {
	   if((opt=="z")&&(ocd[i].autoz)){continue;};
	   itemarea=plotarea.viewPort("itemarea"+i,minx,temp-yspace,maxx-minx,yspace*2);
	   itemlabelarea=textarea.viewPort("itemarea"+i,minx,temp-yspace,maxx-minx,yspace*2);
	  };
	  itemarea.drawDist(i);
	  plotarea.appendChild(itemarea.element);
	  itemlabelarea.drawLabel(i);
	  textarea.appendChild(itemlabelarea.element);
	 };
	};	
  
	// put all of the elements on the plot
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	mainViewport.appendChild(tabarea.element);
	mainViewport.appendChild(plotarea.element);
	mainViewport.appendChild(textarea.element);
	mainViewport.appendChild(mainViewport.rectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	  plotOptions.plotWidth,plotOptions.plotHeight,"border"));
	
	// draw x axis
	mainViewport.maxXLabel=8/plotOptions.scaleFont;
	if(plotOptions.showPosterior)
	{
	 mainViewport.drawXAxis(minx,maxx,label.xAxisPost,label.axisPostPrefix,true);
	}
	else
	{
	 if((plotOptions.showLikelihood && plotOptions.radiocarbon)||opt=="curve")
	 {
	  mainViewport.drawXAxis(minx,maxx,label.xAxisCalib,label.axisCalibPrefix,true);
	 }
	 else
	 {
	  mainViewport.drawXAxis(minx,maxx,label.xAxisCalend,label.axisCalendPrefix,true);
	 };
	};

	// draw y axis
    mainViewport.ylabel=label.yAxisPos; 
	switch(opt)
	{
	case "curve":
	 if(plotOptions.showF14C||plotOptions.showDelta14C)
	 {
	  if(plotOptions.showDelta14C)
	  {
       mainViewport.ylabel=label.yAxisDelta14C; 
	  }
	  else
	  {
       mainViewport.ylabel=label.yAxisF14C; 
      };
	 }
	 else
	 {
      mainViewport.ylabel=label.yAxisBP; 
	 };
	case "z":
     mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
     if(plotOptions.showReversed)
     {
  	  mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
  	   plotOptions.plotWidth,plotOptions.plotHeight,
  	   0,maxy,plotOptions.plotWidth,miny-maxy);
     }
     else
     {
  	  mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY,
  	   plotOptions.plotWidth,plotOptions.plotHeight,
  	   0,miny,plotOptions.plotWidth,maxy-miny);
  	 };
	 if(plotOptions.labelY!=""){mainViewport.ylabel=plotOptions.labelY;};
	 if(plotOptions.showGrid){mainViewport.grid("y",miny,maxy,plotOptions.plotWidth,plotOptions.majory);};
	 mainViewport.axis("y",miny,maxy,plotOptions.majory,plotOptions.minory);
	 break;
	};
	
	// put the data on top if required
	if(plotOptions.showData)
	{
     mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
     // put the data curve on the plot
	 plotarea=mainViewport.viewPort("dataarea",
	  plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.plotHeight,
	  plotOptions.plotWidth,plotOptions.plotHeightData);
	// set clipping region
	 plotarea.map(0,0,1,1);
	 plotarea.clipPath(plotarea.rectangle(0,0,1,1,""));
	 plotarea.map(minx,plotOptions.data.min,maxx-minx,plotOptions.data.max-plotOptions.data.min);
	 // set up the data curve over this range
	 xc.length=0;
	 yc.length=0;
	 var lastin=false;
	 var ind=0;
	 for(i=0;i<plotOptions.data.t.length;i++)
	 {
	  if((plotOptions.data.t[i]>=minx) &&
	     (plotOptions.data.t[i]<=maxx))
	  {
	   if((!lastin) && (i>0))
	   {
	    // add previous on to go to the edge
	    xc[ind]=plotOptions.data.t[i-1];
	    yc[ind]=plotOptions.data.d[i-1];
	    ind++;
	   };
	   xc[ind]=plotOptions.data.t[i];
	   yc[ind]=plotOptions.data.d[i];
	   ind++;
	   lastin=true;
	  }
	  else
	  {
	   if(lastin)
	   {
	    // add this one to go to the edge;
	    xc[ind]=plotOptions.data.t[i];
	    yc[ind]=plotOptions.data.d[i];
	    ind++;
	   };
	   lastin=false;
	  };
	 };
	 plotarea.appendChild(plotarea.polyline("datacurve",xc,yc,"datacurve"));
	 mainViewport.appendChild(plotarea.element);
	 mainViewport.appendChild(mainViewport.rectangle(plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.plotHeight,
	  plotOptions.plotWidth,plotOptions.plotHeightData,"border"));
  	 mainViewport.mapRectangle(plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.plotHeight,
  	   plotOptions.plotWidth,plotOptions.plotHeightData,
  	   0,plotOptions.data.min,plotOptions.plotWidth,plotOptions.data.max-plotOptions.data.min);
     mainViewport.maxYLabel=5;	 
     mainViewport.ylabel=tidyText(decodeURI(plotOptions.data.label));
	 mainViewport.axis("y",plotOptions.data.min,plotOptions.data.max,plotOptions.majory,plotOptions.minory);
	};
	// put title on
	if(plotOptions.labelTitle)
	{
	 mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refarea=mainViewport.viewPort("references",0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refarea.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refarea.appendChild(refarea.text(plotOptions.plotPosX+plotOptions.plotWidth/2,plotOptions.plotPosY+plotOptions.plotHeight,
	   plotOptions.labelTitle,14,"middle",1.5));
	 mainViewport.appendChild(refarea.element);
    };
	// put reference on
	if(plotOptions.showReference)
	{
	 mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refarea=mainViewport.viewPort("references",0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refarea.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	 refstr="";
	 if(ocd[0] && ocd[0].ref)
	 {
	  refstr=ocd[0].ref;
	 };
	 if(plotOptions.reflist.length==1)
	 {
	  if(calib[plotOptions.reflist[0]] && calib[plotOptions.reflist[0]].ref)
	  {
	   refstr+=" "+calib[plotOptions.reflist[0]].ref;
	  }
	  else
	  {
	   if(ocd[plotOptions.reflist[0]])
	   {
	    refstr+=" "+makeTitle(ocd[plotOptions.reflist[0]],true);
	   };
	  };
	 }
	 else
	 {
 	  if(calib[0] && calib[0].ref && (calib.length<2) && (opt=="curve"))
	  {
	   refstr+=" "+calib[0].ref;
	  };
	 };
	 refarea.appendChild(refarea.text(plotOptions.plotPosX,plotOptions.plotPosY+plotOptions.plotHeight,
	   refstr,6,"",0.3));
	 x_ref=plotOptions.frameWidth*0.6;
	 y_ref=plotOptions.plotPosY+plotOptions.plotHeight-0.1;
	 if(plotOptions.reflist.length >= 2)
	 {
	  for(n=0;n<plotOptions.reflist.length;n++)
	  {
	   if(ocd[plotOptions.reflist[n]].selectNo>0){}else{continue;};
	   if(calib[plotOptions.reflist[n]])
	   {
	   	if(plotOptions.reflist[n]==0 && !plotOptions.showCurve && !plotOptions.showCurveRaw){continue;};
	    if(!calib[plotOptions.reflist[n]]){continue;};
	    y_ref-=0.25*plotOptions.scaleFont*plotOptions.scaleLine;
	    curveElement=refarea.text(x_ref,y_ref,calib[plotOptions.reflist[n]].ref,6,"",0.3,calib[plotOptions.reflist[n]].color);
 	    refarea.appendChild(curveElement);
 	    if(calib[plotOptions.reflist[n]].marker && calib[plotOptions.reflist[n]].showCurveRaw && (!calib[plotOptions.reflist[n]].showCurve))
 	    {
 	     curveElement=refarea.symbol(x_ref-0.2,y_ref+0.15*plotOptions.scaleFont*plotOptions.scaleLine,calib[plotOptions.reflist[n]].markerSize,calib[plotOptions.reflist[n]].marker);
 	     curveElement.setAttributeNS(null,"style","stroke:"+calib[plotOptions.reflist[n]].color+";fill:"+calib[plotOptions.reflist[n]].fill+";");
 	     refarea.appendChild(curveElement);
 	    };
 	   }
 	   else
 	   {
	    if(ocd[plotOptions.reflist[n]])
	    {
	     if(!ocd[plotOptions.reflist[n]]){continue;};
	     y_ref-=0.25*plotOptions.scaleFont*plotOptions.scaleLine;
	     curveElement=refarea.text(x_ref,y_ref,makeTitle(ocd[plotOptions.reflist[n]],true),6,"",0.3,ocd[plotOptions.reflist[n]].color);
 	     refarea.appendChild(curveElement);
	    };
	   };
	  };
	 };
	 mainViewport.appendChild(refarea.element);
	};
  };
  
  
  function keymap(i,j)
  {
   return (j-5)/40;
  };
  function correlation()
  {
    var el;
    var plotarea;
    var distarea;
    var mainViewport;
    var mainFrame;
    var keyarea;
    var i,j;
    var val;
    var minx,maxx;
    var miny,maxy;
    var minz,maxz;
    var item;
    // link plot to SVG document
    mainFrame=attachSVGDocument(plotOptions.frameWidth,plotOptions.frameHeight,plotOptions.scale,plotOptions.scaleFont,plotOptions.scaleLine);
	mainFrame.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
   
	// set up main viewport
	mainViewport=mainFrame.viewPort("mainviewport",0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
    mainFrame.appendChild(mainViewport.element);

	item=ocd[plotOptions.plotFrom].correlation;
	if(!item){return;};

    minx=plotOptions.minx;
    maxx=plotOptions.maxx;
    miny=plotOptions.miny;
    maxy=plotOptions.maxy;
    minz=plotOptions.minz;
    maxz=plotOptions.maxz;

	plotarea=mainViewport.viewPort("plotarea",plotOptions.plotPosX,plotOptions.plotPosY,
	 plotOptions.plotWidth,plotOptions.plotHeight);
	// set clipping region
	plotarea.map(0,0,1,1);
	plotarea.clipPath(plotarea.rectangle(0,0,1,1,""));

	plotarea.map(0,0,1,1);
	plotarea.appendChild(plotarea.rectangle(0,0,1,1,"fillbackground"));
	plotarea.map(minx,miny,maxx-minx,maxy-miny);
    if(plotOptions.show3DEffects)
    {
	 plotarea.mapPlot(item.x.start,item.y.start,item.x.end-item.x.start,item.y.end-item.y.start,
	  item.x.divisions,
	  item.x.divisions,normmap,"filter:url(#Correlation2)");
    }
    else
    {
	 plotarea.mapPlot(item.x.start,item.y.start,item.x.end-item.x.start,item.y.end-item.y.start,
	  item.x.divisions,
	  item.x.divisions,normmap,"filter:url(#Correlation1)");
    };

	// put the title on the plot
	if(ocd[plotOptions.plotFrom] && plotOptions.showTitle && ocd[plotOptions.plotFrom].name)
	{
	 plotarea.map(0,0,1,1);
	 if(plotOptions.labelTitle=="")
	 {
	  plotarea.appendChild(plotarea.text(0.5,1,ocd[plotOptions.plotFrom].name,12,"middle",-1.25,"white"));
	 }
	 else
	 {
	  plotarea.appendChild(plotarea.text(0.5,1,plotOptions.labelTitle,12,"middle",-1.25,"white"));
	 };
	};

 	mainViewport.appendChild(plotarea.element);
	mainViewport.appendChild(mainFrame.rectangle(plotOptions.plotPosX,plotOptions.plotPosY,
	 plotOptions.plotWidth,plotOptions.plotHeight,"border"));

    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	keyarea=mainViewport.viewPort("keyarea",plotOptions.plotPosX+plotOptions.plotWidth+1,plotOptions.plotPosY,
	 0.8,plotOptions.plotHeight);
	// set clipping region
	keyarea.map(0,0,1,1);
	keyarea.clipPath(keyarea.rectangle(0,0,1,1,""));

	keyarea.map(0,0,1,1,"blur");
	keyarea.appendChild(keyarea.rectangle(0,0,1,1,"fillbackground"));
 
    keyarea.mapPlot(-2,-0.125,6,1.25,4,50,keymap,"filter:url(#Correlation1)");
	mainViewport.appendChild(keyarea.element);

	// change colors of axis elements
	mainViewport.gridStyle="gridline2";
	mainViewport.minorpipStyle="minorpip2";
	mainViewport.majorpipStyle="majorpip2";

	// draw X axis
	mainViewport.drawXAxis(minx,maxx,label.xAxisPost,"",true);

	// draw Y axis
	mainViewport.drawYAxis(miny,maxy,label.xAxisPost,"",true);

	// draw Z axis
    mainViewport.map(0,0,plotOptions.frameWidth,plotOptions.frameHeight);
	mainViewport.mapRectangle(plotOptions.plotPosX+plotOptions.plotWidth+1,plotOptions.plotPosY,
	 0.8,plotOptions.plotHeight,0,minz,0.8,maxz-minz);
	mainViewport.ylabel="";
	mainViewport.ylabeller=ViewPort.prototype.ylabeller;
	mainViewport.axis("y",minz,maxz,0,0);
 };

 ViewPort.prototype.drawGroup=function(grp,test)
 {
  var start,end,len,cont,vp,i,x,xa,ya,newel,ps,startop,endop;
  var color;
  var spn,offs;
  xa=new Array();ya=new Array;
  color=getColor(grp);
  start=model.element[model.group[grp].start];
  end=model.element[model.group[grp].end];
  startop=model.group[grp].start_op;
  endop=model.group[grp].end_op;
  len=Math.abs(end.timepos-start.timepos);
  cont=model.group[grp].contents;
  // find horizontal position
  if(this.grpPos)
  {
	if(!model.element[model.group[grp].parent])
	{
	  model.element[model.group[grp].parent]=new Object;
	};
	if(!model.element[model.group[grp].parent].grpPos)
	{
	  this.grpPos+=1.0;
 	  model.element[model.group[grp].parent].grpPos=this.grpPos;
	}
	else
	{
	  this.grpPos=
 	  model.element[model.group[grp].parent].grpPos;
	};
  }
  else
  {
	this.grpPos=0.2;
	if(!model.element[model.group[grp].parent])
	{
	  model.element[model.group[grp].parent]=new Object;
	};
	model.element[model.group[grp].parent].grpPos=this.grpPos;
  };
  if(test){return 0;};

  vp=this.viewPort("group"+grp,this.grpPos,start.timepos,0.6,end.timepos-start.timepos);
  switch(startop)
  {
  case "NoOp":
   switch(endop)
   {
   case "D_Sequence": case "Combine":
    start={};
	if(isNaN(start.timepos)||(model.element[model.group[grp].parent].timepos<start.timepos))
	{
	 start=model.element[model.group[grp].parent];
	};
	for(i=0;i<cont.length;i++)
	{
	 if(isNaN(start.timepos)||(model.element[cont[i]].timepos<start.timepos))
	 {
	  start=model.element[cont[i]];
	 };
	 if(isNaN(end.timepos)||(model.element[cont[i]].timepos>end.timepos))
	 {
	  end=model.element[cont[i]];
	 };
	 vp=this.viewPort("group"+grp,this.grpPos,start.timepos,0.6,end.timepos-start.timepos);
	 xa[0]=0;ya[0]=0;
	 xa[1]=0.25;ya[1]=0;
	 xa[2]=0.25;ya[2]=1;
	 xa[3]=0;ya[3]=1;
     vp.map(0,0,1,1);
	};
    break;
   };
   break;
  case "Boundary":
   switch(endop)
   {
   case "Boundary":
	xa[0]=0;ya[0]=0;
	xa[1]=cont.length/len;ya[1]=0;
	xa[2]=cont.length/len;ya[2]=1;
	xa[3]=0;ya[3]=1;
    vp.map(0,0,1,1);
	break;
   case "Sigma_Boundary":
    vp.map(0,0,1,1);
	xa[0]=0;ya[0]=0;
	for(i=0;i<26;i++){xa[i+1]=Math.exp(-i*i/200);ya[i+1]=i/10;};
	xa[i+1]=0;ya[i+1]=(i-1)/10;
	break;
   case "Tau_Boundary":
    vp.map(0,0,1,1);
	xa[0]=0;ya[0]=0;
	for(i=0;i<30;i++){xa[i+1]=Math.exp(-i/10);ya[i+1]=i/10;};
	xa[i+1]=0;ya[i+1]=(i-1)/10;
	break;
   case "Zero_Boundary":
    vp.map(0,0,1,1);
	xa[0]=0;ya[0]=0;
	xa[1]=1,ya[1]=0;
	xa[2]=0,ya[2]=1;
	break;
   };
   break;
  case "Sigma_Boundary":
   switch(endop)
   {
   case "Boundary":
    vp.map(0,-1,1,1);
	xa[0]=0;ya[0]=-2.5;
	for(i=-25;i<1;i++){xa[i+26]=Math.exp(-i*i/200);ya[i+26]=i/10;};
	xa[i+26]=0;ya[i+26]=(i-1)/10;
	break;
   case "Sigma_Boundary":
    vp.map(0,-1,1,2);
	xa[0]=0;ya[0]=-2.5;
	for(i=-25;i<26;i++){xa[i+26]=Math.exp(-i*i/200);ya[i+26]=i/10;};
	xa[i+26]=0;ya[i+26]=(i-1)/10;
	break;
   };
   break;
  case "Tau_Boundary":
   switch(endop)
   {
   case "Boundary":
    vp.map(0,-1,1,1);
	xa[0]=0;ya[0]=-2.5;
	for(i=-30;i<1;i++){xa[i+31]=Math.exp(i/10);ya[i+31]=i/10;};
	xa[i+31]=0;ya[i+31]=(i-1)/10;
	break;
   };
   break;
  case "Zero_Boundary":
   switch(endop)
   {
   case "Boundary":
    vp.map(0,-1,1,1);
	xa[0]=0;ya[0]=-1;
	xa[1]=1;ya[1]=0;
	xa[2]=0;ya[2]=0;
	break;
   };
   break;
  };
  if(xa.length>0)
  {
   newel=vp.polygon("dist"+grp,xa,ya,"modeldist");
   newel.setAttributeNS(null,"style","fill:url(#"+model.group[grp].type+");stroke:"+color);
   vp.appendChild(newel);
   newel=vp.polygon("dist"+grp,xa,ya,"modeldist");
   newel.setAttributeNS(null,"style","fill:"+color+";stroke:"+color);
   vp.appendChild(newel);
  };
  vp.map(0,start.timepos,1,end.timepos-start.timepos);
  for(i=0;i<cont.length;i++)
  {
   ps=model.element[cont[i]].timepos;
   switch(startop)
   {
   case "NoOp":
    switch(endop)
    {
    case "D_Sequence": case "Combine":
	 x=0.25;
	 break;
	};
	break;
   case "Boundary":
    spn=end.timepos-start.timepos;
    offs=ps-start.timepos;
    switch(endop)
    {
    case "Boundary":
	 x=cont.length/len;
	 break;
	case "Sigma_Boundary":
	 x=Math.exp(-(offs*offs)/(2*spn*spn));
	 break;
	case "Tau_Boundary":
	 x=Math.exp(-offs/spn);
	 break;
	case "Zero_Boundary":
	 x=1.0-offs/spn;
	 break;
	};
	break;
   case "Sigma_Boundary":
    switch(endop)
    {
    case "Boundary":
     spn=end.timepos-start.timepos;
     offs=end.timepos-ps;
	 x=Math.exp(-(offs*offs)/(2*spn*spn));
	 break;
	case "Sigma_Boundary":
     spn=(end.timepos-start.timepos)/2;
     offs=(start.timepos+end.timepos)/2-ps;
	 x=Math.exp(-(offs*offs)/(2*spn*spn));
	 break;
	};
	break;
   case "Tau_Boundary":
    switch(endop)
    {
    case "Boundary":
     spn=end.timepos-start.timepos;
     offs=end.timepos-ps;
	 x=Math.exp(-offs/spn);
	 break;
	};
	break;
   case "Zero_Boundary":
    switch(endop)
    {
    case "Boundary":
     spn=end.timepos-start.timepos;
     offs=end.timepos-ps;
	 x=1.0-offs/spn;
	 break;
    };
    break;
   };
   newel=vp.line(0,model.element[cont[i]].timepos,x,model.element[cont[i]].timepos,"modelline");
   newel.setAttributeNS(null,"style","stroke:"+color);
   vp.appendChild(newel);
  };
  if(startop!="NoOp")
  {
   newel=vp.line(0,start.timepos,1,start.timepos,"modelline");
   newel.setAttributeNS(null,"style","stroke:"+color);
   vp.appendChild(newel);
   newel=vp.line(0,end.timepos,1,end.timepos,"modelline");
   newel.setAttributeNS(null,"style","stroke:"+color);
   vp.appendChild(newel);
  }
  else
  {
   newel=vp.line(0,model.element[model.group[grp].parent].timepos,1,model.element[model.group[grp].parent].timepos,"modelline");
   newel.setAttributeNS(null,"style","stroke:"+color);
   vp.appendChild(newel);
  };
  this.appendChild(vp.element);
  return 0;
 }; 

 function model_schematic()
 {
    var mainViewport;
    var mainFrame;
	var i;
	var sub;
	var line;
	var used;

     // link plot to SVG document
    mainFrame=attachSVGDocument(plotOptions.frameWidth,plotOptions.frameHeight,plotOptions.scale,plotOptions.scaleFont,plotOptions.scaleLine);
	mainFrame.map(0,0,plotOptions.frameWidth,plotOptions.maxz-plotOptions.minz+14);
   
	// set up main viewport
	mainViewport=mainFrame.viewPort("mainviewport",0.5,10,plotOptions.plotWidth,plotOptions.maxz-plotOptions.minz+4);
    mainViewport.map(0,0,plotOptions.plotWidth,plotOptions.plotHeight);
    mainFrame.appendChild(mainViewport.element);

	// define limits
	if(plotOptions.showReversed)
	{
 	 mainViewport.map(0,plotOptions.minz-2,1,plotOptions.maxz-plotOptions.minz+4);
	}
	else
	{
 	 mainViewport.map(0,plotOptions.maxz+2,1,plotOptions.minz-plotOptions.maxz-4);
	};

	// names
	sub=mainViewport.viewPort("names",0,plotOptions.minz,0.3,plotOptions.maxz-plotOptions.minz);
	sub.map(0,plotOptions.minz,1,plotOptions.maxz-plotOptions.minz);
//	sub.appendChild(sub.rectangle(0,plotOptions.minz-1,1,plotOptions.maxz-plotOptions.minz+2,"fillmodel"));
    for(i=1;i<model.element.length;i++)
	{
	 if(model.element[i] && (!isNaN(model.element[i].timepos)) && ocd[i])
	 {
	  sub.appendChild(sub.text(0.9,model.element[i].timepos,makeTitle(ocd[i]),8,"end",-0.5));
	 };
	};
	mainViewport.appendChild(sub.element);

	// constraints
	sub=mainViewport.viewPort("constraints",0.3,plotOptions.minz,0.1,plotOptions.maxz-plotOptions.minz);
	sub.map(0,plotOptions.minz,1,plotOptions.maxz-plotOptions.minz);
	used=new Array();
    for(i=0;i<model.element.length;i++)
	{
	 used[i]=false;
	 if(model.element[i])
	 {
	  sub.appendChild(sub.line(0,model.element[i].timepos,1,model.element[i].timepos,"gridline"));
	 };
	};
	sub.appendChild(sub.rectangle(0,plotOptions.minz-1,1,plotOptions.maxz-plotOptions.minz+2,"fillmodel"));
	for(i=0;i<model.constr.length;i++)
	{
	 if(!model.constr[i]){continue;};
	 var cn=model.constr[i];
	 used[cn[0]]=true;used[cn[1]]=true;
	};
    for(i=0;i<model.element.length;i++)
	{
	 if(used[i])
	 {
	  sub.appendChild(sub.line(0,model.element[i].timepos,1,model.element[i].timepos,"gridline"));
	 };
	};
	for(i=0;i<model.constr.length;i++)
	{
	 if(!model.constr[i]){continue;};
	 var ps=((i%10+1)/(12.5)+((i/11)%10)/21)*0.8;
	 var cn=model.constr[i];
	 var ln;
	 used[cn[0]]=true;used[cn[1]]=true;
	 if(model.element[cn[0]].timepos < model.element[cn[1]].timepos)
	 {
 	  sub.appendChild(sub.line(ps,model.element[cn[0]].timepos+0.5,ps,model.element[cn[1]].timepos-0.5,"constrshadow"));
	  sub.appendChild(ln=sub.line(ps,model.element[cn[0]].timepos,ps,model.element[cn[1]].timepos,"constrok"));
	 }
	 else
	 {
 	  sub.appendChild(sub.line(ps,model.element[cn[1]].timepos+0.5,ps,model.element[cn[0]].timepos-0.5,"constrshadow"));
	  sub.appendChild(ln=sub.line(ps,model.element[cn[1]].timepos,ps,model.element[cn[0]].timepos,"constrerror"));
	 };
	 if(cn[3]=="Q")
	 {
	  ln.setAttributeNS(null,"style","stroke:#00ff00");
	 };
	};
	mainViewport.appendChild(sub.element);

	// main groupings
 	sub=mainViewport.viewPort("groups",0.4,plotOptions.minz,0.5,plotOptions.maxz-plotOptions.minz);
	sub.map(0,plotOptions.minz,1,plotOptions.maxz-plotOptions.minz);
    for(i=0;i<model.element.length;i++)
	{
	 if(model.element[i])
	 {
	  sub.appendChild(sub.line(0,model.element[i].timepos,1,model.element[i].timepos,"gridline"));
	 };
	};
	sub.appendChild(sub.rectangle(0,plotOptions.minz-1,1,plotOptions.maxz-plotOptions.minz+2,"fillmodel"));
	sub.clipPath(sub.rectangle(0,plotOptions.minz-1,1,plotOptions.maxz-plotOptions.minz+2,""));
	sub.map(0,plotOptions.minz,6,plotOptions.maxz-plotOptions.minz);
	for(i=0;i<model.group.length;i++)
	{
		sub.drawGroup(i,true);
	};
	sub.map(0,plotOptions.minz,sub.grpPos+2,plotOptions.maxz-plotOptions.minz);
	sub.grpPos=undefined;
	for(i=0;i<model.group.length;i++)
	{
		sub.drawGroup(i,false);
	};
    mainViewport.appendChild(mainViewport.rectangle(0.3,plotOptions.minz-1,0.6,plotOptions.maxz-plotOptions.minz+2,"border"));
	mainViewport.appendChild(sub.element);
};

 	function do_action()
 	{
 	 var x,y;
 	 var cm=plotOptions.pxPerCm*plotOptions.scale;
     switch(mover.action)
     {
      case "zoom":
       if((mover.frm_clientX>10) && (mover.frm_clientY>10) && 
         (mover.to_clientX>10) && (mover.to_clientY>10))
       {
        parent.left.changeZoom((mover.to_clientX/mover.frm_clientX + mover.to_clientY/mover.frm_clientY)/2);
       };
       break;
      case "span":
       x=Math.exp((mover.frm_clientX-mover.to_clientX)/(plotOptions.plotWidth*cm));
       y=Math.exp((mover.to_clientY-mover.frm_clientY)/(plotOptions.plotHeight*cm));       
       if((mover.frm_clientX>10) && (mover.frm_clientY>10) && 
         (mover.to_clientX>10) && (mover.to_clientY>10))
       {
        parent.left.changeSpan(x);
        if(plotOptions.viewType=="individual"){break;};
        parent.left.changeYRange(y);
       };
       break;
      case "move":
       x=(mover.frm_clientX-mover.to_clientX)/(plotOptions.plotWidth*cm);
       parent.left.moveCentre(x);
       if(plotOptions.viewType=="individual"){break;};
       y=(mover.frm_clientY-mover.to_clientY)/(plotOptions.plotHeight*cm);
       switch(plotOptions.viewType)
       {
       case "multiple":
       case "select":
       case "stack":
        y=-y;
        break;
       };
       if(plotOptions.showReversed)
       {
        y=-y;
       };
       parent.left.changeMinY(-y);
       break;
     };
     draw();
     mover.frm_clientX=mover.to_clientX;
     mover.frm_clientY=mover.to_clientY;
 	};
 	function setup_action(event,delay)
 	{
 	 if(mover.tmr){window.clearTimeout(mover.tmr);};
     mover.to_clientX=event.clientX;
     mover.to_clientY=event.clientY;
 	 if(delay)
 	 {
 	  if(!mover.relax)
 	  {
 	   do_action();
 	   mover.relax=true;
 	   window.setTimeout("mover.relax=false;",delay);
 	  };
 	 }
 	 else
 	 {
 	  mover.active=false;
 	  do_action();
      switch(plotOptions.viewType)
      {
      case "multiple":
      case "select":
      case "stack":
 	   plotOptions.miny=Math.round(plotOptions.miny);
 	   plotOptions.maxy=Math.round(plotOptions.maxy);
 	   if(mover.action=="span")
 	   {
 	    parent.left.setPlotsPerPage(Math.round(
 	     (plotOptions.maxy-plotOptions.miny)/mover.prop_filled)-1);
 	   };
       parent.left.updater();
       break;
 	  };
 	 };
 	};
 	function move_bd(event)
 	{
 	 var targ;
     if(!event){event=window.event;};//needed for IE
     if(event.button>1){return;};
	 if (event.target){targ = event.target;}
	 else if(e.srcElement){targ = event.srcElement;}; // for IE
	 if(targ.nodeType == 3){targ = targ.parentNode;}; // defeat Safari bug
     if(event.preventDefault){event.preventDefault();};
     switch(event.type)
     {
     case "mousedown": 
     	mover.active=true;
     	mover.frm_clientX=event.clientX;
     	mover.frm_clientY=event.clientY;
     	mover.action="move";
     	if(event.shiftKey)
     	{
     	 switch(plotOptions.viewType)
     	 {
     	 case "multiple":
     	 case "select":
     	 case "stack":
     	  mover.prop_filled=(plotOptions.maxy-plotOptions.miny)/(plotOptions.plotsPerPage+1);
     	  break;
     	 };
     	 mover.action="span";
     	};
     	if(event.altKey){mover.action="zoom";};
     	break;
     case "mouseout":
     case "mouseup": 
        if(mover.active){setup_action(event,0);};
        break;
     case "mousemove":
        if(mover.active){setup_action(event,100);};
        break;
     };
 	};
