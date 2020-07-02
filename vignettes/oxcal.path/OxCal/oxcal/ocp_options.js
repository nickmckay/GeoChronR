// ocp_options.js
// OxCal Plotting option settings
   var plotOptions=new Object();
   function resetOptions()
   {
    plotOptions.minx=undefined;
    plotOptions.maxx=undefined;
    plotOptions.minorx=0;
    plotOptions.majorx=0;
    plotOptions.miny=undefined;
    plotOptions.maxy=undefined;
    plotOptions.minory=0;
    plotOptions.majory=0;
    plotOptions.minz=0;
    plotOptions.maxz=1;
    plotOptions.labelX="";
    plotOptions.labelY="";
    plotOptions.labelTitle="";
   };
   resetOptions();
   plotOptions.scale=1.0; // zoom scaling
   plotOptions.scaleFont=0.8; // font scaling
   plotOptions.scaleLine=1.0; // line scaling
   plotOptions.pxPerCm=40; // px per cm
   plotOptions.rcConst=8033.0;
   plotOptions.frameWidth=18.0; // cm
   plotOptions.frameWidthSingle=15.5; //cm
   plotOptions.frameWidthNormal=18.0; //cm
   plotOptions.frameHeight=10.0; // cm
   plotOptions.frameHeightSingle=10.0; // cm
   plotOptions.frameHeightMulti=24.0; // cm
   plotOptions.frameHeightDepth=24.0; // cm
   plotOptions.frameHeightCurve=16.5; // cm
   plotOptions.frameHeightCorrel=13.5; // cm
   plotOptions.plotWidth=11.5; // cm
   plotOptions.plotWidthSingle=11.5; // cm
   plotOptions.plotWidthDepth=14.5; // cm
   plotOptions.plotWidthData=14.5; // cm
   plotOptions.plotWidthMulti=17.0; // cm
   plotOptions.plotWidthCurve=14.5; // cm
   plotOptions.plotWidthCorrel=11.5; // cm
   plotOptions.plotHeight=8.0; // cm
   plotOptions.plotHeightSingle=8.0; // cm
   plotOptions.plotHeightMulti=22.0; // cm
   plotOptions.plotHeightDepth=22.0; // cm
   plotOptions.plotHeightCurve=14.5; // cm
   plotOptions.plotHeightCorrel=11.5; // cm
   plotOptions.plotHeightData=4.0; //cm
   plotOptions.plotHeightModel=0.5; //cm
   
   plotOptions.plotHeightTitle=1.0; //cm
   plotOptions.plotPosX=3.0; // cm
   plotOptions.plotPosXNormal=3.0; // cm
   plotOptions.plotPosXMulti=0.5; // cm
   plotOptions.plotPosY=1.5;  // cm
   plotOptions.plotPosYNormal=1.5; // cm
   plotOptions.plotsPerPage=20;
   plotOptions.levelMargin=0.01; // proportion of plot width
   plotOptions.rangeMargin=0.16; // proportion of plot height
   plotOptions.distHeight=0.33; // proportion of plot height
   plotOptions.showNormalised=false; // normalise plot areas
   plotOptions.showCurve=true;
   plotOptions.showF14C=false;
   plotOptions.showDelta14C=false;
   plotOptions.showCurveRaw=false;
   plotOptions.showDistribution=true;
   plotOptions.showRanges=true;
   plotOptions.showNormal=true;
   plotOptions.showReference=true;
   plotOptions.showTitle=true;
   plotOptions.showText=true;
   plotOptions.showParameters=false;
   plotOptions.showVerbs=true;
   plotOptions.showAgreement=false;
   plotOptions.showConvergence=false;
   plotOptions.showEnsembles=0;
   plotOptions.showOutliers=false;
   plotOptions.colorOutliers=false;
   plotOptions.mergeRanges=false;
   plotOptions.showInterpolation=false;
   plotOptions.show3DEffects=false;
   plotOptions.showItalics=true;
   plotOptions.showGrid=true;
   // options needed for table
   plotOptions.showRange=new Array(false,false,true,false);
   plotOptions.showMean=false;
   plotOptions.showSigma=false;
   plotOptions.showMedian=false;
   plotOptions.showWhole=true;
   plotOptions.showLikelihood=true;
   plotOptions.showPosterior=true;
   plotOptions.showIndices=true;
   plotOptions.showReversed=false;
   plotOptions.BPDatum=1950.5; // half way through the year AD1950
   plotOptions.reportingStyle=1;
   plotOptions.showADFirst=false;
   plotOptions.showPropPlace=2;
   plotOptions.roundBy=1;
   plotOptions.showStructure=true;
   plotOptions.showBrackets=false;
   plotOptions.showBandW=false;
   plotOptions.viewType="table";
   plotOptions.dataType="date";
   plotOptions.BCAxis=false;
   plotOptions.radiocarbon=false;
   plotOptions.reflist=new Array();
   plotOptions.radiocarbon=false;
   plotOptions.plotFrom=0;
   plotOptions.plotLast=0;
   plotOptions.plotPrev=0;
   plotOptions.plotTo=0;
   plotOptions.plotColors=new Array("#0000ff","#00ff00","#ff0000","#990099","#009999","#999900","#333333",
      "#000099","#009900","#990000","#330033","#003333","#333300","#999999",
	  "#6666ff","#66ff66","#ff6666","#996699","#669999","#999966","#000000");
   // options associated with data;
   plotOptions.data=new Object();
   plotOptions.data.min=0;
   plotOptions.data.max=1;
   plotOptions.data.label="";
   plotOptions.data.t=new Array();
   plotOptions.data.d=new Array();
   plotOptions.showData=false;
   // options associated with sample data type
   plotOptions.sampleData=new Object();
   // mapping variables
   plotOptions.player_min="NaN";
   plotOptions.player_max="NaN";
   plotOptions.player_proxy="";
   plotOptions.player_proxies=new Array("");
   plotOptions.current=0;
   plotOptions.backwards=false;
   plotOptions.currentMax=100;
   plotOptions.probMax="NaN";
   plotOptions.mapPlotNormalise=true;
   plotOptions.mapPlotCircleZoom=1.0;
   plotOptions.mapColorBy="";
   plotOptions.mapPlotMultiIncr=1;

   function findSize()
   {
    if(plotOptions.viewType)
    {
     // sort out size and method
     plotOptions.plotPosY=plotOptions.plotPosYNormal;
     plotOptions.frameWidth=plotOptions.frameWidthNormal;
     switch(plotOptions.viewType)
     {
     case "table":
     case "individual":
      if(ocd[plotOptions.plotFrom] && ocd[plotOptions.plotFrom].correlation)
      {
       // use correlation view
       plotOptions.frameHeight=plotOptions.frameHeightCorrel;
       plotOptions.plotHeight=plotOptions.plotHeightCorrel;
       plotOptions.plotWidth=plotOptions.plotWidthCorrel;
       plotOptions.plotPosX=plotOptions.plotPosXNormal;
      }
      else
      {
       // normal view
       plotOptions.frameHeight=plotOptions.frameHeightSingle;
       plotOptions.frameWidth=plotOptions.frameWidthSingle;
       plotOptions.plotHeight=plotOptions.plotHeightSingle;
       plotOptions.plotWidth=plotOptions.plotWidthSingle;
       plotOptions.plotPosX=plotOptions.plotPosXNormal;
      };
      break;
	 case "multiple":
	 case "select":
	 case "stack":
      plotOptions.frameHeight=plotOptions.frameHeightMulti;
	  if(plotOptions.showData)
	  {
       plotOptions.plotHeight=plotOptions.plotHeightMulti-plotOptions.plotHeightData;
       plotOptions.plotWidth=plotOptions.plotWidthData;
       plotOptions.plotPosX=plotOptions.plotPosXNormal;
	  }
	  else
	  {
       plotOptions.plotHeight=plotOptions.plotHeightMulti;
       plotOptions.plotWidth=plotOptions.plotWidthMulti;
       plotOptions.plotPosX=plotOptions.plotPosXMulti;
      };
      if(plotOptions.labelTitle)
      {
       plotOptions.plotHeight-=plotOptions.plotHeightTitle;
      };
      break;
	 case "curve":
      plotOptions.frameHeight=plotOptions.frameHeightCurve;
	  if(plotOptions.showData)
	  {
       plotOptions.frameHeight+=plotOptions.plotHeightData;
	  };
      plotOptions.plotHeight=plotOptions.plotHeightCurve;
      plotOptions.plotWidth=plotOptions.plotWidthCurve;
      plotOptions.plotPosX=plotOptions.plotPosXNormal;
      if(plotOptions.labelTitle)
      {
       plotOptions.plotHeight-=plotOptions.plotHeightTitle;
      };
      break;
     case "x":
     case "y":
     case "z":
      plotOptions.frameHeight=plotOptions.frameHeightMulti;
	  if(plotOptions.showData)
	  {
       plotOptions.frameHeight+=plotOptions.plotHeightData;
	  };
      plotOptions.plotHeight=plotOptions.plotHeightMulti;
      plotOptions.plotWidth=plotOptions.plotWidthDepth;
      plotOptions.plotPosX=plotOptions.plotPosXNormal;
      if(plotOptions.labelTitle)
      {
       plotOptions.plotHeight-=plotOptions.plotHeightTitle;
      };
      break;
     case "correlation":
      break;
	 case "model":
	  plotOptions.frameHeight=plotOptions.frameHeightMulti;
     };
     return; 
    };
   };

  function setupPlot()
  {
    var item;
    var minx,maxx;
    var minxt,maxxt;
    var miny,maxy;
    var minz,maxz;
	var yparam;
	var xmargin,zmargin;
	var pagePassed;
	var i,j;
	var shiftUp,topMargin;
    var indexOcd=new Array;
	var defMinX,defMaxX;
	var thisType;
	var plotter;
	var crv=0,sap=0;
	var start, finish;
    start=1;finish=ocd.length;
	if(plotOptions.plotFrom<1){plotOptions.plotFrom=1;};
	// copy simulated dates to data
	for(i=0;i<ocd.length;i++)
	{
	 if(!ocd[i]){continue;};
	 switch(ocd[i].op)
	 {
	 case "R_Simulate":
	 case "C_Simulate":
	  if(!ocd[i].data){ocd[i].data={};};
	  ocd[i].data.sim_date=Number(ocd[i].param.split(":")[0]);
	  break;
	 };
	};
	// copy outliers to main data
	if(model && model.proportional && (plotOptions.showOutliers||plotOptions.colorOutliers))
	{
	 for(i=0;i<model.proportional.length;i++)
	 {
	  if(model.proportional[i].outlier_prior)
	  {
	   j=model.proportional[i].param[0];
	   if(ocd[j].posterior)
	   {
	    ocd[j].posterior.outlier_prior=model.proportional[i].outlier_prior*100;
	    ocd[j].posterior.outlier_possible=model.proportional[i].outlier_possible;
	    ocd[j].posterior.outlier_post=model.proportional[i].outlier_post*100;
	   };
	  };
	 };
	};
    if(plotOptions.viewType)
    {
     // sort out size and method
     switch(plotOptions.viewType)
     {
	 case "model":
	  if(model)
	  {
	   minz="NaN";maxz="NaN";
	   for(i=0;i<model.element.length;i++)
	   {
	    if(model.element[i])
		{
		 if(isNaN(minz)||(model.element[i].timepos<minz)){minz=model.element[i].timepos;};
		 if(isNaN(maxz)||(model.element[i].timepos>maxz)){maxz=model.element[i].timepos;};
		};
	   };
	   plotOptions.plotWidth=plotOptions.plotWidthMulti;
	   plotOptions.plotHeight=(4+maxz-minz)*plotOptions.plotHeightModel;
	   plotOptions.frameHeight=plotOptions.plotHeight+10*plotOptions.plotHeightModel;
	   plotOptions.minz=minz;
	   plotOptions.maxz=maxz;
	  };
	  return;
     case "table":
     case "individual":
      if(ocd[plotOptions.plotFrom] && ocd[plotOptions.plotFrom].correlation)
      {
       // use correlation view
	   item=ocd[plotOptions.plotFrom].correlation;
	   if(!item){return;};
        
       minx=plotOptions.minx;
       maxx=plotOptions.maxx;
       miny=plotOptions.miny;
       maxy=plotOptions.maxy;
       minz=plotOptions.minz;
       maxz=plotOptions.maxz;
       if((!minx) || (!maxx))
       {
        plotOptions.labelX=item.x.name;
        plotOptions.labelY=item.y.name;
        plotOptions.labelTitle=item.name;
        minx=item.x.start;
        maxx=item.x.end;
        miny=item.y.start;
        maxy=item.y.end;
        minz=plotOptions.minz;
        maxz=plotOptions.maxz;
        minz=map(0,0);maxz=map(0,0);
        for(i=0;i<40;i++)
        {
         for(j=0;j<40;j++)
         {
          v=map(i,j);
          if(v<minz){minz=v;};
          if(v>maxz){maxz=v;};
         };
        };
        // save settings
	    plotOptions.minx=minx;
	    plotOptions.maxx=maxx;
	    plotOptions.miny=miny;
	    plotOptions.maxy=maxy;
	    plotOptions.minz=minz;
	    plotOptions.maxz=maxz;
       };
      }
      else
      {
       // individual view
       // adjust paging
	   setupYValues();
    
       for(j=plotOptions.plotFrom-1;(j>0)&&(!individualPlottable(j));j--){};
       if(individualPlottable(j)){plotOptions.plotLast=j;}else{plotOptions.plotLast=plotOptions.plotFrom;};
       for(j=plotOptions.plotFrom-10;(j>0)&&(!individualPlottable(j));j--){};
       if(individualPlottable(j)){plotOptions.plotPrev=j;}else{plotOptions.plotPrev=plotOptions.plotFrom;};
       for(j=plotOptions.plotFrom+1;(j<ocd.length)&&(!individualPlottable(j));j++){};
       if(individualPlottable(j)){plotOptions.plotNext=j;}else{plotOptions.plotNext=plotOptions.plotFrom;};
       for(j=plotOptions.plotFrom+10;(j<ocd.length)&&(!individualPlottable(j));j++){};
       if(individualPlottable(j)){plotOptions.plotTo=j;}else{plotOptions.plotTo=plotOptions.plotFrom;};
 

	   // sort out what to base ranges on
	   item=ocd[plotOptions.plotFrom];
	   if(!item){return;};
       if(!individualPlottable(plotOptions.plotFrom)){return;};
       plotOptions.dataType=item.type;
	    
	   // find the range (if needed)
       minx=plotOptions.minx;
       maxx=plotOptions.maxx;
       miny=plotOptions.miny;
       maxy=plotOptions.maxy;
	   if((isNaN(minx))||(isNaN(maxx)))
	   {
	    miny=0;maxy=1;
        if(item.likelihood && item.likelihood.prob && item.likelihood.prob.length &&       (plotOptions.showLikelihood || !plotOptions.showPosterior))
        {
         plotter=item.likelihood;
         minxt=plotter.start;maxxt=minxt+plotter.resolution*(plotter.prob.length-1);
         if((isNaN(minx))||(minxt<minx)){minx=minxt;};
         if((isNaN(maxx))||(maxxt>maxx)){maxx=maxxt;};
        };
        if(item.posterior && item.posterior.prob && item.posterior.prob.length && (plotOptions.showPosterior || !plotOptions.showLikelihood))
        {
         plotter=item.posterior;
  	     minxt=plotter.start;maxxt=minxt+plotter.resolution*(plotter.prob.length-1);
         if((isNaN(minx))||(minxt<minx)){minx=minxt;};
         if((isNaN(maxx))||(maxxt>maxx)){maxx=maxxt;};
        };
        if(!(isNaN(minx) || isNaN(maxx)))
        {
	     xmargin=(maxx-minx)/10;
	     minx-=xmargin;maxx+=xmargin;
  	    }
  	    else
  	    {
  	     minx=0;maxx=1000;
  	    };
	    plotOptions.minx=minx;
	    plotOptions.maxx=maxx;
	    plotOptions.miny=miny;
	    plotOptions.maxy=maxy;
	   };
      };
      break;
	 case "multiple":
	 case "select":
	 case "stack":
	 case "curve":
     case "x":
     case "y":  
	 case "z":
	  // sort out how to plot

	  setupYValues();
	  
	  thisType="date";
	   
      // find the ranges of all the data
      minx=plotOptions.minx;
      maxx=plotOptions.maxx;
      miny=plotOptions.miny;
      maxy=plotOptions.maxy;
      minz=plotOptions.minz;
      maxz=plotOptions.maxz;
      plotOptions.reflist=new Array();
      calib[0].showCurve=plotOptions.showCurve;
      calib[0].showCurveRaw=plotOptions.showCurveRaw;
      for(i=start;i<finish;i++)
      {
	   // sort out references and curve colors
       if(!ocd[i]){continue;};
       if((typeof(ocd[i].calib)!="undefined")&&(i))
       {
        if(typeof(ocd[i].date)!="undefined")
        {
         plotOptions.radiocarbon=true;
        };
        for(j=0;j<plotOptions.reflist.length;j++)
        {
         if(plotOptions.reflist[j]==ocd[i].calib){break;};
        };
  	    if((typeof(plotOptions.reflist[j])=="undefined")||(plotOptions.reflist[j]!=ocd[i].calib))
  	    {
  	     if(calib[ocd[i].calib])
  	     {
          plotOptions.reflist[j]=ocd[i].calib;
          if(!plotOptions.showBandW && ocd[ocd[i].calib].data && ocd[ocd[i].calib].data.color)
          {
	       calib[ocd[i].calib].color=ocd[ocd[i].calib].data.color;
          }
          else
          {
           if(crv)
           {
	        calib[ocd[i].calib].color=getColor(crv);
	       }
	       else
	       {
	        calib[ocd[i].calib].color=false;
	       };
	      };
	      if(ocd[ocd[i].calib].data)
	      {
	       if(typeof(ocd[ocd[i].calib].data.showCurve)!="undefined")
	       {
	        calib[ocd[i].calib].showCurve=ocd[ocd[i].calib].data.showCurve; 
	       }
	       else
	       {
	        calib[ocd[i].calib].showCurve=true;
	       };
	       if(typeof(ocd[ocd[i].calib].data.showCurveRaw)!="undefined")
	       {
	        calib[ocd[i].calib].showCurveRaw=ocd[ocd[i].calib].data.showCurveRaw; 
	       }
	       else
	       {
	        calib[ocd[i].calib].showCurveRaw=plotOptions.showCurveRaw;
	       };
	       if(typeof(ocd[ocd[i].calib].data.markerSize)!="undefined")
	       {
	        calib[ocd[i].calib].markerSize=ocd[ocd[i].calib].data.markerSize; 
	       }
	       else
	       {
	        calib[ocd[i].calib].markerSize=2.5;
	       };
	       if(typeof(ocd[ocd[i].calib].data.marker)!="undefined")
	       {
	        calib[ocd[i].calib].marker=ocd[ocd[i].calib].data.marker; 
	       }
	       else
	       {
	        calib[ocd[i].calib].marker="circle";
	       };
	       if(typeof(ocd[ocd[i].calib].data.fill)!="undefined")
	       {
	        calib[ocd[i].calib].fill=ocd[ocd[i].calib].data.fill; 
	       }
	       else
	       {
	        calib[ocd[i].calib].fill="white";
	       };
	      }
	      else
	      {
	       calib[ocd[i].calib].showCurve=true;
	       calib[ocd[i].calib].showCurveRaw=plotOptions.showCurveRaw;
	      };
  	      crv++;
  	     };
  	     if(ocd[ocd[i].calib] && ocd[ocd[i].calib].op && ocd[ocd[i].calib].op=="Sapwood_Model")
  	     {
          plotOptions.reflist[j]=ocd[i].calib;
  	      if(sap)
  	      {
  	       ocd[ocd[i].calib].color=getColor(sap);
  	      }
  	      else
  	      {
  	       ocd[ocd[i].calib].color=false;
  	      };
  	      sap++;
  	     };
  	    };
       };
      };
      if(plotOptions.viewType=="curve")
      {
       if(calib[0])
       {
        for(j=0;j<plotOptions.reflist.length;j++)
        {
         if(plotOptions.reflist[j]==0){break;};
        };
  	    // add to list and work out the colours of the curves
  	    if(plotOptions.reflist[j]!=0)
  	    {
         plotOptions.reflist[j]=0;
	     calib[0].color=getColor(crv);
         crv++;
	    };
       };
      };
      if((isNaN(minx)) || (isNaN(maxx)))
      {
       maxz=undefined;
       pagePassed=false;
       for(i=start;i<finish;i++)
       {
        if(!ocd[i]){continue;};
		if(ocd[i].axis && (ocd[i].selectNo>0))
		{
		 if((plotOptions.viewType=="multiple")&&(ocd[i].yParam<plotOptions.plotFrom)){continue;};
		 if(!pagePassed && (plotOptions.dataType=="date"))
		 {
		  defMinX=ocd[i].axis.min;
		  defMaxX=ocd[i].axis.max;
		 };
		};
        if(ocd[i].newPage && ocd[i].yParam>=plotOptions.plotTo)
        {
         pagePassed=true;
        };
        if(ocd[i].axis)
        {
/*         if(plotOptions.viewType=="curve")
         {
 		  defMinX=ocd[i].axis.min;
		  defMaxX=ocd[i].axis.max;
         };*/
         continue;
        };
        if(ocd[i].type!="date")
        {
         if((plotOptions.viewType!="multiple")&&(plotOptions.viewType!="select")&&(plotOptions.viewType!="stack")){continue;};
        };
        item=ocd[i];
        if(!isNaN(item.yParam) && item.selectNo)
        {
		 if((plotOptions.viewType=="multiple")||(plotOptions.viewType=="select")||(plotOptions.viewType=="stack"))
		 {
          if(item.yParam < plotOptions.plotFrom){continue;};
          if(item.yParam > plotOptions.plotTo){continue;};
          if(((plotOptions.viewType=="select")||(plotOptions.viewType=="stack"))&&item.Type)
          {
 		   thisType=item.Type;
 		  };
		 };
         if((isNaN(miny))||(item.yParam<miny)){miny=item.yParam;};
         if((isNaN(maxy))||(item.yParam>maxy)){maxy=item.yParam;};
         if(item.likelihood && item.likelihood.prob)
         {
          plotter=item.likelihood;
          minxt=plotter.start;maxxt=minxt+plotter.resolution*(plotter.prob.length-1);
          if((isNaN(minx))||(minxt<minx)){minx=minxt;};
          if((isNaN(maxx))||(maxxt>maxx)){maxx=maxxt;};
          if((isNaN(maxz))||(plotter.probNorm>maxz)){maxz=plotter.probNorm;};
         };
         if(item.posterior && item.posterior.prob)
         {
          plotter=item.posterior;
  	      minxt=plotter.start;maxxt=minxt+plotter.resolution*(plotter.prob.length-1);
          if((isNaN(minx))||(minxt<minx)){minx=minxt;};
          if((isNaN(maxx))||(maxxt>maxx)){maxx=maxxt;};
          if((isNaN(maxz))||(plotter.probNorm>maxz)){maxz=plotter.probNorm;};
         };
        };
       };
       // extend ranges to give a little margin
       if((isNaN(minx))||(isNaN(maxx))){minx=0;maxx=1000;};
       if((isNaN(miny))||(isNaN(maxy))){miny=0;maxy=1000;};
       if(minx==maxx){maxx=minx+1;};
       //if(miny==maxy){maxy=miny+1;};
       xmargin=(maxx-minx)/10;
       minx-=xmargin*3;maxx+=xmargin;
	   if(!(isNaN(defMinX)||isNaN(defMaxX)||(thisType!="date")))
	   {
	    switch(plotOptions.viewType)
		{
	    case "x":
	    case "y":
		case "z":
		case "curve":
		case "select":
		case "stack":
		case "multiple":
	     minx=defMinX;maxx=defMaxX;
		 break;
		};
	   };
       switch(plotOptions.viewType)
       {
	   case "x":
	   case "y":
	   case "z":
	   case "curve":
        zmargin=(maxy-miny)/10;
        miny-=zmargin;maxy+=zmargin;
        break;
	   case "multiple":
	   case "select":
	   case "stack":
	    miny=plotOptions.plotFrom;
		maxy=plotOptions.plotTo;
	    zmargin=0.4;
	    miny-=0.5;
	    maxy+=0.5;
	    miny-=zmargin;
	    maxy+=zmargin;
	    break;
       };
       // save settings
	   plotOptions.minx=minx;
	   plotOptions.maxx=maxx;
	   plotOptions.miny=miny;
	   plotOptions.maxy=maxy;
	   plotOptions.minz=minz;
	   plotOptions.maxz=maxz;
      };
    
      // scale multiple plots for number of plots shown
      switch(plotOptions.viewType)
      {
	  case "multiple":
	  case "select":
	  case "stack":
	   zmargin=0.4;
	   topMargin=0.0;
	   if(plotOptions.showData){topMargin+=plotOptions.plotHeightData;};
	   if(plotOptions.labelTitle){topMargin+=plotOptions.plotHeightTitle;};
	   plotOptions.plotHeight=
	     (plotOptions.plotHeightMulti-topMargin)*(maxy-miny)
	     /(plotOptions.plotsPerPage+2*zmargin);
       plotOptions.plotPosY=plotOptions.plotPosYNormal
         +plotOptions.plotHeightMulti-plotOptions.plotHeight-topMargin;
	   // reduce the frame height to scale
	   shiftUp=(plotOptions.plotHeightMulti-plotOptions.plotHeight);
       shiftUp-=topMargin;
	   plotOptions.frameHeight=plotOptions.frameHeightMulti-shiftUp;
	   plotOptions.plotPosY-=shiftUp;
      };
      break;
     case "correlation":
      break;
     };
    };
  };
