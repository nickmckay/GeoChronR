// ocp_utils.js
// Range writing routines
 var label=new Object();
 
 // toFixxed() :
function toFixxed(num,f) 
 {
  f = parseInt(f/1 || 0);
  if (f < 0 || f > 20)
    alert("The number of fractional digits is out of range");
  if (isNaN(num))
    return "NaN";
  var s = num < 0 ? "-" : "", x = Math.abs(num);
  if (x > Math.pow(10, 21))
    return s+x.toString();
  var m = Math.round(x*Math.pow(10, f)).toString();
  if (!f)
    return s+m;
  while (m.length <= f)
    m = "0"+m;
  return s+m.substring(0, m.length-f)+"."+m.substring(m.length-f);
 };

 function doRound(d,dirn)
 {
  if(plotOptions.roundBy>1)
  {
   d=d/plotOptions.roundBy;
   switch(dirn)
   {
   case -1:
    return Math.floor(d)*plotOptions.roundBy;
   case 1:
    return Math.ceil(d)*plotOptions.roundBy;
   default:
    return Math.round(d)*plotOptions.roundBy;
   };
  };
  return Math.round(d);
 };

 function showDateT(d,type,dirn)
 {
  switch(type)
  {
  case "date":
   switch(plotOptions.reportingStyle)
   {
   case 0:
    return doRound(plotOptions.BPDatum-d,-dirn);
   case 1: case 2:
    if(d>=1)
    {
     d=doRound(d-0.5,dirn);
     if(d==0){return 1;};
     return d;
    };
    d=-doRound(1.5-d,-dirn);
    if(d==0){return -1;};
    return d;
   case 3:
    return doRound(d-0.5,dirn);
   case 4:
    return d;
   };
   break;
  case "interval":
   switch(plotOptions.reportingStyle)
   {
   case 0:
   case 1: case 2:
   case 3:
    return doRound(d,dirn);
   case 4:
    return d;
   };
   break;
  };
  return d;
 };
 
 function getDateT(d,typ)
 {
   if(typ!="date"){return d;};
   d=Number(d);
   switch(plotOptions.reportingStyle)
   {
   case 0:
    return doRound(plotOptions.BPDatum-d);
   case 1: case 2:
    if(d>0){return d;};
    if(d<0){return d+1;};
    return 1;
   case 3: case 4:
    return d;
   };
   return d;
 };
 
 function showDate(d,type,calib,nolabel)
 {
  var pre=label.dateCalendPrefix;
  if(calib){pre=label.dateCalibPrefix;};
  if(nolabel)
  {
   switch(type)
   {
   case "date":
    switch(plotOptions.reportingStyle)
    {
    case 1:
    case 2:
     if(d>=1){return d;};
     return -d;
    default:
     return d;
    };
   };
  };
  switch(type)
  {
  case "date":
   switch(plotOptions.reportingStyle)
   {
   case 0:
    return d+pre+"BP";
   case 1:
    if(d>=1)
    {
     if(plotOptions.showADFirst)
     {
      return pre+"AD"+d;
     }
     else
     {
      return d+pre+"AD";
     };
    };
    return -d+pre+"BC";
   case 2:
    if(d>=1){return d+pre+"CE";};
    return -d+pre+"BCE";
   case 3:
    return d;
   case 4:
    return "G"+d;
   };
  };
  return d;
 };

 function makeTitle(item,full)
 {
  var str="";
  if(!item){return str;};
  if(full||plotOptions.showParameters)
  {
   str=item.name;
   if(item.op && (item.op!="Calculate") && (item.op!="Label") && plotOptions.showVerbs)
   {
    str+=" "+item.op;
   };
   if(item.param)
   {
    if((item.op && (item.op=="Calculate"))||(!item.name && !plotOptions.showVerbs))
    {
     str+=" "+item.param;
    }
    else
    {
     if(!plotOptions.showVerbs){str+=" ";};
     str+="("+item.param+")";
    };
   };   
   return str;
  };
  if(item.op && (item.op!="Calculate") && (item.op!="Label") && plotOptions.showVerbs)
  {
   str+=item.op;
  };
  if(item.name && item.name!="")
  {
   if(str){str+=" ";};
   str+=item.name;
  }
  else
  {
   if(item.param && plotOptions.showVerbs)
   {
    if(item.op && (item.op=="Calculate"))
    {
     str+=item.param;
    }
    else
    {
     str+="("+item.param+")";
    };
   };
  };
  return str;
 };
 
 function simplifyDateRange(r1,type,showWhole)
 {
  var n,m,c,merge;
  if(!r1){return false;};
  if(!r1[0]){return false;};
  var r2=new Array();
  for(n=0;n<r1.length;n++)
  {
   r2[n]=new Array();
   for(m=0;m<3;m++){r2[n][m]=r1[n][m];};
   if(r2[n][0]!="..."){r2[n][0]=showDateT(r2[n][0],type,-1);};
   if(r2[n][1]!="..."){r2[n][1]=showDateT(r2[n][1],type,1);};
  };
  c=0;
  for(n=1;n<r2.length;n++)
  {
   if((type=="date") && (plotOptions.reportingStyle==0))
   {
    merge=(r2[c][1]<=r2[n][0])
   }
   else
   {
    merge=(r2[c][1]>=r2[n][0])
   };
   if(merge || showWhole)
   {
    r2[c][1]=r2[n][1];
    r2[c][2]+=r2[n][2];
   }
   else
   {
    c++;
    r2[c][0]=r2[n][0];
    r2[c][1]=r2[n][1];
    r2[c][2]=r2[n][2];
   };
  };
  r2.length=c+1;
  return r2;
 };

 function textRange(dist,type,calib)
 {
  var line=0;
  var rtn=new Array();
  var r,signChange;
  var count,m,n,k,p,intercept;
  if(!dist){return rtn;};
  intercept=false;p=0;
  r=dist.range[1];
  if(r)
  {
   count=r.length;
   for(n=0;n<count;n++)
   {
    p+=r[n][2];
   };
   if(p<2){intercept=true;};
  };
  for(k=1;k<4;k++)
  {
   if(!plotOptions.showRange[k]){continue;};
   if(!dist.range[k]){continue;};
   rtn[line]=label.rangeTitlePrefix;
   if(intercept)
   {
    rtn[line]+=k+label.rangeTitleSigma;
   }
   else
   {
    switch(k)
    {
    case 1:
     rtn[line]+="68.2";
     break;
    case 2:
     rtn[line]+="95.4";
     break;
    case 3:
     rtn[line]+="99.7";
     break;
    };
    rtn[line]+=label.rangeTitleProb;
   };
   line++;
   if(dist && dist.range)
   {
    r=simplifyDateRange(dist.range[k],type);
    if(r)
    {
     count=r.length;
     for(n=0;n<count;n++)
     {
      signChange=(isNaN(r[n][0])||isNaN(r[n][1])||(r[n][0]*r[n][1] < 0));
	  rtn[line]=label.rangePrefix;
      for(m=0;m<3;m++)
      {
       if(m==(plotOptions.showPropPlace-1))
       {
        if(m){rtn[line]+=" ";};
        if(intercept)
        {
         rtn[line]+="["+toFixxed((r[n][2]),3)+"] ";
        }
        else
        {
         rtn[line]+="("+toFixxed((r[n][2]),1)+"\%) ";
        };
        if(m<2){rtn[line]+=" ";};
       };
       if(m==2){break;};
       if(r[n][m]=="...")
       {
        rtn[line]+=label.rangeUnknown;
       }
       else
       {
        if(signChange)
        {
         rtn[line]+=showDate(r[n][m],type,calib);
        }
        else
        {
         switch(m)
         {
         case 0:
          if((plotOptions.showADFirst && (r[n][m]>0) && (plotOptions.reportingStyle==1))||(plotOptions.reportingStyle==4))
          {
           rtn[line]+=showDate(r[n][m],type,calib);
          }
          else
          {
           rtn[line]+=showDate(r[n][m],type,calib,true);
          };
          break;
         case 1:
          if((plotOptions.showADFirst && (r[n][m]>0) && (plotOptions.reportingStyle==1))||(plotOptions.reportingStyle==4))
          {
           rtn[line]+=showDate(r[n][m],type,calib,true);
          }
          else
          {
           rtn[line]+=showDate(r[n][m],type,calib);
          };
          break;
         };
        };
        if((m==0)&&(r[n][m]*r[n][1] > 0))
        {
        }
        else
        {
        };
       };
       if(m==0)
       {
        if(plotOptions.showPropPlace==2)
        {
         rtn[line]+=" ";
        }
        else
        {
         rtn[line]+="-";
        };
       };
      };
      line++;
     };
    };
   };
  };
  if(plotOptions.showMean && dist.mean)
  {
   rtn[line]=label.rangeTitlePrefix+label.mean+" "+showDate(showDateT(dist.mean,type,0),type,calib);
   line++;
  };
  if(plotOptions.showSigma && dist.sigma)
  {
   if(type=='date')
   {
    rtn[line]=label.rangeTitlePrefix+label.sigma+" " +showDate(showDateT(dist.sigma,'interval',0),'interval',calib);
   }
   else
   {
    rtn[line]=label.rangeTitlePrefix+label.sigma+" "+showDate(showDateT(dist.sigma,type,0),type,calib);
   };
   line++;
  };
  if(plotOptions.showMedian && dist.median)
  {
   rtn[line]=label.rangeTitlePrefix+label.median+" "+showDate(showDateT(dist.median,type,0),type,calib);
   line++;
  };
  return rtn;
 };

 function typePlottable(i)
 {
  if(!ocd[i]){return false;};
  if(!(ocd[i].selectNo>0)){return false;};
  if(!ocd[i].type){return false;};
  if(ocd[i].type==plotOptions.dataType){return true;};
  if(ocd[i].op=="P_Sequence"){return true;};
  if(ocd[i].type=="model")
  {
   switch(ocd[i].op)
   {
   case "Sapwood_Model":
    return false;
   };
   return true;
  };
  return false;
 };

 function individualPlottable(j)
 {
  var plotter;
  if(!ocd[j]){return false;};
  switch(ocd[j].type)
  {
  case "correlation":
   return true; 
  case "date": case "number": case "interval":
   break;
  default:
   return false;
  };
  if(ocd[j].likelihood)
  {
   plotter=ocd[j].likelihood;
   if((plotter.range && plotter.range.length>0) || plotter.prob){return true;};
  };
  if(ocd[j].posterior)
  {
   plotter=ocd[j].posterior;
   if((plotter.range && plotter.range.length>0) || plotter.prob){return true;};
  };
  return false;
 };

  function setupYValues()
  {
	  var i;
	  var indexOcd=new Array;
	  var type="";
	  var yparm;
	  var newPage;
	  var forceNewPage;
      plotOptions.plotTo=undefined;
      plotOptions.plotPrev=0;
	  for(i=0;i<ocd.length;i++)
	  {
	   if(ocd[i])
	   {
	    ocd[i].yParam="NaN";
	    ocd[i].xParam="NaN";
	    if(ocd[i].data)
	    {
	     if(ocd[i].data.date){ocd[i].date=ocd[i].data.date;};
	     if(ocd[i].data.error){ocd[i].error=ocd[i].data.error ;};
	    };
	   };
	  };
	  switch(plotOptions.viewType)
	  {
	  case "individual":
	   i=plotOptions.plotFrom;
	   while(!individualPlottable(i)&&(i<ocd.length))
	   {
	    i++;
	   };
	   if(individualPlottable(i))
	   {
	    plotOptions.plotFrom=i;
	   }
	   else
	   {
		while(!individualPlottable(i)&&(i>0))
	    {
	     i--;
	    };
		plotOptions.plotFrom=i;
	   };
	   plotOptions.plotPrev=plotOptions.plotFrom-10;
	   plotOptions.plotTo=plotOptions.plotFrom+9;
	   break;
	  case "multiple":
	   yparm=0;
	   for(i=0;i<ocd.length;i++)
	   {
	    if(typePlottable(i))
	    {
	     yparm++;
	     ocd[i].yParam=yparm;
	     if((yparm>plotOptions.plotFrom) && !plotOptions.plotTo && ocd[i].newPage)
	     {
	      plotOptions.plotTo=yparm;
	     };
	     if((yparm+1<plotOptions.plotFrom) && (yparm+1>plotOptions.plotPrev) && ocd[i].newPage)
	     {
	      plotOptions.plotPrev=yparm+1;
	     };
	    };
		if(ocd[i] && ocd[i].axis)
		{
		 ocd[i].yParam=yparm;
		};
	   };
	   if(!plotOptions.plotTo){plotOptions.plotTo=yparm;};
	   if(plotOptions.plotTo>(plotOptions.plotFrom+plotOptions.plotsPerPage-1))
	   {
	    plotOptions.plotTo=plotOptions.plotFrom+plotOptions.plotsPerPage-1;
	   };
	   if(plotOptions.plotPrev<(plotOptions.plotFrom-plotOptions.plotsPerPage))
	   {
	    plotOptions.plotPrev=plotOptions.plotFrom-plotOptions.plotsPerPage;
	   };
	   break;
	  case "curve":
	   plotOptions.dataType="date";
	   for(i=0;i<ocd.length;i++)
	   {
	    if(typePlottable(i))
		{
		 ocd[i].yParam=ocd[i].date;
		 if(plotOptions.showLikelihood && ocd[i].likelihood)
		 {
		  ocd[i].xParam=ocd[i].likelihood.mean;
		 };
		 if(plotOptions.showPosterior && ocd[i].posterior)
		 {
		  ocd[i].xParam=ocd[i].posterior.mean;
		 };
		};
	   };
	   break;	
	  case "z":
	   plotOptions.dataType="date";
	   for(i=0;i<ocd.length;i++)
	   {
	    if(typePlottable(i) && ocd[i].data && !isNaN(ocd[i].data.z))
		{
		 ocd[i].yParam=ocd[i].data.z;
		};
	   };
	   break;
	  case "select":
	  case "stack":
	   for(i=0;i<ocd.length;i++)
	   {
	    if(ocd[i] && ocd[i].selectNo)
		{
		 indexOcd[ocd[i].selectNo]=i;
		};
	   };
	   yparm=0;
	   for(i=0;i<indexOcd.length;i++)
	   {
	    if(!indexOcd[i]){continue;};
		if(!ocd[indexOcd[i]]){continue;};
		switch(ocd[indexOcd[i]].type)
		{
		case "interval": case "number": case "date":case "model":
		 break;
		default:
		 continue;
		};
		if(plotOptions.viewType=="select")
		{
		 yparm++;
//		 yparm=ocd[indexOcd[i]].selectNo;
	     ocd[indexOcd[i]].yParam=yparm;
	     newPage=ocd[indexOcd[i]].newPage;
	    }
	    else  //stack
	    {
	     if(yparm==0){yparm++;};
	     ocd[indexOcd[i]].yParam=yparm;
	     if(ocd[indexOcd[i]].newPage){yparm++;};
	    };
		forceNewPage=false;
	    // set new page if type is changing
	    if(ocd[indexOcd[i]].type!="model")
	    {
		 if((type!="") && (type!=ocd[indexOcd[i]].type))
		 {
	      forceNewPage=true;
		 };
		 type=ocd[indexOcd[i]].type;
		};
	    if((yparm>plotOptions.plotFrom) && (!plotOptions.plotTo||(yparm<plotOptions.plotTo)) && newPage)
	    {
	     plotOptions.plotTo=yparm;
	    };
	    if((yparm+1<plotOptions.plotFrom) && (yparm+1>plotOptions.plotPrev) && newPage)
	    {
	     plotOptions.plotPrev=yparm+1;
	    };
	    if((yparm>plotOptions.plotFrom) && (!plotOptions.plotTo||(yparm<plotOptions.plotTo)) && forceNewPage)
	    {
	     plotOptions.plotTo=yparm-1;
	    };
	    if((yparm<plotOptions.plotFrom) && (yparm>plotOptions.plotPrev) && forceNewPage)
	    {
	     plotOptions.plotPrev=yparm;
	    };
		if(yparm==plotOptions.plotFrom)
		{
		 plotOptions.dataType=type;
		};
	   };
	   if(!plotOptions.plotTo){plotOptions.plotTo=yparm;};
	   if(plotOptions.plotTo>(plotOptions.plotFrom+plotOptions.plotsPerPage-1))
	   {
	    plotOptions.plotTo=plotOptions.plotFrom+plotOptions.plotsPerPage-1;
	   };
	   if(plotOptions.plotPrev<(plotOptions.plotFrom-plotOptions.plotsPerPage))
	   {
	    plotOptions.plotPrev=plotOptions.plotFrom-plotOptions.plotsPerPage;
	   };
	   break;
	  };
  }; 

// routines for correlation plots
  function map(i,j)
  {
   return ocd[plotOptions.plotFrom].correlation.prob[i+j*ocd[plotOptions.plotFrom].correlation.x.divisions];
  };
  function normmap(i,j)
  {
   return (map(i,j)-plotOptions.minz)/(plotOptions.maxz-plotOptions.minz);
  };

  function getColor(ind)
  {
   if(plotOptions.showBandW)
   {
    return "#000000";
   };
   return plotOptions.plotColors[ind % plotOptions.plotColors.length];
  };

