 var plotOptions=parent.plotOptions;
 var plt=1;
 
 function fullOxCal()
 {
  if(parent.onAServer() || parent.localFilePossible())
  {
   parent.fileDialog("Open");
  }
  else
  {
   parent.newFile();
  };
 };
 function smallerFont()
 {
  plotOptions.scaleFont/=1.0443;
  if(!plotOptions.scaleFont){plotOptions.scaleFont=0.8;};
  updater(false);
 };
 function largerFont()
 {
  plotOptions.scaleFont*=1.0443;
  if(!plotOptions.scaleFont){plotOptions.scaleFont=0.8;};
  updater(false);
 };
 function narrowerFont()
 {
  plotOptions.scaleLine/=1.0443;
  if(!plotOptions.scaleLine){plotOptions.scaleLine=1.0;};
  updater(false);
 };
 function widerFont()
 {
  plotOptions.scaleLine*=1.0443;
  if(!plotOptions.scaleLine){plotOptions.scaleLine=1.0;};
  updater(false);
 };
 function setPlotFrom(no)
 {
  parent.defaultPaging(no);
  updater(true);
  readback();
 };
 function setPlotsPerPage(no)
 {
  if(!no){no=10;};
  if(no<1){no=1;};
  plotOptions.plotsPerPage=no;
  readback();
 };
 function setCentre()
 {
  var cen;
  var val=Number(parent.getDateT(document.getElementById("Centre").value,plotOptions.dataType));
  cen=(plotOptions.maxx+plotOptions.minx)/2;
  val-=cen;
  plotOptions.minx+=val;
  plotOptions.maxx+=val;
  readback();
 };
 function moveCentre(val)
 {
  var cen;
  cen=(plotOptions.maxx+plotOptions.minx)/2;
  val*=plotOptions.maxx-plotOptions.minx;
  plotOptions.minx+=val;
  plotOptions.maxx+=val;
  readback();
 };
 function setSpan()
 {
  var cen;
  var val=Number(document.getElementById("Span").value);
  plotOptions=parent.plotOptions;
  cen=(plotOptions.maxx+plotOptions.minx)/2;
  val/=2;
  plotOptions.minx=cen-val;
  plotOptions.maxx=cen+val;
  readback();
 }; 
 function changeSpan(val)
 {
  var cen;
  plotOptions=parent.plotOptions;
  cen=(plotOptions.maxx+plotOptions.minx)/2;
  val*=(plotOptions.maxx-plotOptions.minx)/2;
  plotOptions.minx=cen-val;
  plotOptions.maxx=cen+val;
  readback();
 };
 function setZoom()
 {
  var val=Number(document.getElementById("Zoom").value);
  plotOptions.scale=val/100;
  if(plotOptions.scale>8){plotOptions.scale=8;};
  readback();
 };
 function changeZoom(val)
 {
  plotOptions.scale*=val;
  if(plotOptions.scale>8){plotOptions.scale=8;};
  readback();
 };
 function changeMinY(val)
 {
  val*=plotOptions.maxy-plotOptions.miny;
  plotOptions.miny+=val;
  plotOptions.maxy+=val;
  readback();
 };
 function setMinY()
 {
  var val=Number(document.getElementById("MinY").value);
  plotOptions.maxy+=val-plotOptions.miny;
  plotOptions.miny=val;
  readback();
 };
 function changeYRange(val)
 {
  val*=plotOptions.maxy-plotOptions.miny;
  plotOptions.maxy=plotOptions.miny+val;
  readback();
 };
 function setYRange()
 {
  var val=Number(document.getElementById("YRange").value);
  plotOptions.maxy=plotOptions.miny+val;
  readback();
 };
 function changeMinZ(val)
 {
  val*=plotOptions.maxz-plotOptions.minz;
  plotOptions.minz+=val;
  plotOptions.maxz+=val;
  readback();
 };
 function setMinZ()
 {
  var val=Number(document.getElementById("MinZ").value)-plotOptions.minz;
  plotOptions.minz+=val;
  plotOptions.maxz+=val;
  readback();
 };
 function changeZRange(val)
 {
  val*=plotOptions.maxz-plotOptions.minz;
  plotOptions.maxz=plotOptions.minz+val;
  readback();
 };
 function setZRange()
 {
  var val=Number(document.getElementById("ZRange").value);
  plotOptions.maxz=plotOptions.minz+val;
  readback();
 };
 function getRoundBy()
 {
  var ind=document.getElementById("roundBy").selectedIndex;
  var vals=[1,5,10,50,100,500,1000];
  plotOptions.roundBy=vals[ind];
 };
 function getoptions()
 {
  var po;
  if(plotOptions)
  {
   po=plotOptions;
   po.showRange[1]=document.getElementById("showRange1").checked;
   po.showRange[2]=document.getElementById("showRange2").checked;
   po.showRange[3]=document.getElementById("showRange3").checked; 
   po.showMean=document.getElementById("showMean").checked; 
   po.showSigma=document.getElementById("showSigma").checked; 
   po.showMedian=document.getElementById("showMedian").checked; 
   po.showCurve=document.getElementById("showCurve").checked; 
   po.showCurveRaw=document.getElementById("showCurveRaw").checked; 
   po.showF14C=document.getElementById("showF14C").checked; 
   po.showDelta14C=document.getElementById("showDelta14C").checked; 
   po.showDistribution=document.getElementById("showDistribution").checked; 
   po.showRanges=document.getElementById("showRanges").checked; 
   po.showNormal=document.getElementById("showNormal").checked; 
   po.showReference=document.getElementById("showReference").checked; 
   po.showTitle=document.getElementById("showTitle").checked; 
   po.showLikelihood=document.getElementById("showLikelihood").checked; 
   po.showPosterior=document.getElementById("showPosterior").checked; 
   po.showIndices=document.getElementById("showIndices").checked; 
   po.reportingStyle=document.getElementById("reportingStyle").selectedIndex; 
   po.showPropPlace=document.getElementById("showPropPlace").selectedIndex+1; 
   po.showADFirst=document.getElementById("showADFirst").checked; 
   po.showReversed=document.getElementById("showReversed").checked; 
   po.show3DEffects=document.getElementById("show3DEffects").checked; 
   po.showStructure=document.getElementById("showStructure").checked; 
   po.showInterpolation=document.getElementById("showInterpolation").checked; 
   po.showNormalised=document.getElementById("showNormalised").checked; 
   po.showAgreement=document.getElementById("showAgreement").checked; 
   po.showConvergence=document.getElementById("showConvergence").checked; 
   po.showOutliers=document.getElementById("showOutliers").checked; 
   po.colorOutliers=document.getElementById("colorOutliers").checked; 
   po.mergeRanges=document.getElementById("mergeRanges").checked; 
   po.showParameters=document.getElementById("showParameters").checked; 
   po.showVerbs=document.getElementById("showVerbs").checked; 
   po.showItalics=document.getElementById("showItalics").checked; 
   po.showGrid=document.getElementById("showGrid").checked; 
   po.showText=document.getElementById("showText").checked; 
   po.showWhole=document.getElementById("showWhole").checked;
   po.showBandW=document.getElementById("showBandW").checked;
   po.showBrackets=document.getElementById("showBrackets").checked;
   po.minorx=Number(document.getElementById("minorx").value);
   po.majorx=Number(document.getElementById("majorx").value);
   po.minory=Number(document.getElementById("minory").value);
   po.majory=Number(document.getElementById("majory").value);
   parent.showControls=document.getElementById("showControls").checked;
   po.showEnsembles=Number(document.getElementById("ensembleno").value);
   document.getElementById("Centre").value=parent.showDateT(Math.round((po.maxx+po.minx)/2),plotOptions.dataType);
   getRoundBy();
   document.getElementById('mapPlotMin').value=
   Math.round(parent.showDateT(plotOptions.player_min,plotOptions.dataType));
   document.getElementById('mapPlotMax').value=
   Math.round(parent.showDateT(plotOptions.player_max,plotOptions.dataType));
  };
 };
 function readback()
 {
  var po;
  if(plotOptions)
  {
   po=plotOptions;
   document.getElementById("showRange1").checked=po.showRange[1];
   document.getElementById("showRange2").checked=po.showRange[2];
   document.getElementById("showRange3").checked=po.showRange[3];
   document.getElementById("showMean").checked=po.showMean;
   document.getElementById("showSigma").checked=po.showSigma;
   document.getElementById("showMedian").checked=po.showMedian;
   document.getElementById("showCurve").checked=po.showCurve;
   document.getElementById("showCurveRaw").checked=po.showCurveRaw;
   document.getElementById("showF14C").checked=po.showF14C;
   document.getElementById("showDelta14C").checked=po.showDelta14C;
   document.getElementById("reportingStyle").selectedIndex=po.reportingStyle;
   document.getElementById("showPropPlace").selectedIndex=po.showPropPlace-1;
   document.getElementById("showADFirst").checked=po.showADFirst;
   document.getElementById("showBandW").checked=po.showBandW;
   document.getElementById("showAgreement").checked=po.showAgreement;
   document.getElementById("showConvergence").checked=po.showConvergence;
   document.getElementById("showOutliers").checked=po.showOutliers;
   document.getElementById("colorOutliers").checked=po.colorOutliers;
   document.getElementById("mergeRanges").checked=po.mergeRanges;
   document.getElementById("showParameters").checked=po.showParameters;
   document.getElementById("showVerbs").checked=po.showVerbs;
   document.getElementById("showReversed").checked=po.showReversed;
   document.getElementById("showNormalised").checked=po.showNormalised;
   document.getElementById("showItalics").checked=po.showItalics;
   document.getElementById("showGrid").checked=po.showGrid;
   document.getElementById("showText").checked=po.showText;
   document.getElementById("showBrackets").checked=po.showBrackets;
   document.getElementById("showIndices").checked=po.showIndices;
   document.getElementById("showPosterior").checked=po.showPosterior;
   document.getElementById("Position").value=po.plotFrom;
   document.getElementById("perPage").value=po.plotsPerPage;
   document.getElementById("Centre").value=parent.showDateT(Math.round((po.maxx+po.minx)/2),plotOptions.dataType);
   document.getElementById("Span").value=Math.round(po.maxx-po.minx);
   document.getElementById("Zoom").value=Math.round(po.scale*1000)/10;
   document.getElementById("minorx").value=po.minorx;
   document.getElementById("majorx").value=po.majorx;
   document.getElementById("MinY").value=po.miny;
   document.getElementById("YRange").value=po.maxy-po.miny;
   document.getElementById("minory").value=po.minory;
   document.getElementById("majory").value=po.majory;
   document.getElementById("MinZ").value=po.minz;
   document.getElementById("ZRange").value=po.maxz-po.minz;
   document.getElementById("labelX").value=po.labelX;
   document.getElementById("labelY").value=po.labelY;
   document.getElementById("labelTitle").value=po.labelTitle; 
   document.getElementById("showWhole").checked=po.showWhole;  
   document.getElementById("ensembleno").value=po.showEnsembles;
  };
 };
 function updater(reConfigure)
 {
  var po;
  getoptions();
  po=plotOptions;
  if(!reConfigure)
  {
   parent.redrawView();  
  }
  else
  {
   parent.updateView();
  };
  readback();
 };
 function getMapParams()
 {
  var i,sel;
  parent.plotColor.z_calc="";
  parent.plotColor.showKey=false;
  plotOptions.player_min=parent.getDateT(Number(document.getElementById('mapPlotMin').value),plotOptions.dataType);
  plotOptions.player_max=parent.getDateT(Number(document.getElementById('mapPlotMax').value),plotOptions.dataType);
  plotOptions.currentMax=1+Math.round((plotOptions.player_max-plotOptions.player_min)/
   document.getElementById('mapPlotIncr').value);
  plotOptions.player_max=plotOptions.player_min+
  	plotOptions.currentMax*document.getElementById('mapPlotIncr').value;
  plotOptions.mapPlotNormalise=document.getElementById("mapPlotNormalise").checked;
  plotOptions.mapPlotCircleZoom=Math.round(document.getElementById('mapPlotCircleZoom').value*10)/10;
  sel=document.getElementById('mapColorBy');
  parent.plotColor.z_calc=sel.options[sel.selectedIndex].text;
  plotOptions.mapPlotMultiIncr=Math.round(document.getElementById('mapPlotMultiIncr').value);
  parent.multiPlots.columns=Number(document.getElementById('mapPlotColumns').value);
  plotOptions.player_proxy=false;
  if(parent.plotColor.z_calc)
  {
   parent.plotColor.autoz=1;
   parent.plotColor.zlabel=
   		parent.plotColor.z_calc.slice(0,1).toUpperCase()+parent.plotColor.z_calc.slice(1);
   switch(parent.plotColor.z_calc)
   {
   case "mean":
    parent.plotColor.dz_calc="sigma";
    break;
   case "median":
    parent.plotColor.dz_calc="";
    break;
   };
   if(plotOptions.showPosterior)
   {
    parent.plotColor.zlabel+=" modelled ";
   }
   else
   {
    parent.plotColor.zlabel+=" unmodelled ";
   };
   switch(plotOptions.reportingStyle)
   {
    case 0: 
     parent.plotColor.autoz=2;
     parent.plotColor.z_calc="calBP("+parent.plotColor.z_calc+")";
     parent.plotColor.zlabel+="age (calBP)";
     break;
    case 1:
     if((parent.plotColor.minz+parent.plotColor.maxz)/2>0)
     {
      parent.plotColor.z_calc="AD("+parent.plotColor.z_calc+")";
      parent.plotColor.zlabel+="date (AD)";
     }
     else
     {
      parent.plotColor.autoz=2;
      parent.plotColor.z_calc="BC("+parent.plotColor.z_calc+")";
      parent.plotColor.zlabel+="date (BC)";
     };
     break;
    case 2:
     if((parent.plotColor.minz+parent.plotColor.maxz)/2>0)
     {
      parent.plotColor.z_calc="CE("+parent.plotColor.z_calc+")";
      parent.plotColor.zlabel+="date (CE)";
     }
     else
     {
     parent.plotColor.autoz=2;
      parent.plotColor.z_calc="BCE("+parent.plotColor.z_calc+")";
      parent.plotColor.zlabel+="date (BCE)";
     };
     break;
    default:
     parent.plotColor.zlabel+="date (+/-CE)";
     break;
   };
   parent.plotColor.showKey=true;
  }
  else
  {
   parent.plotColor.dz_calc="";
   sel=document.getElementById('mapPlotProxy');
   parent.plotColor.z_calc=plotOptions.player_proxies[sel.selectedIndex];
   if(parent.plotColor.z_calc){plotOptions.player_proxy=true;};
  };
  setMapParams();
 };
 function setMapParams()
 {
  var i,sel;
  document.getElementById('mapPlotMin').value=
   Math.round(parent.showDateT(plotOptions.player_min,plotOptions.dataType));
  document.getElementById('mapPlotMax').value=
   Math.round(parent.showDateT(plotOptions.player_max,plotOptions.dataType));
  document.getElementById('mapPlotIncr').value=
   Math.round(Math.abs((plotOptions.player_max-plotOptions.player_min)/plotOptions.currentMax));
  document.getElementById("mapPlotNormalise").checked=plotOptions.mapPlotNormalise;
  document.getElementById('mapPlotCircleZoom').value=plotOptions.mapPlotCircleZoom;
  document.getElementById('mapPlotMultiIncr').value=plotOptions.mapPlotMultiIncr;
  document.getElementById('mapPlotColumns').value=parent.multiPlots.columns;
  document.getElementById('mapPlotRows').value=Math.ceil(
  	(1+Math.round((plotOptions.player_max-plotOptions.player_min)
  	/plotOptions.mapPlotMultiIncr))/parent.multiPlots.columns);
  sel=document.getElementById('mapPlotProxy');
  sel.options.length=0;
  for(i=0;i<plotOptions.player_proxies.length;i++)
  {
   sel.options[i]=new Option(plotOptions.player_proxies[i],i);
   if(plotOptions.player_proxies[i]==plotOptions.player_proxy){sel.selectedIndex=i;};
  };
 };
 function prepareMap(findValues)
 {
  if(findValues){parent.findMapMinMax();};
  setMapParams();
  makeActive("main","8"); 
 };
 function viewClick(i)
 {
  switch(i)
  {
  case 0: plotOptions.plotFrom=0;parent.setView("table");break;
  case 2: parent.setView("individual");break;
  case 3: plotOptions.plotFrom=0;
   plotOptions.dataType="date";parent.setView("multiple");break;
  case 4: plotOptions.plotFrom=0;
   plotOptions.dataType="interval";parent.setView("multiple");break;
  case 5: plotOptions.plotFrom=0;
   plotOptions.dataType="number";parent.setView("multiple");break;
  case 6: plotOptions.plotFrom=0;
   parent.setView("select");break;
  case 7: plotOptions.plotFrom=0;
   parent.setView("stack");break;
  case 8: parent.setView("curve");break;
  case 9: parent.setView("z");break;
  case 10: prepareMap(true);break;
  case 12: parent.setView("parameter");break;
  case 13: parent.setView("model");break;
  case 14: parent.setView("outlier");break;
  case 15:parent.setView("raw");break;
  case 16:parent.setView("log");break;
  case 17:parent.setView("tab");break;
  case 19:parent.setView("status");break;
  };
 };
 function fileClick(i)
 {
  // New|Open||Close|Save as||Manager|Change password|Logout||Print...
  var newdir;
  switch(i)
  {
  case 0:
   parent.newFile();
   break;
  case 1:
   if(parent.onAServer() || parent.localFilePossible())
   {
    parent.fileDialog("Open");
   }
   else
   {
    parent.right.location.replace("ocp_open.html");
   };
   break;
  case 3:
   parent.window.close();
   break;
  case 4:
   if(parent.right.location.toString().match(parent.getPlotName()) && parent.canSVG())
   {
	parent.filename=parent.source;
	parent.filecontent=parent.getSVGContent();
	parent.saveFileAs("svg");
	plt++;
   }
   else
   {
    if(parent.right.location.toString().match("ocp_right.html")||
       parent.right.location.toString().match("ocp_param.html")||
       parent.right.location.toString().match("ocp_outlier.html"))
    {
     parent.filename=parent.source;
     parent.saveFileAs("csv");
     break;
    };
    if(parent.right.location.toString().match("ocp_raw.html"))
    {
     parent.filename=parent.ocd[parent.plotOptions.plotFrom].name;
     if(parent.filecontent && (parent.ocd[parent.plotOptions.plotFrom].order))
     {
      parent.saveFileAs("csv");
      break;
     };
     alert("Save specific data set from right hand window");
     break;
    };
    alert("Cannot save this");
   };
   break;
  case 6:
   parent.fileDialog();
   break;
  case 7:
   parent.resetSettings();
   break;
  case 8:
   if(parent.onAServer())
   {
    parent.location.replace('../login/login.php?Action=Logout&Change=true&Location='+encodeURIComponent(parent.location.pathname));
   };
   break;
  case 9:
   if(parent.onAServer())
   {
    parent.location.replace('../login/login.php?Action=Logout&Location='+encodeURIComponent(parent.location.pathname));
   };
   break;
  case 11:
   parent.printPages();
   break;
  case 12:
   parent.setView("export");
   break;
  };
 };
 function dataClick(i)
 {
  switch(i)
  {
  case 0:
   plotOptions.showData=false;
   parent.updateView();
   break;
  case 1:
   parent.dataEditor("ocp_data.html",true);
   break;
  case 2:
   parent.dataEditor("ocp_data.html");
   break;
  case 4:
   parent.dataEditor("ocp_data_d18O.html");
   break;
  case 5:
   parent.dataEditor("ocp_data_d18O_N.html");
   break;
  case 6:  
   parent.dataEditor("ocp_data_D14C.html");
   break;
  };
 };
 function initialise()
 {
  initialiseVersion();
  makeActive("opt","0",true);
  makeActive("edit","0",true);
  if(!parent.args.source){makeActive("main","0");}
  else{makeActive("main","-1");};
  parent.leftLoaded=true;
  parent.initialise();
 };
 // calibration section
 function quickCal()
 {
  var sl=document.getElementById("quickVerb");
  parent.runOxCal('R_'+
   sl.options[sl.selectedIndex].text.replace(/\s/g,"")+'("' +
   document.getElementById("name").value + '", ' +
   document.getElementById("date").value + ', ' +
   document.getElementById("error").value + ');\n');
 };
 function viewCurve()
 {
  if(isBomb())
  {
   parent.runOxCal("Axis(1950,2000)");
  }
  else
  {
   parent.runOxCal("Axis(0,2000)");
  };
 };

 // version section
 function myWriteDate(dt,tm)
 {
  var d,m,y,h,n,s,str;
  d=dt.getDate();
  m=dt.getMonth()+1;
  y=dt.getFullYear();
  h=dt.getHours();
  n=dt.getMinutes();
  s=dt.getSeconds();
  str=d+"/"+m+"/"+y;
  if(tm)
  str+=" "+h+":"+n+":"+s;
  return str;
 };
 function initialiseVersion()
 {
  var ver,oldver;
  ver=header.title+" ["+header.version+"]";
  oldver=parent.getCookie("OxCalVersion");
  if(oldver!=ver)
  {
   if(oldver){oldver="\n\nPrevious version was:\n"+oldver;}else{oldver="";};
   if(parent.onAServer())
   {
    alert("New version of OxCal:\n"+ver+"\n\nPlease clear your browser's cache or\nreload any pages if you encounter problems"+oldver);
   };
  };
  parent.setCookie("OxCalVersion",ver);
 };
