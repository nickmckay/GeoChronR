 var ocd;
 var calib;
 var tableWindow;
 var textWindow;
 var showControls=true;
 var selectNo=0;
 var source="ocp_null.js";
 var args;
 var wasWorking=false;
 var optionCommand="";
 var curveCommand="";
 var is_focussed=false;
 var ocdIndex;
 var tableDrawn=false;
 var leftLoaded=false;
 var loaderLoaded=false;
 var framesLoaded=false;
 var showingSequence=false;
 var svgContent="";
 var page;
 var headerSet=true;
 var multiPlots={plots:[],showKey:false,keyColumn:0,keyRow:0,columns:3};
 var canvas,plotInfo,plotData,ocPlotWindow;
 var plotColor={min_col:"#0000ff",max_col:"#ff0000"};
 var playerMin,playerMax,ocMapWindow;
 var playerDisable=false;
 
 function makePlotOptionsString()
 {
  var str="";
  function addItem(ind)
  {
   str+="plotOptions."+ind+"="+plotOptions[ind]+";\n";
  };
  str+="plotOptions.showRange[1]="+plotOptions.showRange[1]+";\n";
  str+="plotOptions.showRange[2]="+plotOptions.showRange[2]+";\n";
  str+="plotOptions.showRange[3]="+plotOptions.showRange[3]+";\n";
  addItem("reportingStyle");
  addItem("showReversed");
  addItem("showWhole");
  addItem("plotsPerPage");
  addItem("scale");
  addItem("scaleFont");
  addItem("scaleLine");
  addItem("showIndices");
  addItem("showPropPlace");
  addItem("showADFirst");
  addItem("showNormalised");
  addItem("showAgreement");
  addItem("showConvergence");
  addItem("showOutliers");
  addItem("colorOutliers");
  addItem("showParameters");
  addItem("showVerbs");
  addItem("showItalics");
  addItem("showGrid");
  addItem("showText");
  addItem("showBandW");
  addItem("showBrackets");
  return str;
 };
 
 function readPlotOptionsString(str)
 {
  eval(str);
 };
 
 function saveOptions()
 {
  setCookie("OxCalPlotOptions",makePlotOptionsString());
 };
 
 function restoreOptions()
 {
  var str=getCookie("OxCalPlotOptions");
  if(str.length>100)
  {
   readPlotOptionsString(str);
  };
 };
 
 function getPlotName()
 {
  if(plotOptions.showBandW)
  {
   return "ocp_plot_bw_svg.html";
  };
  return "ocp_plot_svg.html";
 };
 
 function resetSettings()
 {
  if(!confirm("Reset OxCal to default settings?")){return;};
  if(!onAServer())
  {
   resetAddon();
  };
  delCookie("OxCalPlotOptions");
  delCookie("OxCalVersion");
  delCookie("OxCalWorkingPath");
  delCookie("OxCalHomePath");
  if(onAServer())
  {
   window.location=window.location;
  };
 };
  
 function tidyText(str)
 {
  str=str.replace(/\x2b\x2f\x2d/g,String.fromCharCode(0x00b1));
  str=str.replace(/\</g,"&lt;");
  return str;
 };

 function defaultPaging(no)
 {
  var old=plotOptions.plotFrom;
  if(!no){no=1;};
  if(no > (window.parent.ocd.length-1)){return false;};
  if(no < 1){no=1;};
  plotOptions.plotFrom=no;
  plotOptions.plotLast=no-1;
  plotOptions.plotNext=no+1;
  plotOptions.plotPrev=no-plotOptions.plotsPerPage;
  plotOptions.plotTo=no+plotOptions.plotsPerPage;
  if(plotOptions.plotPrev<0){plotOptions.plotPrev=0;};
  if(plotOptions.plotLast<0){plotOptions.plotLast=0;};
  return old!=plotOptions.plotFrom;
 };
 
 function startPrint(single)
 {
  filecontent="\\documentclass[a4paper]{article}\n"
   +"\\usepackage{graphicx}\n"
   +"\\pagestyle{empty}\n"
   +"\\parindent=0pt\n"
   +"\\parskip=0pt\n"
   +"\\oddsidemargin  0cm\n"
   +"\\evensidemargin 0cm\n"
   +"\\textwidth      16.35cm\n"
   +"\\headheight     0cm\n"
   +"\\topmargin     -1cm\n"
   +"\\textheight  28cm\n"
   +"\\headsep  0cm\n";
  filecontent+="\\newcommand{\\oxcalfig}[1]{\\includegraphics[width=";
  if((plotOptions.viewType=="individual")&&(!single))
  {
   filecontent+="8";
  }
  else
  {
   filecontent+="16";
  };
  filecontent+="cm]{#1.pdf}}\n";
  filecontent+="\\begin{document}\n";
  svgContent="";
  page=0;
 };
 
 function getSVGContent()
 {
  var printPossible=right.location.toString().match(getPlotName()) && canSVG();
  var cont;
  if(printPossible)
  {
   cont=xml2Str(right.document.getElementById("top"));
   // get rid of duplicate header (Opera)
   cont=cont.replace('\<\?xml version="1.0"\?\>\<\?xml version="1.0" \?\>','\<\?xml version="1.0" \?\>');
   // add header if not there at all (Safari)
   if(cont.indexOf("\<\?xml")==-1)
   {
    cont="\<\?xml version=\"1.0\" encoding=\"UTF-8\"\?\>"+cont;
   };
   // get rid of any non-ascii characters
//   cont=cont.replace(/[^\u000A\u000D\u0020-\u007F]/g,"_");
   // remove problem parts of IE9 header
   if(cont.indexOf("xmlns:NS1=")!=-1)
   {
    cont=cont.replace(' xmlns:NS1=""','');
    cont=cont.replace(' NS1:xmlns:ev="http://www.w3.org/2001/xml-events"','');
    cont=cont.replace(' version="1.1"','');
   };
   // correct problems with Safari symbol links
   cont=cont.replace(/ xlink\=/g,' xmlns:xlink=');
   cont=cont.replace(/use href\=/g,'use xlink:href=');
   return cont;
  };
  return false;
 };
 
 function pagePrint()
 {
  var cont;
  if(plotOptions.plotFrom > plotOptions.plotTo){return;};
  cont=getSVGContent();
  if(cont)
  {
   filename=source;
   svgContent+=cont;
   page++;
   filecontent+="\\oxcalfig{temp_"+page+"}\n";
   if(plotOptions.viewType=="individual")
   {
    if(!(page%2)){filecontent+="\n";};
   }
   else
   {
    filecontent+="\n";
   };
  };
 };
 
 function endPrint()
 {
   filecontent+="\\end{document}\n";
   filecontent+=svgContent;
   svgContent="";
   showingSequence=false;
   saveFileAs("tex");
 };
 
 function nextSlide()
 {
  var changed=false;
  pagePrint();
  switch(plotOptions.viewType)
  {
  case "individual":
   do
   {
    changed=defaultPaging(plotOptions.plotNext);
   }
   while(!ocd[plotOptions.plotFrom].selectNo && changed);
   break;
  case "select":
  case "stack":
  case "multiple":
   changed=defaultPaging(plotOptions.plotTo+1);
   break;
  };
  if(changed)
  {
   updateView();
  }
  else
  {
   left.makeActive("main","7");
  };
 };

 function printPages()
 {
  switch(plotOptions.viewType)
  {
  case "individual":
  case "multiple":
  case "select":
  case "stack":
   if(!canSVG()){alert("Not possible with this browser");return;};
   if(confirm("Print all pages?\n(this will lose any adjustments on this page)"))
   {
    startPrint(false);
    showingSequence=true;
    defaultPaging(0);
    updateView();
    break;
   };
   if(!confirm("Print just this page?"))
   {
    break;
   };
  case "curve":
  case "z":
   if(!canSVG()){alert("Not possible with this browser");return;};
   startPrint(true);
   pagePrint();
   endPrint();
   break;
  default:
   window.right.print();
   break;
  };
 };

 function goPlotData(i)
 {
  plotOptions.plotFrom=i;
  setView("individual");
 };
 
 function rawData(i)
 {
  plotOptions.plotFrom=i;
  setView("raw");
 };
 
 // sort first on whether relative, then on select order
 function selectSort(a,b)
 {
  if(a.rel && !b.rel){return 1;};
  if(b.rel && !a.rel){return -1;};
  return a.sel-b.sel;
 };
 
 function sortSelection()
 {
  ocdIndex=new Array;
  var i,j,last;
  j=0;last=0;
  for(i=0;i<ocd.length;i++)
  {
   if(!ocd[i]){continue;};
   if(ocd[i].selectNo>0)
   {
    if(ocd[i].op && (ocd[i].op=="Page") && (last!=0))
    {
     ocd[last].newPage=true;
     ocd[i].selectNo=0;
    }
    else
    {
     ocdIndex[j]=new Object;
     ocdIndex[j].ind=i;
     ocdIndex[j].sel=ocd[i].selectNo;
     ocdIndex[j].rel=ocd[i].relative;
     if(ocd[i].type)
     {
      ocdIndex[j].rel=(ocd[i].type=="interval");
     }
     else
     {
      ocdIndex[j].rel=ocd[i].relative;
     };
     // put page breaks on main distributions, not intervals
     if(!ocd[i].relative)
     {
      last=i;
     };
     j++;
    };
   };
  };
  selectNo=j;
  ocdIndex.sort(selectSort);
  for(j=0;j<ocdIndex.length;j++)
  {
   ocd[ocdIndex[j].ind].selectNo=j+1;
  };
 };

 function isInternetExplorer()
 {
  return navigator.appName.indexOf("Microsoft")!=-1;
 };
 
 function runOxCal(command,direct)
 {
  var fullCmd;
  curveCommand=left.addCurve(true);
  left.setOptions();
  if(direct)
  {
   fullCmd=command;
  }
  else
  {
   fullCmd=optionCommand+
    "Plot(){"+curveCommand+command+"};";
  };
  if(onAServer())
  {
   source="/Quick.js";
   loader.document.getElementById("runFileCommand").value=fullCmd;
   loader.document.getElementById("runFile").submit();
   return true;
  }
  else
  {
   if(localFilePossible())
   {
    source="/Quick.js";
    localFileWrite(fullPath("Quick.oxcal"),fullCmd);
    source=fullPath("Quick.js");
    if(fullCmd.indexOf('Mix_')!=-1)
    {
     localOxCalLaunch(fullPath("Quick.oxcal"),false,
     function(){window.setTimeout("reload();plotOxCal('','status')",500);});
    }
    else
    {
     localOxCalLaunch(fullPath("Quick.oxcal"),true,
     function(){window.loader.location.reload();});
    };
    return true;
   }
   else
   {
    alert("You cannot run OxCal from this Browser:\nSee documentation");
   };
  };
  return false;
 };

 function xml2Str(xmlNode)
 {
  try {
    return (new XMLSerializer()).serializeToString(xmlNode);
  }
  catch (e) {
    try {
      // Internet Explorer.
      return xmlNode.xml;
    }
    catch (e)
    {
     alert('Xmlserializer not supported');
    };
  };
  return false;
 };
 
 function canSVG()
 {
  if(isInternetExplorer())
  {
   if(window.navigator.appVersion.indexOf("MSIE 9")!=-1)
   {
    return true;
   }
   else
   {
    return false;
   };
  }
  else
  {
   if(XMLSerializer){return true;};
  };
  return false;
 };
 
 function setView(opt,redraw)
 {
  var comm,oldopt;
  if((ocd.length<2)&&(!calib.length))
  {
   right.location.replace("../oxcalhelp/hlp_contents.html");
   return;
  };
  switch(opt)
  {
  case "tab":
   editFile(source.replace('.js','.txt'),"txt");
   return;
  case "log":
   editFile(source.replace('.js','.log'),"log");
   return;
  };
  oldopt=plotOptions.viewType;
  plotOptions.viewType=opt;
  left.getoptions();
  saveOptions();
  switch(opt)
  {
  case "status":
   if(onAServer())
   {
    right.location.replace("ocp_status.php");
   }
   else
   {
    right.location.replace("ocp_status.html");
   };
   break;
  case "table":
   sortSelection();
   right.location.replace("ocp_right.html");
   break;
  case "parameter":
   right.location.replace("ocp_param.html");
   break;
  case "outlier":
   right.location.replace("ocp_outlier.html");
   break;
  case "raw":
   right.location.replace("ocp_raw.html");
   break;
  case "export":
   right.location.replace("ocp_export.html");
   break;
  case "select":
  case "stack":
   sortSelection();
  case "individual":
  case "multiple":
  case "x":
  case "y":
  case "z":
  case "curve":
  case "correlation":
  case "model":
   sortSelection();
   if(!redraw){resetOptions();};
   findSize();
   setupPlot();
   left.readback();
   if((onAServer()) && (!isInternetExplorer()))
   {
    if(canSVG())
	{
     right.location.replace(getPlotName());
	}
	else
	{
     right.location.replace("ocp_plot_loader.html");
	};
   }
   else
   {
    if(canSVG())
	{
     right.location.replace(getPlotName());
	}
	else
	{
     right.location.replace("ocp_plot.html");
	};
   };
   break;
  };
  if(!left){return;};
  switch(opt)
  {
  case "individual":
  case "multiple":
  case "select":
  case "stack":
  case "raw":
     left.document.getElementById("Position").value=plotOptions.plotFrom;
     if(!mainMode)
     {
      left.document.getElementById("Pager").style.display="block";
     };
   break;
  default:
   if(left.document.getElementById("Pager"))
   {
    left.document.getElementById("Pager").style.display="none";
   };
   break;
  };
  if(args.Command)
  {
   comm=args.Command;
   args.Command=false;
   runOxCal(comm,true);
  };
  if((header_right&&(oldopt!=plotOptions.viewType))||(!headerSet))
  {
   headerSet=true;
   window.header_right.location.replace("oc_header_right.html");
  };
 };
 
 function updateView()
 {
  setView(plotOptions.viewType);
 };

 function redrawView()
 {
  setView(plotOptions.viewType,true);
 };
 
// function associated with mapping

 function ocdObj(i)
 {
  if(plotOptions.showPosterior && ocd[i].posterior){return ocd[i].posterior;};
  if(plotOptions.showLikelihood && ocd[i].likelihood){return ocd[i].likelihood;};
  return false;
 };
 function ocdMin(i)
 {
  var obj=ocdObj(i);
  if(!obj){return "NaN";};
  return obj.start;
 };
 function ocdMax(i)
 {
  var obj=ocdObj(i);
  if(!obj){return "NaN";};
  if(!obj.prob){return "NaN";};
  return obj.start+obj.prob.length*obj.resolution;
 };
 function ocdValMax(i)
 {
  var j,obj=ocdObj(i);
  if(!obj){return "NaN";};
  if(!obj.prob){return "NaN";};
//  if(!plotOptions.mapPlotNormalise){return 1;};
  return obj.probNorm;
 };
 function ocdVal(i,v,forceNorm)
 {
  var j,obj=ocdObj(i);
  if(!obj){return "NaN";};
  if(!obj.prob){return "NaN";};
  j=Math.round((v-obj.start)/obj.resolution);
  if((j<0) || (j>=obj.prob.length)){return 0;};
  if(!plotOptions.mapPlotNormalise && !forceNorm){return obj.prob[j];};
  return obj.prob[j]*obj.probNorm;
 };
 
 function findMapMinMax()
 {
  var sumMax,i,sm;
  plotOptions.player_proxy=false; // not using proxies
  plotOptions.player_min="NaN";plotOptions.player_max="NaN";sumMax="NaN";plotOptions.probMax="NaN";

  plotData=[{name:"",line:"",markerColor:"rgb(255,0,0)",
    markerFill:"rgba(255,0,0,0.5)",marker:"circle",selected:true,
    include_id:true,include_url:true,include_color:true,include_marker:true}];
  
  plotData[0].data=new Array();
   
  for(i=0;i<ocd.length;i++)
  {
   if(ocd[i])
   {
    if(ocd[i].data)
    {
     if(ocd[i].data.longitude || ocd[i].data.latitude)
     {
      if(!ocd[i].data.id){ocd[i].data.id='ocd'+i;};
      plotData[0].data.push(ocd[i].data);
      if(plotOptions.showPosterior)
      {
       if(typeof(ocd[i].posterior.mean)!='undefined')
       {
        ocd[i].data.mean=ocd[i].posterior.mean;
        ocd[i].data.median=ocd[i].posterior.median;
        ocd[i].data.sigma=ocd[i].posterior.sigma;
       };
      }
      else
      {
       if(plotOptions.showLikelihood)
       {
        if(typeof(ocd[i].likelihood.mean)!='undefined')
        {
         ocd[i].data.mean=ocd[i].likelihood.mean;
         ocd[i].data.median=ocd[i].likelihood.median;
         ocd[i].data.sigma=ocd[i].likelihood.sigma;
        };
       };
      };
      if(isNaN(plotOptions.player_min)||(ocdMin(i)<plotOptions.player_min))
      {
       plotOptions.player_min=ocdMin(i);
      };
      if(isNaN(plotOptions.player_max)||(ocdMax(i)>plotOptions.player_max))
      {
       plotOptions.player_max=ocdMax(i);
      };
      if(ocd[i].op=='Sum')
      {
       if(isNaN(sumMax)||(ocdValMax(i)>sumMax))
       {
        sumMax=ocdValMax(i);
       };
      }
      else
      {
       if(isNaN(plotOptions.probMax)||(ocdValMax(i)>plotOptions.probMax))
       {
        plotOptions.probMax=ocdValMax(i);
       };
      };
     };
    };
   };
  };
  if(isNaN(plotOptions.probMax))
  {
   if(!isNaN(sumMax))
   {
    plotOptions.probMax=SumMax/100;
   };
  }
  else
  {
   if(!isNaN(sumMax))
   {
    if(sumMax>100*plotOptions.probMax){plotOptions.probMax=sumMax/100;};
   };
  };
  if((plotOptions.player_max-plotOptions.player_min)>0)
  {
   plotOptions.mapPlotMultiIncr=Math.round((plotOptions.player_max-plotOptions.player_min)/12.5);
  };
  sumMax=0;
  for(plotOptions.current=0;plotOptions.current<plotOptions.currentMax;plotOptions.current++)
  {
   sm=0;
   for(i=0;i<plotData[0].data.length;i++)
   {
    sm+=playerWeight(plotData[0].data[i].id);
   };
   if(sm>sumMax){sumMax=sm;};
  };
  plotOptions.contourProbMax=sumMax;
  plotOptions.current=0;
 };
 
 function plotOnMap()
 {
   var k,plt;
   playerDisable=false;
   
   canvas=new Object;
   canvas.scale=1.0; // zoom scaling
   canvas.scaleFont=0.8; // font scaling
   canvas.scaleLine=1.3; // line scaling
   canvas.pxPerCm=35; // px per cm
   canvas.frameWidth=18.0; // cm
   canvas.frameHeight=13.5; // cm
   
   plotInfo=new Object;
   plotInfo.minx=0;
   plotInfo.maxx=100;
   plotInfo.autox=true;
   plotInfo.miny=0;
   plotInfo.maxy=100;
   plotInfo.autoy=true;
   plotInfo.x_calc="longitude";
   plotInfo.y_calc="latitude";
   plotInfo.dx_calc="";
   plotInfo.dy_calc="";
   plotInfo.xlabel="";
   plotInfo.ylabel="";
   plotInfo.weight="playerWeight(id)";
   plotInfo.title="";
   plotInfo.category="";
   plotInfo.table="";
   plotInfo.keyTitle="Key";
   plotInfo.pcaVariables="longitude,latitude";
   plotInfo.nonlinx="";
   plotInfo.nonliny="mercator";
   plotInfo.mapPlot=true;
      
   plotOptions.background="rgb(255,255,255)";
   plotOptions.plotBackground="rgba(255,255,255,0)";
   plotOptions.multiPlot=false; // gives number of columns if multi
   plotOptions.plotPosX=3.0; // cm
   plotOptions.plotPosY=1.5;  // cm
   plotOptions.plotWidth=13.5; // cm
   plotOptions.plotHeight=13.5; // cm
   plotOptions.showKey=false;
   plotOptions.BandW=false;
   plotOptions.contours="95%";
   plotOptions.contourFactor=1;
      
   playerMin=showDate(showDateT(plotOptions.player_min,"date"),"date");
   playerMax=showDate(showDateT(plotOptions.player_max,"date"),"date");
   
   if(plotOptions.player_proxy)
   {
    findProxyLevels(plotColor,plotData);
   };
   
   ocMapWindow=window.open("","Mapper");
   if(ocMapWindow.editOptions)
   {
    ocMapWindow.focus();
    ocMapWindow.editOptions.window=window;
    ocMapWindow.setDataTables();
   }
   else
   {
    ocMapWindow=window.open("../oxplot/OxPlot.html","Mapper");
   };
   multiPlots.tiedXAxes=3;
   multiPlots.tiedYAxes=3;
   multiPlots.plots=[];
   for(k=0;(k<1+Math.round((plotOptions.player_max-plotOptions.player_min)
  	/plotOptions.mapPlotMultiIncr))&&(k<100);k++)
   {
    plt=duplItem(plotInfo);
    plt.selected=true;
    plt.row=Math.floor(k / multiPlots.columns);
    plt.column=k % multiPlots.columns;
    plt.weight="playerWeight(id,"+k+")";
    multiPlots.plots.push(plt);
   };
   plotOptions.current=0;
 };

 function plotOnPlot(dta,items,this_name,this_id,ts_id)
 {
   var k,plt,col,crv,nm;
   playerDisable=true;
   canvas=new Object;
   canvas.scale=1.0; // zoom scaling
   canvas.scaleFont=0.8; // font scaling
   canvas.scaleLine=1.3; // line scaling
   canvas.pxPerCm=35; // px per cm
   canvas.frameWidth=18.0; // cm
   canvas.frameHeight=13.5; // cm
   
   plotInfo=new Object;
   plotInfo.minx=0;
   plotInfo.maxx=100;
   plotInfo.miny=0;
   plotInfo.maxy=100;
 
   plotColor.z_calc="";
   plotColor.dz_calc="";
   plotColor.showKey=false;


   nm=ts_id.slice(0,1).toUpperCase()+ts_id.slice(1);
   switch(plotOptions.reportingStyle)
   {
   case 0:
    plotInfo.x_calc="calBP("+ts_id+"_t)";
    plotInfo.xlabel=nm+" Age (cal BP)";
    plotInfo.autox=2;
    break;
   case 1:
    plotInfo.x_calc="AD("+ts_id+"_t)";
    plotInfo.xlabel=nm+" Date (AD)";
    plotInfo.autox=1;
    break;
   case 2:
    plotInfo.x_calc="CE("+ts_id+"_t)";
    plotInfo.xlabel=nm+" Date (CE)";
    plotInfo.autox=1;
    break;
   default:
    plotInfo.x_calc=ts_id+"_t";
    plotInfo.xlabel=nm+" Date (+/- CE)";
    plotInfo.autox=1;
    break;
   };
   plotInfo.y_calc="z";
   plotInfo.dx_calc=ts_id+"_dt";
   plotInfo.dy_calc="";
   if(this_id.slice(0,3)!="ocd")
   {
    plotInfo.ylabel=this_id.slice(0,1).toUpperCase()+this_id.slice(1)+" Depth";
   }
   else
   {
    plotInfo.ylabel="Depth";
   };
   plotInfo.autoy=2;
   plotInfo.title="";
   plotInfo.category="";
   plotInfo.table="";
   plotInfo.keyTitle="Key";
   plotInfo.pcaVariables="";
   plotInfo.nonlinx="";
   plotInfo.nonliny="";
   plotInfo.mapPlot=false;
      
   plotOptions.background="rgb(255,255,255)";
   plotOptions.plotBackground=false;
   plotOptions.multiPlot=(items.length>0); // gives number of columns if multi
   plotOptions.plotPosX=3.0; // cm
   plotOptions.plotPosY=1.5;  // cm
   plotOptions.plotWidth=13.5; // cm
   if(plotOptions.plotWidth<2){plotOptions.plotWidth=2;};
   plotOptions.plotHeight=13.5; // cm
   if(items.length>0)
   {
    plotOptions.plotHeight=3; // cm
   };
   plotOptions.showKey=1;
   plotOptions.BandW=false;
   plotOptions.contours="95%";
   plotOptions.current=0;
   
   multiPlots.columns=1;
   multiPlots.plots=[];
   multiPlots.tiedXAxes=3;
   multiPlots.tiedYAxes=0;
   for(k=0;k<items.length;k++)
   {
    plt=duplItem(plotInfo);
    plt.selected=true;
    plt.x_calc=plotInfo.x_calc;
    plt.y_calc=items[k];
    plt.dx_calc=plotInfo.dx_calc;
    plt.dy_calc="";
    plt.autox=plotInfo.autox;
    plt.autoy=1;
    plt.xlabel=plotInfo.xlabel;
    plt.ylabel=items[k];
    plt.row=k;
    plt.column=0;
    multiPlots.plots.push(plt);
   };
   ocPlotWindow=window.open("","Plotter");
   if(false /*ocPlotWindow.editOptions*/)
   {
    col=ocPlotWindow.colors[ocPlotWindow.plotData.length % ocPlotWindow.colors.length];
    plotData=[{name:this_name,line:"solid",
    lineColor:col.replace( "rgb","rgba").replace(")",",1)"),
    markerColor:"",
    markerFill:col.replace( "rgb","rgba").replace(")",",0.2)"),
    marker:"",selected:true,id:this_id}];
  
    plotData[0].data=duplRemoteItem(right,dta);
    ocPlotWindow.focus();
    ocPlotWindow.editOptions.window=window;
    ocPlotWindow.setDataTables();
   }
   else
   {
    plotData=[{name:this_name,line:"solid",lineColor:"rgba(255,0,0,1.0)",markerColor:"",
    markerFill:"rgba(255,0,0,0.2)",marker:"",selected:true,id:this_id}];
  
    plotData[0].data=duplRemoteItem(right,dta);
    ocPlotWindow=window.open("../oxplot/OxPlot.html","Plotter");
   };
  };


 function setupData()
 {
  var anyPosterior=false;
  var datacount=0;
  var first="NaN";
  if(ocd.length)
  {
   var i;
   for(i=0;i<ocd.length;i++)
   {
    // select the data
    if(ocd[i])
    {
     ocd[i].selectNo=i+1;
     if(ocd[i].likelihood)
     {
      if(ocd[i].likelihood.prob)
      {
       if(isNaN(first)){first=i;};
       datacount++;
      };
     };
     // check if there is any posterior
     if(ocd[i].posterior)
     {
      if(ocd[i].posterior.prob)
      {
       if(isNaN(first)){first=i;};
       datacount++;anyPosterior=true;
      };
     };
    };
   };
  };
  // if there is any posterior data then show it
  if(anyPosterior)
  {
   plotOptions.showPosterior=true;
   plotOptions.showIndices=true;
  }
  else
  {
   plotOptions.showPosterior=false;
   plotOptions.showIndices=false;
  };
  switch(datacount)
  {
  case 0:
   if(calib && calib.length>0)
   {
    plotOptions.viewType="curve";
    headerSet=false;
   };
   break;
  case 1:
   plotOptions.plotFrom=first;
   if(document.implementation.hasFeature("org.w3c.dom.svg","1.0"))
   {
    plotOptions.viewType="individual";
    headerSet=false;
   };
   break;
  };
  sortSelection();
 };
 function setData()
 {
  var test,i,podt,podd;
  if(!plotOptions.data){return;};
  podt=plotOptions.data.t;
  podd=plotOptions.data.d;
  if(!podt || !podd){return;};
  test=new Array;
  for(i=0;(i< podd.length) && (i< podt.length);i++)
  {
    if(plotOptions.reportingStyle!=0)
    {
     test[i]=Math.round((podt[i])*100)/100 + "\t" + podd[i];
    }
    else
    {
     test[i]=Math.round((plotOptions.BPDatum-podt[i])*100)/100 + "\t" + podd[i];
    };
  };
  right.document.getElementById("dataValues").value=test.join("\n");
  // set up data table from defaults
  right.document.getElementById("dataLabel").value=plotOptions.data.label;
  right.document.getElementById("dataMin").value=plotOptions.data.min;
  right.document.getElementById("dataMax").value=plotOptions.data.max;
  right.document.getElementById("dataBCAD").checked=(plotOptions.reportingStyle!=0);
 };
 function getData()
 {
  var test,i,podt,podd,line;
  if(!plotOptions.data){return;};
  plotOptions.data.t=new Array();
  plotOptions.data.d=new Array();
  podt=plotOptions.data.t;
  podd=plotOptions.data.d;
  test=right.document.getElementById("dataValues").value.split("\n");
  for(i=0;i< test.length;i++)
  {
   line=test[i].split("\t");
   if(line.length && line.length>1)
   {
    podt[i]=Number(line[0]);
    podd[i]=Number(line[1]);
   };
  };
  if(!right.document.getElementById("dataBCAD").checked)
  {
   for(i=0;i< podt.length;i++)
   {
    podt[i]=plotOptions.BPDatum-podt[i];
   };
  };
  plotOptions.data.label=right.document.getElementById("dataLabel").value;
  plotOptions.data.min=Number(right.document.getElementById("dataMin").value);
  plotOptions.data.max=Number(right.document.getElementById("dataMax").value);
 };
 function dataEditor(filename,clear)
 {
  if(clear)
  {
   plotOptions.data.label="";
   plotOptions.data.min=0;
   plotOptions.data.max=1;
   plotOptions.data.t=new Array();
   plotOptions.data.d=new Array();
  };
  right.location.replace(filename);
 };
 function dataOk()
 {
  getData();
  plotOptions.showData=true;
  updateView();
 };
 function dataCancel()
 {
  plotOptions.showData=false;
  updateView();
 };
 function reload()
 {
  loader.location.reload();
  window.status=source;
 };
 
 // set things up for viewing the data
 if(window.parent && window.parent.ocd)
 {
  ocd=window.parent.ocd;
  calib=window.parent.calib;
  setupData();
 }
 else
 {
  ocd=new Array;
  calib=new Array;
 };
 args=getArgs();
 if(args.source)
 {
  source=args.source;
 };
 if(args.view)
 {
  plotOptions.viewType=args.view;
 };
 window.status=source;
 window.focus();
 
