var header;
function getCurve(item)
{
 var sl,crv;
 sl=document.getElementById(item);
 crv=sl.options[sl.selectedIndex].text.replace(/\s/g,"");
 if(crv.indexOf(".14c")==-1){crv+=".14c";};
 return crv;
};
function curveCmd(crv)
{
 return 'Curve("'+crv.replace('.14c','')+'","'+crv+'")';
};
function isBomb()
{
 var crv=document.getElementById("Curve").value;
 return (crv.indexOf("Bomb")!=-1)||(crv.indexOf("Kueppers")!=-1);
};
function dealWithCurve(item)
{
 var crv=getCurve(item);
 document.getElementById("Curve").value=crv;
 if(document.getElementById("quickVerb"))
 {
  // this is simple input set some options
  if(isBomb())
  {
   document.getElementById("quickVerb").selectedIndex=1;
   document.getElementById("showF14C").checked=true;
   plotOptions.showF14C=true;
  }
  else
  {
   document.getElementById("quickVerb").selectedIndex=0;
   document.getElementById("showF14C").checked=false;
   plotOptions.showF14C=false;
  };
 };
 if(isBomb())
 {
  document.getElementById("Resolution").value="0.2";
 }
 else
 {
  document.getElementById("Resolution").value="";
 };
 return document.getElementById(item).selectedIndex;
};
function pickCurveFunc()
{
 var ind,sl;
 ind=dealWithCurve("pickCurve");
 sl=document.getElementById("pickCurveAtmos");
 if(ind<sl.options.length){sl.selectedIndex=ind;};
};
function pickCurveAtmos()
{
 var ind,sl;
 ind=dealWithCurve("pickCurveAtmos");
 sl=document.getElementById("pickCurve");
 if(ind<sl.options.length){sl.selectedIndex=ind;};
 if(parent.mainMode && (parent.mainMode=="Plot"))
 {
  setOptions();
 };
};
function addCurve(onlyIfMixed)
{
 var res,atm;
 var cmd;
 atm=getCurve("pickCurveAtmos");
 res=document.getElementById("Reservoir").value;
 if(Number(res)!=0)
 {
   res="{Reservoir("+res+","+document.getElementById("ReservoirSigma").value+");}";
   onlyIfMixed=false;
 }
 else
 {
   res="";
 };
 if(document.getElementById("Atmospheric").value==100)
 {
   if(onlyIfMixed){return "";};
   cmd=curveCmd(atm)+res+';\n';
 }
 else
 {
  if(document.getElementById("Marine").value==100)
  {
   cmd=curveCmd(getCurve("pickCurveMarine"))+';\n';
   if((document.getElementById("MarineDeltaR").value!=0) || (document.getElementById("MarineDeltaRSigma").value!=0))
   {
    cmd+='Delta_R("LocalMarine",'
    +document.getElementById("MarineDeltaR").value
    +','
    +document.getElementById("MarineDeltaRSigma").value
    +');\n';
   };
  }
  else
  {
   cmd=curveCmd(atm)+res+';\n'
    +curveCmd(getCurve("pickCurveMarine"))+';\n';
   if((document.getElementById("MarineDeltaR").value!=0) || (document.getElementById("MarineDeltaRSigma").value!=0))
   {
    cmd+='Delta_R("LocalMarine",'
    +document.getElementById("MarineDeltaR").value
    +','
    +document.getElementById("MarineDeltaRSigma").value
    +');\n'
    +'Mix_Curve("Mixed","'
    +atm.replace('.14c','')
    +'","LocalMarine",'
    +document.getElementById("Marine").value
    +','
    +document.getElementById("MarineSigma").value
    +');\n';
   }
   else
   {
    cmd+='Mix_Curve("Mixed","'
    +atm.replace('.14c','')
    +'","'+getCurve('pickCurveMarine').replace('.14c','')+'",'
    +document.getElementById("Marine").value
    +','
    +document.getElementById("MarineSigma").value
    +');\n';
   };
  };
 };
 return cmd;
};
function radioOption(name)
{
 var radios,i;
 radios=document.getElementsByName(name);
 if(!radios){return "";};
 for(i=0;i<radios.length;i++)
 {
  if(radios[i].checked)
  {
   switch(radios[i].value)
   {
   case "true":
    return name+"=TRUE;";
   case "false":
    return name+"=FALSE;";
   };
  };
 };
 return "";
};
function setOptions()
{
 var code="Options(){";
 if(document.getElementById("Resolution").value)
 { code+="Resolution="+document.getElementById("Resolution").value+";" };
 if(document.getElementById("Curve").value)
 { code+="Curve=\""+document.getElementById("Curve").value+"\";" };
 code+=radioOption("Cubic");
 code+=radioOption("RawData");
 code+=radioOption("UseF14C");
 code+=radioOption("BCAD");
 code+=radioOption("PlusMinus");
 code+=radioOption("Intercept");
 code+=radioOption("Floruit");
 code+=radioOption("SD1");
 code+=radioOption("SD2");
 code+=radioOption("SD3");
 code+=radioOption("ConvergenceData");
 code+=radioOption("UniformSpanPrior");
 if(document.getElementById("kIterations"))
 {
  if(document.getElementById("kIterations").value)
  { code+="kIterations="+document.getElementById("kIterations").value+";" };
 };
 if(document.getElementById("ensembles"))
 {
  if(document.getElementById("ensembles").value)
  { code+="Ensembles="+document.getElementById("ensembles").value+";" };
 };
 code+="};";
 parent.setOptions(code);
};
function helpClick(c)
{
 var s,wnd;
 switch(c)
 {
 case "Contents":
 case "Curves":
 case "Input":
 case "Analysis":
 case "Output":
 case "About":
 case "Commands":
  wnd=parent.open("../oxcalhelp/hlp_"+c.toLowerCase()+".html","help",
   "width=800,height=600,scrollbars=yes,toolbar=yes");
  wnd.focus();
  wnd.opener=parent;
  break;
 case "References":
  wnd=parent.open("../oxcalhelp/ref.html","help",
   "width=800,height=600,scrollbars=yes,toolbar=yes");
  wnd.focus();
  wnd.opener=parent;
  break;
 case "Tips":
  toggleHelp();
  break;
 };
};
