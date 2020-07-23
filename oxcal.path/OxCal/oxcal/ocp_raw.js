 var ocd=parent.ocd;
 var model=parent.model;
 var calib=parent.calib;
 var plotOptions=parent.plotOptions;
 var zmem=9999.9e99;
 var headers;
 var depthModelData;
 var z_name;
 var t_name,dt_name;
 var sigProbs=["","68_2","95_4","99_7"];
 function sigfig(v,s)
 {
  if(v==0){return v;};
  var mult=Math.round(Math.exp(Math.LN10*(-Math.round(Math.log(Math.abs(v))/Math.LN10)+s)));
  if(mult==0){return v;};
  return Math.round(v*mult)/mult;
 };
 function prob(v)
 {
  return sigfig(v,4);
 };
 function sqr(x){return x*x;};
 function doSave(id,ext)
 {
  parent.filecontent=document.getElementById(id).value;
  parent.filename=ocd[plotOptions.plotFrom].name;
  parent.saveFileAs(ext);
 };
 function doSaveStr(str,ext)
 {
  parent.filecontent=str;
  parent.filename=ocd[plotOptions.plotFrom].name;
  parent.saveFileAs(ext);
 };
 function outputDepthModelHeader(sep)
 {
  var str;
  if(sep==',')
  {
   str='"name","z",';
   if(plotOptions.showMean){str+='"mu",';};
   if(plotOptions.showSigma){str+='"sigma",';};
   if(plotOptions.showMedian){str+='"median",';};
   for(j=1;j<4;j++)
   {
    if(plotOptions.showRange[j])
    {
     switch(j)
     {
     case 1:str+='"from_68_2","to_68_2",';break;
     case 2:str+='"from_95_4","to_95_4",';break;
     case 3:str+='"from_99_7","to_99_7",';break;
     };
    };
   };
  }
  else
  {
   str='name'+sep+'z'+sep;
   if(plotOptions.showMean){str+='mu'+sep;};
   if(plotOptions.showSigma){str+='sigma'+sep;};
   if(plotOptions.showMedian){str+='median'+sep;};
   for(j=1;j<4;j++)
   {
    if(plotOptions.showRange[j])
    {
     switch(j)
     {
     case 1:str+='from_68_2'+sep+'to_68_2'+sep;break;
     case 2:str+='from_95_4'+sep+'to_95_4'+sep;break;
     case 3:str+='from_99_7'+sep+'to_99_7'+sep;break;
     };
    };
   };
  };
  str+="\n";
  return str;
 };
 function outputDepthModelItem(i,sep)
 {
  var d_m,d_ocd,post,r;
  var str="";
  d_m=model.element[i];
  d_ocd=ocd[i];
  if(!d_m || !d_ocd){return "";};
  post=d_ocd.posterior;
  if(!post){return "";};
  if(d_m.z==zmem){return "";};
  zmem=d_m.z;
  if(sep==',')
  {
   str+='"'+d_ocd.name+'",'+zmem+',';
  }
  else
  {
   str+=d_ocd.name+sep+zmem+sep;
  };
  if(plotOptions.showMean){str+=showDateT(post.mean,"date",0)+sep;};
  if(plotOptions.showSigma){str+=showDateT(post.sigma,"interval",0)+sep;};
  if(plotOptions.showMedian){str+=showDateT(post.median,"date",0)+sep;};
  for(j=1;j<4;j++)
  {
   if(plotOptions.showRange[j])
   {
    r=simplifyDateRange(post.range[j],"date",true);
    if(r[0][0]=='...'){r[0][0]=='"..."';};
    if(r[0][1]=='...'){r[0][1]=='"..."';};
    str+=r[0][0]+sep+r[0][1]+sep;
   };
  };
  str+="\n";
  return str;
 };
 function outputDepthModel(sep)
 {
  var i,k;
  str=outputDepthModelHeader(sep);
  for(i=0;i<model.group.length;i++)
  {
   if((model.group[i].parent==plotOptions.plotFrom)&&(model.group[i].depth_list))
   {
    str+=outputDepthModelItem(model.group[i].start,sep);
    for(k=0;k<model.group[i].depth_list.length;k++)
    {
     str+=outputDepthModelItem(model.group[i].depth_list[k],sep);
    };
    str+=outputDepthModelItem(model.group[i].end,sep);
   };
  };
  return str;
 };
 function makeDepthModelObject(i)
 {
  var d_m,d_ocd,post,r,j;
  d_m=model.element[i];
  d_ocd=ocd[i];
  if(!d_m || !d_ocd){return false;};
  post=d_ocd.posterior;
  if(!post){return false;};
  if(isNaN(d_m.z)){return false;};
  if(d_m.z==zmem){return false;};
  zmem=d_m.z;
  var obj=new Object();
  obj[z_name]=zmem;
  obj[t_name]=post.mean;
  obj[dt_name]=post.sigma; 
  if(plotOptions.showMedian){obj.median=post.median;};
  for(j=1;j<4;j++)
  {
   if(plotOptions.showRange[j])
   {
    r=simplifyDateRange(post.range[j],"date",true);
    obj["from_"+sigProbs[j]]=r[0][0];
    obj["to_"+sigProbs[j]]=r[0][1];
   };
  };
  return obj;
 };
 function checkIDs()
 {
  z_name="z";
  t_name=document.getElementById("ts_id").value+"_t";
  dt_name=document.getElementById("ts_id").value+"_dt";
 };
 function makeDepthModelArray()
 {
  var i,k,obj,min="NaN",max="NaN",a;
  depthModelData=new Array();
  checkIDs();
  function checkLims()
  {
   if((min=="NaN")||(obj[z_name]<min)){min=obj[z_name];};
   if((max=="NaN")||(obj[z_name]>max)){max=obj[z_name];};
  };
  if(typeof(model.element[plotOptions.plotFrom].age_depth)!="undefined")
  {
   for(i=0;i<model.element[plotOptions.plotFrom].age_depth.length;i++)
   {
    obj=makeDepthModelObject(model.element[plotOptions.plotFrom].age_depth[i]);
    if(obj)
    {
     if(i==0)
     {
      obj.sed=model.element[plotOptions.plotFrom].age_depth_rate[i];
     }
     else
     {
      if(i==model.element[plotOptions.plotFrom].age_depth.length-1)
      {
       obj.sed=model.element[plotOptions.plotFrom].age_depth_rate[i-1];
      }
      else
      {
       obj.sed=(model.element[plotOptions.plotFrom].age_depth_rate[i]
       	+model.element[plotOptions.plotFrom].age_depth_rate[i-1])/2;
      };
     };
     obj.sed=1/obj.sed;
     for(j=0;(j<plotOptions.showEnsembles)&&
     	(j<model.element[plotOptions.plotFrom].age_depth_ensembles.length);j++)
     {
      obj["ens_"+j]=model.element[plotOptions.plotFrom].age_depth_ensembles[j][i];
     };
     checkLims();depthModelData.push(obj);
    };
   };
  }
  else
  {
   /* for older model runs */
   for(i=0;i<model.group.length;i++)
   {
    if((model.group[i].parent==plotOptions.plotFrom)&&(model.group[i].depth_list))
    {
     obj=makeDepthModelObject(model.group[i].start);
     if(obj){checkLims();depthModelData.push(obj);};
     for(k=0;k<model.group[i].depth_list.length;k++)
     {
      obj=makeDepthModelObject(model.group[i].depth_list[k]);
      if(obj){checkLims();depthModelData.push(obj);};
     };
     obj=makeDepthModelObject(model.group[i].end);
     if(obj){checkLims();depthModelData.push(obj);};
    };
   };
   depthModelData[0].sed="NaN";
   for(i=1;i<depthModelData.length-1;i++)
   {
    if(depthModelData[i+1][t_name]!=depthModelData[i-1][t_name])
    {
     depthModelData[i].sed=Math.abs(
     	(depthModelData[i+1][z_name]-depthModelData[i-1][z_name])/
     	(depthModelData[i+1][t_name]-depthModelData[i-1][t_name]));
    }else{depthModelData[i].sed="NaN";};
   };
   depthModelData[i].sed="NaN";
   for(i=1;i<depthModelData.length;i++)
   {
    if(depthModelData[i].sed=="NaN"){depthModelData[i].sed=depthModelData[i-1].sed;};
   };
   for(i=depthModelData.length-2;i>=0;i--)
   {
    if(depthModelData[i].sed=="NaN"){depthModelData[i].sed=depthModelData[i+1].sed;};
   };
  };
  document.getElementById('from').value=min;
  document.getElementById('to').value=max;
  a=ocd[plotOptions.plotFrom].param.split(",");
  if(a.length>1){document.getElementById('incr').value=1/Number(a[1]);}
  else{document.getElementById('incr').value=Math.round((max-min)/100);};
 };
 function drawTable()
 {
  var units;
  var dist;
  var mult;
  var thisocd;
  var left_col,right_col,depthModel=false;
  var thiscalib;
  var i,j,k,d_m,z;
  var str;
  thisocd=ocd[plotOptions.plotFrom];
  if(!thisocd)
  {
   plotOptions.plotFrom=0;
   thisocd=ocd[plotOptions.plotFrom];
  };
  thiscalib=calib[plotOptions.plotFrom];
  if(thisocd)
  {
   if(model && model.group)
   {
    for(i=0;i<model.group.length;i++)
    {
     if((model.group[i].parent==plotOptions.plotFrom)&&(model.group[i].depth_list))
     {
      depthModel=true;
     };
    };
   };
   if(thiscalib)
   {
    left_col=plotOptions.showLikelihood;
    right_col=plotOptions.showPosterior;
   }
   else
   {
    left_col=true;
    right_col=true;
   };
   document.write("<h2>");
   if(thisocd.op)
   {
    document.write(thisocd.op+" ");
   };
   if(thisocd.name)
   {
    document.write(thisocd.name);
   };
   document.write("<\/h2>");
   if(thisocd.order)
   {
    document.writeln("<table class='param'>");
    document.write("<tr class='header'><th colspan="
     + (thisocd.order.param.length + 1).toString() + ">");
    switch(thisocd.op)
    {
    case "Order":
     document.write("Probability <i>t<\/i><sub>1<\/sub> &lt; <i>t<\/i><sub>2<\/sub>");
     parent.filecontent='"Order",';
	 break;
    case "Correl_Matrix":
     document.write("Pearson correlation coefficients");
     parent.filecontent='"Correlation",';
	 break;
    case "Covar_Matrix":
     document.write("Covariance");
     parent.filecontent='"Covariance",';
	 break;
    };
    document.writeln("<\/th><\/tr>");
    document.writeln("<tr class='header'><th rowspan=2><i>t<\/i><sub>1<\/sub><\/th><th colspan="
     + (thisocd.order.param.length).toString()
     + "><i>t<\/i><sub>2<\/sub><\/th><\/tr>");
    document.write("<tr class='header'>");
    for(i=0;i<thisocd.order.param.length;i++)
    {
     document.write("<th>"+thisocd.order.param[i]+"<\/th>");
     parent.filecontent+='"'+thisocd.order.param[i]+'",';
    };
    parent.filecontent+="\n";
    document.writeln("<\/tr>");
    for(j=0;j < thisocd.order.param.length;j++)
    {
     if(j % 2)
     {
      document.write("<tr class='odd'>");
     }
     else
     {
      document.write("<tr class='even'>");
     };
     document.write("<th>"+thisocd.order.param[j]+"<\/th>");
     parent.filecontent+='"'+thisocd.order.param[j]+'",';
     for(i=0;i<thisocd.order.param.length;i++)
     {
      document.write("<td>"+sigfig(thisocd.order.matrix[j][i],4)+"<\/td>");
      parent.filecontent+=sigfig(thisocd.order.matrix[j][i],4)+',';
     };
     document.writeln("<\/tr>");
     parent.filecontent+="\n";
    };
    document.writeln("<\/table>");
   }
   else
   {
    document.writeln("<table class='param'>");
    document.write("<tr class='header'>");
    if((left_col)&&thisocd.likelihood)
    {
     document.writeln("<th>Unmodelled Data<\/th>");
    };
    if(left_col&&thisocd.posterior)
    {
     document.writeln("<th>Modelled Data<\/th>");
    };
    document.writeln("<\/tr>");
    // text row
    document.writeln("<tr class='odd'>");
    if(left_col)
    {
     document.writeln("<td><pre>");
     if(thisocd.likelihood&&thisocd.likelihood.comment)
     {
      for(i=0;i<thisocd.likelihood.comment.length;i++)
      {
       document.writeln(parent.tidyText(thisocd.likelihood.comment[i]));
      };
     };
     document.writeln("<\/pre><\/td>");
    };
    if(left_col)
    {
     document.writeln("<td><pre>");
     if(thisocd.posterior&&thisocd.posterior.comment)
     {
      for(i=0;i<thisocd.posterior.comment.length;i++)
      {
       document.writeln(parent.tidyText(thisocd.posterior.comment[i]));
      };
     };
     document.writeln("<\/pre><\/td>");
    };
    document.writeln("<\/tr>");
    // error row
    document.writeln("<tr class='odd'>");
    if(left_col)
    {
     document.writeln("<td class=\"warning\"><pre>");
     if(thisocd.likelihood&&thisocd.likelihood.warning)
     {
      for(i=0;i<thisocd.likelihood.warning.length;i++)
      {
       document.writeln(thisocd.likelihood.warning[i]);
      };
     };
     document.writeln("<\/pre><\/td>");
    };
    if(left_col)
    {
     document.writeln("<td class=\"warning\"><pre>");
     if(thisocd.posterior&&thisocd.posterior.warning)
     {
      for(i=0;i<thisocd.posterior.warning.length;i++)
      {
       document.writeln(thisocd.posterior.warning[i]);
      };
     };
     document.writeln("<\/pre><\/td>");
    };
    document.writeln("<\/tr>");
    // depth model row
    if(depthModel)
    {
     document.writeln("<tr class='even'><td>");
     document.writeln("<h3>Proxy data \/ interpolation \/ sedimentation<\/h3>");
     document.writeln("<input type='button' onclick='doSaveStr(putProxyData(parseProxyData(document.getElementById(\"proxy_data\").value),\",\"),\"csv\")' value='Save'\/>");
     document.writeln("<input type='button' onclick='doPlotProxy()' value='Plot'\/>");
     document.writeln("<input type='button' onclick='getDates(true)' value='Get dates <<'\/>");
     document.writeln("<br\/><textarea id='proxy_data' rows=25 cols=40 class='compact'>");
     document.writeln("// Paste proxy data in here\n//   (tab delimited eg from spreadsheet).\n// First line should give fields including z for depth.\n//Alternatively use [<< Depths] button to interpolate\n//   and get sedimentation rate.");
     document.writeln("<\/textarea>");
     document.writeln("<\/td><td>");
     document.writeln("<h3>Depth model<\/h3>");
     document.writeln("<input type='button' onclick='doSaveStr(outputDepthModel(\",\"),\"csv\")' value='Save'\/>");
     document.writeln("<input type='button' onclick='doPlotDepth()' value='Plot'\/>");
     document.writeln("<br\/><textarea id='depth_model' rows=25 cols=40 class='compact'>");
     document.write(outputDepthModel('\t'));
     document.writeln("<\/textarea>");
     document.writeln("<\/td><\/tr>");
     document.writeln("<tr class='odd'><td>");
     document.writeln("<p>TS: <input type='text' id='ts_id' size=10 \/><\/p>");     
     document.writeln("<p><input type='button' onclick='putOnMap()' value='Map at:'\/>");     
     document.writeln("Lat: <input type='text' id='latitude' size=10 \/> ");     
     document.writeln("Long: <input type='text' id='longitude' size=10 \/><\/p>");     
     document.writeln("<\/td><td>");
     document.writeln("<p><input type='button' onclick='getDates(false)' value='<< Depths:'\/>");     
     document.writeln("From: <input type='text' id='from' size=5 \/> ");     
     document.writeln("To: <input type='text' id='to' size=5 \/> ");     
     document.writeln("Incr: <input type='text' id='incr' size=5 \/><\/p>");     
     document.writeln("<p>ID: <input type='text' id='base_id' size=10 \/><\/p>");     
     document.writeln("<\/td><\/tr>");
    };
    // raw data row
    document.writeln("<tr class='even'>");
    if(left_col)
    {
     document.writeln("<td>");
     if((thisocd.likelihood&&thisocd.likelihood.prob)||(thiscalib && thiscalib.rawbp))
     {
      if(thiscalib)
      {
       document.writeln("<p>Raw data<\/p>");
       document.writeln("<input type='button' onclick='doSave(\"likelihood\",\"14c\")' value='Save'\/>");
      }
      else
      {
       document.writeln("<input type='button' onclick='doSave(\"likelihood\",\"prior\")' value='Save'\/>");
      };
      document.writeln("<br\/><textarea id='likelihood' rows=25 cols=40 class='compact'>");
      if(thiscalib)
      {
       str="";
       for(i=0;i<thiscalib.rawbp.length;i++)
       {
        str+=thiscalib.rawcal[i]+"\t",thiscalib.rawbp[i]+"\t",thiscalib.rawsigma[i]+"\n";
       };
       document.write(str);
      }
      else
      {
       dist=thisocd.likelihood;
       if(dist.probNorm)
       {
        mult=dist.probNorm;
       }
       else
       {
        mult=1.0;
       };
       for(i=0;i<dist.prob.length;i++)
       {
        document.writeln(dist.start+i*dist.resolution,"\t",prob(dist.prob[i]*mult));
       };
      };
      document.writeln("<\/textarea>");
     };
     document.writeln("<\/td>");
    };
    if(left_col)
    {
     document.writeln("<td>");
     if((thisocd.posterior&&thisocd.posterior.prob)||(thiscalib))
     {
      if(thiscalib)
      {
       document.writeln("<p>Calibration curve</p>");
       document.writeln("<input type='button' onclick='doSave(\"posterior\",\"14c\")' value='Save'\/>");
      }
      else
      {
       document.writeln("<input type='button' onclick='doSave(\"posterior\",\"prior\")' value='Save'\/>");
      };
      document.writeln("<br\/><textarea id='posterior' rows=25 cols=40 class='compact'>");
      if(thiscalib)
      {
       str="";
       for(i=0;i < thiscalib.bp.length;i++)
       {
        str+=(thiscalib.start+i*thiscalib.resolution)+
        	"\t"+thiscalib.bp[i]+"\t"+thiscalib.sigma[i]+"\n";
       };
       document.write(str);
      }
      else
      {
       dist=thisocd.posterior;
       if(dist.probNorm)
       {
        mult=dist.probNorm;
       }
       else
       {
        mult=1.0;
       };
       for(i=0;i<dist.prob.length;i++)
       {
        document.writeln(dist.start+i*dist.resolution,"\t",prob(dist.prob[i]*mult));
       };
      };
      document.writeln("<\/textarea>");
     };
     document.writeln("<\/td>");
    };
    document.writeln("<\/tr>");
    document.writeln("<\/table>");
   };
  };
 };
 function parseProxyData(txt)
 {
  var dat,lines,items,i,j,txts,nos,k;
  checkIDs();
  dat=new Array();
  k=0;
  lines=txt.split("\n");
  for(i=0;i<lines.length;i++)
  {
   if(lines[i].slice(0,2)=="//"){continue;};
   items=lines[i].split("\t");
   txts=0;nos=0;
   for(j=0;j<items.length;j++)
   {
    if(items[j]!="")
    {
     if(isNaN(items[j])){txts++;}else{nos++;};
    };
   };
   if(((nos==0)&&(txts>1))||(i==0))
   {
    headers=items;
   }
   else
   {
    if(nos==0){continue;};
    dat[k]=new Object();
    for(j=0;j<items.length;j++)
    {
     if(!headers[j]){continue;};
     if((items[j]=="")||isNaN(items[j]))
     {
      switch(items[j])
      {
      case "true":
       dat[k][headers[j]]=true;
       break;
      case "false":
       dat[k][headers[j]]=false;
       break;
      default:
       dat[k][headers[j]]=items[j];
       break;
      };
     }
     else
     {
      dat[k][headers[j]]=Number(items[j]);
     };
    };
    k++;
   };
  };
  return dat;
 };
 function putProxyData(dta,sep)
 {
  var i,j,str="";
  if(!headers){return "";};
  if(!headers.length){return "";};
  for(j=0;j<headers.length;j++)
  {
   if(!headers[j]){continue;};
   if(sep==',')
   {
    str+='"'+headers[j]+'",';
   }
   else
   {
    str+=headers[j]+sep;
   };
  };
  str+="\n";
  for(i=0;i<dta.length;i++)
  {
   for(j=0;j<headers.length;j++)
   {
    if(!headers[j]){continue;};
    if((typeof(dta[i][headers[j]])=="string")&&(sep==','))
    {
     str+='"'+dta[i][headers[j]]+'",';
    }
    else
    {
     str+=dta[i][headers[j]]+sep;
    };
   };
   str+="\n";
  };
  return str;
 };
 function depthSort(a,b)
 {
  if(a[z_name]>b[z_name]){return 1;};
  if(a[z_name]<b[z_name]){return -1;};
  return 0;
 };
 function dateSort(a,b)
 {
  if(a[t_name]>b[t_name]){return 1;};
  if(a[t_name]<b[t_name]){return -1;};
  return 0;
 };
 function getDates(proxy)
 {
  var dta=[],i,j,k,l;
  var from,to,incr;
  var extr=[],vs;
  var grad={},gerr;
  var inta=new Array;
  checkIDs();
  extr=[t_name,dt_name];
  if(proxy)
  {
   dta=parseProxyData(document.getElementById("proxy_data").value);
  }
  else
  {
   headers=[z_name];
   from=Number(document.getElementById("from").value);
   to=Number(document.getElementById("to").value);
   incr=Number(document.getElementById("incr").value);
   j=0;
   for(i=from;(i<=to)&&(incr)&&((to-from)/incr>0);i+=incr)
   {
    dta[j]={};dta[j][z_name]=i;j++;
   };
  };
  vs=[t_name];
  if(plotOptions.showMedian){vs.push('median');};
  if(depthModelData.length && typeof(depthModelData[0].sed != 'undefined'))
  {
   vs.push('sed');
   for(j=0;(j<plotOptions.showEnsembles)&&(typeof(depthModelData[0]["ens_"+j]) != 'undefined');j++)
   {
    vs.push('ens_'+j);
   };
  };

  for(j=1;j<4;j++)
  {
   if(plotOptions.showRange[j])
   {
    vs.push("from_"+sigProbs[j]);
    vs.push("to_"+sigProbs[j]);
   };
  };
  function findGrad(v,l,k){return (inta[l][v]-inta[k][v])/(inta[l][z_name]-inta[k][z_name]);};
  function findGrads(l,k){let v; for(v=0;v<vs.length;v++){grad[vs[v]]=findGrad(vs[v],l,k);};};
  function linearExtrap(v,i,k)
  {
   if(v=="sed" && (inta[i][z_name]!=inta[k][z_name])){return Math.abs(1/grad[t_name]);};
   return inta[k][v]+(inta[i][z_name]-inta[k][z_name])*grad[v];
  };
  function linearExtraps(i,k)
  {
   let v;
   for(v=0;v<vs.length;v++){inta[i][vs[v]]=linearExtrap(vs[v],i,k);};
  };
  function linearInterps(i,j,k)
  {
   let v;
   var f=(inta[i][z_name]-inta[k][z_name])/(inta[j][z_name]-inta[k][z_name]);
   for(v=0;v<vs.length;v++)
   {
    inta[i][vs[v]]=(1-f)*inta[k][vs[v]]+f*inta[j][vs[v]];
   };
   inta[i][dt_name]=(1-f)*inta[k][dt_name]+f*inta[j][dt_name];
  };
  if(headers[0]!=t_name)
  {
   if(plotOptions.showMean){headers.push('mu');};
   if(plotOptions.showSigma){headers.push('sigma');};
   headers=extr.concat(headers).concat(vs.slice(1));
  };
  k=0;
  for(i=0;i<dta.length;i++)
  {
   if(typeof(dta[i][z_name])=='undefined'){continue;};
   inta[k]=dta[i];
   k++;
  };
  for(i=0;i<depthModelData.length;i++)
  {
   if(typeof(depthModelData[i][z_name])=='undefined'){continue;};
   inta[k]=depthModelData[i];
   k++;
  };
  if(inta.length<1){alert("No data!");return;};
  inta.sort(depthSort);
  for(k=0;(k<inta.length)&&(typeof(inta[k][t_name])=='undefined');k++);
   // finds next date model item
  for(l=inta.length-1;(l>=0)&&(typeof(inta[l][t_name])=='undefined');l--);
   //finds last date model item
  if(l<=k){alert("No model!");return;};
  findGrads(l,k);
  gerr=Math.sqrt(sqr(inta[l][dt_name])+sqr(inta[k][dt_name]))/(inta[l][z_name]-inta[k][z_name]);
  if(k!=0)
  {
   if(k>1){alert("Interpolating to z="+inta[k][z_name]);};
   for(i=0;i<k;i++)
   {
    linearExtraps(i,k);
	inta[i][dt_name]=
		Math.sqrt(sqr(inta[k][dt_name])+sqr((inta[i][z_name]-inta[k][z_name])*gerr));
   };
  };
  if(l!=inta.length-1)
  {
   alert("Interpolating from z="+inta[l][z_name]);
   for(i=l;i<inta.length;i++)
   {
    linearExtraps(i,l);
    inta[i][dt_name]=
    	Math.sqrt(sqr(inta[l][dt_name])+sqr((inta[i][z_name]-inta[l][z_name])*gerr));
   };
  };
  i=k+1;
  while(i<l)
  {
   for(j=i;(j<l)&&(typeof(inta[j][t_name])=='undefined');j++);
    // finds next date model item
   for(;i<j;i++)
   {
    linearInterps(i,j,k);
   };
   k=j;i=k+1; // new starting position   
  };
  for(i=0;i<dta.length;i++)
  {
   if(typeof(dta[i][t_name])=='undefined'){continue;};
   dta[i][t_name]=dta[i][t_name].toFixed(1);
   dta[i][dt_name]=dta[i][dt_name].toFixed(1);
   if(plotOptions.showMean){dta[i].mu=showDateT(dta[i][t_name],"date",0);};
   if(plotOptions.showSigma){dta[i].sigma=showDateT(dta[i][dt_name],"interval",0);};
   if(plotOptions.showMedian){dta[i].median=showDateT(dta[i].median,"date",0);};
   dta[i].sed=dta[i].sed.toPrecision(3);
   for(j=0;(j<plotOptions.showEnsembles)&&(typeof(dta[i]["ens_"+j])!='undefined');j++)
   {
    dta[i]["ens_"+j]=showDateT(dta[i]["ens_"+j],"date",0);
   };
   for(j=1;j<4;j++)
   {
    if(plotOptions.showRange[j])
    {
     dta[i]["from_"+sigProbs[j]]=dta[i]["from_"+sigProbs[j]].toFixed(0);
     dta[i]["to_"+sigProbs[j]]=dta[i]["to_"+sigProbs[j]].toFixed(0);
    };
   };
  };
  document.getElementById("proxy_data").value=putProxyData(dta,"\t");
 };
 function doPlotProxy()
 {
  var i,id,name,ts_id;
  id=document.getElementById("base_id").value;
  ts_id=document.getElementById("ts_id").value;
  if(ocd[plotOptions.plotFrom].name)
  {
   name=ocd[plotOptions.plotFrom].name+" Proxies";
  }
  else
  {
   name="Proxies";
  };
  var items=new Array;
  for(i=0;i<headers.length;i++)
  {
   switch(headers[i])
   {
   case t_name:
   case dt_name:
   case z_name:
   case "median":
   case "mu":
   case "sigma":
   case '':
    break;
   default:
    if((headers[i].indexOf("from_")==0)||(headers[i].indexOf("to_")==0)||
    	(headers[i].indexOf("ens_")==0)){break;};
    items.push(headers[i]);
    break;
   };
  };
  parent.plotOnPlot(parseProxyData(document.getElementById("proxy_data").value),items,name,id,ts_id);
 };
 function doPlotDepth()
 {
  var i,id,name,ts_id;
  id=document.getElementById("base_id").value;
  ts_id=document.getElementById("ts_id").value;
  if(ocd[plotOptions.plotFrom].data)
  {
   if(ocd[plotOptions.plotFrom].data.id)
   {
    id=ocd[plotOptions.plotFrom].data.id;
   }
  };
  if(ocd[plotOptions.plotFrom].name)
  {
   name=ocd[plotOptions.plotFrom].name+" Age-Depth";
  }
  else
  {
   name="";
  };
  parent.plotOnPlot(depthModelData,[],name,id,ts_id);
 };
 function putOnMap()
 {
  var i,j,id,name,dta;
  checkIDs();
  if(!(document.getElementById("longitude").value || document.getElementById("latitude").value))
  {
   return;
  };
  plotOptions.player_proxies=new Array();
  id=document.getElementById("base_id").value;
  if(ocd[plotOptions.plotFrom].name)
  {
   name=ocd[plotOptions.plotFrom].name+" Location";
  }
  else
  {
   name="";
  };
  for(i=0;i<headers.length;i++)
  {
   switch(headers[i])
   {
   case t_name:
   case dt_name:
   case z_name:
   case "median":
   case "mu":
   case "sigma":
   case '':
    break;
   default:
    if((headers[i].indexOf("from_")==0)||(headers[i].indexOf("to_")==0)||
    	(headers[i].indexOf("ens_")==0)){break;};
    for(j=0;j<plotOptions.player_proxies.length;j++)
    {
     if(plotOptions.player_proxies[j]==headers[i]){break;};
    };
    if(j==plotOptions.player_proxies.length)
    {
     plotOptions.player_proxies.push(headers[i]);
    };
    break;
   };
  };
  plotOptions.player_proxies.sort();
  dta=[{name:name,id:id+"_location",line:"",markerColor:"rgb(0,0,0)",
     markerFill:"rgba(0,0,0,0.5)",marker:"circle",selected:true,
     include_id:true,include_url:true,include_color:true,include_marker:true},
     {name:name.replace(" Location"," Proxies"),
     id:id+"_proxy",line:"solid",
     lineColor:"rgba(0,0,0,1)",markerColor:"",markerFill:"rgba(0,0,0,0.2)",
     marker:"",selected:true}];
  dta[0].data=[{id:id+"_loc",longitude:
     Number(document.getElementById("longitude").value),
     latitude:Number(document.getElementById("latitude").value)}];
  dta[1].data=parseProxyData(document.getElementById("proxy_data").value).sort(dateSort);
  if(plotOptions.player_proxies.length>0)
  {
   plotOptions.player_proxy=plotOptions.player_proxies[1];
  };
  plotOptions.player_timescale=t_name;
  plotOptions.player_min="NaN";plotOptions.player_max="NaN";
  for(i=0;i<dta[1].data.length;i++)
  {
   if(isNaN(plotOptions.player_min)||
    (dta[1].data[i][plotOptions.player_timescale]<plotOptions.player_min))
   {
    plotOptions.player_min=dta[1].data[i][plotOptions.player_timescale];
   };
   if(isNaN(plotOptions.player_max)||
    (dta[1].data[i][plotOptions.player_timescale]>plotOptions.player_max))
   {
    plotOptions.player_max=dta[1].data[i][plotOptions.player_timescale];
   };
  };
  if(isNaN(plotOptions.player_min))
  {
   plotOptions.player_min=0;
   plotOptions.player_max=100;
   plotOptions.mapPlotMultiIncr=100;
  }
  else
  {
   if((plotOptions.player_max-plotOptions.player_min)>0)
   {
    plotOptions.mapPlotMultiIncr=Math.round((plotOptions.player_max-plotOptions.player_min)/12.5);
   };
  };
  parent.plotData=parent.duplRemoteItem(parent.right,dta);
  parent.left.prepareMap(false);
 };
 function initialise()
 {
  if(!document.getElementById("ts_id")){return;};
  if(ocd[plotOptions.plotFrom].data && ocd[plotOptions.plotFrom].data.timescale)
  {
   document.getElementById("ts_id").value=
   	ocd[plotOptions.plotFrom].data.timescale.replace(/[^a-zA-Z0-9]/g,"_");
  }
  else
  {
   document.getElementById("ts_id").value=
   	model.element[ocd[0].calib].name.replace(/[^a-zA-Z0-9]/g,"_");
  };
  if(ocd[plotOptions.plotFrom].data && ocd[plotOptions.plotFrom].data.id)
  {
   document.getElementById("base_id").value=
    ocd[plotOptions.plotFrom].data.id.replace(/[^a-zA-Z0-9]/g,"_");
  }
  else
  {
   if(ocd[plotOptions.plotFrom].name)
   {
    document.getElementById("base_id").value=
     ocd[plotOptions.plotFrom].name.replace(/[^a-zA-Z0-9]/g,"_");
   }
   else
   {
    document.getElementById("base_id").value="ocd_"+plotOptions.plotFrom;
   };
  };
  if(ocd[plotOptions.plotFrom].data && ocd[plotOptions.plotFrom].data.longitude && ocd[plotOptions.plotFrom].data.latitude)
  {
   document.getElementById("longitude").value=ocd[plotOptions.plotFrom].data.longitude;
   document.getElementById("latitude").value=ocd[plotOptions.plotFrom].data.latitude;
  };
  checkIDs();
  headers=[z_name,t_name,dt_name];
  makeDepthModelArray();
 };
