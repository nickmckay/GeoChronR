 var ocd=parent.ocd;
 var calib=parent.calib;
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
  parent.filecontent=exportstring(',','"','\n');
  parent.filename=parent.source.replace(".js","");
  parent.saveFileAs(ext);
 };
 function drawTable()
 {
  document.writeln("<input type='button' onclick='doSave(\"data\",\"csv\")' value='Save'\/>");
  document.writeln("<br\/><textarea id='data' rows=25 cols=80 class='compact'>");
  document.writeln("<\/textarea>");
 };
 function exportstring(sep,quote,eol)
 {
  var dist;
  var thisocd;
  var i,j;
  var str,datastr;
  datastr=quote+"index"+quote+sep+quote+"op"+quote+sep+quote+"name"+quote+sep+quote+"z"+quote+sep+quote+"type"+quote+sep+quote+"value"+quote+sep+quote+"probability"+quote+eol;
  for(j=0;j<ocd.length;j++)
  {
   if(calib[j]){continue;};
   thisocd=ocd[j];
   if(!thisocd){continue;};
   if(thisocd.order){continue;};
   str=j+sep+quote;
   if(thisocd.op){str+=thisocd.op;};
   str+=quote+sep+quote;
   if(thisocd.name){str+=thisocd.name;};
   str+=quote+sep;
   if(thisocd.data && (typeof(thisocd.data.z)!='undefined'))
   {
    str+=thisocd.data.z;
   };
   str+=sep;
   if(thisocd.likelihood)
   {
    dist=thisocd.likelihood;
    if(dist && dist.prob)
    {
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
      datastr+=str+quote+"likelihood"+quote+sep+(dist.start+i*dist.resolution)
       +sep+prob(dist.prob[i]*mult)+eol;
     };
    };
   };
   if(thisocd.posterior)
   {
    dist=thisocd.posterior;
    if(dist && dist.prob)
    {
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
      datastr+=str+quote+"posterior"+quote+sep+(dist.start+i*dist.resolution)
       +sep+prob(dist.prob[i]*mult)+eol;
     };
    };
   };
  };
  return datastr;
 };
 function initialise()
 {
  document.getElementById("data").value=exportstring('\t','','\n');
 };
