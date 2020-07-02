var commandsRef=new Object();
var commandsIndex=new Array();
var commands=new Array(
 "After,[Name],[Expression],{",
 "Age,[Name],Expression",
 "Axis,From,To",
 "Before,[Name],[Expression],{",
 "Boundary,[Name],[Expression]",
 "C_Date,[Name],Cal Date,Uncertainty",
 "C_Combine,[Name],[Systematic],{",
 "C_Simulate,[Name],Cal Date,Uncertainty",
 "Color,color,=",
 "Combine,[Name],{",
 "Comment,Text",
 "Correl_Matrix,[Name],{",
 "Correlation,Name,Parameter1,Parameter2",
 "Covar_Matrix,[Name],{",
 "Curve,Name,Filename",
 "Date,[Name],Expression",
 "Delta_R,[Name],Shift/Expression,[Uncertainty]",
 "D_Sequence,[Name],{",
 "Difference,Name,Parameter1,Parameter2,[Expression]",
 "End,[Name],[Expression],@",
 "Exp,[Name],Tau,From,To",
 "First,[Name],[Expression]",
 "Gap,Gap,[Uncertainty]",
 "Interval,[Name],[Expression]",
 "KDE_Model,[Name],[Kernel],[Factor],{",
 "KDE_Plot,[Name],[Kernel],[Factor],{",
 "Label,Text",
 "Last,[Name],[Expression]",
 "Latitude,latitude,=",
 "Longitude,longitude,=",
 "Line",
 "LnN,Name,mu,sigma,[Resolution]",
 "MCMC_Sample,[Name],[Interval],[Max],{",
 "Mix_Curves,Name,Curve1,Curve2,%Curve2/Expression,[Uncertainty]",
 "N,Name,mu,sigma,[Resolution]",
 "Number,Name,Expression",
 "Order,[Name],{",
 "Offset,Shift,Uncertainty,@",
 "Outlier,[Name],[Probability],@",
 "Outlier_Model,[Name],Distribution,[Magnitude],[Type (t/r/s)]",
 "P,Variable,From,To,Expression,[Resolution]",
 "P_Sequence,[Name],k0,[Interpolation],[log10(k/k0) expression],{",
 "Page",
 "Phase,[Name],{",
 "Plot,[Name],{",
 "Pois,[Name],Mean,[Scale]",
 "Position,z,=",
 "Prior,Name,[Filename]",
 "Probability,[Name],At,Distribution",
 "R_Date,[Name],14C Date,Uncertainty",
 "R_Combine,[Name],[Systematic],{",
 "R_F14C,[Name],F14C,Uncertainty",
 "R_Simulate,[Name],Cal Date,Uncertainty",
 "Reservoir,Tau,Uncertainty,@",
 "Sample,[Name],Distribution",
 "Sapwood,[Name],Hw/Sw date,Hw rings,Sw rings,MRW",
 "Sapwood_Model,Name,a,br,bm,&sigma;",
 "Sequence,[Name],{",
 "Sigma_Boundary,[Name],[Expression]",
 "Shift,Name,Parameter1,Parameter2,[Expression]",
 "Span,[Name],[Expression]",
 "Start,[Name],[Expression],@",
 "Sum,[Name],{",
 "T,[Name],Freedom,[Scale],[Resolution]",
 "Tau_Boundary,[Name],[Expression]",
 "Top_Hat,[Name],Centre,HalfWidth",
 "Transition,[Name],[Expression],@",
 "U,[Name],From,To,[Resolution]",
 "U_Sequence,[Name],[Interpolation],{",
 "V_Sequence,[Name],{",
 "Year,Year,=",
 "Zero_Boundary,[Name],[Expression]");

function parseCommands()
{
 var i,j;
 var c;
 for(i=0;i<commands.length;i++)
 {
  c=commands[i].split(",");
  commandsIndex[i]=c[0];
  commandsRef[commandsIndex[i]]=new Object();
  commandsRef[commandsIndex[i]].param=new Array();
  commandsRef[commandsIndex[i]].type="simple";
  commandsRef[commandsIndex[i]].expanded=false;
  for(j=1;j<c.length;j++)
  {
   switch(c[j])
   {
   case "{":
    commandsRef[commandsIndex[i]].type="group";
    break;
   case "@":
    commandsRef[commandsIndex[i]].type="modifier";
    break;
   case "=":
    commandsRef[commandsIndex[i]].type="attribute";
    break;
   default:
    commandsRef[commandsIndex[i]].param[j-1]=c[j];
    break;
   };
  };
 };
};

function commandSyntax(d,name,end,old)
{
 var r;
 var i;
 r=commandsRef[name];
 if(r)
 {
  if(r.type=="attribute")
  {
   d.write("<span class='quote'>"+r.param[0]+
            "=<\/span><span class='expression'>value<\/span>");
  }
  else
  {
   if(old)
   {
    d.write("<span class='quote'>"+name+" <\/span>");
   }
   else
   {
    d.write("<span class='quote'>"+name+"(<\/span>");
   };
   for(i=0;i<r.param.length;i++)
   {
    if(i>0)
    {
     if(old)
     {
      d.write(" ");
     }
     else
     {
      d.write("<span class='quote'>, <\/span>");
     };
    };
    d.write("<span class='expression'>"+r.param[i]+"<\/span>");
   };
   if(!old)
   {
    d.write("<span class='quote'>)<\/span>");
   };
   if(r.type=="group")
   {
    d.write("<span class='quote'>{ &hellip; }<\/span>");
   };
  };
 };
 d.writeln("<span class='quote'>"+end+"<\/span>");
};
