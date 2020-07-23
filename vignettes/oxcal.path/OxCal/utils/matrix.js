function matrix(rows,columns,content,weights)
{
 var i,j,k;
 this.rows=rows;
 this.columns=columns;
 this.weights=false;
 if(weights && weights.length)
 {
  if(weights.length==rows)
  {
   this.weights=[];
   this.sumWeights=0;
   for(i=0;i<weights.length;i++){this.sumWeights+=Number(weights[i]);};
   for(i=0;i<weights.length;i++){this.weights[i]=Number(weights[i])/this.sumWeights;};
  }
  else
  {
   alert("weights length does not agree with data");
  };
 };
 if(content)
 {
  if(content.length)
  {
   if(content[0].length)
   {
    this.m=content;
   }
   else
   {
    this.m=new Array();
    k=0;
    for(i=0;i<this.rows;i++)
    {
     this.m[i]=new Array();
     for(j=0;j<this.columns;j++)
     {
      if(k<content.length)
      {
       this.m[i][j]=Number(content[k]);
      }
      else
      {
       this.m[i][j]=0;
      };
      k++;
     };
    }; 
   };
  };
 }
 else
 {
  this.m=new Array();
  for(i=0;i<this.rows;i++)
  {
   this.m[i]=new Array();
   for(j=0;j<this.columns;j++)
   {
    this.m[i][j]=0;
   };
  };
 };
};
new matrix(0,0);
matrix.prototype.equals=function(a)
{
 var i,j;
 if(typeof(a.rows)!='undefined')
 {
  this.rows=a.rows;
  this.columns=a.columns;
  if(this.rows && this.columns)
  {
   for(i=0;i<this.rows;i++)
   {
    this.m[i][j]=a.m[i][j];
   };
  };
  return this;
 }
 else
 {
  if(a.length)
  {
   this.rows=a.length;
   this.columns=a[0].length;
   this.m=new Array();
   if(this.rows && this.columns)
   {
    for(i=0;i<this.rows;i++)
    {
     this.m[i]=new Array();
     for(j=0;j<this.columns;j++)
     {
      if(isNaN(a[i][j]))
      {
       this.m[i][j]=0;
      }
      else
      {
       this.m[i][j]=Number(a[i][j]);
      };
     };
    };
   };
   return this;
  };
 };
 return false;
};
matrix.prototype.times=function(a,b)
{
 var i,j,k;
 var c;
 if(b){c=this;}else{b=this;c=new matrix(a.rows,b.columns);};
 c.rows=a.rows;
 c.columns=b.columns;
 c.m=new Array();
 for(i=0;i<c.rows;i++)
 {
  c.m[i]=new Array();
  for(j=0;j<c.columns;j++)
  {
   c.m[i][j]=0;
   for(k=0;(k<a.columns)&&(k<b.rows);k++)
   {
    c.m[i][j]+=a.m[i][k]*b.m[k][j];
   };
  };
 };
 return c;
};
matrix.prototype.transpose=function(a)
{
 var i,j;
 var b;
 if(a){b=this;}else{a=this;b=new matrix(a.columns,a.rows);};
 b.rows=a.columns;
 b.columns=a.rows;
 b.m=new Array();
 for(i=0;i<b.rows;i++)
 {
  b.m[i]=new Array();
  for(j=0;j<b.columns;j++)
  {
   b.m[i][j]=a.m[j][i];
  };
 };
 return b;
};
matrix.prototype.array=function()
{
 var i,j,k;
 var a=new Array();
 k=0;
 for(i=0;i<this.rows;i++)
 {
  for(j=0;j<this.columns;j++)
  {
   a[k]=this.m[i][j];
   k++;
  };
 };
 return a;
};
matrix.prototype.identity=function()
{
 var i,j;
 this.m=new Array();
 for(i=0;i<this.rows;i++)
 {
  this.m[i]=new Array();
  for(j=0;j<this.columns;j++)
  {
   if(i==j)
   {
    this.m[i][j]=1;
   }
   else
   {
    this.m[i][j]=0;
   };
  };
 };
 return this;
};
matrix.prototype.output=function(dp,csv)
{
 var t,b,r,d,i,j;
 t="";
 if(this.colTitle)
 {
  if((this.rowNames && (this.rowNames.length==this.rows))||(this.rowTitle))
  {
   if(this.rowTitle && !(this.colNames && (this.colNames.length==this.columns)))
   {
    if(csv){t+='"'+this.rowTitle+'"';}else{t+=this.rowTitle;};
   };
   if(csv){t+=',';}else{t+='\t';};
  };
  if(csv){t+='"'+this.colTitle+'"';}else{t+=this.colTitle;};
  t+='\n';
 };
 if(this.colNames && (this.colNames.length==this.columns))
 {
  if(this.rowNames && (this.rowNames.length==this.rows))
  {
   if(this.rowTitle)
   {
    if(csv){t+='"'+this.rowTitle+'"';}else{t+=this.rowTitle;};
   };
   if(csv){t+=',';}else{t+='\t';};
  };
  for(j=0;j<this.columns;j++)
  {
   if(csv){t+='"'+this.colNames[j]+'"';}else{t+=this.colNames[j];};
   if(csv){t+=',';}else{t+='\t';};
  };
  t+='\n';
 };
 for(i=0;i<this.rows;i++)
 {
  if(this.rowNames && (this.rowNames.length==this.rows))
  {
   if(csv){t+='"'+this.rowNames[i]+'"';}else{t+=this.rowNames[i];};
   if(csv){t+=',';}else{t+='\t';};
  };
  for(j=0;j<this.columns;j++)
  {
   if(!isNaN(dp) && (dp!==false))
   {
    t+=Number(this.m[i][j]).toFixed(dp);
   }
   else
   {
    t+=Number(this.m[i][j]);
   };
   if(csv){t+=',';}else{t+='\t';};
  };
  t+='\n';
 };
 if(this.eigenvalues && this.rowNames && (this.rowNames.length==this.rows))
 {
  if(csv){t+='"lambda"';}else{t+="lambda";};
  if(csv){t+=',';}else{t+='\t';};
  for(j=0;j<this.eigenvalues.length;j++)
  {
   if(!isNaN(dp) && (dp!==false))
   {
    t+=Number(this.eigenvalues[j]).toFixed(dp);
   }
   else
   {
    t+=Number(this.eigenvalues[j]);
   };
   if(csv){t+=',';}else{t+='\t';};
  };
  t+='\n';
 };
 return t;
};
matrix.prototype.display=function(dp,classname)
{
 var t,b,r,d,i,j;
 t=document.createElement("TABLE");
 if(classname){t.setAttributeNS(null,"class",classname);};
 b=document.createElement("TBODY");
 if(this.colTitle)
 {
  r=document.createElement("TR");
  if((this.rowNames && (this.rowNames.length==this.rows))||(this.rowTitle))
  {
   d=document.createElement("TH");
   if(this.rowTitle && !(this.colNames && (this.colNames.length==this.columns)))
   {
    d.appendChild(document.createTextNode(this.rowTitle));
   };
   r.appendChild(d);   
  };
  d=document.createElement("TH");
  d.appendChild(document.createTextNode(this.colTitle));
  d.colSpan=this.columns;
  r.appendChild(d);   
  b.appendChild(r);
 };
 if(this.colNames && (this.colNames.length==this.columns))
 {
  r=document.createElement("TR");
  if(this.rowNames && (this.rowNames.length==this.rows))
  {
   d=document.createElement("TH");
   if(this.rowTitle)
   {
    d.appendChild(document.createTextNode(this.rowTitle));
   };
   r.appendChild(d);   
  };
  for(j=0;j<this.columns;j++)
  {
   d=document.createElement("TH");
   d.appendChild(document.createTextNode(this.colNames[j]));
   r.appendChild(d);   
  };
  b.appendChild(r);
 };
 for(i=0;i<this.rows;i++)
 {
  r=document.createElement("TR");
  if(this.rowNames && (this.rowNames.length==this.rows))
  {
   d=document.createElement("TH");
   d.appendChild(document.createTextNode(this.rowNames[i]));
   r.appendChild(d);   
  };
  for(j=0;j<this.columns;j++)
  {
   d=document.createElement("TD");
   if(!isNaN(dp) && (dp!==false))
   {
    d.appendChild(document.createTextNode(Number(this.m[i][j]).toFixed(dp)));
   }
   else
   {
    d.appendChild(document.createTextNode(Number(this.m[i][j])));
   };
   r.appendChild(d);
  };
  b.appendChild(r);
 };
 if(this.eigenvalues && this.rowNames && (this.rowNames.length==this.rows))
 {
  r=document.createElement("TR");
  d=document.createElement("TH");
  d.appendChild(document.createTextNode("\u03BB"));
  r.appendChild(d);   
  for(j=0;j<this.eigenvalues.length;j++)
  {
   d=document.createElement("TD");
   if(!isNaN(dp) && (dp!==false))
   {
    d.appendChild(document.createTextNode(Number(this.eigenvalues[j]).toFixed(dp)));
   }
   else
   {
    d.appendChild(document.createTextNode(Number(this.eigenvalues[j])));
   };
   r.appendChild(d);
  };
  b.appendChild(r);
 };
 t.appendChild(b);
 return t;
};
// finds the eigenvectors of square matrices using Jacobi Cyclic method
// eigenvalues stored in special eigenvalues property.
matrix.prototype.eigenvectors=function(aa)
{
 var row,i,j,k,m;
 var pAk,pAm,pr,pe;
 var threshold_norm;
 var threshold;
 var tan_phi, sin_phi, cos_phi, tan2_phi, sin2_phi, cos2_phi;
 var sin_2phi, cos_2phi, cot_2phi;
 var dum1;
 var dum2;
 var dum3;
 var r;
 var max;

 // section to set up javascript matrix of eigenvectors
 var b;
 if(aa){b=this;}else{aa=this;b=new matrix(this.rows,this.columns);};
 var A=aa.array();
 var n=aa.rows;
 b.rows=n;
 b.columns=n;
          // Initialize the eigenvectors to the identity matrix.
 b.identity();
 b.eigenvalues=new Array(); // special for this method
 if(aa.rows != aa.columns){return false;};
 
 // Take care of trivial cases

 if(n<1){return false;};
 if(n==1)
 {
  b.eigenvalues[0]=aa.m[0][0];
  b.m[0][0]=1.0;
  return b;
 };
 for(threshold=0,i=0;i<(n-1);i++)
 {
  for(j=i+1;j<n;j++)
  {
   threshold+=aa.m[i][j]*aa.m[i][j];
  };
 };
 threshold=Math.sqrt(threshold+threshold);
 threshold_norm=threshold * 1.11e-16;
 max=threshold+1.0;
 while(threshold > threshold_norm)
 {
  threshold /= 10.0;
  if(max < threshold){continue;};
  max=0;
  for(pAk=0,k=0;k<(n-1);pAk+=n,k++)
  {
   for(pAm=pAk+n, m=k+1;m<n;pAm +=n, m++)
   {
    if(Math.abs(A[pAk+m])<threshold){continue;};
       // Calculate the sin and cos of the rotation angle which
       // annihilates A[k][m].
    cot_2phi = 0.5 * ( A[pAk + k] - A[pAm + m] ) / A[pAk + m];
    dum1 = Math.sqrt( cot_2phi * cot_2phi + 1.0);
    if(cot_2phi < 0.0){ dum1 = -dum1; };
    tan_phi = -cot_2phi + dum1;
    tan2_phi = tan_phi * tan_phi;
    sin2_phi = tan2_phi / (1.0 + tan2_phi);
    cos2_phi = 1.0 - sin2_phi;
    sin_phi = Math.sqrt(sin2_phi);
    if (tan_phi < 0.0) sin_phi = - sin_phi;
    cos_phi = Math.sqrt(cos2_phi); 
    sin_2phi = 2.0 * sin_phi * cos_phi;
    cos_2phi = cos2_phi - sin2_phi;
       // Rotate columns k and m for both the matrix A 
       //     and the matrix of eigenvectors.

    p_r = 0;
    dum1 = A[pAk + k];
    dum2 = A[pAm + m];
    dum3 = A[pAk + m];
    A[pAk + k] = dum1 * cos2_phi + dum2 * sin2_phi + dum3 * sin_2phi;
    A[pAm + m] = dum1 * sin2_phi + dum2 * cos2_phi - dum3 * sin_2phi;
    A[pAk + m] = 0.0;
    A[pAm + k] = 0.0;
    for(i = 0;i < n;p_r += n,i++)
    {
     if((i == k)||(i == m)){continue;};
     if(i < k){dum1 = A[p_r + k];}else{ dum1 = A[pAk + i];};
     if( i < m ){dum2 = A[p_r + m];}else{ dum2 = A[pAm + i];};
     dum3 = dum1 * cos_phi + dum2 * sin_phi;
     if( i < k ){ A[p_r + k] = dum3;}else{ A[pAk + i] = dum3;};
     dum3 = - dum1 * sin_phi + dum2 * cos_phi;
     if( i < m ){ A[p_r + m] = dum3;}else{ A[pAm + i] = dum3;};
    };
    for(i = 0;i < n;i++) 
    {
     dum1 = b.m[i][k];
     dum2 = b.m[i][m];
     b.m[i][k] = dum1 * cos_phi + dum2 * sin_phi;
     b.m[i][m] = - dum1 * sin_phi + dum2 * cos_phi;
    };
   };
   for(i = 0; i < n; i++)
   {
    if(i == k)
    {
     continue;
    }
    else
    {
     if ( max < Math.abs(A[pAk + i])){ max = Math.abs(A[pAk + i]);};
    };
   };
  };
 };
 for(pAk = 0,k = 0;k < n; pAk += n, k++)
 {
  b.eigenvalues[k] = A[pAk + k];
 };
 return b;
};
function ev_sorter(a,b)
{
 return (Math.abs(b.val)-Math.abs(a.val));
};
matrix.prototype.sort_eigenvalues=function(a)
{
 var b,ord,i,j;
 if(a){b=this;}else{a=this;b=new matrix(this.rows,this.columns);};
 ord=new Array();
 for(i=0;i<a.eigenvalues.length;i++)
 {
  ord[i]=new Object();
  ord[i].val=a.eigenvalues[i];
  ord[i].ind=i;
 };
 ord.sort(ev_sorter);
 b.eigenvalues=new Array();
 for(i=0;i<a.eigenvalues.length;i++)
 {
  b.eigenvalues[i]=a.eigenvalues[ord[i].ind];
 };
 for(i=0;i<a.rows;i++)
 {
  b.m[i]=new Array();
  for(j=0;j<a.columns;j++)
  {
   b.m[i][j]=a.m[i][ord[j].ind];
  };
 };
 return b;
};
matrix.prototype.subtract_mean=function(a)
{
 var b,i,j;
 if(a){b=this;}else{a=this;b=new matrix(this.rows,this.columns,false,this.weights);};
 b.mean=new Array();
 for(j=0;j<a.columns;j++)
 {
  b.mean[j]=0;
 };
 for(i=0;i<a.rows;i++)
 {
  for(j=0;j<a.columns;j++)
  {
   if(this.weights)
   {
    b.mean[j]+=a.m[i][j]*this.weights[i];
   }
   else
   {
    b.mean[j]+=a.m[i][j];
   };
  };
 };
 if(!this.weights)
 {
  for(j=0;j<a.columns;j++)
  {
   b.mean[j]/=a.rows;
  };
 };
 b.rows=a.rows;
 b.columns=a.columns;
 for(i=0;i<a.rows;i++)
 {
  for(j=0;j<a.columns;j++)
  {
   b.m[i][j]=a.m[i][j]-b.mean[j];
  };
 };
 return b;
};
matrix.prototype.normalise=function(a)
{
 var b,i,j,sws,v;
 if(a){b=this;}else{a=this;b=new matrix(this.rows,this.columns,false,this.weights);};
 b.mean=new Array();
 b.std=new Array();
 for(j=0;j<a.columns;j++)
 {
  b.mean[j]=0;
  b.std[j]=0;
 };
 if(this.weights)
 {
  sws=0;
  for(i=0;i<a.rows;i++)
  {
   for(j=0;j<a.columns;j++)
   {
    b.mean[j]+=a.m[i][j]*this.weights[i];
    b.std[j]+=a.m[i][j]*a.m[i][j]*this.weights[i];
   };
   sws+=this.weights[i]*this.weights[i];
  };
  for(i=0;i<a.rows;i++)
  {
   for(j=0;j<a.columns;j++)
   {
    v=a.m[i][j]=b.mean[j];
    b.std[j]+=v*v*this.weights[i];
   };
  };
  for(j=0;j<a.columns;j++)
  {
   if(a.rows<2)
   {
    b.std[j]=0;
   }
   else
   {
    b.std[j]=Math.sqrt(b.std[j]/(1-sws));
   };
  };
 }
 else
 {
  for(i=0;i<a.rows;i++)
  {
   for(j=0;j<a.columns;j++)
   {
    b.mean[j]+=a.m[i][j];
    b.std[j]+=a.m[i][j]*a.m[i][j];
   };
  };
  for(j=0;j<a.columns;j++)
  {
   if(a.rows<2)
   {
    b.std[j]=0;
   }
   else
   {
    b.std[j]=Math.sqrt(b.std[j]/(a.rows-1)-b.mean[j]*b.mean[j]/(a.rows*(a.rows-1)));
   };
   b.mean[j]/=a.rows;
  };
 };
 b.rows=a.rows;
 b.columns=a.columns;
 for(i=0;i<a.rows;i++)
 {
  for(j=0;j<a.columns;j++)
  {
   if(b.std[j]==0)
   {
    b.m[i][j]=(a.m[i][j]-b.mean[j]);
   }
   else
   {
    b.m[i][j]=(a.m[i][j]-b.mean[j])/b.std[j];
   };
  };
 };
 return b;
};
matrix.prototype.normalise_by=function(a,c)
{
 var b,i,j;
 if(c){b=this;}else{c=a;a=this;b=new matrix(this.rows,this.columns);};
 b.rows=a.rows;
 b.columns=a.columns;
 for(i=0;i<a.rows;i++)
 {
  for(j=0;j<a.columns;j++)
  {
   if(c.std[j]==0)
   {
    b.m[i][j]=(a.m[i][j]-c.mean[j]);
   }
   else
   {
    b.m[i][j]=(a.m[i][j]-c.mean[j])/c.std[j];
   };
  };
 };
 return b;
};
matrix.prototype.covariance=function(a)
{
 var b,i,j,k,sws=0;
 if(a){b=this;}else{a=this;b=new matrix(this.columns,this.columns);};
 b.rows=a.columns;
 b.columns=a.columns;
 if(a.rows < 2){return false;};
 if(a.weights)
 {
  for(i=0;i<a.rows;i++){sws+=a.weights[i]*a.weights[i];};
  for(i=0;i<b.rows;i++)
  {
   for(j=i;j<b.columns;j++)
   {
    for(k=0;k<a.rows;k++)
    {
     b.m[i][j]+=a.m[k][i]*a.m[k][j]*this.weights[k];
    };
   };
  };
  for(i=0;i<b.rows;i++)
  {
   for(j=i;j<b.columns;j++)
   {
    b.m[i][j]/=(1-sws);
    b.m[j][i]=b.m[i][j];
   };
  };
 }
 else
 {
  for(i=0;i<b.rows;i++)
  {
   for(j=i;j<b.columns;j++)
   {
    for(k=0;k<a.rows;k++)
    {
     b.m[i][j]+=a.m[k][i]*a.m[k][j];
    };
   };
  };
  for(i=0;i<b.rows;i++)
  {
   for(j=i;j<b.columns;j++)
   {
    b.m[i][j]/=(a.rows-1);
    b.m[j][i]=b.m[i][j];
   };
  };
 };
 return b;
};
matrix.prototype.min=function()
{
 var i,j;
 var lim=new Array();
 for(j=0;j<this.columns;j++)
 {
  lim[j]=this.m[0][j];
 };
 for(i=1;i<this.rows;i++)
 {
  for(j=0;j<this.columns;j++)
  {
   if(lim[j]>this.m[i][j])
   {
    lim[j]=this.m[i][j];
   };
  };  
 };
 this.mins=lim;
 return lim;
};
matrix.prototype.max=function()
{
 var i,j;
 var lim=new Array();
 for(j=0;j<this.columns;j++)
 {
  lim[j]=this.m[0][j];
 };
 for(i=1;i<this.rows;i++)
 {
  for(j=0;j<this.columns;j++)
  {
   if(lim[j]<this.m[i][j])
   {
    lim[j]=this.m[i][j];
   };
  };  
 };
 this.maxs=lim;
 return lim;
};
matrix.prototype.ave=function()
{
 var i,j;
 var lim=new Array();
 for(j=0;j<this.columns;j++)
 {
  lim[j]=0;
 };
 for(i=1;i<this.rows;i++)
 {
  for(j=0;j<this.columns;j++)
  {
   if(this.weights)
   {
    lim[j]+=this.m[i][j]*this.weights[i];
   }
   else
   {
    lim[j]+=this.m[i][j];
   };
  };  
 };
 for(j=0;j<this.columns;j++)
 {
  if(!this.weights)
  {
   lim[j]/=this.rows;
  };
 };
 this.aves=lim;
 return lim;
};