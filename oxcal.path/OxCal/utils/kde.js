function kde(rows,columns,content,weights)
{
 var i,j;
 this.data=new matrix(rows,columns,content,weights);
 if(rows)
 {
  this.delta=this.data.subtract_mean();
  this.cov=this.delta.covariance();
 };
 this.norm=1/Math.sqrt(2*Math.PI);
 this.max=0;
};
new kde(0,0);
kde.prototype.cvexp=function(k1,k2,x1,x2)
{
 return this.norm*Math.exp(-0.5*(x1*x2)/this.cov.m[k1][k2]);
};
kde.prototype.kde_1d_setup=function(k1)
{
 var d;
 this.h=Math.sqrt(this.cov.m[k1][k1]);
 d=1;
 this.h=this.h*Math.exp((1/(d+4))*Math.log(4/((d+2)*this.data.rows)));
 // using Scott 1992 as reported in Zhang et al 2004
};
kde.prototype.kde_1d=function(k1,x1)
{
 var i,p,pp;
 p=0;
 for(i=0;i<n;i++)
 {
  pp=this.norm*Math.exp(-0.5*((x1-this.data.m[i][k1])*(x1-this.data.m[i][k1]))/this.h);
  p+=pp/n;
 };
 return p;
};
kde.prototype.angle_2d=function(k1,k2)
{
 var f;
 if(this.cov.m[k1][k2]==0)
 {
  if(this.cov.m[k2][k2]>this.cov.m[k1][k1])
  {
   this.q=Math.PI/2;
  }
  else
  {
   this.q=0;
  };
 }
 else
 {
  if(this.cov.m[k1][k1]==this.cov.m[k2][k2])
  {
   if(this.cov.m[k][k2]>0)
   {
    this.q=Math.PI/4;
   }
   else
   {
    this.q=-Math.PI/4;
   };
  }
  else
  {
   this.q=0.5*Math.atan(2*this.cov.m[k1][k2]/(this.cov.m[k1][k1]-this.cov.m[k2][k2]));
   if(this.cov.m[k1][k2]>0)
   {
    if(this.cov.m[k2][k2]>this.cov.m[k1][k1])
    {
     this.q=Math.PI/2+this.q;
    };
   }
   else
   {
    if(this.cov.m[k2][k2]>this.cov.m[k1][k1])
    {
     this.q=-Math.PI/2+this.q;
    };
   };
  };
 };
 this.sinq=Math.sin(this.q);
 this.cosq=Math.cos(this.q);
 if(this.sinq==0)
 {
  this.su=this.cov.m[k1][k1];
  this.sv=this.cov.m[k2][k2];
 }
 else
 {
  if(this.cosq==0)
  {
   this.su=this.cov.m[k2][k2];
   this.sv=this.cov.m[k1][k1];
  }
  else
  {
   this.su=(this.cov.m[k1][k1]+this.cov.m[k2][k2]+this.cov.m[k1][k2]/(this.sinq*this.cosq))/2;
   this.sv=(this.cov.m[k1][k1]+this.cov.m[k2][k2]-this.cov.m[k1][k2]/(this.sinq*this.cosq))/2;
  };
 };
};
kde.prototype.kde_2d_setup=function(k1,k2)
{
 var d=2;
 this.angle_2d(k1,k2);
 f=Math.exp((1/(d+4))*Math.log(4/((d+2)*this.data.rows)));
 // using Scott 1992 as reported in Zhang et al 2004
 f=f*f;
 this.sv*=f;
 this.su*=f;
};
kde.prototype.ellipse_2d_setup=function(k1,k2)
{
 this.angle_2d(k1,k2);
 this.su=Math.sqrt(this.su);
 this.sv=Math.sqrt(this.sv);
};
kde.prototype.ellipse_2d=function(k1,k2,no,incr)
{
 var i,j,k,x,y,v;
 if(!incr){incr=100;};
 var contour=new matrix(2,0);
 for(k=0,j=0;(j<no)||(j==0);j++)
 {
  if(no==0)
  {
   v=2.447;
  }
  else
  {
   v=Math.sqrt(-2*Math.log((j+1)/(no+1)));
  };
  for(i=0;i<incr;i++)
  {
   x=this.su*v*Math.cos(i*2*Math.PI/incr);
   y=this.sv*v*Math.sin(i*2*Math.PI/incr);
   contour.m[0][k]=this.delta.mean[k1]+this.cosq*x-this.sinq*y;
   contour.m[1][k]=this.delta.mean[k2]+this.sinq*x+this.cosq*y;
   k++;
  };
  contour.m[0][k]=NaN;
  contour.m[1][k]=NaN;
  k++;
 };
 contour.rows=k;
 return contour;
};
kde.prototype.kde_2d=function(k1,k2,x1,x2,debug)
{
 var i,p,pp,u,v;
 p=0;
 for(i=0;i<this.data.rows;i++)
 {
  pp=1;
  u=this.cosq*(x1-this.data.m[i][k1])+this.sinq*(x2-this.data.m[i][k2]);
  v=-this.sinq*(x1-this.data.m[i][k1])+this.cosq*(x2-this.data.m[i][k2]);
  pp*=Math.exp(-0.5*(u*u)/this.su);
  pp*=Math.exp(-0.5*(v*v)/this.sv);
  if(this.data.weights)
  {
   p+=pp*this.data.weights[i];
  }
  else
  {
   p+=pp/this.data.rows;
  };
 };
 return p;
};
kde.prototype.contTest=function(a,b,t)
{
 if(a==t)
 {
  if(a==b){return 0.5;};
  return 0.0;
 };
 if(a==b){return -1.0;};
 return (t-a)/(b-a);
};
kde.prototype.addPoint=function(cm,i,j,min1,min2,incr1,incr2,p,hor)
{
 var x,y;
 if(!incr1)
 {
  x=NaN;y=NaN;
 }
 else
 {
  x=min1+i*incr1;
  y=min2+j*incr2;
  if(hor)
  {
   x+=p*incr1;
  }
  else
  {
   y+=p*incr2;
  };
 };
 cm.m[0].push(x);
 cm.m[1].push(y);
 cm.columns++;
};
kde.prototype.kde_contour_2d=function(k1,k2,no,gridsize)
{
 var test;
 var min1,max1,min2,max2;
 var incr1,incr2;
 var horiz=new Array;
 var vert=new Array;
 var grid=new Array;
 var vals=new Array;
 var i,j,k,ii,jj,v,pos,lost,tot,t;
 var contour=new matrix(2,0);
 this.data.min();
 this.data.max();
 min1=this.data.mins[k1]-2.447*(Math.abs(this.cosq)*Math.sqrt(this.su)+
   Math.abs(this.sinq)*Math.sqrt(this.sv));
 max1=this.data.maxs[k1]+2.447*(Math.abs(this.cosq)*Math.sqrt(this.su)+
   Math.abs(this.sinq)*Math.sqrt(this.sv));
 min2=this.data.mins[k2]-2.447*(Math.abs(this.sinq)*Math.sqrt(this.su)+
   Math.abs(this.cosq)*Math.sqrt(this.sv));
 max2=this.data.maxs[k2]+2.447*(Math.abs(this.sinq)*Math.sqrt(this.su)+
   Math.abs(this.cosq)*Math.sqrt(this.sv));
 incr1=(max1-min1)/gridsize;
 incr2=(max2-min2)/gridsize;
 if(!no){no=0;};
 if(!gridsize){gridsize=40;};
 v=0;tot=0;
 for(i=0;i<gridsize;i++)
 {
  grid[i]=new Array();
  vert[i]=new Array();
  horiz[i]=new Array();
  for(j=0;j<gridsize;j++)
  {
   grid[i][j]=this.kde_2d(k1,k2,(min1+i*incr1),(min2+j*incr2));
   if((i==30)&&(j==30))
   {
    grid[i][j]=this.kde_2d(k1,k2,(min1+i*incr1),(min2+j*incr2),true);
   };
   vals[v]=grid[i][j];
   tot+=vals[v];
   v++;
   if(grid[i][j]>this.max){this.max=grid[i][j];};
   vert[i][j]=-1;
   horiz[i][j]=-1;
  };
 };
 for(k=0;(k<no)||(k==0);k++)
 {
  if(no==0)
  {
   vals=vals.sort();
   test=0;
   for(t=0,i=0;i<v;i++)
   {
    t+=vals[i];
    if(t/tot>0.05) // 95% contour
    {
     test=(vals[i]+vals[i-1])/2;
     break;
    };
   };
  }
  else
  {
   test=(k+1)*this.max/no;
  };
  for(i=0;i<gridsize-1;i++)
  {
   for(j=0;j<gridsize-1;j++)
   {
    if(horiz[i][j]<k)
    {
     pos=this.contTest(grid[i][j],grid[i+1][j],test);
     if((pos>=0) && (pos<1))
     {
      this.addPoint(contour,i,j,min1,min2,incr1,incr2,pos,true);
      horiz[i][j]=k;
      ii=i;jj=j;
      do
      {
       lost=true;
       if((ii+1<gridsize)&&(horiz[ii][jj]<k))
       {
        pos=this.contTest(grid[ii][jj],grid[ii+1][jj],test);
        if((pos>=0) && (pos<1))
        {
         this.addPoint(contour,ii,jj,min1,min2,incr1,incr2,pos,true);
         horiz[ii][jj]=k;
         jj=jj-1;
         lost=(jj<0);
         continue;
        };
       };
       if(((jj+1)<gridsize)&&(vert[ii][jj]<k))
       {
        pos=this.contTest(grid[ii][jj],grid[ii][jj+1],test);
        if((pos>=0) && (pos<1))
        {
         this.addPoint(contour,ii,jj,min1,min2,incr1,incr2,pos,false);
         vert[ii][jj]=k;
         ii=ii-1;
         lost=(ii<0);
         continue;
        };
       };
       if((ii+1<gridsize)&&(jj+1<gridsize)&&(horiz[ii][jj+1]<k))
       {
        pos=this.contTest(grid[ii][jj+1],grid[ii+1][jj+1],test);
        if((pos>=0) && (pos<1))
        {
         this.addPoint(contour,ii,jj+1,min1,min2,incr1,incr2,pos,true);
         horiz[ii][jj+1]=k;
         jj=jj+1;
         lost=(jj+1>=gridsize);
         continue;
        };
       };
       if((ii+1<gridsize)&&(jj+1<gridsize)&&(vert[ii+1][jj]<k))
       {
        pos=this.contTest(grid[ii+1][jj],grid[ii+1][jj+1],test);
        if((pos>=0) && (pos<1))
        {
         this.addPoint(contour,ii+1,jj,min1,min2,incr1,incr2,pos,false);
         vert[ii+1][jj]=k;
         ii=ii+1;
         lost=(ii+1>=gridsize);
         continue;
        }
        else
        {
         if(pos==1)
         {
          ii=ii+1;
          jj=jj+1;
          lost=((ii+1>=gridsize)||(jj+1>=gridsize));
          continue;
         };
        };
       };
      }while(!lost);
      pos=this.contTest(grid[i][j],grid[i+1][j],test);
      this.addPoint(contour,i,j,min1,min2,incr1,incr2,pos,true);
      this.addPoint(contour);
     };
    };
   };
  };
 };
 return contour;
};
kde.prototype.pca_setup=function()
{
 this.dn=this.data.normalise();
 this.tr=this.dn.covariance().eigenvectors().sort_eigenvalues().transpose();
 this.pcadta=this.dn.transpose().times(this.tr).transpose().normalise();
 this.f=Math.exp((1/(this.data.columns+4))*Math.log(4/((this.data.columns+2)*this.data.rows)));
 // using Scott 1992 as reported in Zhang et al 2004
 this.ell_self=this.ellipsoid_prob(this.data).ave()[0];
 this.kde_self=this.kde_prob(this.data,true).ave()[0];
};
kde.prototype.pca_pos=function(m)
{
 return m.normalise_by(this.dn).transpose().times(this.tr).transpose().normalise_by(this.pcadta);
};
kde.prototype.ellipsoid_prob=function(m)
{
 var i,j;
 var b=new matrix(m.rows,1);
 m=this.pca_pos(m);
 for(i=0;i<m.rows;i++)
 {
  b.m[i][0]=0;
  for(j=0;j<m.columns;j++)
  {
   b.m[i][0]+=m.m[i][j]*m.m[i][j];
  };
  b.m[i][0]=Math.exp(-b.m[i][0]/2);
 };
 return b;
};
kde.prototype.ellipsoid_rel_prob=function(m)
{
 return this.ellipsoid_prob(m).ave()[0]/this.ell_self;
};
kde.prototype.kde_prob=function(m,self)
{
 var i,j,k,p,t;
 var b=new matrix(m.rows,1);
 m=this.pca_pos(m);
 for(i=0;i<m.rows;i++)
 {
  b.m[i][0]=1;
  for(j=0;j<m.columns;j++)
  {
   p=0;
   for(k=0;k<this.pcadta.rows;k++)
   {
    if(self && (k==i)){continue;};
    t=(m.m[i][j]-this.pcadta.m[k][j])/this.f;
    if(this.pcadta.weights)
    {
     p+=Math.exp(-t*t/2)*pcadta.weights[k];
    }
    else
    {
     p+=Math.exp(-t*t/2);
    };
   };
   if(self && (this.pcadta.rows>1))
   {
    p/=(this.pcadta.rows-1);
   }
   else
   {
    p/=(this.pcadta.rows);
   };
   b.m[i][0]*=p;
  };
 };
 return b;
};
kde.prototype.kde_rel_prob=function(m)
{
 return this.kde_prob(m,m==this.data).ave()[0]/this.kde_self;
};
