function update()
{
 var i;
 if(data.length)
 {
  if(data[0].desription)
  {
   for(i=0;i<data.length;i++)
   {
    data[i].link=data[i].desription;
    data[i].drillDown=undefined;
    data[i].desription=undefined;
   };
  };
 };
};
update();
var spec=new itemSpec("data","Links","Array");
spec.appendChild("location","","Text",true);
spec.appendChild("link","Link","Action",false).action="doLink(this.parent.object.location,this.object)";
